import BlasterRpcApi.Types
import Std.Data.HashMap
import Lake.Config.InstallPath
import Lake.Config.Env
import Lake.Load.Config
import Lake.Load.Workspace
import Lake.Util.Log
import Std.Internal.Async.System
import Lean.Data.Lsp.Utf16

namespace BlasterRpcApi.TestTree

open BlasterRpcApi
open System (FilePath)

def findTestTreeRoot (path : FilePath) : IO (Option FilePath) := do
  let mut current :=
    if (← path.isDir) then path else path.parent.getD path
  let mut done := false
  while !done do
    if (← (current / "lakefile.toml").pathExists) || (← (current / "lakefile.lean").pathExists) then
      return some current
    let parent := current.parent.getD current
    if parent == current then
      done := true
    current := parent
  return none

def findTestTreeSysroot (root : FilePath) : IO (Option FilePath) := do
  -- Read lean-toolchain to get the test tree's toolchain version
  let toolchainFile := root / "lean-toolchain"
  if ← toolchainFile.pathExists then
    let toolchain := (← IO.FS.readFile toolchainFile).trim
    -- Convert "leanprover/lean4:v4.24.0" -> "leanprover--lean4---v4.24.0"
    let dirName := toolchain.replace "/" "--" |>.replace ":" "---"
    let elanHome ←
      if let some h ← IO.getEnv "ELAN_HOME" then pure (FilePath.mk h)
      else if let some h ← IO.getEnv "USERPROFILE" then pure (FilePath.mk h / ".elan")
      else if let some h ← IO.getEnv "HOME" then pure (FilePath.mk h / ".elan")
      else return none
    let toolchainPath := elanHome / "toolchains" / dirName
    if ← toolchainPath.pathExists then
      return some toolchainPath
  -- Fall back to current binary's sysroot
  try return some (← Lean.findSysroot)
  catch _ => return none

def loadTestTreeSearchPath (root : FilePath) : IO (Option System.SearchPath) := do
  let lakeDir := root / ".lake"
  let mut paths : System.SearchPath := []

  -- Add Lean toolchain stdlib matching the test tree's toolchain version
  if let some sysroot ← findTestTreeSysroot root then
    Lean.initSearchPath sysroot
    let sysrootLib := sysroot / "lib" / "lean"
    if ← sysrootLib.pathExists then
      paths := paths.append [sysrootLib.toString]

  -- Add main project build output
  let buildLib := lakeDir / "build" / "lib" / "lean"
  if ← buildLib.pathExists then
    paths := paths.append [buildLib.toString]
  else
    IO.eprintln s!"Warning: {buildLib} not found. Run 'lake build' in {root} first."
    return none

  -- Add all package build outputs
  let packagesDir := lakeDir / "packages"
  if ← packagesDir.pathExists then
    for entry in ← packagesDir.readDir do
      let pkgBuildLib := entry.path / ".lake" / "build" / "lib" / "lean"
      if ← pkgBuildLib.pathExists then
        paths := paths.append [pkgBuildLib.toString]

  return some paths

/-- Generate a unique test ID based on file path, label, and line number. -/
def generateTestId (filePath : String) (label : String) (line : Nat) : IO String := do
  let hash := s!"{filePath}:{label}:{line}".hash
  return s!"{hash}"

private def trimCommandLabel (cmdText : String) : String :=
  let firstLine :=
    match cmdText.splitOn "\n" with
    | line :: _ => line
    | [] => cmdText
  let withoutComment :=
    match firstLine.splitOn "--" with
    | line :: _ => line
    | [] => firstLine
  withoutComment.trim

private def commandTestLabel? (cmdText : String) : Option String :=
  let label := trimCommandLabel cmdText
  if label.startsWith "#bmc" || label.startsWith "#kind" then
    some label
  else
    none

private def testNameFromLine? (line : String) : Option String :=
  let trimmed := line.trimLeft
  if !trimmed.startsWith "theorem " then
    none
  else
    let rest := trimmed.drop 8
    let name :=
      rest.takeWhile fun c =>
        c.isAlphanum || c == '_' || c == '\''
    if name.isEmpty then none else some name

private def mkTestFromLine
    (filePath : String)
    (label : String)
    (lineIdx : Nat)
    (lineText : String) : IO Test := do
  let id ← generateTestId filePath label lineIdx
  return {
    id := id
    label := label
    path := filePath
    range := {
      startLine := lineIdx
      startCharacter := 0
      endLine := lineIdx
      endCharacter := lineText.utf16Length
    }
  }

private def extractTestsFallback (filePath : String) (content : String) : IO (Array Test) := do
  let mut acc := #[]
  let lines := content.splitOn "\n"
  for h : i in [:lines.length] do
    let line := lines[i]
    if let some label := testNameFromLine? line then
      let test ← mkTestFromLine filePath label i line
      acc := acc.push test
    else if let some label := commandTestLabel? line then
      let test ← mkTestFromLine filePath label i line
      acc := acc.push test
  return acc

/-- Extract tests from file content using line-based scanning. -/
def extractTests (filePath : String) (content : String) : IO (Array Test) := do
  extractTestsFallback filePath content

/-- Scan a single .lean file for tests.
    Returns None if the file cannot be read or is not a valid UTF-8 file. -/
def scanFile (path : String) : IO (Option Node) := do
  if path.endsWith ".blastertmp.lean" then return none
  try
    -- Read file content
    let content ← IO.FS.readFile path

    -- Extract tests
    let tests ← extractTests path content

    -- Only include files with tests
    if tests.isEmpty then
      return none

    let fileName := (FilePath.mk path).fileName.getD path
    return some (Node.file path fileName tests)
  catch e =>
    IO.eprintln s!"Warning: Failed to scan file {path}: {e}"
    return none

/-- Recursively scan a folder for .lean files.
    Returns None if the folder cannot be read. -/
partial def scanFolder (path : String) : IO (Option Node) := do
  try
    let folderPath := FilePath.mk path

    -- Read directory entries
    let entries ← folderPath.readDir
    let mut children := #[]

    for entry in entries do
      let entryPath := entry.path.toString

      if ← entry.path.isDir then
        if entry.path.fileName == some ".lake" || entry.path.fileName == some ".git" || entry.path.fileName == some "node_modules" then
          continue
        -- Recursively scan subdirectory
        if let some childNode ← scanFolder entryPath then
          children := children.push childNode
      else
        -- Check if it's a .lean file
        if entry.path.extension == some "lean" then
          if let some fileNode ← scanFile entryPath then
            children := children.push fileNode

    -- Only include folders with children
    if children.isEmpty then
      return none

    let folderName := folderPath.fileName.getD path
    return some (Node.folder path folderName children)
  catch e =>
    IO.eprintln s!"Warning: Failed to scan folder {path}: {e}"
    return none

/-- Scan a test tree directory.
    Returns None if the test tree cannot be read or contains no tests. -/
def scanTestTree (path : String) : IO (Option TestTree) := do
  try
    let folderPath := FilePath.mk path

    -- Check if path exists and is a directory
    if !(← folderPath.pathExists) then
      IO.eprintln s!"Warning: Test tree path does not exist: {path}"
      return none

    if !(← folderPath.isDir) then
      IO.eprintln s!"Warning: Test tree path is not a directory: {path}"
      return none

    -- Read directory entries
    let entries ← folderPath.readDir
    let mut children := #[]

    for entry in entries do
      let entryPath := entry.path.toString

      if ← entry.path.isDir then
        if entry.path.fileName == some ".lake" || entry.path.fileName == some ".git" || entry.path.fileName == some "node_modules" then
          continue
        -- Recursively scan subdirectory
        if let some childNode ← scanFolder entryPath then
          children := children.push childNode
      else
        -- Check if it's a .lean file
        if entry.path.extension == some "lean" then
          if let some fileNode ← scanFile entryPath then
            children := children.push fileNode

    -- Only include test tree if it has children
    if children.isEmpty then
      IO.eprintln s!"Warning: Test tree contains no tests: {path}"
      return none

    let testTreeName := folderPath.fileName.getD path
    return some {
      path := path
      label := testTreeName
      children := children
    }
  catch e =>
    IO.eprintln s!"Warning: Failed to scan test tree {path}: {e}"
    return none

/-- Scan multiple test tree paths, filtering out failures. -/
def scanTestTrees (paths : Array String) : IO (Array TestTree) := do
  let mut testTrees := #[]

  for path in paths do
    if let some testTree ← scanTestTree path then
      testTrees := testTrees.push testTree

  return testTrees

end BlasterRpcApi.TestTree
