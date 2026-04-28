import AfvRpcApi.Types
import Std.Data.HashMap
import Lake.Config.InstallPath
import Lake.Config.Env
import Lake.Load.Config
import Lake.Load.Workspace
import Lake.Util.Log
import Std.Internal.Async.System
import Lean.Data.Lsp.Utf16

namespace AfvRpcApi.Workspace

open AfvRpcApi
open System (FilePath)

def findWorkspaceRoot (path : FilePath) : IO (Option FilePath) := do
  let mut current :=
    if (← path.isDir) then path else path.parent.getD path
  let root := current
  let mut done := false
  while !done do
    let candidate := current / "lakefile.toml"
    if ← candidate.pathExists then
      return some current
    let parent := current.parent.getD current
    if parent == current || current == root.parent.getD root then
      done := true
    current := parent
  return none

def findWorkspaceSysroot (root : FilePath) : IO (Option FilePath) := do
  -- Read lean-toolchain to get the workspace's toolchain version
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

def loadWorkspaceSearchPath (root : FilePath) : IO (Option System.SearchPath) := do
  let lakeDir := root / ".lake"
  let mut paths : System.SearchPath := []

  -- Add Lean toolchain stdlib matching the workspace's toolchain version
  if let some sysroot ← findWorkspaceSysroot root then
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

/-- Global counter for generating unique theorem IDs. -/
initialize idCounterRef : IO.Ref Nat ← IO.mkRef 0

/-- Generate a unique theorem ID based on file path, label, and line number. -/
def generateTheoremId (filePath : String) (label : String) (line : Nat) : IO String := do
  let counter ← idCounterRef.get
  idCounterRef.modify (· + 1)
  -- Create a simple hash-like ID
  let hash := filePath.hash.toNat + label.hash.toNat + line + counter
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

private def commandTheoremLabel? (cmdText : String) : Option String :=
  let label := trimCommandLabel cmdText
  if label.startsWith "#bmc" || label.startsWith "#kind" then
    some label
  else
    none

private def theoremNameFromLine? (line : String) : Option String :=
  let trimmed := line.trimLeft
  if !trimmed.startsWith "theorem " then
    none
  else
    let rest := trimmed.drop 8
    let name :=
      rest.takeWhile fun c =>
        c.isAlphanum || c == '_' || c == '\''
    if name.isEmpty then none else some name

private def mkTheoremFromLine
    (filePath : String)
    (label : String)
    (lineIdx : Nat)
    (lineText : String) : IO Theorem := do
  let id ← generateTheoremId filePath label lineIdx
  return {
    id := id
    label := label
    path := filePath
    range := {
      startLine := lineIdx
      startCharacter := 0
      endLine := lineIdx
      endCharacter := lineText.length
    }
  }

private def extractTheoremsFallback (filePath : String) (content : String) : IO (Array Theorem) := do
  let mut acc := #[]
  let lines := content.splitOn "\n"
  for h : i in [:lines.length] do
    let line := lines[i]
    if let some label := theoremNameFromLine? line then
      let thm ← mkTheoremFromLine filePath label i line
      acc := acc.push thm
    else if let some label := commandTheoremLabel? line then
      let thm ← mkTheoremFromLine filePath label i line
      acc := acc.push thm
  return acc

/-- Extract theorems from file content using line-based scanning. -/
def extractTheorems (filePath : String) (content : String) : IO (Array Theorem) := do
  extractTheoremsFallback filePath content

/-- Scan a single .lean file for theorems.
    Returns None if the file cannot be read or is not a valid UTF-8 file. -/
def scanFile (path : String) : IO (Option Node) := do
  try
    -- Read file content
    let content ← IO.FS.readFile path

    -- Extract theorems
    let theorems ← extractTheorems path content

    -- Only include files with theorems
    if theorems.isEmpty then
      return none

    let fileName := (FilePath.mk path).fileName.getD path
    return some (Node.file path fileName theorems)
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
        if entry.path.fileName == some ".lake" then
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

/-- Scan a workspace directory.
    Returns None if the workspace cannot be read or contains no theorems. -/
def scanWorkspace (path : String) : IO (Option Workspace) := do
  try
    let folderPath := FilePath.mk path

    -- Check if path exists and is a directory
    if !(← folderPath.pathExists) then
      IO.eprintln s!"Warning: Workspace path does not exist: {path}"
      return none

    if !(← folderPath.isDir) then
      IO.eprintln s!"Warning: Workspace path is not a directory: {path}"
      return none

    -- Read directory entries
    let entries ← folderPath.readDir
    let mut children := #[]

    for entry in entries do
      let entryPath := entry.path.toString

      if ← entry.path.isDir then
        if entry.path.fileName == some ".lake" then
          continue
        -- Recursively scan subdirectory
        if let some childNode ← scanFolder entryPath then
          children := children.push childNode
      else
        -- Check if it's a .lean file
        if entry.path.extension == some "lean" then
          if let some fileNode ← scanFile entryPath then
            children := children.push fileNode

    -- Only include workspace if it has children
    if children.isEmpty then
      IO.eprintln s!"Warning: Workspace contains no theorems: {path}"
      return none

    let workspaceName := folderPath.fileName.getD path
    return some {
      path := path
      label := workspaceName
      children := children
    }
  catch e =>
    IO.eprintln s!"Warning: Failed to scan workspace {path}: {e}"
    return none

/-- Scan multiple workspace paths, filtering out failures. -/
def scanWorkspaces (paths : Array String) : IO (Array Workspace) := do
  let mut workspaces := #[]

  for path in paths do
    if let some workspace ← scanWorkspace path then
      workspaces := workspaces.push workspace

  return workspaces

end AfvRpcApi.Workspace
