import AfvRpcApi.Types
import Std.Data.HashMap

namespace AfvRpcApi.Workspace

open AfvRpcApi
open System (FilePath)

/-- Global counter for generating unique theorem IDs. -/
initialize idCounterRef : IO.Ref Nat ← IO.mkRef 0

/-- Generate a unique theorem ID based on file path, label, and line number. -/
def generateTheoremId (filePath : String) (label : String) (line : Nat) : IO String := do
  let counter ← idCounterRef.get
  idCounterRef.modify (· + 1)
  -- Create a simple hash-like ID
  let hash := filePath.hash.toNat + label.hash.toNat + line + counter
  return s!"{hash}"

/-- Extract theorems from file content using simple regex matching. -/
def extractTheorems (filePath : String) (content : String) : IO (Array Theorem) := do
  let lines := content.splitOn "\n"
  let mut theorems := #[]

  for idx in [:lines.length] do
    let line := lines[idx]!
    -- Simple pattern matching: lines starting with "theorem "
    if line.trimLeft.startsWith "theorem " then
      -- Extract theorem name (word after "theorem ")
      let afterTheorem := (line.trimLeft.drop 8).trim
      let name := afterTheorem.takeWhile (fun c => c.isAlphanum || c == '_' || c == '\'')

      if !name.isEmpty then
        let id ← generateTheoremId filePath name idx
        let thm : Theorem := {
          id := id
          label := name
          path := filePath
          range := {
            startLine := idx
            startCharacter := 0
            endLine := idx
            endCharacter := line.length
          }
        }
        theorems := theorems.push thm

  return theorems

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
