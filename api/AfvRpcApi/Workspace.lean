import AfvRpcApi.Types
import Std.Data.HashMap
import Lean.Parser.Module
import Lean.Elab.DeclModifiers
import Lean.Data.Lsp.Utf16

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

/-- Extract theorems from file content using Lean's parser. -/
def extractTheorems (filePath : String) (content : String) : IO (Array Theorem) := do
  let inputCtx := Lean.Parser.mkInputContext content filePath
  let theorems := #[]

  let (_, parserState, headerMessages) ← Lean.Parser.parseHeader inputCtx
  if headerMessages.hasErrors then
    headerMessages.forM fun msg => do
      let rendered ← msg.toString
      IO.eprintln s!"Warning: Failed to parse header for {filePath}: {rendered}"
    return theorems

  let env ← Lean.mkEmptyEnvironment
  let pmctx : Lean.Parser.ParserModuleContext := { env := env, options := {} }

  let mut state := parserState
  let mut messages := headerMessages
  let mut acc := theorems
  let mut done := false

  while !done do
    let (stx, state', messages') := Lean.Parser.parseCommand inputCtx pmctx state messages
    state := state'
    messages := messages'

    if stx.isOfKind ``Lean.Parser.Command.declaration then
      let decl := stx[1]
      if decl.getKind == ``Lean.Parser.Command.theorem then
        let declId := decl[1]
        let (name, _) := Lean.Elab.expandDeclIdCore declId
        if let some range := decl.getRange? then
          let lspRange := Lean.FileMap.utf8RangeToLspRange inputCtx.fileMap range
          let id ← generateTheoremId filePath name.toString lspRange.start.line
          let thm : Theorem := {
            id := id
            label := name.toString
            path := filePath
            range := {
              startLine := lspRange.start.line
              startCharacter := lspRange.start.character
              endLine := lspRange.end.line
              endCharacter := lspRange.end.character
            }
          }
          acc := acc.push thm

    if Lean.Parser.isTerminalCommand stx then
      done := true

  let theorems := acc

  if messages.hasErrors then
    messages.forM fun msg => do
      let rendered ← msg.toString
      IO.eprintln s!"Warning: Parse issue in {filePath}: {rendered}"

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
