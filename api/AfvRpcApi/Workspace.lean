import AfvRpcApi.Types
import Std.Data.HashMap
import Lean.Parser.Module
import Lean.Elab.DeclModifiers
import Lean.Elab.Import
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

private def findWorkspaceRoot (path : FilePath) : IO (Option FilePath) := do
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

private def loadWorkspaceSearchPath (root : FilePath) : IO (Option System.SearchPath) := do
  let toolchainFile := root / "lean-toolchain"
  let mut prevToolchain? : Option String := none
  if ← toolchainFile.pathExists then
    prevToolchain? ← IO.getEnv "ELAN_TOOLCHAIN"
    let toolchain := (← IO.FS.readFile toolchainFile).trim
    if !toolchain.isEmpty then
      Std.Internal.IO.Async.System.setEnvVar "ELAN_TOOLCHAIN" toolchain
  let envSysroot? ← try
    let sysroot ← Lean.findSysroot
    let lean ← Lake.LeanInstall.get sysroot (collocated := true)
    let lake := Lake.LakeInstall.ofLean lean
    let elan? ← Lake.findElanInstall?
    let envResult ← Lake.Env.compute lake lean elan? |>.toIO'
    match envResult with
    | Except.ok env =>
      pure (some env)
    | Except.error err =>
      IO.eprintln s!"Error computing Lake environment via sysroot: {err}"
      pure none
  catch _ =>
    pure none
  let env? ← match envSysroot? with
  | some env =>
    pure (some env)
  | none =>
    let (elan?, lean?, lake?) ← Lake.findInstall?
    match lean?, lake? with
    | some lean, some lake =>
      let envResult ← Lake.Env.compute lake lean elan? |>.toIO'
      match envResult with
      | Except.ok env => pure (some env)
      | Except.error err =>
        IO.eprintln s!"Error computing Lake environment via install detection: {err}"
        pure none
    | _, _ =>
      IO.eprintln "Error: Unable to locate Lean/Lake installation"
      pure none
  match prevToolchain? with
  | some value => Std.Internal.IO.Async.System.setEnvVar "ELAN_TOOLCHAIN" value
  | none => Std.Internal.IO.Async.System.unsetEnvVar "ELAN_TOOLCHAIN"
  match env? with
  | none =>
    return none
  | some env =>
    let config : Lake.LoadConfig := {
      lakeEnv := env
      wsDir := root
    }
    let ws? ← Lake.loadWorkspace config |>.toBaseIO (minLv := .error) (ansiMode := .auto) (out := .stderr)
    match ws? with
    | none =>
      IO.eprintln s!"Error loading Lake workspace at {root}"
      return none
    | some ws =>
      return some ws.augmentedLeanPath

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

private def mkTheoremFromRange
    (inputCtx : Lean.Parser.InputContext)
    (filePath : String)
    (label : String)
    (range : String.Range) : IO Theorem := do
  let lspRange := Lean.FileMap.utf8RangeToLspRange inputCtx.fileMap range
  let id ← generateTheoremId filePath label lspRange.start.line
  return {
    id := id
    label := label
    path := filePath
    range := {
      startLine := lspRange.start.line
      startCharacter := lspRange.start.character
      endLine := lspRange.end.line
      endCharacter := lspRange.end.character
    }
  }

/-- Extract theorems from file content using Lean's parser. -/
def extractTheorems (filePath : String) (content : String) : IO (Array Theorem) := do
  let inputCtx := Lean.Parser.mkInputContext content filePath
  let theorems := #[]

  match ← findWorkspaceRoot (FilePath.mk filePath) with
  | some root =>
    let sp? ← loadWorkspaceSearchPath root
    match sp? with
    | some sp => Lean.searchPathRef.set sp
    | none =>
      IO.eprintln s!"Warning: Unable to load workspace search path for {root}"
  | none =>
    IO.eprintln s!"Warning: No lakefile.toml found for {filePath}"

  let (header, parserState, headerMessages) ← Lean.Parser.parseHeader inputCtx
  if headerMessages.hasErrors then
    headerMessages.forM fun msg => do
      let rendered ← msg.toString
      IO.eprintln s!"Warning: Failed to parse header for {filePath}: {rendered}"
    return theorems

  let opts := Lean.Elab.inServer.set {} true
  let opts := Lean.Elab.async.setIfNotSet opts true
  let (env, headerMessages) ← Lean.Elab.processHeader header opts headerMessages inputCtx
  let moduleStx ← Lean.Parser.testParseModule env filePath content
  let mut acc := theorems
  let cmds :=
    match moduleStx.raw.getArgs[1]? with
    | some cmdsStx => cmdsStx.getArgs
    | none => #[]

  for stx in cmds.toList do
    if stx.isOfKind ``Lean.Parser.Command.declaration then
      let decl := stx[1]
      if decl.getKind == ``Lean.Parser.Command.theorem then
        let declId := decl[1]
        let (name, _) := Lean.Elab.expandDeclIdCore declId
        if let some range := stx.getRangeWithTrailing? then
          let thm ← mkTheoremFromRange inputCtx filePath name.toString range
          acc := acc.push thm
    else if let some range := stx.getRangeWithTrailing? then
      let cmdText := content.extract range.start range.stop
      if let some label := commandTheoremLabel? cmdText then
        let thm ← mkTheoremFromRange inputCtx filePath label range
        acc := acc.push thm

  if headerMessages.hasErrors then
    headerMessages.forM fun msg => do
      let rendered ← msg.toString
      IO.eprintln s!"Warning: Parse issue in {filePath}: {rendered}"

  return acc

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
