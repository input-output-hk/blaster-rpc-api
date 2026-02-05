import AfvRpcApi.Types
import AfvRpcApi.State
import Std.Data.HashMap
import Std.Data.HashSet
import Lean.Data.Lsp
import Lean.Parser.Module
import Lean.Elab.Command
import Lean.Elab.Import
import Lean.Language.Lean
import Lake.Config.InstallPath
import Lake.Config.Env
import Lake.Load.Config
import Lake.Load.Workspace
import Lake.Util.Log
import Std.Internal.Async.System

namespace AfvRpcApi.TheoremRunner

open AfvRpcApi

/-! ## LSP-style Verification -/

open Lean
open Std (HashMap HashSet)
open System (FilePath)

private def toLspPos (pos : Position) : Lsp.Position :=
  let line := if pos.line == 0 then 0 else pos.line - 1
  { line := line, character := pos.column }

private def normalizeRange (r : Lsp.Range) : Lsp.Range :=
  let start := r.start
  let stop := r.end
  let isAfter :=
    start.line > stop.line || (start.line == stop.line && start.character > stop.character)
  if isAfter then
    { start := stop, «end» := start }
  else
    r

private def posLt (a b : Lsp.Position) : Bool :=
  a.line < b.line || (a.line == b.line && a.character < b.character)

private def posLe (a b : Lsp.Position) : Bool :=
  posLt a b || (a.line == b.line && a.character == b.character)

private def rangesIntersect (a b : Lsp.Range) : Bool :=
  let a := normalizeRange a
  let b := normalizeRange b
  let aBeforeB := posLe a.end b.start
  let bBeforeA := posLe b.end a.start
  !(aBeforeB || bBeforeA)

private def theoremRangeToLsp (r : Range) : Lsp.Range :=
  { start := { line := r.startLine, character := r.startCharacter }
    «end» := { line := r.endLine, character := r.endCharacter } }

private def messageRange (msg : Message) : Lsp.Range :=
  let start := toLspPos msg.pos
  let stop := toLspPos (msg.endPos.getD msg.pos)
  normalizeRange { start := start, «end» := stop }

private def diagnosticFromMessage (msg : Message) : IO (String × Json) := do
  let severity :=
    match msg.severity with
    | .error => "error"
    | .warning => "warning"
    | .information => "information"
  let range := messageRange msg
  let messageText ← msg.toString
  let key := s!"{severity}|{range.start.line}|{range.start.character}|{range.end.line}|{range.end.character}|{messageText}"
  let json := Json.mkObj [
    ("severity", Json.str severity),
    ("message", Json.str messageText),
    ("range", Json.mkObj [
      ("startLine", Json.num (range.start.line.succ)),
      ("startCharacter", Json.num range.start.character),
      ("endLine", Json.num (range.end.line.succ)),
      ("endCharacter", Json.num range.end.character)
    ])
  ]
  return (key, json)

private def collectFileMessages (path : String) : IO (Array Message) := do
  let content ← IO.FS.readFile path
  let inputCtx := Parser.mkInputContext content path
  let (header, parserState, headerMessages) ← Parser.parseHeader inputCtx
  let opts := Elab.inServer.set {} true
  let opts := Elab.async.setIfNotSet opts true
  let (env, headerMessages) ← Elab.processHeader header opts headerMessages inputCtx
  let cmdState := Elab.Command.mkState env headerMessages opts
  let task ← Language.Lean.processCommands inputCtx parserState cmdState
  let snap := task.get
  let messages := Language.toSnapshotTree snap
    |>.getAll.map (·.diagnostics.msgLog)
    |>.foldl (· ++ ·) {}
  return messages.reportedPlusUnreported.toArray

private def resolveWorkspaceRoot (workspaces : Array Workspace) (path : String) : Option String :=
  workspaces.foldl
    (fun best ws =>
      if path.startsWith ws.path then
        match best with
        | none => some ws.path
        | some current =>
          if ws.path.length > current.length then some ws.path else best
      else
        best)
    none

private def loadWorkspaceSearchPath (root : String) : IO (Option SearchPath) := do
  let toolchainFile := FilePath.mk root / "lean-toolchain"
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
      wsDir := FilePath.mk root
    }
    let ws? ← Lake.loadWorkspace config |>.toBaseIO (minLv := .error) (ansiMode := .auto) (out := .stderr)
    match ws? with
    | none =>
      IO.eprintln s!"Error loading Lake workspace at {root}"
      return none
    | some ws =>
      return some ws.augmentedLeanPath

private def getFileMessagesCached
    (cacheRef : IO.Ref (HashMap String (Array Message)))
    (path : String) : IO (Array Message) := do
  let cache ← cacheRef.get
  match cache.get? path with
  | some msgs =>
    return msgs
  | none =>
    let msgs ← collectFileMessages path
    cacheRef.modify (·.insert path msgs)
    return msgs

/-- Run verification for a single theorem.
    Updates state and invokes callback with result when complete. -/
def runTheoremVerificationWithCache
    (state : State.StateRef)
    (cacheRef : IO.Ref (HashMap String (Array Message)))
    (theoremId : String)
    (onComplete : TheoremResult → IO Unit) : IO Unit := do
  -- Get the theorem from state
  match ← State.getResult state theoremId with
  | none =>
    IO.eprintln s!"Error: Theorem not found: {theoremId}"
    return
  | some result =>
    -- Set status to running
    State.updateTheoremStatus state theoremId .running

    let startTime ← IO.monoMsNow
    let (status, details) ← try
      let path := result.«theorem».path
      let workspaces ← State.getWorkspaces state
      let workspaceRoot? := resolveWorkspaceRoot workspaces path
      let searchPath? ← match workspaceRoot? with
        | none =>
          pure none
        | some root =>
          match ← State.getWorkspaceSearchPath state root with
          | some sp =>
            pure (some sp)
          | none =>
            let sp? ← loadWorkspaceSearchPath root
            match sp? with
            | some sp =>
              State.setWorkspaceSearchPath state root sp
              pure (some sp)
            | none =>
              pure none
      match searchPath? with
      | some sp =>
        Lean.searchPathRef.set sp
      | none =>
        IO.eprintln s!"Warning: No workspace context for {path}"
      let theoremRange := theoremRangeToLsp result.«theorem».range
      let msgs ← getFileMessagesCached cacheRef path
      let relevantMsgs := msgs.filter fun msg =>
        msg.fileName == path && rangesIntersect theoremRange (messageRange msg)
      let hasError := relevantMsgs.any (·.severity == .error)
      let mut seen : HashSet String := {}
      let mut diagnosticArr : Array Json := #[]
      for msg in relevantMsgs do
        let (key, diagnostic) ← diagnosticFromMessage msg
        if !seen.contains key then
          seen := seen.insert key
          diagnosticArr := diagnosticArr.push diagnostic
      let diagnostics := Json.arr diagnosticArr
      let details := Json.mkObj [("diagnostics", diagnostics)]
      let status := if hasError then TheoremStatus.invalid else TheoremStatus.valid
      pure (status, details)
    catch e =>
      IO.eprintln s!"Error processing theorem {theoremId}: {e}"
      let details := Json.mkObj [("diagnostics", Json.arr #[])]
      pure (TheoremStatus.invalid, details)
    let endTime ← IO.monoMsNow
    let elapsed := endTime - startTime

    -- Update result with final status and time
    State.updateTheoremStatus state theoremId status (some elapsed) (some details)

    -- Get the updated result and invoke callback
    match ← State.getResult state theoremId with
    | some finalResult => onComplete finalResult
    | none => IO.eprintln s!"Error: Theorem disappeared: {theoremId}"

/-- Run verification for multiple theorems sequentially. -/
def runTheoremsSequentially
    (state : State.StateRef)
    (theoremIds : Array String)
    (onComplete : TheoremResult → IO Unit) : IO Unit := do
  let cacheRef ← IO.mkRef ({} : HashMap String (Array Message))
  for theoremId in theoremIds do
    runTheoremVerificationWithCache state cacheRef theoremId onComplete

end AfvRpcApi.TheoremRunner
