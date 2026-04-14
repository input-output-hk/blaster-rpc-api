import AfvRpcApi.Types
import AfvRpcApi.State
import AfvRpcApi.Workspace
import Std.Data.HashMap
import Std.Data.HashSet
import Lean.Data.Lsp
import Lean.Data.Json

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
  let aBeforeB := posLt a.end b.start
  let bBeforeA := posLt b.end a.start
  !(aBeforeB || bBeforeA)

private def theoremRangeToLsp (r : Range) : Lsp.Range :=
  { start := { line := r.startLine, character := r.startCharacter }
    «end» := { line := r.endLine, character := r.endCharacter } }

private structure LeanJsonPos where
  line : Nat
  column : Nat
  deriving FromJson

private structure DiagnosticEntry where
  fileName : String
  severity : String
  message : String
  range : Lsp.Range

private def diagnosticFromEntry (diag : DiagnosticEntry) : String × Json :=
  let key := s!"{diag.severity}|{diag.range.start.line}|{diag.range.start.character}|{diag.range.end.line}|{diag.range.end.character}|{diag.message}"
  let json := Json.mkObj [
    ("severity", Json.str diag.severity),
    ("message", Json.str diag.message),
    ("range", Json.mkObj [
      ("startLine", Json.num (diag.range.start.line.succ)),
      ("startCharacter", Json.num diag.range.start.character),
      ("endLine", Json.num (diag.range.end.line.succ)),
      ("endCharacter", Json.num diag.range.end.character)
    ])
  ]
  (key, json)

private def parseLeanJsonDiagnosticLine? (line : String) : Option DiagnosticEntry := do
  let json <- (Json.parse line).toOption
  let fileName : String <- (json.getObjValAs? String "fileName").toOption
  let severity : String <- (json.getObjValAs? String "severity").toOption
  let message : String <- (json.getObjValAs? String "data").toOption
  let pos : LeanJsonPos <- (json.getObjValAs? LeanJsonPos "pos").toOption
  let endPos : Option LeanJsonPos :=
    match json.getObjVal? "endPos" with
    | Except.ok Json.null => none
    | Except.ok endJson => (fromJson? endJson).toOption
    | Except.error _ => none
  let startLine := if pos.line == 0 then 0 else pos.line - 1
  let endRaw := endPos.getD pos
  let endLine := if endRaw.line == 0 then 0 else endRaw.line - 1
  let range : Lsp.Range := {
    start := { line := startLine, character := pos.column }
    «end» := { line := endLine, character := endRaw.column }
  }
  some {
    fileName := fileName
    severity := severity
    message := message
    range := normalizeRange range
  }

private def rewriteBySorryInTheorem (path : String) (theoremRange : Range) : IO String := do
  let _ := theoremRange
  IO.FS.readFile path

private def collectFileDiagnosticsForTheorem (path : String) (theoremRange : Range) : IO (Array DiagnosticEntry) := do
  let content ← rewriteBySorryInTheorem path theoremRange
  let root ←
    match ← Workspace.findWorkspaceRoot (FilePath.mk path) with
    | some r => pure r.toString
    | none =>
      let parent := (FilePath.mk path).parent.getD (FilePath.mk path)
      pure parent.toString
  let tmpName :=
    let stem := (FilePath.mk path).fileStem.getD "AfvTmp"
    s!"{stem}.afvtmp.lean"
  let originalDir := (FilePath.mk path).parent.getD (FilePath.mk root)
  let tmpPath := originalDir / tmpName
  IO.FS.writeFile tmpPath content
  let out ←
    try
      IO.Process.output {
        cmd := "lake"
        args := #["env", "lean", "--json", tmpPath.toString]
        cwd := some (FilePath.mk root)
      }
    finally
      try
        IO.FS.removeFile tmpPath
      catch _ =>
        pure ()
  let combinedOutput :=
    if out.stderr.isEmpty then out.stdout else out.stdout ++ "\n" ++ out.stderr
  let mut diags : Array DiagnosticEntry := #[]
  for line in combinedOutput.splitOn "\n" do
    if let some diag := parseLeanJsonDiagnosticLine? line then
      diags := diags.push { diag with fileName := path }
  if diags.isEmpty then
    let fallbackRange := theoremRangeToLsp theoremRange
    for line in combinedOutput.splitOn "\n" do
      let trimmed := line.trim
      if !trimmed.isEmpty then
        let severity :=
          if trimmed.contains '"' && trimmed.contains 'e' then
            if trimmed.startsWith "{" then
              match (Json.parse trimmed).toOption with
              | some j =>
                match (j.getObjValAs? String "severity").toOption with
                | some s => s
                | none => "information"
              | none => "information"
            else "information"
          else "information"
        diags := diags.push {
          fileName := path
          severity := severity
          message := trimmed
          range := fallbackRange
        }
  return diags

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

private def loadWorkspaceSearchPath (root : String) : IO (Option SearchPath) :=
  Workspace.loadWorkspaceSearchPath (FilePath.mk root)

private def ensureToolchainSearchPath (path : String) : IO Unit := do
  if let some root ← Workspace.findWorkspaceRoot (FilePath.mk path) then
    if let some sysroot ← Workspace.findWorkspaceSysroot root then
      Lean.initSearchPath sysroot

private def getFileDiagnosticsCached
    (cacheRef : IO.Ref (HashMap String (Array DiagnosticEntry)))
    (path : String)
    (theoremRange : Range) : IO (Array DiagnosticEntry) := do
  let cacheKey :=
    s!"{path}::{theoremRange.startLine}:{theoremRange.startCharacter}-{theoremRange.endLine}:{theoremRange.endCharacter}"
  let cache ← cacheRef.get
  match cache.get? cacheKey with
  | some diags =>
    return diags
  | none =>
    let diags ← collectFileDiagnosticsForTheorem path theoremRange
    cacheRef.modify (·.insert cacheKey diags)
    return diags

private def findNextTheoremStartLine
    (results : HashMap String TheoremResult)
    (path : String)
    (startLine : Nat) : Option Nat :=
  results.toList.foldl
    (fun best (_, r) =>
      if r.theorem.path == path && r.theorem.range.startLine > startLine then
        match best with
        | none => some r.theorem.range.startLine
        | some current => some (Nat.min current r.theorem.range.startLine)
      else
        best)
    none

private def diagnosticBelongsToTheorem
    (theoremRange : Lsp.Range)
    (nextStartLine? : Option Nat)
    (diagRange : Lsp.Range) : Bool :=
  if rangesIntersect theoremRange diagRange then
    true
  else
    let line := diagRange.start.line
    let afterStart := line >= theoremRange.start.line
    let beforeNext :=
      match nextStartLine? with
      | some nextLine => line < nextLine
      | none => true
    afterStart && beforeNext

/-- Run verification for a single theorem.
    Updates state and invokes callback with result when complete. -/
def runTheoremVerificationWithCache
    (state : State.StateRef)
    (cacheRef : IO.Ref (HashMap String (Array DiagnosticEntry)))
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
      ensureToolchainSearchPath path
      let pathReal ← IO.FS.realPath path
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
        let current ← Lean.searchPathRef.get
        Lean.searchPathRef.set (current ++ sp)
      | none =>
        IO.eprintln s!"Warning: No workspace context for {path}"
      let theoremRange := theoremRangeToLsp result.«theorem».range
      let allResults ← State.getAllResults state
      let nextStartLine? := findNextTheoremStartLine allResults path result.theorem.range.startLine
      let diags ← getFileDiagnosticsCached cacheRef path result.theorem.range
      let mut relevantDiags : Array DiagnosticEntry := #[]
      for diag in diags do
        let msgPathReal ← IO.FS.realPath diag.fileName
        if msgPathReal == pathReal && diagnosticBelongsToTheorem theoremRange nextStartLine? diag.range then
          relevantDiags := relevantDiags.push diag
      let hasError := relevantDiags.any (·.severity == "error")
      let mut seen : HashSet String := {}
      let mut diagnosticArr : Array Json := #[]
      for diag in relevantDiags do
        let (key, diagnostic) := diagnosticFromEntry diag
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
  let cacheRef ← IO.mkRef ({} : HashMap String (Array DiagnosticEntry))
  for theoremId in theoremIds do
    runTheoremVerificationWithCache state cacheRef theoremId onComplete

end AfvRpcApi.TheoremRunner
