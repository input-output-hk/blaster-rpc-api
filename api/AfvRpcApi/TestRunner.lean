import AfvRpcApi.Types
import AfvRpcApi.State
import AfvRpcApi.TestTree
import Std.Data.HashMap
import Std.Data.HashSet
import Lean.Data.Lsp
import Lean.Data.Json

namespace AfvRpcApi.TestRunner

open AfvRpcApi

/-! ## LSP-style Verification -/

open Lean
open Std (HashMap HashSet)
open System (FilePath)

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

private def rangesIntersect (a b : Lsp.Range) : Bool :=
  let a := normalizeRange a
  let b := normalizeRange b
  let aBeforeB := posLt a.end b.start
  let bBeforeA := posLt b.end a.start
  !(aBeforeB || bBeforeA)

private def testRangeToLsp (r : Range) : Lsp.Range :=
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

private def rewriteBySorryInTest (path : String) (testRange : Range) : IO String := do
  let _ := testRange
  IO.FS.readFile path

private def collectFileDiagnosticsForTest (path : String) (testRange : Range) : IO (Array DiagnosticEntry) := do
  let content ← rewriteBySorryInTest path testRange
  let root ←
    match ← TestTree.findTestTreeRoot (FilePath.mk path) with
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
    let fallbackRange := testRangeToLsp testRange
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

private def resolveTestTreeRoot (testTrees : Array TestTree) (path : String) : Option String :=
  testTrees.foldl
    (fun best tree =>
      if path.startsWith tree.path then
        match best with
        | none => some tree.path
        | some current =>
          if tree.path.length > current.length then some tree.path else best
      else
        best)
    none

private def loadTestTreeSearchPath (root : String) : IO (Option SearchPath) :=
  TestTree.loadTestTreeSearchPath (FilePath.mk root)

private def ensureToolchainSearchPath (path : String) : IO Unit := do
  if let some root ← TestTree.findTestTreeRoot (FilePath.mk path) then
    if let some sysroot ← TestTree.findTestTreeSysroot root then
      Lean.initSearchPath sysroot

private def getFileDiagnosticsCached
    (cacheRef : IO.Ref (HashMap String (Array DiagnosticEntry)))
    (path : String)
    (testRange : Range) : IO (Array DiagnosticEntry) := do
  let cacheKey :=
    s!"{path}::{testRange.startLine}:{testRange.startCharacter}-{testRange.endLine}:{testRange.endCharacter}"
  let cache ← cacheRef.get
  match cache.get? cacheKey with
  | some diags =>
    return diags
  | none =>
    let diags ← collectFileDiagnosticsForTest path testRange
    cacheRef.modify (·.insert cacheKey diags)
    return diags

private def findNextTestStartLine
    (results : HashMap String TestResult)
    (path : String)
    (startLine : Nat) : Option Nat :=
  results.toList.foldl
    (fun best (_, r) =>
      if r.test.path == path && r.test.range.startLine > startLine then
        match best with
        | none => some r.test.range.startLine
        | some current => some (Nat.min current r.test.range.startLine)
      else
        best)
    none

private def diagnosticBelongsToTest
    (testRange : Lsp.Range)
    (nextStartLine? : Option Nat)
    (diagRange : Lsp.Range) : Bool :=
  if rangesIntersect testRange diagRange then
    true
  else
    let line := diagRange.start.line
    let afterStart := line >= testRange.start.line
    let beforeNext :=
      match nextStartLine? with
      | some nextLine => line < nextLine
      | none => true
    afterStart && beforeNext

/-- Run verification for a single test.
    Updates state and invokes callback with result when complete. -/
def runTestVerificationWithCache
    (state : State.StateRef)
    (cacheRef : IO.Ref (HashMap String (Array DiagnosticEntry)))
    (testId : String)
    (onComplete : TestResult → IO Unit) : IO Unit := do
  -- Get the test from state
  match ← State.getResult state testId with
  | none =>
    IO.eprintln s!"Error: Test not found: {testId}"
    return
  | some result =>
    -- Set status to running
    State.updateTestStatus state testId .running

    let startTime ← IO.monoMsNow
    let (status, details) ← try
      let path := result.test.path
      ensureToolchainSearchPath path
      let pathReal ← IO.FS.realPath path
      let testTrees ← State.getTestTrees state
      let testTreeRoot? := resolveTestTreeRoot testTrees path
      let searchPath? ← match testTreeRoot? with
        | none =>
          pure none
        | some root =>
          match ← State.getTestTreeSearchPath state root with
          | some sp =>
            pure (some sp)
          | none =>
            let sp? ← loadTestTreeSearchPath root
            match sp? with
            | some sp =>
              State.setTestTreeSearchPath state root sp
              pure (some sp)
            | none =>
              pure none
      match searchPath? with
      | some sp =>
        let current ← Lean.searchPathRef.get
        Lean.searchPathRef.set (current ++ sp)
      | none =>
        IO.eprintln s!"Warning: No test tree context for {path}"
      let testRange := testRangeToLsp result.test.range
      let allResults ← State.getAllResults state
      let nextStartLine? := findNextTestStartLine allResults path result.test.range.startLine
      let diags ← getFileDiagnosticsCached cacheRef path result.test.range
      let mut relevantDiags : Array DiagnosticEntry := #[]
      for diag in diags do
        let msgPathReal ← IO.FS.realPath diag.fileName
        if msgPathReal == pathReal && diagnosticBelongsToTest testRange nextStartLine? diag.range then
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
      let details := Json.mkObj [(
        "diagnostics", diagnostics
      )]
      let status := if hasError then TestStatus.invalid else TestStatus.valid
      pure (status, details)
    catch e =>
      IO.eprintln s!"Error processing test {testId}: {e}"
      let details := Json.mkObj [("diagnostics", Json.arr #[])]
      pure (TestStatus.invalid, details)
    let endTime ← IO.monoMsNow
    let elapsed := endTime - startTime

    -- Update result with final status and time
    State.updateTestStatus state testId status (some elapsed) (some details)

    -- Get the updated result and invoke callback
    match ← State.getResult state testId with
    | some finalResult => onComplete finalResult
    | none => IO.eprintln s!"Error: Test disappeared: {testId}"

/-- Run verification for multiple tests sequentially. -/
def runTestsSequentially
    (state : State.StateRef)
    (testIds : Array String)
    (onComplete : TestResult → IO Unit) : IO Unit := do
  let cacheRef ← IO.mkRef ({} : HashMap String (Array DiagnosticEntry))
  for testId in testIds do
    runTestVerificationWithCache state cacheRef testId onComplete

end AfvRpcApi.TestRunner
