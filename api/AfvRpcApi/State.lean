import AfvRpcApi.Types
import Lean.Data.Json
import Std.Data.HashMap

namespace AfvRpcApi.State

open AfvRpcApi
open Lean
open Std (HashMap)

/-- Server state containing test trees and test results. -/
structure ServerState where
  testTrees : Array TestTree
  results   : HashMap String TestResult
  testTreeSearchPaths : HashMap String System.SearchPath
  deriving Inhabited

/-- Thread-safe reference to server state. -/
abbrev StateRef := IO.Ref ServerState

/-- Initialize empty server state. -/
def initState : IO StateRef := do
  IO.mkRef {
    testTrees := #[]
    results := {}
    testTreeSearchPaths := {}
  }

/-- Recursively initialize results for all tests in nodes. -/
partial def initializeTestTreeResults (nodes : Array Node) (results : HashMap String TestResult) : HashMap String TestResult :=
  nodes.foldl (fun acc node =>
    match node with
    | .folder _ _ children =>
      initializeTestTreeResults children acc
    | .file _ _ tests =>
      tests.foldl (fun acc' test =>
        acc'.insert test.id {
          test := test
          time := none
          status := .undetermined
          details := Lean.Json.mkObj [("diagnostics", Lean.Json.arr #[])]
        }
      ) acc
  ) results

/-- Get all test trees from state. -/
def getTestTrees (state : StateRef) : IO (Array TestTree) := do
  let s ← state.get
  return s.testTrees

/-- Set test trees and initialize test results. -/
def setTestTrees (state : StateRef) (testTrees : Array TestTree) : IO Unit := do
  let mut results : HashMap String TestResult := {}

  -- Initialize all tests with undetermined status
  for testTree in testTrees do
    results := initializeTestTreeResults testTree.children results

  state.set {
    testTrees := testTrees
    results := results
    testTreeSearchPaths := {}
  }

/-- Get result for a specific test ID. -/
def getResult (state : StateRef) (testId : String) : IO (Option TestResult) := do
  let s ← state.get
  return s.results.get? testId

/-- Update result for a specific test ID. -/
def setResult (state : StateRef) (testId : String) (result : TestResult) : IO Unit := do
  state.modify fun s => {
    s with results := s.results.insert testId result
  }

/-- Get all test results. -/
def getAllResults (state : StateRef) : IO (HashMap String TestResult) := do
  let s ← state.get
  return s.results

def getTestTreeSearchPath (state : StateRef) (root : String) : IO (Option System.SearchPath) := do
  let s ← state.get
  return s.testTreeSearchPaths.get? root

def setTestTreeSearchPath (state : StateRef) (root : String) (sp : System.SearchPath) : IO Unit := do
  state.modify fun s => {
    s with testTreeSearchPaths := s.testTreeSearchPaths.insert root sp
  }

/-- Update test status. -/
def updateTestStatus (state : StateRef) (testId : String) (status : TestStatus)
    (time : Option Nat := none) (details : Option Lean.Json := none) : IO Unit := do
  let s ← state.get
  match s.results.get? testId with
  | some result =>
    let updatedResult := {
      result with
      status := status
      time := time <|> result.time
      details := details.getD result.details
    }
    state.set {
      s with results := s.results.insert testId updatedResult
    }
  | none =>
    IO.eprintln s!"Warning: Attempted to update non-existent test: {testId}"

end AfvRpcApi.State
