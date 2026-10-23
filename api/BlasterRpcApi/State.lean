import BlasterRpcApi.Types
import Lean.Data.Json
import Std.Data.HashMap
import Std.Sync.Mutex

namespace BlasterRpcApi.State

open BlasterRpcApi
open Lean
open Std (HashMap)

/-- Server state containing test trees and test results. -/
structure ServerState where
  testTrees : Array TestTree
  results   : HashMap String TestResult
  testTreeSearchPaths : HashMap String System.SearchPath
  deriving Inhabited

/-- Thread-safe reference to server state. -/
abbrev StateRef := Std.Mutex ServerState

/-- Initialize empty server state. -/
def initState : IO StateRef := do
  Std.Mutex.new {
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
        acc'.insert test.key {
          test := test
          time := none
          status := .undetermined
          details := Lean.Json.mkObj [("diagnostics", Lean.Json.arr #[])]
        }
      ) acc
  ) results

/-- Get all test trees from state. -/
def getTestTrees (state : StateRef) : IO (Array TestTree) := do
  let s ← state.atomically get
  return s.testTrees

/-- Set test trees and initialize test results. -/
def setTestTrees (state : StateRef) (testTrees : Array TestTree) : IO Unit := do
  let mut results : HashMap String TestResult := {}

  -- Initialize all tests with undetermined status
  for testTree in testTrees do
    results := initializeTestTreeResults testTree.children results

  state.atomically <| set ({
    testTrees := testTrees
    results := results
    testTreeSearchPaths := {}
  } : ServerState)

/-- Get result for a specific test ID. -/
def getResult (state : StateRef) (testId : String) : IO (Option TestResult) := do
  let s ← state.atomically get
  return s.results.get? testId

/-- Update result for a specific test ID. -/
def setResult (state : StateRef) (testId : String) (result : TestResult) : IO Unit := do
  state.atomically <| modify fun s => {
    s with results := s.results.insert testId result
  }

/-- Get all test results. -/
def getAllResults (state : StateRef) : IO (HashMap String TestResult) := do
  let s ← state.atomically get
  return s.results

def getTestTreeSearchPath (state : StateRef) (root : String) : IO (Option System.SearchPath) := do
  let s ← state.atomically get
  return s.testTreeSearchPaths.get? root

def setTestTreeSearchPath (state : StateRef) (root : String) (sp : System.SearchPath) : IO Unit := do
  state.atomically <| modify fun s => {
    s with testTreeSearchPaths := s.testTreeSearchPaths.insert root sp
  }

/-- Update test status. -/
def updateTestStatus (state : StateRef) (testId : String) (status : TestStatus)
    (time : Option Nat := none) (details : Option Lean.Json := none) : IO Unit := do
  state.atomically do
   let s ← get
   match s.results.get? testId with
  | some result =>
    let updatedResult := {
      result with
      status := status
      time := time
      details := details.getD (Json.mkObj [])
      isWaiting := false
    }
    set {
      s with results := s.results.insert testId updatedResult
    }
  | none =>
    IO.eprintln s!"Warning: Attempted to update non-existent test: {testId}"

def hasPendingTests (state : StateRef) : IO Bool := do
  let results ← getAllResults state
  return results.toList.any fun (_, result) => result.isWaiting || (result.status matches .running)

def queueTest (state : StateRef) (key : String) : IO Bool :=
  state.atomically do
    let current ← get
    match current.results.get? key with
    | none => return false
    | some result =>
      if result.isWaiting || (result.status matches .running) then return false
      set { current with results := current.results.insert key {
        result with status := .undetermined, time := none, details := Json.mkObj [], isWaiting := true
      } }
      return true

partial def nodesToPublicJson (nodes : Array Node) (results : HashMap String TestResult) : Json :=
  Json.mkObj (nodes.toList.map fun node =>
    let fields := [("name", toJson node.label), ("isOpen", toJson false)]
    let value := match node with
      | .folder _ _ children => Json.mkObj (fields ++ [
          ("type", toJson "folder"), ("nodes", nodesToPublicJson children results)])
      | .file _ _ tests => Json.mkObj (fields ++ [
          ("type", toJson "file"), ("tests", Json.mkObj (tests.toList.map fun test =>
            (test.id, (results.getD test.key { test, time := none, status := .undetermined, details := Json.mkObj [] }).toPublicJson)))])
    (node.label, value))

def toPublicJson (state : StateRef) : IO Json := do
  let snapshot ← state.atomically get
  let workspaces := snapshot.testTrees.toList.map fun tree =>
    let results := snapshot.results.toList.filterMap fun (_, result) =>
      if result.test.workspaceId == tree.workspaceId then some result else none
    let running := results.any fun result => result.status matches .running
    let waiting := results.any (·.isWaiting)
    let status := if results.any (fun result => result.status matches .invalid) then "invalid"
      else if !results.isEmpty && results.all (fun result => result.status matches .valid) then "valid"
      else "undetermined"
    let fields := [
      ("id", toJson tree.workspaceId), ("path", toJson tree.path), ("status", toJson status),
      ("isRunning", toJson running), ("isWaiting", toJson (waiting && !running)),
      ("isOpen", toJson false), ("nodes", nodesToPublicJson tree.children snapshot.results)]
    let timing := if running || waiting then [] else [
      ("time", toJson (results.foldl (fun total result => total + result.time.getD 0) 0))]
    (tree.workspaceId, Json.mkObj (fields ++ timing))
  return Json.mkObj [("testTree", Json.mkObj [("workspaces", Json.mkObj workspaces)])]

end BlasterRpcApi.State
