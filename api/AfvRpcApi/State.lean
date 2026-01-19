import AfvRpcApi.Types
import Std.Data.HashMap

namespace AfvRpcApi.State

open AfvRpcApi
open Std (HashMap)

/-- Server state containing workspaces and theorem results. -/
structure ServerState where
  workspaces : Array Workspace
  results    : HashMap String TheoremResult
  deriving Inhabited

/-- Thread-safe reference to server state. -/
abbrev StateRef := IO.Ref ServerState

/-- Initialize empty server state. -/
def initState : IO StateRef := do
  IO.mkRef {
    workspaces := #[]
    results := {}
  }

/-- Recursively initialize results for all theorems in nodes. -/
partial def initializeWorkspaceResults (nodes : Array Node) (results : HashMap String TheoremResult) : HashMap String TheoremResult :=
  nodes.foldl (fun acc node =>
    match node with
    | .folder _ _ children =>
      initializeWorkspaceResults children acc
    | .file _ _ theorems =>
      theorems.foldl (fun acc' thm =>
        acc'.insert thm.id {
          thm := thm
          time := none
          status := .undetermined
        }
      ) acc
  ) results

/-- Get all workspaces from state. -/
def getWorkspaces (state : StateRef) : IO (Array Workspace) := do
  let s ← state.get
  return s.workspaces

/-- Set workspaces and initialize theorem results. -/
def setWorkspaces (state : StateRef) (workspaces : Array Workspace) : IO Unit := do
  let mut results : HashMap String TheoremResult := {}

  -- Initialize all theorems with undetermined status
  for workspace in workspaces do
    results := initializeWorkspaceResults workspace.children results

  state.set {
    workspaces := workspaces
    results := results
  }

/-- Get result for a specific theorem ID. -/
def getResult (state : StateRef) (theoremId : String) : IO (Option TheoremResult) := do
  let s ← state.get
  return s.results.get? theoremId

/-- Update result for a specific theorem ID. -/
def setResult (state : StateRef) (theoremId : String) (result : TheoremResult) : IO Unit := do
  state.modify fun s => {
    s with results := s.results.insert theoremId result
  }

/-- Get all theorem results. -/
def getAllResults (state : StateRef) : IO (HashMap String TheoremResult) := do
  let s ← state.get
  return s.results

/-- Update theorem status. -/
def updateTheoremStatus (state : StateRef) (theoremId : String) (status : TheoremStatus) (time : Option Nat := none) : IO Unit := do
  let s ← state.get
  match s.results.get? theoremId with
  | some result =>
    let updatedResult := {
      result with
      status := status
      time := time <|> result.time
    }
    state.set {
      s with results := s.results.insert theoremId updatedResult
    }
  | none =>
    IO.eprintln s!"Warning: Attempted to update non-existent theorem: {theoremId}"

end AfvRpcApi.State
