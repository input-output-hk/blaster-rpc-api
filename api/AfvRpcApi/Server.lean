import Lean.Data.JsonRpc
import Lean.Data.Lsp.Communication
import AfvRpcApi.Types
import AfvRpcApi.Workspace
import AfvRpcApi.State
import AfvRpcApi.TheoremRunner

namespace AfvRpcApi.Server

open Lean
open Lean.JsonRpc
open AfvRpcApi

/-- Server context containing state and I/O streams. -/
structure ServerContext where
  state : State.StateRef
  hIn   : IO.FS.Stream
  hOut  : IO.FS.Stream

/-- Server monad with access to context. -/
abbrev ServerM := ReaderT ServerContext IO

/-- Send a notification to the client. -/
def sendNotification {α : Type} [ToJson α] (method : String) (params : α) : ServerM Unit := do
  let ctx ← read
  let notif : Notification α := {
    method := method
    param := params
  }
  ctx.hOut.writeLspNotification notif

/-- Send a successful response to the client. -/
def sendResponse {α : Type} [ToJson α] (id : RequestID) (result : α) : ServerM Unit := do
  let ctx ← read
  let response : Response α := {
    id := id
    result := result
  }
  ctx.hOut.writeLspResponse response

/-- Send an error response to the client. -/
def sendError (id : RequestID) (code : ErrorCode) (message : String) : ServerM Unit := do
  let ctx ← read
  let errorResp : ResponseError Unit := {
    id := id
    code := code
    message := message
  }
  ctx.hOut.writeLspResponseError errorResp

/-- Handle initializeWorkspace notification. -/
def handleInitializeWorkspace (params : WorkspaceParams) : ServerM Unit := do
  let ctx ← read

  -- Scan all workspace paths
  let workspaces ← Workspace.scanWorkspaces params.workspacePaths

  -- Update state
  State.setWorkspaces ctx.state workspaces

  -- Send workspaceInitialized notification
  sendNotification "workspaceInitialized" Json.null

/-- Handle updateWorkspace notification. -/
def handleUpdateWorkspace (params : WorkspaceParams) : ServerM Unit := do
  let ctx ← read

  -- Scan all workspace paths
  let workspaces ← Workspace.scanWorkspaces params.workspacePaths

  -- Update state
  State.setWorkspaces ctx.state workspaces

/-- Handle runTheorem notification. -/
def handleRunTheorem (theoremIds : Array String) : ServerM Unit := do
  let ctx ← read

  -- Define callback to send testResult notification
  let onComplete (result : TheoremResult) : IO Unit := do
    try
      let notif : Notification TheoremResult := {
        method := "testResult"
        param := result
      }
      ctx.hOut.writeLspNotification notif
      -- Flush to ensure message is sent immediately
      ctx.hOut.flush
    catch e =>
      IO.eprintln s!"Error sending testResult notification: {e}"

  -- Spawn background task to run theorems (don't wait)
  let _ ← IO.asTask do
    TheoremRunner.runTheoremsSequentially ctx.state theoremIds onComplete

  return ()

/-- Handle getWorkspace request. -/
def handleGetWorkspace (id : RequestID) : ServerM Unit := do
  let ctx ← read
  let workspaces ← State.getWorkspaces ctx.state
  sendResponse id workspaces

/-- Handle getTheoremResult request. -/
def handleGetTheoremResult (id : RequestID) (theoremId : String) : ServerM Unit := do
  let ctx ← read
  match ← State.getResult ctx.state theoremId with
  | some result =>
    sendResponse id result
  | none =>
    sendError id ErrorCode.invalidParams s!"Theorem not found: {theoremId}"

/-- Dispatch a notification to the appropriate handler. -/
def dispatchNotification (method : String) (params : Json) : ServerM Unit := do
  match method with
  | "initializeWorkspace" =>
    match fromJson? params with
    | Except.ok (p : WorkspaceParams) => handleInitializeWorkspace p
    | Except.error err =>
      IO.eprintln s!"Error parsing initializeWorkspace params: {err}"

  | "updateWorkspace" =>
    match fromJson? params with
    | Except.ok (p : WorkspaceParams) => handleUpdateWorkspace p
    | Except.error err =>
      IO.eprintln s!"Error parsing updateWorkspace params: {err}"

  | "runTheorem" =>
    match fromJson? params with
    | Except.ok (ids : Array String) => handleRunTheorem ids
    | Except.error err =>
      IO.eprintln s!"Error parsing runTheorem params: {err}"

  | "exit" =>
    -- Exit notification - we'll handle this in the main loop
    return

  | _ =>
    IO.eprintln s!"Unknown notification method: {method}"

/-- Dispatch a request to the appropriate handler. -/
def dispatchRequest (id : RequestID) (method : String) (params : Json) : ServerM Unit := do
  match method with
  | "getWorkspace" =>
    handleGetWorkspace id

  | "getTheoremResult" =>
    match fromJson? params with
    | Except.ok (theoremId : String) => handleGetTheoremResult id theoremId
    | Except.error err =>
      sendError id ErrorCode.invalidParams s!"Invalid params: {err}"

  | _ =>
    sendError id ErrorCode.methodNotFound s!"Unknown method: {method}"

/-- Main server loop - reads and processes JSON-RPC messages. -/
partial def serverLoop : ServerM Unit := do
  let ctx ← read

  -- Read message from stdin
  let msgResult ← try
    let msg ← ctx.hIn.readLspMessage
    pure (Except.ok msg)
  catch e =>
    pure (Except.error s!"{e}")

  match msgResult with
  | Except.error err =>
    IO.eprintln s!"Error reading message: {err}"
    -- Continue on read errors
    serverLoop
  | Except.ok msg =>
    -- Dispatch message
    match msg with
    | Message.notification method (some params) =>
      if method == "exit" then
        -- Exit the server
        return ()
      else
        dispatchNotification method (toJson params)
        serverLoop

    | Message.notification method none =>
      if method == "exit" then
        return ()
      else
        dispatchNotification method Json.null
        serverLoop

    | Message.request id method (some params) =>
      dispatchRequest id method (toJson params)
      serverLoop

    | Message.request id method none =>
      dispatchRequest id method Json.null
      serverLoop

    | Message.response .. =>
      -- We don't send requests, so we shouldn't receive responses
      IO.eprintln "Warning: Received unexpected response message"
      serverLoop

    | Message.responseError .. =>
      -- We don't send requests, so we shouldn't receive error responses
      IO.eprintln "Warning: Received unexpected error response message"
      serverLoop

end AfvRpcApi.Server
