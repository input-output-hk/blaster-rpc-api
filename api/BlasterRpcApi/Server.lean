import Lean.Data.JsonRpc
import Lean.Data.Lsp.Communication
import BlasterRpcApi.Types
import BlasterRpcApi.TestTree
import BlasterRpcApi.State
import BlasterRpcApi.TestRunner

namespace BlasterRpcApi.Server

open Lean
open Lean.JsonRpc
open BlasterRpcApi

/-- Server context containing state and I/O streams. -/
structure ServerContext where
  state : State.StateRef
  hIn   : IO.FS.Stream
  hOut  : IO.FS.Stream
  outputLock : Std.Mutex Unit
  runLock : Std.Mutex Unit

/-- Server monad with access to context. -/
abbrev ServerM := ReaderT ServerContext IO

/-- Send a notification to the client. -/
def sendNotification {α : Type} [ToJson α] (method : String) (params : α) : ServerM Unit := do
  let ctx ← read
  let notif : Notification α := {
    method := method
    param := params
  }
  ctx.outputLock.atomically do
    ctx.hOut.writeLspNotification notif
    ctx.hOut.flush

/-- Send a successful response to the client. -/
def sendResponse {α : Type} [ToJson α] (id : RequestID) (result : α) : ServerM Unit := do
  let ctx ← read
  let response : Response α := {
    id := id
    result := result
  }
  ctx.outputLock.atomically do
    ctx.hOut.writeLspResponse response
    ctx.hOut.flush

/-- Send an error response to the client. -/
def sendError (id : RequestID) (code : ErrorCode) (message : String) : ServerM Unit := do
  let ctx ← read
  let errorResp : ResponseError Unit := {
    id := id
    code := code
    message := message
  }
  ctx.outputLock.atomically do
    ctx.hOut.writeLspResponseError errorResp
    ctx.hOut.flush

private partial def attachWorkspace (workspaceId : String) (group : Array String) (node : Node) : Node :=
  match node with
  | .folder path label children =>
    .folder path label (children.map (attachWorkspace workspaceId (group.push label)))
  | .file path label tests =>
    .file path label (tests.map fun test => { test with workspaceId, group := group.push label })

def handleFetch (id : RequestID) (params : FetchParams) : ServerM Unit := do
  let ctx ← read
  if ← State.hasPendingTests ctx.state then
    sendError id ErrorCode.invalidRequest "Wait for pending tests before refreshing discovery."
    return
  let mut testTrees : Array TestTree := #[]
  let mut workspaceIds : Std.HashSet String := {}
  let mut workspacePaths : Std.HashSet String := {}
  for workspace in params.workspaces do
    if workspace.id.isEmpty || workspace.id.length > 8 || workspaceIds.contains workspace.id || workspacePaths.contains workspace.path then
      throw (IO.userError "Workspace IDs and paths must be nonempty and unique; IDs are at most 8 characters.")
    if workspace.path.isEmpty || !(← (System.FilePath.mk workspace.path).isDir) then
      throw (IO.userError s!"Workspace directory not found: {workspace.path}")
    workspaceIds := workspaceIds.insert workspace.id
    workspacePaths := workspacePaths.insert workspace.path
    let tree ← TestTree.scanTestTree workspace.path
    let tree := tree.getD { path := workspace.path, label := workspace.path, children := #[] }
    testTrees := testTrees.push { tree with workspaceId := workspace.id, children := tree.children.map (attachWorkspace workspace.id #[]) }
  State.setTestTrees ctx.state testTrees
  sendResponse id (← State.toPublicJson ctx.state)

def handleRun (params : RunParams) : ServerM Unit := do
  let ctx ← read
  let trees ← State.getTestTrees ctx.state
  let results ← State.getAllResults ctx.state
  let mut keys : Array String := #[]
  let mut seen : Std.HashSet String := {}
  for selection in params.testRuns do
    match trees.find? (·.path == selection.workspacePath) with
    | none => IO.eprintln s!"Unknown workspace: {selection.workspacePath}"
    | some tree =>
      let ids := selection.testIds.getD (results.toArray.filterMap fun (_, result) =>
        if result.test.workspaceId == tree.workspaceId then some result.test.id else none)
      for testId in ids do
        let key := s!"{tree.workspaceId}:{testId}"
        if seen.contains key then continue
        seen := seen.insert key
        match results.get? key with
        | none =>
          let missing : Test := {
            id := testId, label := testId, path := "", range := default, workspaceId := tree.workspaceId
          }
          sendNotification "testEvent" (testEvent "test-error" missing [
            ("error", Json.mkObj [("message", toJson s!"Test not found: {testId}")])])
        | some _ =>
          if ← State.queueTest ctx.state key then keys := keys.push key
  if keys.isEmpty then return
  let _ ← IO.asTask <| ctx.runLock.atomically do
    TestRunner.runTestsSequentially ctx.state keys fun event =>
      (sendNotification "testEvent" event).run ctx
  return ()

/-- Dispatch a notification to the appropriate handler. -/
def dispatchNotification (method : String) (params : Json) : ServerM Unit := do
  match method with
  | "run" =>
    match fromJson? params with
    | Except.ok (p : RunParams) => handleRun p
    | Except.error err =>
      IO.eprintln s!"Error parsing run params: {err}"

  | "exit" =>
    -- Exit notification - we'll handle this in the main loop
    return

  | _ =>
    IO.eprintln s!"Unknown notification method: {method}"

/-- Dispatch a request to the appropriate handler. -/
def dispatchRequest (id : RequestID) (method : String) (params : Json) : ServerM Unit := do
  match method with
  | "fetch" =>
    match fromJson? params with
    | Except.ok (p : FetchParams) =>
      try handleFetch id p
      catch error => sendError id ErrorCode.invalidParams s!"{error}"
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
    return ()
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

/-- Entry point: signals readiness then enters the main loop. -/
def serverStart : ServerM Unit := do
  sendNotification "serverReady" Json.null
  serverLoop

end BlasterRpcApi.Server
