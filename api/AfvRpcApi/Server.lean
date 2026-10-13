import Lean.Data.JsonRpc
import Lean.Data.Lsp.Communication
import AfvRpcApi.Types
import AfvRpcApi.TestTree
import AfvRpcApi.State
import AfvRpcApi.TestRunner

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
  ctx.hOut.flush

/-- Send a successful response to the client. -/
def sendResponse {α : Type} [ToJson α] (id : RequestID) (result : α) : ServerM Unit := do
  let ctx ← read
  let response : Response α := {
    id := id
    result := result
  }
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
  ctx.hOut.writeLspResponseError errorResp
  ctx.hOut.flush

/-- Handle buildTestTree request. -/
def handleBuildTestTree (id : RequestID) (params : TestTreeParams) : ServerM Unit := do
  let ctx ← read
  -- Scan all test tree paths
  let testTrees ← TestTree.scanTestTrees params.testTreePaths

  -- Update state
  State.setTestTrees ctx.state testTrees

  -- Return the built test trees
  sendResponse id testTrees

/-- Handle updateTestTree notification. -/
def handleUpdateTestTree (params : TestTreeParams) : ServerM Unit := do
  let ctx ← read
  let currentTestTrees ← State.getTestTrees ctx.state

  -- Scan all test tree paths
  let testTrees ← TestTree.scanTestTrees params.testTreePaths

  -- Update state and notify only when test trees changed
  if toJson currentTestTrees != toJson testTrees then
    State.setTestTrees ctx.state testTrees
    sendNotification "testTreeUpdated" testTrees

/-- Handle runTest notification. -/
def handleRunTest (params : RunTestParams) : ServerM Unit := do
  let ctx ← read

  -- Define callback to send testResult notification
  let onComplete (result : TestResult) : IO Unit := do
    try
      let notif : Notification TestResult := {
        method := "testResult"
        param := result
      }
      ctx.hOut.writeLspNotification notif
      -- Flush to ensure message is sent immediately
      ctx.hOut.flush
    catch e =>
      IO.eprintln s!"Error sending testResult notification: {e}"

  -- Spawn background task to run tests (don't wait)
  let _ ← IO.asTask do
    TestRunner.runTestsSequentially ctx.state params.testIds onComplete

  return ()

/-- Handle getTestTree request. -/
def handleGetTestTree (id : RequestID) : ServerM Unit := do
  let ctx ← read
  let testTrees ← State.getTestTrees ctx.state
  sendResponse id testTrees

/-- Handle getTestResult request. -/
def handleGetTestResult (id : RequestID) (testId : String) : ServerM Unit := do
  let ctx ← read
  match ← State.getResult ctx.state testId with
  | some result =>
    sendResponse id result
  | none =>
    sendError id ErrorCode.invalidParams s!"Test not found: {testId}"

/-- Dispatch a notification to the appropriate handler. -/
def dispatchNotification (method : String) (params : Json) : ServerM Unit := do
  match method with
  | "updateTestTree" =>
    match fromJson? params with
    | Except.ok (p : TestTreeParams) => handleUpdateTestTree p
    | Except.error err =>
      IO.eprintln s!"Error parsing updateTestTree params: {err}"

  | "runTest" =>
    match fromJson? params with
    | Except.ok (p : RunTestParams) => handleRunTest p
    | Except.error err =>
      IO.eprintln s!"Error parsing runTest params: {err}"

  | "exit" =>
    -- Exit notification - we'll handle this in the main loop
    return

  | _ =>
    IO.eprintln s!"Unknown notification method: {method}"

/-- Dispatch a request to the appropriate handler. -/
def dispatchRequest (id : RequestID) (method : String) (params : Json) : ServerM Unit := do
  match method with
  | "buildTestTree" =>
    match fromJson? params with
    | Except.ok (p : TestTreeParams) => handleBuildTestTree id p
    | Except.error err =>
      sendError id ErrorCode.invalidParams s!"Invalid params: {err}"

  | "getTestTree" =>
    handleGetTestTree id

  | "getTestResult" =>
    match fromJson? params with
    | Except.ok (p: GetTestResultParams) => handleGetTestResult id p.testId
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

/-- Entry point: signals readiness then enters the main loop. -/
def serverStart : ServerM Unit := do
  sendNotification "serverReady" Json.null
  serverLoop

end AfvRpcApi.Server
