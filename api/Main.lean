import BlasterRpcApi.Server
import BlasterRpcApi.State

open BlasterRpcApi

def main : IO UInt32 := do
  try
    -- Get stdin/stdout streams
    let hIn ← IO.getStdin
    let hOut ← IO.getStdout

    -- Initialize server state
    let state ← State.initState
    let outputLock ← Std.Mutex.new ()
    let runLock ← Std.Mutex.new ()

    -- Create server context
    let ctx : Server.ServerContext := {
      state := state
      hIn := hIn
      hOut := hOut
      outputLock := outputLock
      runLock := runLock
    }

    -- Run server loop
    ReaderT.run Server.serverStart ctx

    return 0
  catch e =>
    IO.eprintln s!"Fatal server error: {e}"
    return 1
