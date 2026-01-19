import AfvRpcApi.Server
import AfvRpcApi.State

open AfvRpcApi

def main : IO UInt32 := do
  try
    -- Get stdin/stdout streams
    let hIn ← IO.getStdin
    let hOut ← IO.getStdout

    -- Initialize server state
    let state ← State.initState

    -- Create server context
    let ctx : Server.ServerContext := {
      state := state
      hIn := hIn
      hOut := hOut
    }

    -- Run server loop
    ReaderT.run Server.serverLoop ctx

    return 0
  catch e =>
    IO.eprintln s!"Fatal server error: {e}"
    return 1
