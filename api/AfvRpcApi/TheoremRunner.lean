import AfvRpcApi.Types
import AfvRpcApi.State

namespace AfvRpcApi.TheoremRunner

open AfvRpcApi

/-! ## Mock Verification -/

/-- Simulate theorem verification with random delay and result.
    Returns (status, time in milliseconds). -/
def simulateVerification : IO (TheoremStatus × Nat) := do
  -- Random delay between 1000ms and 5000ms
  let randomTime := (← IO.rand 1000 5001)

  -- Sleep for the random duration (convert ms to nanoseconds)
  IO.sleep (randomTime * 1000000).toUInt32

  -- Random pass/fail (50/50 chance)
  let randomResult := (← IO.rand 0 2)
  let status := if randomResult == 0 then TheoremStatus.valid else TheoremStatus.invalid

  return (status, randomTime)

/-- Run verification for a single theorem.
    Updates state and invokes callback with result when complete. -/
def runTheoremVerification
    (state : State.StateRef)
    (theoremId : String)
    (onComplete : TheoremResult → IO Unit) : IO Unit := do
  -- Get the theorem from state
  match ← State.getResult state theoremId with
  | none =>
    IO.eprintln s!"Error: Theorem not found: {theoremId}"
    return
  | some _ =>
    -- Set status to running
    State.updateTheoremStatus state theoremId .running

    -- Simulate verification
    let (status, time) ← simulateVerification

    -- Update result with final status and time
    State.updateTheoremStatus state theoremId status (some time)

    -- Get the updated result and invoke callback
    match ← State.getResult state theoremId with
    | some finalResult => onComplete finalResult
    | none => IO.eprintln s!"Error: Theorem disappeared: {theoremId}"

/-- Run verification for multiple theorems sequentially. -/
def runTheoremsSequentially
    (state : State.StateRef)
    (theoremIds : Array String)
    (onComplete : TheoremResult → IO Unit) : IO Unit := do
  for theoremId in theoremIds do
    runTheoremVerification state theoremId onComplete

end AfvRpcApi.TheoremRunner
