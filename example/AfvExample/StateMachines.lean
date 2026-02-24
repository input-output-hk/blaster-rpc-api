import Solver.StateMachine

open Solver.StateMachine

namespace Test.StepCounterExample

inductive Request where
  | Idle
  | Reset


instance counter : StateMachine Request Nat where

  init _    := 0

  next i s  :=
    match i with
    | Request.Idle => if s < 5 then s + 1 else s
    | Request.Reset => 0

  assumptions _ _ := True -- no assumptions

  invariants _ s := s ≤ 5

#bmc (max-depth: 6) [counter] -- Should be valid
#kind (max-depth: 6) [counter] -- Should be valid


instance counterBuggy : StateMachine Request Nat where

  init _    := 0

  next i s  :=
    match i with
    | Request.Idle => if s <= 5 then s + 1 else s
    | Request.Reset => 0

  assumptions _ _ := True -- no assumptions

  invariants _ s := s ≤ 5

#bmc (max-depth: 6) [counterBuggy] -- Should be invalid
#kind (max-depth: 6) [counterBuggy] -- Should be invalid

end Test.StepCounterExample
