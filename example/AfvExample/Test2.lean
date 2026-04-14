import Blaster

namespace AfvExample.Test2

-- Won't call Z3 so no solver stats
theorem example_theorem_1 : 1 * 1 = 1 := by blaster (output-mode: 1) (output-repr: 1)

theorem example_theorem_2 : 2 * 2 = 4 := by blaster (output-mode: 1) (output-repr: 1)

theorem example_theorem_3 : 3 * 3 = 9 := by blaster (output-mode: 1) (output-repr: 1)

-- Will call z3 but counterexample
theorem example_false_theorem : ∀ (x y : Nat), x - y < x := by blaster (output-mode: 1) (output-repr: 1)

-- Will call z3 and prove, so solver stats
theorem example_full_theorem : ∀ (x y : Nat), (x - y)*(y - x) = 0 := by blaster (output-mode: 1) (output-repr: 1)

end AfvExample.Test2
