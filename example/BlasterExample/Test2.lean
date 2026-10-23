import Blaster

namespace BlasterExample.Test2

-- Won't call Z3 so no solver stats
theorem example_test_1 : 1 * 1 = 1 := by blaster

theorem example_test_2 : 2 * 2 = 4 := by blaster

theorem example_test_3 : 3 * 3 = 9 := by blaster

-- Will call z3 but counterexample
theorem example_false_test : ∀ (x y : Nat), x - y < x := by blaster

-- Will call z3 and prove, so solver stats
theorem example_full_test : ∀ (x y : Nat), (x - y)*(y - x) = 0 := by blaster

end BlasterExample.Test2
