import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Slice0

/-! The current Core interface. The slice fragment and its embedding belong to the spec. -/

namespace Concrete.Core
export ConcreteSpec (check check_iff run Checked)
export ConcreteSpec.Slice0 (Program Atom Expr Stmt InSlice0 toCore slice0_exits slice0_never_traps)
end Concrete.Core
