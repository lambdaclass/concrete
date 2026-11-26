import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Accounting

namespace ConcreteSpec.Core.Leak

/-- Instance. Concrete execution check evaluating `run` on an allocate/free/reset sequence from `Memory.empty`.

Accepted: a valid allocate/free/reset sequence. Refused: freeing a byte not owned by the
lineage has no accounting transition. -/
theorem example_execution_check_evaluating_run_on_allocate : (run Memory.empty
    [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3, .reset ⟨0⟩]).map (· ⟨0⟩) = some ⟨8, 0, 8, 8⟩ := by decide

/-- Instance. Concrete refusal check showing `apply` returns `none` when freeing more bytes than are live. -/
theorem example_refusal_check_showing_apply_returns_none : apply Memory.empty (.free ⟨0⟩ 1) = none := by decide

end ConcreteSpec.Core.Leak
