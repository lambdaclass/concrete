import ConcreteSpec.Proofs.Observe2

/-! # Operational classification under a concrete execution transformation

This verifies a producer of executions. It does not assume the transformed execution is
infinite, or assume observation equivalence as an admission condition.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Law (subject `ConcreteSpec.Core.Observe.delayExecution`).
Delaying an execution preserves its entire observation, including whether it terminates,
blocks, emits forever, or diverges silently. The infinite case proves the correspondence of
all retained step indices rather than assuming the output's classification. -/
theorem delay_preserves_observation {S : EventSignature} (execution : Execution S) :
    SameObservation execution (delayExecution execution) := by
  cases execution with
  | finite steps terminal =>
    exact ⟨rfl, rfl⟩
  | blocked steps request =>
    exact ⟨rfl, rfl⟩
  | infinite steps =>
    apply infinite_stuttering (f := fun n => n + 1)
    refine ⟨?_, ?_, ?_⟩
    · intro n
      change n + 1 < n + 1 + 1
      omega
    · intro n
      rfl
    · intro m absent
      cases m with
      | zero => rfl
      | succ n => exact False.elim (absent n rfl)

/-- A transformation fault turns divergence into successful termination. Finite and blocked
executions are treated exactly as by the intended transformation. -/
def terminateInfinite {S : EventSignature} : Execution S → Execution S
  | .infinite _ => .finite [] (.exited 0)
  | other => delayExecution other

/-- Instance (law `ConcreteSpec.Core.Observe.delay_preserves_observation`, case `silent_divergence`). -/
example : SameObservation (Execution.infinite (S := Demo.signature) (fun _ => .silent))
    (delayExecution (.infinite (fun _ => .silent))) :=
  delay_preserves_observation (Execution.infinite (S := Demo.signature) (fun _ => .silent))

/-- Planted (law `ConcreteSpec.Core.Observe.delay_preserves_observation`, case `silent_divergence`,
subject `ConcreteSpec.Core.Observe.delayExecution`, mutant `ConcreteSpec.Core.Observe.terminateInfinite`). -/
example : ¬ SameObservation (Execution.infinite (S := Demo.signature) (fun _ => .silent))
    (terminateInfinite (.infinite (fun _ => .silent))) := by
  intro invalid
  exact invalid

end ConcreteSpec.Core.Observe
