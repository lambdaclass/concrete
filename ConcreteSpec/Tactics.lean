import ConcreteSpec.Tactics.Option
import ConcreteSpec.Tactics.Step
import ConcreteSpec.Tactics.Frame
import ConcreteSpec.Tactics.BoolIte
import ConcreteSpec.Tactics.ListMem

/-!
# Constructive proof tactic library (`ConcreteSpec.Tactics`)

Root import for Concrete's zero-TCB proof tactics (R-0724 item 2, 2026-11-28 owner decision:
`"yes to tactics"`). Every tactic is a pure Lean core macro/syntax expansion that elaborates to
kernel-checked proof terms depending only on `propext` and `Quot.sound` (never `Classical.choice`,
`grind`, or classical `simp` sets).

## Import layering

- `ConcreteSpec/Spec/` **never** imports `ConcreteSpec.Tactics` or any submodule under
  `ConcreteSpec/Tactics/`, keeping the specification plain, readable Lean (`spec_imports.sh`).
- `ConcreteSpec/Tactics/` modules may import only sibling modules under `ConcreteSpec/Tactics/`.
- `ConcreteSpec/Proofs/`, `ConcreteSpecEvidence/`, and `Concrete/` may import
  `ConcreteSpec.Tactics` (or individual tactic-family modules).

## Tactic families

- `ConcreteSpec.Tactics.Option`: `crush_option`, `crush_except`
- `ConcreteSpec.Tactics.Step`: `invert_step`, `unfold_step`
- `ConcreteSpec.Tactics.Frame`: `frame_auto`
- `ConcreteSpec.Tactics.BoolIte`: `crush_bool`, `crush_ite`
- `ConcreteSpec.Tactics.ListMem`: `crush_mem`, `forall_cons`
-/
