import Concrete.Proof
import Concrete.ProofKit

/-!
# proof_patterns — teaching + regression corpus

Small, source-linked proof patterns a new author can copy. Each pattern's
`.con` lives under `examples/proof_patterns/<pattern>/`; its spec PExpr + proof
theorem live here in the per-example namespace `Examples.ProofPatterns.Proofs`
(NOT in `Concrete.Proof` — see the namespace guard). Staleness is caught by the
in-source `#[proof_fingerprint]`; these patterns are intentionally NOT registered
in `Concrete.Proof.specs` (they follow the `loop_invariant` fingerprint model).
-/

namespace Examples.ProofPatterns.Proofs

open Concrete.Proof

/-! ## 1. Straight-line refinement

The extracted body of `straight_line.add_three` computes exactly `x + 3`. -/

/-- Spec = the extracted body of `add_three`. -/
def addThreeExpr : PExpr :=
  .binOp .add (.var "x") (.lit (.int 3))

/-- `add_three(x)` evaluates to `x + 3` for every integer `x`. -/
theorem add_three_correct (x : Int) (fuel : Nat) :
    eval (fun _ => none) (Env.empty.bind "x" (.int x)) (fuel + 1) addThreeExpr
      = some (.int (x + 3)) := by
  simp [addThreeExpr, eval, Env.bind, evalBinOp]

/-! ## (workspace fixture) straight-line multiply

`workspace.scale_by_two` — the function the `--workspace` fixture generates from. -/

/-- Spec = the extracted body of `scale_by_two`. -/
def scaleByTwoExpr : PExpr :=
  .binOp .mul (.var "x") (.lit (.int 2))

/-- `scale_by_two(x)` evaluates to `x * 2` for every integer `x`. -/
theorem scale_by_two_correct (x : Int) (fuel : Nat) :
    eval (fun _ => none) (Env.empty.bind "x" (.int x)) (fuel + 1) scaleByTwoExpr
      = some (.int (x * 2)) := by
  simp [scaleByTwoExpr, eval, Env.bind, evalBinOp]

end Examples.ProofPatterns.Proofs
