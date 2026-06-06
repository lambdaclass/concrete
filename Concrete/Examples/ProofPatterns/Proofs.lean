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

/-! ## 2. Array read/write frame

`arr.put` writes one cell of a fixed `[i32; 4]`. The proof shows the written
cell (index 1) takes the new value while every other cell is framed (unchanged):
`[10, 11, 12, 13]` with `v = 99` becomes `[10, 99, 12, 13]`. -/

/-- Spec = the extracted body of `put`: write index 1, return the array. -/
def putExpr : PExpr :=
  .letIn "a" (.arraySet (.var "a") (.lit (.int 1)) (.var "v")) (.var "a")

/-- Writing index 1 changes that cell and frames the rest (point proof over a
    concrete 4-element array): `[10,11,12,13][1 := 99] = [10,99,12,13]`. -/
theorem put_writes_index_1_frames_rest (fuel : Nat) :
    eval (fun _ => none)
      ((Env.empty.bind "a" (.array_ [.int 10, .int 11, .int 12, .int 13])).bind "v" (.int 99))
      (fuel + 3) putExpr
      = some (.array_ [.int 10, .int 99, .int 12, .int 13]) := by
  simp [putExpr, eval, Env.bind]

end Examples.ProofPatterns.Proofs
