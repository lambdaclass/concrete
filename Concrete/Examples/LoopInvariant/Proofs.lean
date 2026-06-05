import Concrete.Proof

/-!
# loop_invariant.count_up — example proof

The `invariant_preservation` obligation for `loop_invariant.count_up`'s counted
loop, proved end to end and linked in source via `#[proof_by(...)]`.

This is an **example-owned** proof: it lives in a per-example namespace
(`Examples.LoopInvariant.Proofs`), NOT in the `Concrete.Proof` compiler
namespace. The generic loop-induction machinery it builds on
(`eval_while_count`, `evalAssigns`, the PExpr/eval model) stays in
`Concrete.Proof` / `Concrete.ProofKit`.

The loop body (exactly the extracted `count_up` while-assigns):
  acc := acc + i ; i := i + 1     (guard i < 8)
Invariant: 0 <= i && i <= 8.
-/

namespace Examples.LoopInvariant.Proofs

open Concrete.Proof

/-- The extracted loop body of `loop_invariant.count_up`. -/
def count_upBody : List (String × PExpr) :=
  [ ("acc", .binOp .add (.var "acc") (.var "i"))
  , ("i",   .binOp .add (.var "i") (.lit (.int 1))) ]

/-- **Invariant preservation** for `count_up`'s loop (invariant `0 <= i && i <= 8`):
    if the invariant holds and the guard `i < 8` is true, executing the loop body
    once re-establishes the invariant. This is exactly the per-iteration premise
    that `eval_while_count` (ProofKit's loop-induction keystone) consumes — the
    obligation discharged here is the building block that runs the whole loop. -/
theorem count_up_loop_preserves (fns : FnTable) (env : Env) (k acc0 : Int) (fuel : Nat)
    (hacc : env "acc" = some (.int acc0)) (hi : env "i" = some (.int k))
    (hlo : 0 ≤ k) (_hhi : k ≤ 8) (hguard : k < 8) :
    eval.evalAssigns fns env (fuel + 2) count_upBody
        = some ((env.bind "acc" (.int (acc0 + k))).bind "i" (.int (k + 1)))
      ∧ (0 ≤ k + 1 ∧ k + 1 ≤ 8) := by
  refine ⟨?_, by omega, by omega⟩
  simp only [count_upBody, eval.evalAssigns, eval, evalBinOp, hacc, hi, Env.bind,
    beq_iff_eq, String.reduceEq, if_false]

end Examples.LoopInvariant.Proofs
