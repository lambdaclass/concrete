import Concrete.Proof.Proof

/-!
# Concrete Proof Kit — Eval

Domain-agnostic lemmas about the `eval` interpreter semantics, reusable by any
refinement proof (not just SHA-256/HMAC). Extracted from `Concrete.Sha256Refine`
as part of Proof Kit v1.

These live in the `Concrete.Proof` namespace (so callers reference them
unqualified) but in the `Concrete.ProofKit.Eval` module. The loop-induction
keystone (`eval_while_count`) and fuel monotonicity (`eval_fuel_le`) currently
live in `Concrete.Proof`'s ladder section and are part of the same kit surface.
-/

namespace Concrete.Proof

/-- `letIn` evaluates its bound value at `fuel+1`, then the body at `fuel` in the
    extended env. The workhorse for stepping through a chain of `let`s. -/
theorem eval_letIn (fns : FnTable) (env : Env) (fuel : Nat)
    (name : String) (val body : PExpr) (v : PVal)
    (hv : eval fns env (fuel + 1) val = some v) :
    eval fns env (fuel + 1) (.letIn name val body)
      = eval fns (env.bind name v) fuel body := by
  simp only [eval, hv]

/-- A `then` branch taken: the conditional reduces to the `then` value. -/
theorem eval_ite_true (fns : FnTable) (env : Env) (fuel : Nat) (cond t el : PExpr) (v : PVal)
    (hc : eval fns env (fuel + 1) cond = some (.bool true)) (ht : eval fns env fuel t = some v) :
    eval fns env (fuel + 1) (.ifThenElse cond t el) = some v := by
  simp only [eval, hc, ht]

/-- An `else` branch taken: the conditional reduces to the `else` value. -/
theorem eval_ite_false (fns : FnTable) (env : Env) (fuel : Nat) (cond t el : PExpr) (v : PVal)
    (hc : eval fns env (fuel + 1) cond = some (.bool false)) (he : eval fns env fuel el = some v) :
    eval fns env (fuel + 1) (.ifThenElse cond t el) = some v := by
  simp only [eval, hc, he]

/-- Evaluating `n` copies of a literal element yields `n` copies of its value. -/
theorem evalElems_replicate_lit (fns : FnTable) (env : Env) (fuel : Nat)
    (n : Nat) (v : PVal) :
    eval.evalElems fns env (fuel + 1) (List.replicate n (.lit v))
      = some (List.replicate n v) := by
  induction n with
  | zero => simp [List.replicate, eval.evalElems]
  | succ m ih => simp [List.replicate, eval.evalElems, eval, ih]

/-- An array literal of `n` copies of a literal evaluates to the `n`-element
    constant array. The single generic form behind the per-size zero-array
    helpers (`arrayLit_z32`/`z64`/`z384`/…) used in the SHA proofs. -/
theorem arrayLit_replicate_eval (fns : FnTable) (env : Env) (fuel : Nat)
    (n : Nat) (v : PVal) :
    eval fns env (fuel + 2) (.arrayLit (List.replicate n (.lit v)))
      = some (.array_ (List.replicate n v)) := by
  simp only [eval, evalElems_replicate_lit]

end Concrete.Proof
