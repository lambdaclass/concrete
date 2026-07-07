import Concrete.Proof.Proof

/-!
# Concrete Proof Kit — Calls

Reusable call-site / `FnTable` reduction templates: how a `.call` to a
registered function reduces to its refinement fact. Generalized over an
arbitrary `fns : FnTable` (no SHA `shaFns` assumption). Extracted from
`Concrete.Sha256Refine` (Proof Kit v1.1).
-/

namespace Concrete.Proof

/-- A unary `u32 → u32` call `name(xe)` reduces to `specf` of the argument,
    given the function is registered (`hfn`) and its body refines `specf`
    (`href`), for any `fns : FnTable`. The template for `rotr`/σ-style call
    reductions. -/
theorem unary_call (fns : FnTable) (name : String) (body : PExpr) (specf : BitVec 32 → BitVec 32)
    (hfn : fns name = some ⟨name, ["x"], body⟩)
    (href : ∀ (Y : BitVec 32) (f : Nat),
      eval fns (Env.empty.bind "x" (.int Y.toNat)) (f + 2) body = some (.int (specf Y).toNat))
    (X : BitVec 32) (env : Env) (xe : PExpr) (fuel : Nat)
    (hx : eval fns env (fuel + 2) xe = some (.int (X.toNat : Int))) :
    eval fns env (fuel + 3) (.call name [xe]) = some (.int (specf X).toNat) := by
  simp only [eval, hfn, eval.evalArgs, hx, bindArgs]
  exact href X fuel

end Concrete.Proof
