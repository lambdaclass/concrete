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
    -- The hypothesis names only what the reduction USES: that some entry is
    -- registered under `name`, that it takes one parameter `x`, and that its body
    -- is `body`. It deliberately does NOT pin the whole record.
    --
    -- It used to, as `some { displayName := name, params := ["x"], body := body }`,
    -- with a comment claiming that "leaves `callableId` at its default, so the
    -- lemma applies to legacy and identified entries alike". That was FALSE, and
    -- R-0004 step 5 proved it: pinning the record leaves every other field at its
    -- default too, so once `shaFns`' entries carried `identity`, `operationalKey`
    -- and `sourceBodyDigest`, `rfl` no longer closed the hypothesis and seven call
    -- sites stopped elaborating. A hypothesis that mentions fields the conclusion
    -- does not use is not more precise, it is more brittle — and it made a CONTRACT
    -- edge depend on the callee's identity metadata, which is exactly what a
    -- contract edge must not do.
    (d : PFnDef) (hfn : fns.globals name = some d)
    (hparams : d.params = ["x"]) (hbody : d.body = body)
    (href : ∀ (Y : BitVec 32) (f : Nat),
      eval fns (Env.empty.bind "x" (.int Y.toNat)) (f + 2) body = some (.int (specf Y).toNat))
    (X : BitVec 32) (env : Env) (xe : PExpr) (fuel : Nat)
    (hx : eval fns env (fuel + 2) xe = some (.int (X.toNat : Int))) :
    eval fns env (fuel + 3) (.call name [xe]) = some (.int (specf X).toNat) := by
  simp only [eval, hfn, eval.evalArgs, hx, bindArgs, hparams, hbody]
  exact href X fuel

end Concrete.Proof
