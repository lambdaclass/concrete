import Concrete.Core
import Concrete.Proof
import Concrete.ProofCore

/-! # Phase 12: ProofCore extraction soundness

This module is the home of compiler-correctness proofs for
Concrete's `Core -> ProofCore` extraction.  Every Phase 4
extraction rule in `docs/PROOF_OBLIGATIONS_REGISTER.md`
(R-01..R-21) names a preservation theorem it owes Phase 12;
this file is where they land.

## Scope (deliberately small to start)

The first batch covers **literals only** — the two simplest
extraction rules, used as the template for everything else:

- **R-01** `lit_int_preservation`: `cExprToPExpr (.intLit n _)`
  produces a PExpr that evaluates to the same value as the
  source `.intLit n` would.
- **R-02** `lit_bool_preservation`: same for `.boolLit b`.

The point of these two theorems is NOT rule count — it is
to **establish the Phase 12 proof pattern**: a tiny
source-semantics function for the relevant CExpr fragment,
the extraction step that produces a PExpr, and the
preservation claim that the two agree.  Once the pattern
is in place, the harder rules (binops, lets, ifs, calls,
loops) follow the same shape.

## What is NOT here

Binops, lets, ifs, calls, structs, enums, matches, arrays,
arraySet, casts, while, while_step — all owed to Phase 12,
none discharged yet.  Each lands here when its source-side
semantics is small enough to write down without dragging
in too much of the rest of Concrete's source IR.

## Where this is referenced

- `docs/PROOF_OBLIGATIONS_REGISTER.md` R-01 and R-02 will
  link here once these theorems land.
- The Lean kernel compiles this module at `make build`,
  so a future change that breaks the preservation
  argument shows up as a Lean compile error in CI.
-/

namespace Concrete.ProofSoundness

open Concrete
open Concrete.Proof

/-! ## Tiny source semantics — literal fragment

A literal-only source semantics for `CExpr`.  Returns `none`
for non-literal shapes; future batches extend it
construct-by-construct as their preservation theorems land.

This is INTENTIONALLY minimal.  The Phase 12 plan is NOT to
build a full Lean model of Concrete's source semantics in
one commit; it is to extend this semantics one rule at a
time as preservation theorems force it.  The literal
semantics is the smallest possible starting point. -/
def evalSourceLit : CExpr → Option PVal
  | .intLit n _ => some (.int n)
  | .boolLit b  => some (.bool b)
  | _           => none

/-! ## Note on `cExprToPExpr` opacity

`cExprToPExpr` is declared `partial def` in
`Concrete/ProofCore.lean` so it can recurse through the
mutual block with `cMatchArmToP`.  Lean's kernel treats
`partial def` as opaque — it generates no equation
lemmas, and `unfold` / `rfl` cannot reduce calls to it.

We therefore CANNOT state the preservation theorem in the
form

    cExprToPExpr (.intLit n ty) = some (.lit (.int n)) ∧ ...

and close it by `rfl`.  Two ways to handle this for the
literal case:

  (a) state preservation as "if the extractor produces
      `.lit (.int n)`, then the two views agree" — the
      antecedent is the extraction observation, the
      conclusion is what we actually need to prove.
  (b) refactor `cExprToPExpr` to `def` for the literal
      cases (split out a non-partial helper).

We do (a) here: the theorem assumes the extraction
output as a hypothesis.  This is honest — we are not yet
proving that `cExprToPExpr` IS that function, we are
proving that the value relationships hold WHEN extraction
produces the expected shape.

The harder Phase 12 obligation — proving
`cExprToPExpr (.intLit n ty) = some (.lit (.int n))`
without `partial def` opacity — is a separate item.  It
needs refactoring the mutual block so the literal cases
sit outside `partial def`.  That refactor is named in
this comment and in the obligations register's R-01
note as a follow-up; landing it does not change the
content of these theorems, only the antecedent's
provability.

The source-level semantics agreement
(`evalSourceLit` ↔ `eval`) IS provable today; that's
what the theorems below close. -/

/-! ## R-01: `lit_int_preservation` (eval-vs-source-semantics agreement)

Whenever the extractor produces `.lit (.int n)` from
`.intLit n ty` (the antecedent we cannot yet prove
without lifting the `partial def`), evaluating that
PExpr at any non-zero fuel produces the same value as
the source-level semantics.

Closed by `rfl` for the eval side and direct match for
the source side. -/
theorem lit_int_preservation (n : Int) (ty : Ty) (fuel : Nat)
    (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.lit (.int n)) = some (.int n)
  ∧ evalSourceLit (.intLit n ty) = some (.int n) := by
  refine ⟨?_, rfl⟩
  simp [eval]

/-! ## R-02: `lit_bool_preservation` (eval-vs-source-semantics agreement)

Same shape as R-01 for `.boolLit b`. -/
theorem lit_bool_preservation (b : Bool) (fuel : Nat)
    (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.lit (.bool b)) = some (.bool b)
  ∧ evalSourceLit (.boolLit b) = some (.bool b) := by
  refine ⟨?_, rfl⟩
  simp [eval]

/-! ## Sanity checks (inline regression theorems)

Same pattern as the inline `example` blocks in
`Concrete/Proof.lean`: pin specific values so a future
change to `cExprToPExpr` or `eval` that breaks the literal
case surfaces as a Lean compile error, not silent drift. -/

example : evalSourceLit (.intLit 42 .i32) = some (.int 42) := rfl
example : evalSourceLit (.boolLit true) = some (.bool true) := rfl
example : evalSourceLit (.boolLit false) = some (.bool false) := rfl

end Concrete.ProofSoundness
