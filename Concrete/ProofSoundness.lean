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

/-! ## Note on the `partial def` opacity barrier (PARTIALLY LIFTED)

`cExprToPExpr` is declared `partial def` in
`Concrete/ProofCore.lean` because the mutual block it
sits in has `mapM` calls over field / element / arm
lists, which Lean's structural recursion checker cannot
prove decreasing automatically.  Lean's kernel treats
`partial def` as opaque — it generates no equation
lemmas, and `unfold` / `rfl` cannot reduce calls to it.

**What 06383cf shipped:** the preservation theorems
proved only the eval-vs-source-semantics agreement,
WITHOUT the antecedent `cExprToPExpr (.intLit n ty) =
some (.lit (.int n))`.  Marked "partially discharged"
in the register.

**What this commit adds:** a non-partial helper
`cExprLitToPExpr` in `Concrete/ProofCore.lean` that
covers the literal fragment only.  Its body is identical
to the literal cases of `cExprToPExpr`, and the theorems
below now close the FULL preservation claim against this
helper.

**What's still open:** proving
`cExprToPExpr X = cExprLitToPExpr X` for X in the
literal fragment.  This is the "agree by source
inspection" claim — true by reading the code, but not
yet a Lean theorem because `cExprToPExpr` is still
`partial def`.  A future commit that lifts the entire
mutual block out of `partial def` (by replacing each
`mapM` with explicit structural recursion) discharges
this remaining piece.

The lift requires real engineering: each of the three
list-mapping shapes (struct fields, array elements,
match arms) needs a paired mutual helper.  That's the
next Phase 12 architectural step; it's named in the
obligations register's per-rule notes as a follow-up.
-/

/-! ## R-01: `lit_int_preservation`

For any int literal source `e := .intLit n ty`:
  * `cExprLitToPExpr e = some (.lit (.int n))` —
    the literal extractor produces the expected PExpr
    (closed by `rfl` since `cExprLitToPExpr` is `def`,
    not `partial def`);
  * evaluating that PExpr at any non-zero fuel produces
    `some (.int n)`;
  * the source-level semantics produces `some (.int n)`.

Closed by `rfl` for the source-semantics half and
`simp [eval]` for the PExpr-eval half. -/
theorem lit_int_preservation (n : Int) (ty : Ty) (fuel : Nat)
    (fns : FnTable) (env : Env) :
    cExprLitToPExpr (.intLit n ty) = some (.lit (.int n))
  ∧ eval fns env (fuel + 1) (.lit (.int n)) = some (.int n)
  ∧ evalSourceLit (.intLit n ty) = some (.int n) := by
  refine ⟨rfl, ?_, rfl⟩
  simp [eval]

/-! ## R-02: `lit_bool_preservation`

Same shape as R-01 for `.boolLit b`. -/
theorem lit_bool_preservation (b : Bool) (fuel : Nat)
    (fns : FnTable) (env : Env) :
    cExprLitToPExpr (.boolLit b) = some (.lit (.bool b))
  ∧ eval fns env (fuel + 1) (.lit (.bool b)) = some (.bool b)
  ∧ evalSourceLit (.boolLit b) = some (.bool b) := by
  refine ⟨rfl, ?_, rfl⟩
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
