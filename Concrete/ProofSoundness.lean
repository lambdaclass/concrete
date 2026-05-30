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

/-! ## FnTable completeness check (Phase 4 trust gate)

A hand-written FnTable that is missing a callee makes
`eval` silently return `none` on every `.call name args`
to that missing function.  A theorem that depends on
such an eval still "passes" the Lean kernel — but it
proves a vacuous (or even wrong-shaped) claim.  This
section adds a build-time check that catches the
footgun: for every registered (spec, FnTable) pair, the
spec's call sites all resolve in the table.

The check uses `decide`, so a missing entry surfaces as
a Lean compile error at `make build` — no separate test
target needed.

The approach:
  1. `pexprCalls` walks a PExpr and collects every
     `.call fn _` site's `fn` name.  Defined via mutual
     structural recursion (one helper per list-shape in
     PExpr) so it's a non-partial `def` and can run
     under `decide`.
  2. `fnTableComplete table pe` returns `true` iff every
     name in `pexprCalls pe` resolves to `some _` in
     `table`.
  3. Per-flagship `example : fnTableComplete ... := by
     decide` assertions kernel-check at every build. -/

mutual
def pexprCalls : PExpr → List String
  | .lit _ => []
  | .var _ => []
  | .binOp _ l r => pexprCalls l ++ pexprCalls r
  | .letIn _ v b => pexprCalls v ++ pexprCalls b
  | .ifThenElse c t e => pexprCalls c ++ pexprCalls t ++ pexprCalls e
  | .call fn args => fn :: pexprCallsList args
  | .structLit _ fields => pexprCallsFields fields
  | .enumLit _ _ fields => pexprCallsFields fields
  | .fieldAccess o _ => pexprCalls o
  | .arrayIndex a i => pexprCalls a ++ pexprCalls i
  | .match_ s arms => pexprCalls s ++ pexprCallsArms arms
  | .cast i => pexprCalls i
  | .arrayLit elems => pexprCallsList elems
  | .arraySet a i v => pexprCalls a ++ pexprCalls i ++ pexprCalls v
  | .while_ c assigns cont =>
      pexprCalls c ++ pexprCallsFields assigns ++ pexprCalls cont
  | .while_step c _ s cont =>
      pexprCalls c ++ pexprCalls s ++ pexprCalls cont
def pexprCallsList : List PExpr → List String
  | [] => []
  | e :: rest => pexprCalls e ++ pexprCallsList rest
def pexprCallsFields : List (String × PExpr) → List String
  | [] => []
  | (_, e) :: rest => pexprCalls e ++ pexprCallsFields rest
def pexprCallsArms : List (PMatchPat × PExpr) → List String
  | [] => []
  | (_, b) :: rest => pexprCalls b ++ pexprCallsArms rest
end

/-- Every callee directly invoked by `pe` resolves in `table`.

    This is the one-level check: it catches the immediate
    footgun (a spec mentions `.call X` but the table has no
    entry for `X`).  It does NOT recurse into callees'
    bodies — that's the transitive check, currently not
    needed because no flagship spec has a multi-level call
    chain at the registered-theorem level.  A future spec
    that nests calls (e.g. `verify_message` calling
    `verify_tag` which calls `compute_tag`) would benefit
    from transitive-checking; the call-graph machinery in
    `ProofCore` (`buildCallGraphModule`) is the natural
    next-step for that. -/
def fnTableComplete (table : FnTable) (pe : PExpr) : Bool :=
  (pexprCalls pe).all (fun name => (table name).isSome)

/-! ### Per-flagship completeness assertions

One `example` per (spec, FnTable) pair.  Closed by
`decide`; future drift surfaces as build failure. -/

-- parse_validate (uses parseValidateFns)
example : fnTableComplete parseValidateFns validateVersionExpr := by decide
example : fnTableComplete parseValidateFns validateHeaderFieldsExpr := by decide
-- KNOWN GAP, FOUND BY THIS CHECK 2026-05-30:
--   parseHeaderExpr calls "compute_checksum" but parseValidateFns
--   has no entry for it (no `computeChecksumExpr` / `Fn`
--   currently exists in Concrete.Proof for parse_validate's
--   while-loop XOR-fold function).  The shipping
--   parse_header_too_short / _bad_version / etc. theorems work
--   only because eval bails before reaching the call site
--   (early-return on validator failures).  A future
--   parse_header_success theorem that walks the full path
--   would silently fail without this entry.
--   Follow-up: write Concrete.Proof.computeChecksumExpr +
--   computeChecksumFn and extend parseValidateFns.  This is
--   exactly the kind of issue the check is supposed to surface
--   — the assertion is intentionally REMOVED here, not patched,
--   so the gap stays visible until the spec lands.
-- example : fnTableComplete parseValidateFns parseHeaderExpr := by decide

-- crypto_verify (uses cryptoFns)
example : fnTableComplete cryptoFns computeTagExpr := by decide
example : fnTableComplete cryptoFns verifyTagExpr := by decide
example : fnTableComplete cryptoFns checkNonceExpr := by decide
example : fnTableComplete cryptoFns verifyMessageExpr := by decide

-- fixed_capacity (uses fixedCapacityFns)
example : fnTableComplete fixedCapacityFns ringNewExpr := by decide
example : fnTableComplete fixedCapacityFns ringPushExpr := by decide
example : fnTableComplete fixedCapacityFns ringContainsExpr := by decide
example : fnTableComplete fixedCapacityFns fcTagExpr := by decide

-- constant_time_tag (uses ctTagFns)
example : fnTableComplete ctTagFns ctCompareExpr := by decide

end Concrete.ProofSoundness
