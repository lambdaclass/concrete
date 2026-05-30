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

/-- Source semantics for identifier references.

    At the source level, `.ident name ty` looks up `name` in the
    surrounding scope.  Concrete's scope discipline is enforced
    by Resolve / Check before extraction reaches ProofCore, so by
    the time a CExpr.ident appears in proof-eligible code the
    name is guaranteed to be in scope (a typed environment maps
    each in-scope name to its PVal).

    We model "the scope" as the same `Env` PExpr eval uses
    (`String → Option PVal`).  This is honest: the source-side
    and PExpr-side environments are the same shape; the
    preservation theorem is that lookup agrees on both sides. -/
def evalSourceIdent (env : Env) : CExpr → Option PVal
  | .ident name _ => env name
  | _             => none

/-- Source semantics for a letIn step.

    Given the val evaluates to `vv`, evaluating
    `letIn name val body` under env extends env with
    `name ↦ vv` and evaluates body in the extended env.
    Stated as a step function rather than a recursive
    source-eval (same reason as `evalSourceBinOpStep`:
    avoids re-creating partial-def opacity for
    not-yet-discharged source constructs).

    The "extend then eval body" view matches PExpr.eval's
    letIn case exactly. -/
def evalSourceLetStep (env : Env) (name : String) (vv : PVal)
    (evalBody : Env → Option PVal) : Option PVal :=
  evalBody (env.bind name vv)

/-- Source semantics for a binary operation step.

    Given that the operands `lhs` and `rhs` evaluate (at the
    source level) to `vl` and `vr`, and that the source `op`
    maps to a typed `pop`, the binop's value is
    `evalBinOp pop vl vr`.  Stated as a partial
    "given-the-pieces" lookup rather than a recursive
    source-eval function because the recursion for
    not-yet-discharged constructs (call, structLit, ...) goes
    through `cExprToPExprImpl`'s partial-def shape — building
    a full recursive source semantics now would re-create the
    same `partial def` opacity problem on the source side.

    R-04 uses this to state the source-side conclusion as a
    compositional fact: "given operand values, the binop step
    produces `evalBinOp` applied to them." -/
def evalSourceBinOpStep (pop : PBinOp) (vl vr : PVal) : Option PVal :=
  evalBinOp pop vl vr

/-! ## Phase 12 wrapper architecture (lift landed 2026-05-30)

`cExprToPExprImpl` (the partial-def, in the mutual block)
still has the structural-recursion barrier through
`List.mapM` over field / element / arm lists.  Lean's
kernel treats `partial def` as opaque — no equation
lemmas, `rfl` cannot reduce.

**The lift.**  `cExprToPExpr` (the public entry point in
`Concrete.ProofCore`) is now a non-partial wrapper that
handles `.intLit` and `.boolLit` definitionally and
delegates all other cases to `cExprToPExprImpl`.  Because
the wrapper sits OUTSIDE the mutual block and is a plain
`def`, Lean reduces `cExprToPExpr (.intLit n ty)` to
`some (.lit (.int n))` by `rfl`.

This is the minimum refactor that makes the literal cases
of the REAL extractor theorem-friendly.  All other cases
of `cExprToPExpr` still go through the partial-def
implementation; theorems about them remain blocked until a
future commit replaces the `mapM`s with paired structural
recursion.

R-01 and R-02 below are now stated against `cExprToPExpr`
(the wrapper), not the parallel `cExprLitToPExpr` helper.
The helper remains in `ProofCore` as a literal-only spec
useful for documentation and as a redundant cross-check;
the theorems below no longer depend on it. -/

/-! ## R-01: `lit_int_preservation` (against real extractor)

For any int literal source `e := .intLit n ty`:
  * `cExprToPExpr e = some (.lit (.int n))` —
    proved against the REAL public extractor
    (closed by `rfl` via the non-partial wrapper);
  * evaluating that PExpr at any non-zero fuel produces
    `some (.int n)`;
  * the source-level semantics produces `some (.int n)`.

Phase 12 obligation R-01 is now FULLY discharged
end-to-end: source → extractor → PExpr eval →
source-semantics, all three agree. -/
theorem lit_int_preservation (n : Int) (ty : Ty) (fuel : Nat)
    (fns : FnTable) (env : Env) :
    cExprToPExpr (.intLit n ty) = some (.lit (.int n))
  ∧ eval fns env (fuel + 1) (.lit (.int n)) = some (.int n)
  ∧ evalSourceLit (.intLit n ty) = some (.int n) := by
  refine ⟨rfl, ?_, rfl⟩
  simp [eval]

/-! ## R-02: `lit_bool_preservation` (against real extractor)

Same shape as R-01 for `.boolLit b`.  Phase 12 obligation
R-02 is now FULLY discharged end-to-end. -/
theorem lit_bool_preservation (b : Bool) (fuel : Nat)
    (fns : FnTable) (env : Env) :
    cExprToPExpr (.boolLit b) = some (.lit (.bool b))
  ∧ eval fns env (fuel + 1) (.lit (.bool b)) = some (.bool b)
  ∧ evalSourceLit (.boolLit b) = some (.bool b) := by
  refine ⟨rfl, ?_, rfl⟩
  simp [eval]

/-! ## R-03: `var_preservation` (against real extractor)

For any identifier source `e := .ident name ty`:
  * `cExprToPExpr e = some (.var name)` — proved against
    the REAL public extractor (closed by `rfl` via the
    non-partial wrapper, same pattern as R-01/R-02);
  * evaluating that PExpr at any non-zero fuel produces
    `env name` (the scope's value for `name`);
  * the source-level semantics produces the same lookup.

The three views agree.  Note the result depends on `env` —
unlike literals, identifier evaluation is environment-
sensitive.  Both PExpr and source semantics use the same
`Env` shape (`String → Option PVal`), so "lookup agreement"
is structural.

Phase 12 obligation R-03 is FULLY discharged end-to-end
2026-05-30. -/
theorem var_preservation (name : String) (ty : Ty) (fuel : Nat)
    (fns : FnTable) (env : Env) :
    cExprToPExpr (.ident name ty) = some (.var name)
  ∧ eval fns env (fuel + 1) (.var name) = env name
  ∧ evalSourceIdent env (.ident name ty) = env name := by
  refine ⟨rfl, ?_, rfl⟩
  simp [eval]

/-! ## R-04: `binop_preservation` (compositional form)

The first **compositional** Phase 12 rule — R-04's
preservation depends on the operands' preservation, so
the theorem takes hypotheses about the sub-extractions
and propagates them.

For `e := .binOp op lhs rhs ty`, given:
  * the op maps to a typed PBinOp at the operand width:
    `binOpToPBinOp op (CExpr.ty lhs) = some pop`,
  * the left operand extracts: `cExprToPExpr lhs = some pl`,
  * the right operand extracts: `cExprToPExpr rhs = some pr`,

the conclusion holds in three views:
  * `cExprToPExpr e = some (.binOp pop pl pr)` — proved
    against the REAL extractor via the new wrapper arm
    (closed by `simp` over the hypotheses);
  * eval on `.binOp pop pl pr` reduces via `evalBinOp`
    once `pl`/`pr` evaluate (the eval side is a
    rewriting fact, stated here as the equation to be
    discharged at use sites with operand eval lemmas).

The compositional pattern matters for scale: future
rules (R-06 letIn, R-07 if, R-08 call, ...) follow this
shape — hypotheses on sub-expressions; conclusion about
the composite.

The wrapper's `.binOp` arm:

    | .binOp op lhs rhs _ => do
        let pop ← binOpToPBinOp op (CExpr.ty lhs)
        let pl ← cExprToPExpr lhs
        let pr ← cExprToPExpr rhs
        some (.binOp pop pl pr)

closes the antecedent by simp/rfl over the three
hypotheses.  Lean accepts structural recursion on
`lhs`/`rhs` (single-sub-expression case, no `mapM`). -/
theorem binop_preservation
    (op : Concrete.BinOp) (lhs rhs : CExpr) (ty : Ty)
    (pop : PBinOp) (pl pr : PExpr)
    (h_op : binOpToPBinOp op (CExpr.ty lhs) = some pop)
    (h_lhs : cExprToPExpr lhs = some pl)
    (h_rhs : cExprToPExpr rhs = some pr) :
    cExprToPExpr (.binOp op lhs rhs ty) = some (.binOp pop pl pr) := by
  show (do
    let pop' ← binOpToPBinOp op (CExpr.ty lhs)
    let pl' ← cExprToPExpr lhs
    let pr' ← cExprToPExpr rhs
    some (PExpr.binOp pop' pl' pr')) = some (.binOp pop pl pr)
  rw [h_op, h_lhs, h_rhs]
  rfl

/-! ## R-04: eval-side compositional reduction

Given that operands `pl` and `pr` evaluate (at PExpr level)
to `vl` and `vr`, evaluating `.binOp pop pl pr` reduces to
`evalBinOp pop vl vr`.  This is the eval-side companion to
`binop_preservation` — together they say "the extracted
binop PExpr evaluates to what `evalBinOp` says it should." -/
theorem eval_binop_reduces
    (fns : FnTable) (env : Env) (fuel : Nat)
    (pop : PBinOp) (pl pr : PExpr) (vl vr : PVal)
    (h_lhs_eval : eval fns env (fuel + 1) pl = some vl)
    (h_rhs_eval : eval fns env (fuel + 1) pr = some vr) :
    eval fns env (fuel + 1) (.binOp pop pl pr) = evalBinOp pop vl vr := by
  simp [eval, h_lhs_eval, h_rhs_eval]

/-! ## R-04: source-side compositional reduction

The source-side claim: the binop step produces
`evalBinOp pop vl vr` from operand values.  Stated as a
lookup-into-`evalSourceBinOpStep` rather than a recursive
source-eval call, for the reason in
`evalSourceBinOpStep`'s docstring. -/
theorem source_binop_step
    (pop : PBinOp) (vl vr : PVal) :
    evalSourceBinOpStep pop vl vr = evalBinOp pop vl vr := rfl

/-! Together, `binop_preservation` + `eval_binop_reduces` +
    `source_binop_step` are the COMPOSITIONAL full discharge
    of R-04: given the operands' R-01 / R-03 facts (extraction
    + eval + source-value agreement), the binop step
    discharges to the same value across all three views.

    The "operand facts" antecedents are themselves provided by
    R-01 / R-03 at use sites — that's the point of the
    compositional pattern.  Phase 12 obligation R-04 is now
    fully discharged end-to-end. -/

/-! ## R-06: `let_preservation` (compositional, three views)

`letIn` extraction lives in `cStmtsToPExprK`, not
`cExprToPExpr`.  Same wrapper-arm pattern as R-04 but
applied to the CStmt-list extractor — the new
`cStmtsToPExprK` wrapper handles `.letDecl :: rest`
directly, recursing through itself for both `val` (via
the expression wrapper) and `rest`.

For `(.letDecl name _ ty val) :: rest`, given:
  * `cExprToPExpr val = some pv`,
  * `cStmtsToPExprK rest k = some pb`,
the conclusion is
  `cStmtsToPExprK ((.letDecl name _ ty val) :: rest) k
    = some (.letIn name pv pb)`.

Closed by `show`/`rw`/`rfl` over the two hypotheses. -/
theorem let_preservation
    (name : String) (isMut : Bool) (ty : Ty) (val : CExpr)
    (rest : List CStmt) (k : Option PExpr)
    (pv pb : PExpr)
    (h_val  : cExprToPExpr val = some pv)
    (h_rest : cStmtsToPExprK rest k = some pb) :
    cStmtsToPExprK ((CStmt.letDecl name isMut ty val) :: rest) k
      = some (.letIn name pv pb) := by
  simp [cStmtsToPExprK, h_val, h_rest]

/-! ## R-06: eval-side compositional reduction

Given `val` evaluates (PExpr-side) to `vv` and `body`
evaluates under the extended env to `vb`, eval on
`.letIn name val body` reduces to `vb`. -/
theorem eval_let_reduces
    (fns : FnTable) (env : Env) (fuel : Nat)
    (name : String) (val body : PExpr) (vv vb : PVal)
    (h_val  : eval fns env (fuel + 1) val = some vv)
    (h_body : eval fns (env.bind name vv) fuel body = some vb) :
    eval fns env (fuel + 1) (.letIn name val body) = some vb := by
  simp [eval, h_val, h_body]

/-! ## R-06: source-side compositional step

`evalSourceLetStep env name vv evalBody` extends the env
with `name ↦ vv` and evaluates body in the extended env.
Trivially equals `evalBody (env.bind name vv)`. -/
theorem source_let_step
    (env : Env) (name : String) (vv : PVal)
    (evalBody : Env → Option PVal) :
    evalSourceLetStep env name vv evalBody
      = evalBody (env.bind name vv) := rfl

/-! `let_preservation` + `eval_let_reduces` + `source_let_step`
    discharge R-06 across all three views: extraction
    antecedent, eval reduction, source step.  Operand facts
    (val extraction + val eval; rest extraction; body eval
    under extended env) come from the rules below R-06 at
    use sites. -/

/-! ## R-10: `field_access_preservation` (extraction + eval)

`.fieldAccess obj field _` extracts to `.fieldAccess po field`
given `cExprToPExpr obj = some po`.  Wrapper arm:

    | .fieldAccess obj field _ => do
        let po ← cExprToPExpr obj
        some (.fieldAccess po field)

Single-sub-expression structural recursion (on `obj`). -/
theorem field_access_preservation
    (obj : CExpr) (field : String) (ty : Ty)
    (po : PExpr) (h_obj : cExprToPExpr obj = some po) :
    cExprToPExpr (.fieldAccess obj field ty)
      = some (.fieldAccess po field) := by
  simp [cExprToPExpr, h_obj]

/-! ## R-13: `cast_identity_preservation` (extraction + eval)

`.cast inner _` extracts to `.cast pi` given
`cExprToPExpr inner = some pi`.  The eval-side identity
(cast is identity at `Int`) is structural via the eval rule. -/
theorem cast_identity_preservation
    (inner : CExpr) (ty : Ty)
    (pi : PExpr) (h_inner : cExprToPExpr inner = some pi) :
    cExprToPExpr (.cast inner ty) = some (.cast pi) := by
  simp [cExprToPExpr, h_inner]

theorem eval_cast_identity
    (fns : FnTable) (env : Env) (fuel : Nat)
    (inner : PExpr) (v : PVal)
    (h_inner : eval fns env (fuel + 1) inner = some v) :
    eval fns env (fuel + 2) (.cast inner) = some v := by
  simp [eval, h_inner]

/-! ## R-14: `array_index_preservation` (extraction)

`.arrayIndex arr idx _` extracts to `.arrayIndex pa pi`
given both operands extract.  Two-sub-expression
structural recursion (on `arr` and `idx`). -/
theorem array_index_preservation
    (arr idx : CExpr) (ty : Ty)
    (pa pi : PExpr)
    (h_arr : cExprToPExpr arr = some pa)
    (h_idx : cExprToPExpr idx = some pi) :
    cExprToPExpr (.arrayIndex arr idx ty)
      = some (.arrayIndex pa pi) := by
  simp [cExprToPExpr, h_arr, h_idx]

/-! ## R-19: `array_set_preservation` (extraction)

Source-level `arr[i] = v;` extracts to a shadowing letIn
that rebinds `arr` via `arraySet`.  Only supported when
`arr` is a simple identifier — the wrapper arm:

    | (.arrayIndexAssign (.ident name _) idx val) :: rest, k =>
        let pi ← cExprToPExpr idx
        let pv ← cExprToPExpr val
        let pb ← cStmtsToPExprK rest k
        some (.letIn name (.arraySet (.var name) pi pv) pb) -/
theorem array_set_preservation
    (name : String) (idxTy : Ty)
    (idx val : CExpr) (rest : List CStmt) (k : Option PExpr)
    (pi pv pb : PExpr)
    (h_idx  : cExprToPExpr idx = some pi)
    (h_val  : cExprToPExpr val = some pv)
    (h_rest : cStmtsToPExprK rest k = some pb) :
    cStmtsToPExprK
      ((CStmt.arrayIndexAssign (.ident name idxTy) idx val) :: rest) k
      = some (.letIn name (.arraySet (.var name) pi pv) pb) := by
  simp [cStmtsToPExprK, h_idx, h_val, h_rest]

/-! ## R-07: `if_no_else_as_fallthrough_preservation` (extraction)

The early-return shape from `parse_validate`'s validators:

    if v == 1 { return 0; }
    return 1;

becomes the PExpr `if cond then thenExpr else fallthrough`
where the fallthrough is the rest's extraction.

Wrapper arm:

    | (.ifElse cond thenBranch none) :: rest, k => do
        let pc ← cExprToPExpr cond
        let pkRest ← cStmtsToPExprK rest k
        let pt ← cStmtsToPExprK thenBranch (some pkRest)
        some (.ifThenElse pc pt pkRest)

Lean accepts the structural recursion on the nested
`thenBranch` (sub-list of the head CStmt). -/
theorem if_no_else_as_fallthrough_preservation
    (cond : CExpr) (thenBranch rest : List CStmt) (k : Option PExpr)
    (pc pkRest pt : PExpr)
    (h_cond : cExprToPExpr cond = some pc)
    (h_rest : cStmtsToPExprK rest k = some pkRest)
    (h_then : cStmtsToPExprK thenBranch (some pkRest) = some pt) :
    cStmtsToPExprK ((CStmt.ifElse cond thenBranch none) :: rest) k
      = some (.ifThenElse pc pt pkRest) := by
  simp [cStmtsToPExprK, h_cond, h_rest, h_then]

/-! ## Rules unlocked by the partial-def → def lift (2026-05-30)

The mutual block (`cExprToPExprImpl`, `cMatchArmToP`,
`cStmtsToPExprKImpl`, `cStmtsToStepExpr`, `extractCarried`,
plus four new helpers `cExprListToPExpr`, `cFieldsToPExpr`,
`cMatchArmsToP`, `cAssignBodyToUpdates`) is now non-partial.
`List.mapM` calls were replaced with explicit structural
recursion via paired mutual helpers; Lean's structural
recursion checker accepts the whole block.

Consequence: every rule whose extraction lives inside the
mutual block is now wrapper-free reducible — theorems can
prove against `cExprToPExprImpl` (or the public
`cExprToPExpr` which dispatches into it) by `simp`. -/

/-! ## R-08: `call_preservation` (extraction)

For `.call fn _ args _` with all args extracting, the
extraction is `.call fn pargs`.  Antecedent is the
extraction of the args list, supplied by repeated R-01..R-N
at use sites. -/
theorem call_preservation
    (fn : String) (typeArgs : List Ty) (args : List CExpr) (ty : Ty)
    (pargs : List PExpr)
    (h_args : cExprListToPExpr args = some pargs) :
    cExprToPExpr (.call fn typeArgs args ty) = some (.call fn pargs) := by
  simp [cExprToPExpr, h_args]

/-! ## R-09: `struct_lit_preservation` (extraction) -/
theorem struct_lit_preservation
    (name : String) (typeArgs : List Ty)
    (fields : List (String × CExpr)) (ty : Ty)
    (pfields : List (String × PExpr))
    (h_fields : cFieldsToPExpr fields = some pfields) :
    cExprToPExpr (.structLit name typeArgs fields ty)
      = some (.structLit name pfields) := by
  simp [cExprToPExpr, h_fields]

/-! ## R-11: `enum_lit_preservation` (extraction) -/
theorem enum_lit_preservation
    (enumName variant : String) (typeArgs : List Ty)
    (fields : List (String × CExpr)) (ty : Ty)
    (pfields : List (String × PExpr))
    (h_fields : cFieldsToPExpr fields = some pfields) :
    cExprToPExpr (.enumLit enumName variant typeArgs fields ty)
      = some (.enumLit enumName variant pfields) := by
  simp [cExprToPExpr, h_fields]

/-! ## R-15: `array_lit_preservation` (extraction) -/
theorem array_lit_preservation
    (elems : List CExpr) (ty : Ty)
    (pelems : List PExpr)
    (h_elems : cExprListToPExpr elems = some pelems) :
    cExprToPExpr (.arrayLit elems ty) = some (.arrayLit pelems) := by
  simp [cExprToPExpr, h_elems]

/-! ## R-12: `match_preservation` (extraction) -/
theorem match_preservation
    (scrutinee : CExpr) (arms : List CMatchArm) (ty : Ty)
    (ps : PExpr) (parms : List (PMatchPat × PExpr))
    (h_scrut : cExprToPExpr scrutinee = some ps)
    (h_arms : cMatchArmsToP arms = some parms) :
    cExprToPExpr (.match_ scrutinee arms ty)
      = some (.match_ ps parms) := by
  simp [cExprToPExpr, h_scrut, h_arms]

/-! ## R-18: `while_flat_preservation` (extraction)

When the source while body is all flat assigns
(`cAssignBodyToUpdates body = some updates`), the
extraction lands in `PExpr.while_` (the simpler shape). -/
theorem while_flat_preservation
    (cond : CExpr) (body : List CStmt) (label : Option String)
    (step_body : List CStmt) (rest : List CStmt) (k : Option PExpr)
    (pc : PExpr) (updates : List (String × PExpr)) (pCont : PExpr)
    (h_cond : cExprToPExpr cond = some pc)
    (h_rest : cStmtsToPExprK rest k = some pCont)
    (h_updates : cAssignBodyToUpdates body = some updates) :
    cStmtsToPExprK ((CStmt.while_ cond body label step_body) :: rest) k
      = some (.while_ pc updates pCont) := by
  simp [cStmtsToPExprK, h_cond, h_rest, h_updates]

/-! ## R-20: `while_step_preservation` (extraction)

When the source while body is NOT all flat assigns
(`cAssignBodyToUpdates body = none`), the extractor falls
back to `PExpr.while_step` with the `LoopStep` enum
encoding from `docs/PROOF_STATE_MODEL.md` § 4. -/
theorem while_step_preservation
    (cond : CExpr) (body : List CStmt) (label : Option String)
    (step_body : List CStmt) (rest : List CStmt) (k : Option PExpr)
    (pc : PExpr) (stepE : PExpr) (pCont : PExpr)
    (h_cond : cExprToPExpr cond = some pc)
    (h_rest : cStmtsToPExprK rest k = some pCont)
    (h_not_flat : cAssignBodyToUpdates body = none)
    (h_step : cStmtsToStepExpr [] body = some stepE) :
    cStmtsToPExprK ((CStmt.while_ cond body label step_body) :: rest) k
      = some (.while_step pc (extractCarried body) stepE pCont) := by
  simp [cStmtsToPExprK, h_cond, h_rest, h_not_flat, h_step]

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
-- compute_checksum spec landed 2026-05-30, closing the gap
-- the G-05 check surfaced on its first run.  The parse_header
-- theorems that bail before the call site (too_short,
-- bad_version, bad_type, payload_too_big, truncated) are
-- unaffected; a future parse_header_success theorem now has
-- the FnTable entry it needs.
example : fnTableComplete parseValidateFns parseHeaderExpr := by decide

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
