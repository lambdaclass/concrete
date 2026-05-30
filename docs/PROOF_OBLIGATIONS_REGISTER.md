# ProofCore Pass Contracts and Phase 12 Obligations Register

Status: contract document (ROADMAP Phase 4 item 5).

This document is the **per-rule register** of Phase 4's
extraction surface.  For every PExpr construct, evaluator rule,
and trust gate, it records:

- What Concrete source construct it covers
- What assumptions it introduces
- What it explicitly rejects
- Which example / regression forced it
- What Phase 12 preservation / soundness theorem will
  eventually justify it

The purpose is **load-bearing transparency**.  Today the
compiler extracts source to PExpr and the Lean kernel checks
theorems against extracted forms.  That chain is only sound if
every link's contract is honored.  This document is the link
inventory.  When Phase 12 starts proving preservation, every
entry here gets a corresponding Lean theorem (or an explicit
"this entry is unsoundable as stated; rethink").

## Reading the table

Each rule has a stable identifier `R-<NN>` referenced from
commit messages and Phase 12 obligations.  Entries are
chronological (earliest extension first); the register is
append-only — a removed rule is marked `(retired YYYY-MM-DD)`
in place, not deleted.

The Phase 12 obligation field names the theorem Concrete owes
the user.  A blank field means the obligation is unstated
(should never be blank for a landed rule); `TBD` means stated
informally below the table but not yet a Lean theorem name.

## Register

| ID | PExpr shape | Concrete source | Forcing example | Phase 12 obligation |
|---:|---|---|---|---|
| R-01 | `lit (int n)` | `42`, `0x10`, `-1` | parse_validate | `lit_int_preservation` — **fully discharged against `cExprToPExpr` 2026-05-30** in `Concrete.ProofSoundness`; non-partial wrapper landed same day |
| R-02 | `lit (bool b)` | `true`, `false` | parse_validate | `lit_bool_preservation` — **fully discharged against `cExprToPExpr` 2026-05-30** in `Concrete.ProofSoundness` |
| R-03 | `var n` | identifier reference | parse_validate | `var_preservation` — **fully discharged against `cExprToPExpr` 2026-05-30** in `Concrete.ProofSoundness`; wrapper arm landed same day |
| R-04 | `binOp pop pl pr` (all widths) | `a + b`, `a - b`, `a * b`, etc. | parse_validate | `binop_preservation` + `eval_binop_reduces` + `source_binop_step` — **fully discharged across all three views 2026-05-30** in `Concrete.ProofSoundness` (compositional template) |
| R-05 | `binOp .eq/.ne/.lt/.le/.gt/.ge` | `a == b`, `a < b`, etc. | parse_validate | `binop_cmp_preservation` — subsumed by R-04 (comparisons go through the same wrapper arm) |
| R-06 | `letIn name v body` | `let x = v; ...` | parse_validate | `let_preservation` + `eval_let_reduces` + `source_let_step` — **fully discharged across all three views 2026-05-30** in `Concrete.ProofSoundness`; first rule using the `cStmtsToPExprK` wrapper |
| R-07 | `ifThenElse c t e` | `if c { t } else { e }` and `if c { return X; } else-fallthrough` | parse_validate | `if_no_else_as_fallthrough_preservation` — **extraction discharged against `cStmtsToPExprK` 2026-05-30**; full if-else forms still open |
| R-08 | `call fn args` | function call (FnTable lookup) | parse_validate | `call_preservation` |
| R-09 | `structLit name fields` | `Point { x: 1, y: 2 }` | parse_validate (parse_header Ok) | `struct_lit_preservation` |
| R-10 | `fieldAccess obj f` | `obj.f` | parse_validate | `field_access_preservation` — **extraction discharged against `cExprToPExpr` 2026-05-30** |
| R-11 | `enumLit ename var fields` | `Result::Ok { value: v }` | parse_validate (parse_header Err) | `enum_lit_preservation` |
| R-12 | `match_ scrutinee arms` (`PMatchPat`: `enumPat` / `litPat` / `varPat`) | `match c { Pat => body, ... }` | parse_validate (`error_code`) | `match_preservation` + per-arm match for each `PMatchPat` variant |
| R-13 | `arrayIndex arr idx` | `arr[i]` (read) | parse_validate (compute_checksum) | `array_index_preservation` — **extraction discharged against `cExprToPExpr` 2026-05-30**; OOB returns `none` |
| R-14 | `cast inner` (identity on `PVal.int`) | `x as i32`, `u8 as i32` | fixed_capacity (`read_u8`, `read_u16_be`) | `cast_identity_preservation` + `eval_cast_identity` — **extraction discharged against `cExprToPExpr` 2026-05-30**; sound for widening; narrowing is excluded by eligibility |
| R-15 | `arrayLit elems` | `[0, 0, 0, ...]` | fixed_capacity (`ring_new`) | `array_lit_preservation` |
| R-16 | `binOp (.mod width signed)` (BitVec.srem / BitVec.umod) | `a % b` for `i32`/`u32` (other widths reject at extraction) | fixed_capacity (`ring_push`'s `head % cap`) | `mod_width_preservation` — `.mod 32 true` matches LLVM `srem`; `.mod 32 false` matches `urem` (commits 2605fb5, multi-width landed later) |
| R-17 | `binOp (.bitxor width signed)` (BitVec.xor at width; signed/unsigned result interpretation) | `a ^ b` for `i32`/`u32`/`u8` (other widths reject at extraction) | fixed_capacity (`compute_tag`), parse_validate (`compute_checksum`), constant_time_tag (`ct_compare`) | `bitxor_width_preservation` — BitVec.xor at the operand width; signed view via `BitVec.toInt`, unsigned via `Int.ofNat ∘ toNat` |
| R-21 | `binOp (.bitor width signed)` (BitVec.or at width; signed/unsigned result interpretation) | `a \| b` for `u8` (other widths reject at extraction) | constant_time_tag (`ct_compare` OR-accumulate) | `bitor_width_preservation` — BitVec.or at the operand width; unsigned view via `Int.ofNat ∘ toNat` (the only mode currently supported) |
| R-18 | `while_ cond assigns cont` (flat-assign body) | `while cond { x = ...; y = ...; }` | parse_validate (`compute_checksum`), fixed_capacity (`compute_tag`) | `while_flat_assign_preservation`; termination via fuel only — `bounded` profile enforces real termination at Check |
| R-19 | `arraySet arr idx val` | `arr[i] = v` extracted as shadowing `letIn name (arraySet …)` | fixed_capacity (`ring_push`) | `array_set_preservation` — **extraction discharged against `cStmtsToPExprK` 2026-05-30** (named in `docs/PROOF_STATE_MODEL.md` § 2); OOB stuck |
| R-20 | `while_step cond carried step cont` + `LoopStep::Cont`/`Break` enum | `while cond { let x = ...; if c { return v; } i = i + 1 }` | fixed_capacity (`ring_contains`) | `while_step_preservation` + `while_step_early_break` (PROOF_STATE_MODEL § 4) |

## Trust gates (verification, not extraction)

These are verification layers on top of the extraction rules.
They don't add PExpr constructs but do constrain what "proved"
means.

| ID | Gate | What it verifies | Phase 12 obligation |
|---:|---|---|---|
| G-01 | Body fingerprint check | Source `CExpr/CStmt` fingerprint matches `proof-registry.json`'s registered `body_fingerprint`.  Drift downgrades obligation status to `stale`. | `fingerprint_determinism`: source produces a single canonical fingerprint string; equivalent source produces same fingerprint. |
| G-02 | Spec drift check (commit `f371cc1`) | For every registered proof, the source-extracted PExpr equals `normalizePExpr (Concrete.Proof.specs[function])`.  Mismatch downgrades obligation status to `stale` and surfaces `RegistryIssue.specDrift`.  Regression: `tests/programs/adversarial_spec_drift/test_drift.con` with `driftTestSpec` deliberately differing from source; CI asserts the gate fires and status downgrades. | `spec_drift_completeness`: the gate fires iff source-extracted PExpr disagrees with registered spec; no false negatives. |
| G-03 | Registered-proof attachment | `proof-registry.json` references a Lean theorem by name; the Lean module containing the theorem is part of `make build` (Lean kernel rejection fails the build). | `theorem_lookup_completeness`: every registered `proof` field resolves to a Lean theorem; theorem statement uses the registered `spec` symbol. |
| G-04 | Eligibility profile | Functions can only be registered if they pass `predictable` (no recursion, bounded loops, no alloc, no FFI).  Restricts what shapes ever reach extraction. | `eligibility_preservation`: a proof-eligible function's source semantics agrees with PExpr semantics within the modeled fragment. |
| G-05 | FnTable completeness check (commit 2026-05-30 landing) | For every (registered spec, FnTable) pair, every `.call X` site in the spec resolves to `some _` in the FnTable.  A missing entry makes `eval` silently return `none` and the theorem becomes vacuous.  Enforced by `decide`-checked `example` blocks in `Concrete.ProofSoundness`; failure surfaces as a Lean build error. | `fntable_completeness_completeness`: no false negatives — if the assertion fires, the gap is real. |

## Per-rule notes

### R-01, R-02 (literal preservation — Phase 12 first batch)

`Concrete.ProofSoundness.lit_int_preservation` and
`lit_bool_preservation` establish the Phase 12 proof
pattern: a tiny source semantics (`evalSourceLit`) for the
literal fragment of `CExpr`, a non-partial literal
extractor (`cExprLitToPExpr`), and a preservation theorem
stating that the three views (extraction, PExpr eval,
source semantics) agree.

The pattern, in shape:

    theorem lit_X_preservation (...) :
        cExprLitToPExpr (.XLit v ...) = some (.lit (.X v))
      ∧ eval fns env (fuel + 1) (.lit (.X v)) = some (.X v)
      ∧ evalSourceLit (.XLit v ...) = some (.X v)

closed by `rfl` + `simp [eval]` + `rfl`.  Generalizes
structurally to each future literal rule.

**Fully discharged 2026-05-30** (against the real
public `cExprToPExpr`, not just a helper).

The lift architecture: `cExprToPExpr` is now a
non-partial wrapper in `ProofCore.lean` that handles
`.intLit` and `.boolLit` definitionally and delegates
all other cases to a renamed `cExprToPExprImpl` (the
old partial-def, still in the mutual block).  Because
the wrapper is plain `def`, Lean reduces
`cExprToPExpr (.intLit n ty)` to `some (.lit (.int n))`
by `rfl`.

Public API unchanged: every caller of `cExprToPExpr`
(Report, the rest of ProofCore) gets identical
behavior.  The mutual block's internal recursive calls
now go through `cExprToPExprImpl` (just a name change).

**What this unlocks for R-03..R-21.**  The wrapper
pattern is the template.  Each future rule whose
top-level case sits OUTSIDE the partial-def mutual
block can be discharged the same way: add it as a wrapper
arm before the `| e => cExprToPExprImpl e` fallback.
Rules whose extraction logic genuinely lives inside the
mutual block (binops with sub-expressions, structLit
with mapM over fields, etc.) still need the harder
mapM-to-structural-recursion lift — that's the next
Phase 12 architectural piece.

### R-06 (let preservation — first rule via `cStmtsToPExprK` wrapper)

R-06's extraction lives in `cStmtsToPExprK` (the CStmt-list
extractor), not `cExprToPExpr` — `let x = v;` is a statement,
not an expression.  Same wrapper-extension pattern as R-04
but applied to a different mutual-block function.

`cStmtsToPExprK` was renamed to `cStmtsToPExprKImpl` (in the
mutual block, 12 internal call sites updated) and a new
non-partial `cStmtsToPExprK` wrapper sits OUTSIDE the
block, handling `.letDecl :: rest` directly:

    def cStmtsToPExprK : List CStmt → Option PExpr → Option PExpr
      | (.letDecl name _ _ val) :: rest, k => do
          let pv ← cExprToPExpr val      -- expression wrapper
          let pb ← cStmtsToPExprK rest k -- recurse through wrapper
          some (.letIn name pv pb)
      | stmts, k => cStmtsToPExprKImpl stmts k

Recursion is structural on `rest` (shorter list).  Lean
accepts.

Three theorems discharge R-06 across all views, mirroring
R-04's template: `let_preservation` (extraction antecedent
with val + rest hypotheses), `eval_let_reduces` (eval-side
compositional reduction), `source_let_step` (source-side
step lookup via `evalSourceLetStep`).  Each takes
operand-level hypotheses from the rules below.

This is the second pattern-instance after R-04 — the
compositional template generalizes from `cExprToPExpr`
wrapper rules to `cStmtsToPExprK` wrapper rules.

### R-04 (binop preservation — first compositional rule, all three views)

R-04 is the FIRST Phase 12 rule whose preservation depends
on sub-rule preservation (R-01 / R-03 for the operands).
Discharged across all three views by three theorems:

**Extraction antecedent.**  `binop_preservation` takes
hypotheses about operand extraction and concludes the
composite extraction:

    theorem binop_preservation ... :
        binOpToPBinOp op (CExpr.ty lhs) = some pop →
        cExprToPExpr lhs = some pl →
        cExprToPExpr rhs = some pr →
        cExprToPExpr (.binOp op lhs rhs ty)
          = some (.binOp pop pl pr)

Closed by `show / rw / rfl`.  The wrapper arm:

    | .binOp op lhs rhs _ => do
        let pop ← binOpToPBinOp op (CExpr.ty lhs)
        let pl ← cExprToPExpr lhs    -- recursion through wrapper
        let pr ← cExprToPExpr rhs    -- recursion through wrapper
        some (.binOp pop pl pr)

Lean accepts structural recursion on `lhs`/`rhs` (no
`mapM` — one sub-expression per call).

**Eval-side compositional reduction.** `eval_binop_reduces`
takes operand eval facts and concludes the composite eval:

    theorem eval_binop_reduces ... :
        eval fns env (fuel+1) pl = some vl →
        eval fns env (fuel+1) pr = some vr →
        eval fns env (fuel+1) (.binOp pop pl pr)
          = evalBinOp pop vl vr

Closed by `simp [eval, h_lhs_eval, h_rhs_eval]`.

**Source-side compositional reduction.** `source_binop_step`
discharges the source-semantics view:

    theorem source_binop_step (pop : PBinOp) (vl vr : PVal) :
        evalSourceBinOpStep pop vl vr = evalBinOp pop vl vr := rfl

Stated as a step lookup rather than a recursive source-eval
function — recursing through `cExprToPExprImpl`-shape
constructs (not-yet-discharged R-08 / R-09 / R-11 / R-15)
would re-create the `partial def` opacity on the source
side.  Callers provide operand source-semantics agreement
via the lower rules' theorems (R-01 / R-03).

The compositional pattern: every future recursive-shape rule
provides three theorems matching R-04's shape — extraction
antecedent, eval reduction, source step — each taking
operand-level hypotheses from the rules below.

`binOpToPBinOp` made public (was `private`) so the
preservation theorems in `ProofSoundness` can reference it.
No other call sites changed.

### R-03 (identifier preservation — second wrapper rule)

`var_preservation` (landed 2026-05-30) extends the wrapper
to `.ident` and the source semantics to identifier lookup:

    def evalSourceIdent (env : Env) : CExpr → Option PVal
      | .ident name _ => env name
      | _             => none

    theorem var_preservation (name : String) (ty : Ty) ... :
        cExprToPExpr (.ident name ty) = some (.var name)
      ∧ eval fns env (fuel+1) (.var name) = env name
      ∧ evalSourceIdent env (.ident name ty) = env name

Same structural pattern as R-01/R-02.  The result depends
on `env` (unlike literals); both eval and source semantics
use the same `Env` shape, so "lookup agreement" is
structural.

Discharges against the REAL `cExprToPExpr` via the
non-partial wrapper arm.  Public API unchanged.

`cExprIdentToPExpr` added in `ProofCore` as a redundant
literal-style spec helper (mirrors `cExprLitToPExpr`).

### R-14 (cast)

PExpr models cast as identity on `PVal.int`.  This is sound for
**widening** casts in our flagships (`u8 as i32`, `i32 as i64`)
because mathematical `Int` carries no width and widening from
narrower to wider preserves value.  **Narrowing** casts
(`i32 as u8`) are NOT modeled: a proof that assumes narrowing
truncates would be wrong here.  The eligibility profile is
expected to keep narrowing casts out of proof-eligible code,
or to attach an explicit `narrow_int` side-condition the
extractor preserves.  Phase 12 obligation
`cast_identity_preservation` will state this restriction
explicitly.

### R-21 (bitor — typed BitVec round-trip)

New constructor introduced by constant_time_tag's
OR-accumulate loop body (`diff = diff | (a[i] ^ b[i])`).
Same typed-BitVec round-trip shape as R-17 (bitxor):

    evalBinOp (.bitor 8 false) (.int a) (.int b) =
      some (.int (Int.ofNat ((BitVec.ofInt 8 a) ||| (BitVec.ofInt 8 b)).toNat))

`evalBinOp` supports `bitor 8 false` only today (the
u8-unsigned case ct_compare actually uses).  Signed-mode
`bitor` and other widths are append-only follow-ups, each
requiring one `evalBinOp` case + one `binOpToPBinOp`
mapping + an updated row in this register.

Inline regression theorems in `Concrete/Proof.lean` pin the
u8 unsigned behavior:
- `128 | 0 = 128`  (high bit set, still positive)
- `128 | 1 = 129`

### R-16, R-17 (mod, bitxor — typed BitVec round-trip)

`PBinOp.mod (width : Nat) (signed : Bool)` and
`PBinOp.bitxor (width : Nat) (signed : Bool)` both carry
operand width AND a signedness tag.  The signedness tag
controls how the `BitVec` result is reinterpreted as `Int`:
signed view via `BitVec.toInt` (two's-complement), unsigned
via `Int.ofNat ∘ toNat` (always non-negative).

`evalBinOp` supports four (width, signed) combinations today:

    evalBinOp (.bitxor 32 true)  (.int a) (.int b) =     -- i32 xor
      some (.int (BitVec.toInt (BitVec.ofInt 32 a ^^^ BitVec.ofInt 32 b)))

    evalBinOp (.bitxor 32 false) (.int a) (.int b) =     -- u32 xor
      some (.int (Int.ofNat ((BitVec.ofInt 32 a) ^^^ (BitVec.ofInt 32 b)).toNat))

    evalBinOp (.mod 32 true)  (.int a) (.int b) =        -- i32 srem
      some (.int (BitVec.toInt (BitVec.srem (BitVec.ofInt 32 a) (BitVec.ofInt 32 b))))
        when b ≠ 0

    evalBinOp (.mod 32 false) (.int a) (.int b) =        -- u32 urem
      some (.int (Int.ofNat (BitVec.umod (BitVec.ofInt 32 a) (BitVec.ofInt 32 b)).toNat))
        when b ≠ 0

The unsigned cases use `Int.ofNat ∘ toNat`, not `BitVec.toInt`.
This matters when the result's high bit is set: signed view
surfaces as a negative `Int` (which is correct for `i32` but
WRONG for `u32`); unsigned view surfaces as a large positive
`Int` (correct for `u32`).  Inline regression theorems in
`Concrete/Proof.lean` pin this behavior:
- `0xFFFFFFFF ^ 0` signed = `-1`, unsigned = `4294967295`
- `0xFFFFFFFF mod 4` signed = `-1`, unsigned = `3`

`BitVec.srem` matches LLVM `srem`; `BitVec.umod` matches
`urem`.  Both are what `EmitSSA.lean` emits for the
respective sign at i32 width.

**Width extension via the register, not silent fallback.**
`binOpToPBinOp` (in `ProofCore.lean`) reads the operand type
from the source CExpr and emits the matching typed PBinOp.
Source operations at widths other than 32 fail extraction
explicitly (`cExprToPExpr` returns `none`), surfacing as a
precise blocker (`unsupported operator: Concrete.BinOp.X at
Concrete.Ty.YY`) instead of silently re-using i32 semantics.
Adding a new width (e.g. i64 srem) means:
  1. Add the corresponding `evalBinOp` case.
  2. Add the `binOpToPBinOp` mapping (`.i64` / `.u64` / etc.).
  3. Append a row to this register naming the new obligation.
The Phase 12 obligations `mod_width_preservation` and
`bitxor_width_preservation` are parameterized over width and
signedness — one preservation theorem per supported width
combination.

### R-19 (arraySet)

PExpr is a pure value language; mutation is encoded as
functional update + name rebind.  Source-level `arr[i] = v`
extracts to `letIn name (arraySet (var name) idx val) rest` —
a shadowing let that rebinds the array variable.  OOB
(i < 0 or i ≥ length) returns `none` (stuck), matching the
state-model decision in `docs/PROOF_STATE_MODEL.md` § 5
("failure is `none`").  Theorems pass hypotheses to rule out
the stuck case.  Phase 12 obligation
`array_set_preservation` will state: source's mutation of
`arr[i]` after `i` is in bounds produces an array whose `j`th
element is `v` if `j = i` and the original `j`th otherwise.

### R-20 (while_step + LoopStep)

Bounded while loops with rich bodies (nested let, if-with-
return, loop-carried assigns) extract via `while_step cond
carried step cont`.  Step evaluates to a `PVal.enum_
"LoopStep" Cont [(name, val), ...]` to continue (env rebinds
named values) or `Break [("value", v)]` to early-return.

The factoring of `eval.evalWhileStep` (commit `7743579`) is
the durable proof surface: every future while_step proof uses
the three lemmas `while_step_break` / `_cont` / `_exit`
rather than expanding the full eval pattern.  Phase 12
obligations `while_step_preservation` and
`while_step_early_break` will state preservation against
`eval.evalWhileStep`, not the monolithic `eval`.

### G-05 (FnTable completeness)

A `partial def` FnTable is a `String → Option PFnDef`.  When
a registered spec calls `.call X args` but the FnTable has
no entry for `X`, `eval` silently returns `none` on that
call site.  If the theorem's proof structure happens to not
reach that site (e.g. an early-return bails before), the
theorem still kernel-checks — but it proves a vacuous or
narrowly-scoped claim, not the universal property the
reader expects.

The check (commit landing 2026-05-30):

    def fnTableComplete (table : FnTable) (pe : PExpr) : Bool :=
      (pexprCalls pe).all (fun name => (table name).isSome)

with `decide`-closed `example` assertions per (spec,
FnTable) pair in `Concrete.ProofSoundness`.  A missing
entry surfaces as a Lean compile error at `make build`.

**Gap surfaced on first run, CLOSED same day.**  The
check's initial run reported that `parseHeaderExpr` calls
`compute_checksum` but `parseValidateFns` had no entry —
no `computeChecksumExpr`/`Fn` existed for parse_validate's
while-loop XOR-fold function.  The shipped failure-
direction parse_header theorems were safe (early-return
bails before the call site) but precarious: a future
parse_header_success theorem would have silently broken.

`computeChecksumExpr` + `computeChecksumFn` landed
2026-05-30 with the spec mirroring the extracted
fingerprint; `parseValidateFns` extended; the
`parseHeaderExpr` assertion re-enabled and now closes by
`decide`.  Production proof-status totals unchanged
(3 proved, 0 stale).  All 12 production specs are now
FnTable-complete by build-time check.

**One-level today; transitive follow-up.**  Today's
check is one-level (direct call sites in the spec).  It
does NOT walk callee bodies recursively.  No current
flagship's registered spec has a multi-level call chain
where the top-level callee's body itself calls something
missing.  When that arises (e.g. crypto_verify's
`verify_message` calling `verify_tag` calling
`compute_tag`), a transitive variant — reusing
`ProofCore`'s call-graph builder — is the next refinement.

### G-02 (spec drift gate)

The trust gate from commit `f371cc1`, with a **checked-in
regression** under
`tests/programs/adversarial_spec_drift/test_drift.con` that
exercises it every CI build:

- `simple_add(a, b: i32) -> i32 { return a + b; }` extracts
  to `binOp .add (var a) (var b)`.
- The registry's `body_fingerprint` matches the current source
  (so the existing `staleFingerprint` check does NOT fire).
- The registered Lean spec is `Concrete.Proof.driftTestSpec =
  .lit (.int 42)` — deliberately wrong.
- CI asserts the gate produces the diagnostic
  `spec drift for 'test_drift.simple_add' — registered spec
  'Concrete.Proof.driftTestSpec' (via Concrete.Proof.specs)
  does not match the source-extracted PExpr` AND downgrades
  obligation status from `proved` to `stale`.

If either link breaks (the gate stops detecting drift, or the
status pipeline stops downgrading), the test fails.  This is
the regression that prevents future commits from silently
bypassing the gate.

In production: exercised by 11 attached proofs every CI build
(parse_validate's 3, crypto_verify's 4, fixed_capacity's 4)
with zero false positives.  The fixture is the 12th entry in
`Concrete.Proof.specs` and is the only one where the spec
deliberately doesn't match the source.

## What's NOT yet in this register (intentional)

Pending extension rules — designed in `docs/PROOF_STATE_MODEL.md`
but not yet implemented:

- **`structSet`** (PROOF_STATE_MODEL § 3): functional struct
  field update.  Source `obj.f = v` extracts to `letIn name
  (structSet (var name) "f" val) rest`.  No flagship yet
  forces it; ring_push only mutates an array field.
- **Multi-width PBinOp at non-32 / non-u8 widths** (ROADMAP
  Phase 4 item 7): all width-sensitive constructors are
  parameterized (`.mod width signed` / `.bitxor width signed`
  / `.bitor width signed`).  evalBinOp currently supports
  i32/u32 (mod, bitxor), u8 (bitxor, bitor).  Adding u16 /
  i64 / etc. is now a one-case-per-(op, width, signed)
  addition with no shape change.  The
  `adversarial_pbinop_widths` regression in
  `tests/programs/` exercises a still-unsupported width
  (u16 today) and asserts the precise diagnostic; when a
  flagship forces u16 support, the regression pivots to the
  next unsupported width.
- **Multi-iteration while_step proofs**: today
  `ring_push_then_contains_correct` proves the
  single-iteration case.  Future work needs an inductive
  invariant attached to while_step bodies (PROOF_STATE_MODEL
  § 8 "Loop invariants").

Each will get a register entry when it lands.

## How to use this document

**When you land a new extraction rule.** Add a row to the
table.  Pick the next R-NN.  Name the forcing example and the
Phase 12 obligation.  Don't leave the obligation field blank.

**When you land a new trust gate.** Add a row to the gates
table.  Pick the next G-NN.  Name the verification surface
and the corresponding Phase 12 completeness theorem.

**When Phase 12 starts.** Each obligation field is a TODO for
a Lean theorem.  Implementing them is the Phase 12 plan.  The
absence of an obligation entry would be the bug — every rule
that affects what "proved" means needs one.

**When reviewing a Phase 4 commit.** Cross-reference: does
the commit's changes match a register entry?  If new, was a
row added?  If the rule changed (e.g., a softening of
assumptions), was the row updated?  Stale rows are a real
signal.

## See also

- `docs/PROOF_STATE_MODEL.md` — the state-model design.  This
  register is partly the contract surface that doc generates.
- `docs/PROOF_AUDIT_PIPELINE.md` — the target proof pipeline.
- `CHANGELOG.md` — the chronological narrative of how each
  rule landed.  Pair this register with CHANGELOG entries for
  full context.
- `ROADMAP.md` Phase 12 — the formalization track that will
  eventually discharge every obligation here.
