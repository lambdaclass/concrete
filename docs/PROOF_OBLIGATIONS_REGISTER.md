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
| R-01 | `lit (int n)` | `42`, `0x10`, `-1` | parse_validate | `lit_int_preservation` |
| R-02 | `lit (bool b)` | `true`, `false` | parse_validate | `lit_bool_preservation` |
| R-03 | `var n` | identifier reference | parse_validate | `var_preservation` |
| R-04 | `binOp .add/.sub/.mul` | `a + b`, `a - b`, `a * b` | parse_validate | `binop_int_preservation` |
| R-05 | `binOp .eq/.ne/.lt/.le/.gt/.ge` | `a == b`, `a < b`, etc. | parse_validate | `binop_cmp_preservation` |
| R-06 | `letIn name v body` | `let x = v; ...` | parse_validate | `let_preservation` |
| R-07 | `ifThenElse c t e` | `if c { t } else { e }` and `if c { return X; } else-fallthrough` | parse_validate | `if_preservation` + `if_no_else_as_fallthrough` |
| R-08 | `call fn args` | function call (FnTable lookup) | parse_validate | `call_preservation` |
| R-09 | `structLit name fields` | `Point { x: 1, y: 2 }` | parse_validate (parse_header Ok) | `struct_lit_preservation` |
| R-10 | `fieldAccess obj f` | `obj.f` | parse_validate | `field_access_preservation` |
| R-11 | `enumLit ename var fields` | `Result::Ok { value: v }` | parse_validate (parse_header Err) | `enum_lit_preservation` |
| R-12 | `match_ scrutinee arms` (`PMatchPat`: `enumPat` / `litPat` / `varPat`) | `match c { Pat => body, ... }` | parse_validate (`error_code`) | `match_preservation` + per-arm match for each `PMatchPat` variant |
| R-13 | `arrayIndex arr idx` | `arr[i]` (read) | parse_validate (compute_checksum) | `array_index_preservation`; OOB returns `none` |
| R-14 | `cast inner` (identity on `PVal.int`) | `x as i32`, `u8 as i32` | fixed_capacity (`read_u8`, `read_u16_be`) | `cast_identity_preservation` — sound for widening; narrowing is excluded by eligibility |
| R-15 | `arrayLit elems` | `[0, 0, 0, ...]` | fixed_capacity (`ring_new`) | `array_lit_preservation` |
| R-16 | `binOp (.mod width signed)` (BitVec.srem / BitVec.umod) | `a % b` for `i32`/`u32` (other widths reject at extraction) | fixed_capacity (`ring_push`'s `head % cap`) | `mod_width_preservation` — `.mod 32 true` matches LLVM `srem`; `.mod 32 false` matches `urem` (commits 2605fb5, multi-width landed later) |
| R-17 | `binOp (.bitxor width signed)` (BitVec.xor at width; signed/unsigned result interpretation) | `a ^ b` for `i32`/`u32` (other widths reject at extraction) | fixed_capacity (`compute_tag`), parse_validate (`compute_checksum`) | `bitxor_width_preservation` — BitVec.xor at the operand width; signed view via `BitVec.toInt`, unsigned via `Int.ofNat ∘ toNat` |
| R-18 | `while_ cond assigns cont` (flat-assign body) | `while cond { x = ...; y = ...; }` | parse_validate (`compute_checksum`), fixed_capacity (`compute_tag`) | `while_flat_assign_preservation`; termination via fuel only — `bounded` profile enforces real termination at Check |
| R-19 | `arraySet arr idx val` | `arr[i] = v` extracted as shadowing `letIn name (arraySet …)` | fixed_capacity (`ring_push`) | `array_set_preservation` (named in `docs/PROOF_STATE_MODEL.md` § 2); OOB stuck |
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

## Per-rule notes

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
- **Multi-width PBinOp at non-32 widths** (ROADMAP Phase 4
  item 7): the constructor is parameterized
  (`.mod width signed` / `.bitxor width`); evalBinOp supports
  width 32 only.  Adding u8 / u16 / i64 etc. is now a
  one-case-per-width addition with no shape change.  The
  flagship-forcing example for the next width hasn't landed
  yet.
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
