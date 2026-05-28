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
| R-16 | `binOp .mod` (BitVec.srem at i32) | `a % b` for signed i32 | fixed_capacity (`ring_push`'s `head % cap`) | `mod_i32_srem_preservation` — matches LLVM `srem` (commit 2605fb5) |
| R-17 | `binOp .bitxor` (BitVec.xor at i32) | `a ^ b` for i32 | fixed_capacity (`compute_tag`), parse_validate (`compute_checksum`) | `bitxor_i32_preservation` — BitVec.xor at i32 width, round-trip via `BitVec.toInt` |
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
| G-02 | Spec drift check (commit `f371cc1`) | For every registered proof, the source-extracted PExpr equals `normalizePExpr (Concrete.Proof.specs[function])`.  Mismatch downgrades obligation status to `stale` and surfaces `RegistryIssue.specDrift`. | `spec_drift_completeness`: the gate fires iff source-extracted PExpr disagrees with registered spec; no false negatives. |
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

### R-16, R-17 (mod, bitxor — BitVec round-trip at i32)

`evalBinOp` for `.mod` and `.bitxor` converts operands to
`BitVec 32` via `BitVec.ofInt`, runs the BitVec operation, and
reinterprets via `BitVec.toInt`:

    evalBinOp .bitxor (.int a) (.int b) =
      some (.int (BitVec.toInt (BitVec.ofInt 32 a ^^^ BitVec.ofInt 32 b)))

    evalBinOp .mod (.int a) (.int b) =
      some (.int (BitVec.toInt (BitVec.srem (BitVec.ofInt 32 a) (BitVec.ofInt 32 b))))
        when b ≠ 0

`BitVec.srem` matches LLVM `srem` (which is what
`EmitSSA.lean` emits for signed `.mod`).  Width is hardcoded
to 32 — **this is a known limit** (ROADMAP Phase 4 item 7).
Until multi-width PBinOp lands, attaching a proof on a u8 /
u16 / i64 op would extract correctly but the proof would have
to acknowledge the width mismatch.  Phase 12 obligations
`mod_i32_srem_preservation` and `bitxor_i32_preservation`
will state the i32-width restriction.

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

The new trust gate from commit `f371cc1`.  Verified
end-to-end during development: deliberately drifted
`ringNewExpr` (changed head=0 to head=99 in the spec, updated
the theorem consistently so the Lean kernel still accepted
it), confirmed the gate fired:

    error: spec drift for 'fixed_capacity.ring_new' —
      registered spec 'Concrete.Proof.ringNewExpr' (via
      Concrete.Proof.specs) does not match the source-
      extracted PExpr; the theorem 'ring_new_correct' is
      about a different function than the source.

Totals dropped from "2 proved, 0 stale" to "1 proved, 1
stale" — `ring_new` correctly reclassified.  Reverted; report
back to clean.  The gate is in code, exercised by 9 attached
proofs every CI build with zero false positives.

## What's NOT yet in this register (intentional)

Pending extension rules — designed in `docs/PROOF_STATE_MODEL.md`
but not yet implemented:

- **`structSet`** (PROOF_STATE_MODEL § 3): functional struct
  field update.  Source `obj.f = v` extracts to `letIn name
  (structSet (var name) "f" val) rest`.  No flagship yet
  forces it; ring_push only mutates an array field.
- **Multi-width PBinOp** (ROADMAP Phase 4 item 7): currently
  i32 hardcoded.  Future work will extend `PBinOp` to carry
  `Ty` or split into typed ops.  Required before any u8 /
  u16 / i64 proof attaches.
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
