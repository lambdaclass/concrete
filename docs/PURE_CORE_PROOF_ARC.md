# Pure-core proof arc — entry contract

**Status:** Scope pinned 2026-07-16. Slice 1 not started.
This document is the arc's contract, written BEFORE the first theorem (the
open-ended item that can sprawl gets its Definition-of-Done first). It is the
Phase 7 exit obligation: convert the pure core from "well-gated and
workload-tested" to "machine-checked against its documented contracts."

## Why now

Two workloads (base64_cli, png_chunks) exercised and pinned the pure surface;
the hardening pass (H1–H5, `check_std_hardening.sh`) sharpened the
domain-failure/trap boundary those contracts state. Proving before hardening
would have machine-checked wrong semantics (parse_hex silently wrapped until
H1).

## Evidence classes — Lean is the judge, SMT is a worker

- **`proved`** (rendered as `proved_by_kernel_decision` / Lean-checked): ONLY
  evidence replayed through the Lean kernel. This is the top class and the
  only one the word "proved" may describe.
- **`solver_checked`** (SMT-only): legitimate for arithmetic bounds, overflow
  preconditions, cursor invariants, and counterexample discovery — reported
  as HELPER evidence, never silently upgraded to `proved`. A later Lean
  replay upgrades it explicitly.
- Existing conventions carry over: `bv_decide` native trust stays gated by
  `check_axiom_inventory.sh`; the report/ledger renders the class it has,
  never a stronger one.

Rationale: Concrete's public thesis is one checkable evidence story. A core
stdlib guarantee that depends on solver behavior without replay expands the
trusted base and weakens the claim.

## Definition of Done (per API)

An API counts as proved when ALL of:
1. A named theorem exists in `proofs/Examples/PureCore/` (the Examples lib —
   proofs must be IMPORT-REACHABLE; `check-proofs` verifies via
   `import Concrete; import Examples; #check`, so a non-reachable proof is
   decorative by construction).
2. The theorem is stated against the MODEL (`Concrete.Proof` PExpr specs
   registered in the spec-drift oracle), not the Lean-side re-implementation
   of the function.
3. The `.con` source carries `#[proof_by(...)]` + `#[proof_coverage(...)]` +
   `#[proof_fingerprint(...)]` so spec drift is detected when the body
   changes (the HMAC/ParseValidate pattern, now applied to std).
4. The manifest/report marks EXACTLY the proved APIs as proved — no
   spillover to siblings.
5. Mutation check: changing the body or the spec makes the proof/report gate
   fail (proofs must be load-bearing, per the 6C mutation discipline).

## Slice 1 (small vertical, before any breadth)

ORDER (revised 2026-07-16): stdlib has ZERO proof links today, so slice 1
optimizes for the vertical path working end-to-end, not theorem volume.
Start with the least callback-heavy target and wire ONE API all the way to
`proved` (including the mutation test) before expanding:

1. `Bytes.view` — proves the stdlib proof-link machinery works.
2. The H1 radix-overflow guard step fact.
3. Then `Option.map` / `Result.map` structural laws (they expose exactly
   what the model needs next for callbacks).

Scoped to keep the first theorems small (match_/enumLit/call-by-name with a
representative callback; loops exist in the model — `while_`/`while_step` —
but whole-loop invariant proofs are deferred to slice 2+):

- `Option.map` / `Option.and_then`: structural laws — `map(None) = None`,
  `map(Some v) = Some (f v)` for a registered representative callback,
  universally quantified over the payload value. Coverage attribute records
  that the callback is representative, not quantified (an honest limitation,
  see Model gaps).
- `Result.map` / `Result.map_err`: same shape (Ok/Err structural laws).
- One `Bytes` contract: `view` in-range/out-of-range behavior stated against
  the abstract byte-sequence model (`view(start, len)` is `Some` iff
  `start ≤ len_total ∧ len ≤ len_total - start`, H2's overflow-safe form —
  the trusted pointer body is REFINED against the model, the model is what
  the theorem quantifies).
- One parser edge: the H1 radix-overflow guard fact — the hex accumulator
  step preserves `result ≤ u64::MAX` given the guard (SMT-assisted if
  useful, reported as `solver_checked` unless Lean-replayed).

## Slice 1 status (2026-07-16)

COMPLETE — the vertical works end-to-end and the slice's target surface is
linked. Four kernel-backed stdlib proof links: `Option.unwrap_or`,
`Option.map`, `Result.map`, `Result.map_err` — each `proof-status`
fingerprint-fresh and `check-proofs` kernel-verified through the Examples
import ("4 verified, 0 failed"); a body mutation flips to STALE (the gate's
mutation leg pins this). The map laws use a registered representative
callback (`f(x) = x*3+1` in the spec fn-table) with
`#[proof_coverage(representative)]` recording the limitation. The H1
radix-overflow guard step is kernel-checked (`hex_guard_step_preserves_u64`,
by omega — kernel-decision class, no solver needed) and referenced by a
`parse_hex` source comment. Gate: `check_purecore_proofs.sh` (15 checks,
`make test-purecore-proofs`).

Two pipeline discoveries, both fixed/recorded:
- Proof attributes did not PARSE on impl methods (flagships only ever
  attributed free fns) — the impl-method loops now accept
  spec/proof_by/proof_coverage/proof_fingerprint, and
  `synthesizeSourceLinks` walks impl methods (core names resolved by unique
  suffix match; nested module paths re-qualified).
- The registry REJECTS links on trusted fns ("ineligible: from trusted
  impl") — principled, and it means `Bytes.view` cannot be the proved
  exemplar. Its guard-and-geometry theorem
  (`bytes_view_guard_correct`) IS kernel-checked in the Examples lib and
  referenced by a source comment; a distinct trusted-refinement link class
  (`#[model_refined_by]`-style, rendered as its own evidence class, never
  `proved`) is the recorded follow-up for linking it.
- The registry also requires a whole-function `#[spec]` on every link
  ("empty spec name" is an error) — so a STEP lemma (the H1 guard fact)
  cannot ride `#[proof_by]` either. Same resolution as the trusted case:
  kernel theorem + source comment now, the partial/refinement link class
  later. Both exclusions are the machinery refusing overclaims, which is
  the behavior we want.

## Model gaps (recorded, not hidden)

- CORRECTION (2026-07-16): PExpr DOES have loops — `while_` and
  `while_step` constructors exist and eval threads fuel through them (the
  earlier draft of this section claimed otherwise). Full `parse_hex`
  correctness is therefore a MODELING-EFFORT gap (string/char iteration +
  the loop invariant), not a model-capability gap. Slice 1 proves the
  GUARD step only, deliberately: whole-loop correctness is slice-2+
  territory, and the registry's whole-function `#[spec]` requirement keeps
  the step lemma from overclaiming in the report.
- No callback quantification: HOF laws are proven for representative
  callbacks; quantified versions need the fn-table hypothesis pattern
  (`h : fns "f" = some fBody`) — deferred until slice 1 shows the shape.
- Trusted bodies (Bytes internals) are refined against models, never
  "proved" directly — the trusted-boundary class remains visible.

## Slice 2 status (2026-07-16, in progress)

**Stage 1 — numeric checked helpers: DONE.** Five more kernel-backed links
(9 total): `NonZeroU32/NonZeroU64/Port::try_new` (one shared spec+theorem,
`iff` coverage over all values) and `NonZeroU32::try_from_u64` /
`Port::try_from_u32` (shared parameterized spec `numericTryFromExpr max`,
instantiated at u32::MAX / u16::MAX). Narrowing casts in the try_from pair
are guard-dominated, so the model's identity `cast` is faithful on every
reached path — noted in the spec docstring.

**Finding en route (fixed): spec-drift coverage was silently keyed.** The
drift check looks specs up by the registry entry's QUALIFIED name; slice 1's
keys (`option.map` vs `std.option.option_Option_map`) never matched, so the
spec↔source comparison silently did not run for the stdlib links —
fingerprint freshness ran, the model tie did not. Fixed three ways:
1. slice-1 spec keys re-qualified;
2. one shared lookup (`Proof.specFor`) now feeds BOTH the drift check and
   a new per-entry `proof-status` line — `spec: drift-checked` /
   `spec: NOT drift-covered` — so the rendered state is a faithful witness
   of what the drift check consulted (no more silent-uncovered);
3. the gate requires all 9 std links `drift-checked` and 0 uncovered.
The `fabricated_proof` adversarial fixture now visibly renders
`NOT drift-covered`, which is that fixture doing its job.

**Stage 2 — checked conversions: DONE.** `base64.char_of` and
`base64.val_of` (the RFC 4648 alphabet and its inverse, workload-1's pull)
are kernel-linked with total (`iff`) coverage — 11 std links — plus the
ROUNDTRIP corollary `base64_alphabet_roundtrip` (`val_of(char_of(v)) = v`
for all 6-bit v) over the semantic mirror functions. Two machinery notes:
- `val_of`'s `if lo { if hi { return } }` fall-through guards duplicate the
  continuation into both branches (cStmtsToPExprK's if-without-else rule);
  the spec shares the K levels as defs and the theorems layer one lemma per
  level, so the proof is 3 cases per level instead of a 31-leaf tree.
- Free-fn links in std submodules were mis-qualified TWICE over (nested
  module path not accumulated; Core module-prefix name mangling not
  resolved) — fixed in `synthesizeSourceLinks` with the same
  match-the-fingerprint-table approach as impl methods. Flagship (root
  module, unmangled) links unchanged.

**Descoped from slice 2 (recorded, not dropped): the crc32 loop fact.**
`checksum.crc32` fails the predictable profile (its outer loop is bounded
by the runtime `len` — "unbounded" to the profile) so NO registry link is
possible today regardless of proof effort; and bounded-loop proof machinery
is already exercised by the SHA flagship (64-round loops via the
loop-induction keystone), so a std loop fact would demonstrate nothing new.
It waits for the `#[model_refined_by]`-class work, where a loop-invariant
fact can be linked honestly as partial evidence. Checked text/path
conversions beyond ASCII/base64: `text.validate_utf8` is trusted (same
class), so its model refinement also lands with that follow-up.

## Selective-proving rule (pinned 2026-07-16 — the arc's scope ceiling)

The arc's goal was never "prove the whole stdlib"; it was to prove the
stdlib CAN carry real kernel-checked evidence where it matters.
"Everything has an evidence class" > "everything is proved." The rule:

- **Prove**: the tiny stable algebraic core (Option/Result helpers, a
  couple of Bytes contracts) and APIs a real workload has pulled and
  stabilized (the base64 alphabet pair). Done — 11 links.
- **Wait for workload pressure**: `parse_*` (surface may still move),
  further Bytes/endian contracts, anything whose ergonomics are unsettled.
- **Do not prove**: CLI, IO, fs/env/process/net/time, collections in
  active design, formatting — broad or unstable surfaces keep their
  lighter evidence (manifest facts, oracle tests, mutation gates,
  capability/allocation/error gates).
- CRC/checksum stays oracle-tested unless it becomes proof-critical.

**With slice 2 shipped, the proof arc PAUSES**: the seed is planted and
gated; the frontier goes back to workloads and API ergonomics. New proofs
are added one at a time when an API becomes foundational AND stable —
never as a breadth sweep over the manifest.

## Where proofs live (layout rule)

Source APIs carry the proof metadata
(`#[spec]`/`#[proof_by]`/`#[proof_coverage]`/`#[proof_fingerprint]`);
Lean theorem bodies live in `proofs/Examples/PureCore/Proofs.lean`
(namespace `Examples.PureCore.Proofs`), imported through
`proofs/Examples.lean` so `check-proofs` kernel-checks them. Proofs are
evidence, not runtime dependencies — they never move into `std/src/`.
When the suite outgrows one file, split by module
(`PureCore/Option.lean`, `PureCore/Bytes.lean`, ...) under the SAME
import root and namespace.
