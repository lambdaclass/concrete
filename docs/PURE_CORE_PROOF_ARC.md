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

Scoped to what the PExpr model expresses TODAY (match_/enumLit/call-by-name;
NO loops, NO quantification over callbacks):

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

DONE — the vertical works end-to-end. `Option.unwrap_or` is the first
kernel-backed stdlib proof link: `proof-status` shows fingerprint-fresh,
`check-proofs` kernel-verifies `option_unwrap_or_correct` through the
Examples import, and a body mutation flips it to STALE (the gate's mutation
leg pins this). Gate: `check_purecore_proofs.sh` (`make test-purecore-proofs`).

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

## Model gaps (recorded, not hidden)

- PExpr has no loops: full `parse_hex` correctness needs either recursive
  spec functions with fuel (the eval already threads fuel) or a model
  extension. Slice 1 proves the GUARD step, not the whole loop — stated
  honestly in the coverage attribute.
- No callback quantification: HOF laws are proven for representative
  callbacks; quantified versions need the fn-table hypothesis pattern
  (`h : fns "f" = some fBody`) — deferred until slice 1 shows the shape.
- Trusted bodies (Bytes internals) are refined against models, never
  "proved" directly — the trusted-boundary class remains visible.

## Sequencing after slice 1

Only after slice 1's DoD holds end-to-end (stable names, exact manifest
marking, mutation-fails): numeric checked helpers, checksum.crc32 loop
facts, checked text/path conversions. Breadth never precedes a green DoD.
