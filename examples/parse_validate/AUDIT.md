# parse_validate — Pilot Audit

Status: pull-through pilot per ROADMAP Active Dependency Order rule 2. Not a Phase 7 entry yet; this audit lists the gaps that need to close before it can graduate.

## 1. Current behavior and oracle

The example is a 230-line pure-Concrete header validator. Source: `src/main.con`. Project config: `Concrete.toml` declares `predictable = true`.

**Module shape.** One module `parse_validate` containing:

- One Copy enum `ParseError` (6 variants: `TooShort`, `BadVersion`, `BadType`, `PayloadTooBig`, `Truncated`, `BadChecksum`).
- One Copy struct `Header` (4 i32 fields: `version`, `msg_type`, `payload_len`, `checksum`).
- 6 validators (`validate_version`, `validate_msg_type`, `validate_payload_len`, `validate_total_len`, `compute_checksum`, `validate_checksum`).
- 1 composite parser (`parse_header`) returning `Result<Header, ParseError>`.
- 1 error classifier (`error_code`).
- 1 entry point (`main`) running 8 hand-written test cases.

**Compile + run.** Compiles cleanly, runs, prints `0` (all 8 tests pass).

**Oracle (current).** The Phase A semantic oracle in `tests/oracle/vectors.txt` — compiled binary stdout compared against `--interp` stdout. Both produce `0`. No external oracle (no fuzzer, no model comparison, no spec-based check).

**Existing test wiring.** `scripts/tests/run_tests.sh` includes a dedicated `parsevalidate` section (`PV_SRC=examples/parse_validate/src/main.con`) that exercises build/run, predictable check, effects, trusted count, allocation, enum compilation, bounded loop, policy, and purity. Plus the oracle vector and the run_tests inline interp probe.

**Test data is hand-written, not generated.** The 8 cases in `main` cover one valid header, one valid with payload, and one case per error variant. No fuzzing, no property tests, no round-trip.

## 2. Authority / capability surface

- **0 capabilities required.** All 9 functions, including `main`, are reported as `(pure)` by `--report caps` and `--report authority`.
- **0 externs.** No FFI.
- **0 unsafe signatures.** `--report unsafe` reports clean.
- **0 trusted functions.** No trusted core/shell.
- **0 allocations.** `--report alloc` reports "No allocation activity found."

The authority surface is the strongest possible: pure, capability-free, allocation-free. The example demonstrates the "no authority required for pure validation" thesis claim directly.

## 3. Allocation / stack / failure assumptions

**Allocation.** `--report alloc`: none. Confirmed by the absence of any `alloc`, heap construction, `Vec`, `String`, or other allocating stdlib use in source. No heap touch at all.

**Stack.** `--report stack-depth` reports all 9 functions bounded:

```
validate_version       12 bytes   depth 0   stack 12
validate_msg_type      12 bytes   depth 0   stack 12
validate_payload_len   16 bytes   depth 0   stack 16
validate_total_len     16 bytes   depth 0   stack 16
compute_checksum       52 bytes   depth 0   stack 52
validate_checksum      16 bytes   depth 0   stack 16
parse_header           48 bytes   depth 1   stack 100
error_code             12 bytes   depth 0   stack 12
main                  264 bytes   depth 2   stack 364
```

Max stack: 364 bytes (main). Bounded.

**Failure.** No abort, no panic, no exception. Every failure path is an explicit `return Result::Err {...}` with a named `ParseError` variant. The error taxonomy is closed (6 variants).

**Arithmetic.** Uses i32 for all numeric fields. The frozen arithmetic policy applies but is not currently surfaced as an explicit assumption in the example.

**Loops.** One bounded loop (`compute_checksum`). `--report effects` confirms `bounded`. The bound is the i32 `count` parameter, passed as `4` in the only call.

## 4. Report coverage — present vs missing

All 16 reports run cleanly (`caps`, `unsafe`, `layout`, `interface`, `alloc`, `mono`, `authority`, `proof`, `eligibility`, `proof-status`, `obligations`, `stack-depth`, `fingerprints`, `effects`, `recursion`, `consistency`). No report errors.

**Reports with substantive content:**

- `caps` / `authority` / `effects` — clean pure surface, 9 functions classified.
- `layout` — Header (16/4) and ParseError (4/4, tag-only enum) sized.
- `stack-depth` — full per-function bounds.
- `eligibility` — 8/9 functions eligible for proof (main excluded as entry point).
- `proof-status` — all 8 eligible functions **blocked** with the same diagnostic: `unsupported: if without else`. Extraction can't proceed in current ProofCore.
- `obligations` — 8 obligations registered, all with `status: blocked`, no specs attached.
- `fingerprints` — body fingerprints captured for all 9 functions (stable across builds).
- `interface` / `mono` — module/decl shape captured.
- `consistency` — passes.

**Reports that pass but produce no content** (because the example has nothing of that kind):

- `alloc` — "No allocation activity found."
- `unsafe` — "No unsafe signatures found."
- `recursion` — none.

**Reports missing (no surface yet for this example):**

- No `assumptions.toml` (Phase 2 E.24 surface).
- No `policy.toml` for enforceable budgets (Phase 2 E.25 surface). The only policy is `predictable = true` in `Concrete.toml`.
- No proof registry file. No `proof-registry.json` next to the source, so `--report proof` and `--report proof-status` report blocked extractions but no attached theorems.
- No snapshot or report-diff baseline captured (Phase 2 E.21 / E.23 surfaces).
- No release evidence bundle on file (Phase 8 surface — pulled here as audit material).

## 5. Smallest Lean-backed property candidate

**Candidate: `validate_version`.** Three lines:

```concrete
fn validate_version(v: i32) -> i32 {
    if v == 1 { return 0; }
    return 1;
}
```

Pure, no calls, no loops, no allocation. Trivially eligible. The natural property is:

> `validate_version(v) == 0 ↔ v == 1`

**Blocker.** `--report proof-status` says ProofCore extraction fails with `unsupported: if without else`. The function shape (early-return on the true branch, fall through on the false branch) is not yet handled by ProofCore's extractor; this is a Phase 4 item (the ProofCore expansion task in Phase 4.2).

**Two paths to unblock.**

1. **Extend ProofCore.** Add support for "if without else" in `Core → ProofCore` extraction. This is the right path for the pilot to *force* — it's a real Phase 4 gap the pilot is supposed to surface. Cost: extend `Concrete/ProofCore.lean` extractor, add a regression test, surface in proof-status.

2. **Rewrite the source.** Convert each validator to use `if-else` returning a value. Cheaper, but papers over a real ProofCore gap that other flagships will hit too. **Do not do this** without first opening the Phase 4 item — that would violate the "pilot does not paper over gaps" rule in ROADMAP Active Dependency Order rule 2.

Decision deferred to next commit: open Phase 4.2 with this case as the forcing repro, then attach the `validate_version` theorem once extraction works.

**Once unblocked, expand outward** to:

- `validate_msg_type` — property: `result == 0 ↔ 1 ≤ t ≤ 4`.
- `validate_payload_len` — bounded.
- `validate_total_len` — straightforward inequality.
- `validate_checksum` — equality check.
- `compute_checksum` — loop invariant: `acc == XOR of data[0..i]`.
- `parse_header` — composition: returns `Ok` iff all sub-validators return 0.

The composition theorem on `parse_header` would be the actual thesis demonstration: "successful return implies multiple structural invariants." That is the Phase 4 expected-outcome example almost verbatim.

## 6. Missing artifacts / policies / assumptions

**Missing now (pilot-blocking, must close before graduating to Phase 7):**

1. **No attached proof.** No `proof-registry.json`, no theorem checked by Lean, no proof-status `proved` line anywhere. This is the gap the candidate above forces open.
2. **No assumption file.** Nothing declaring what the example assumes about target, alloc, arithmetic policy, runtime. Phase 2 E.24 surface absent for this example.
3. **No enforceable policy file.** `Concrete.toml` has `predictable = true` only. Phase 2 E.25 surface absent. The actual budget — no alloc, max stack 364 bytes, capabilities = `{}` — is implicit, derivable from reports but not written down as an enforced contract.
4. **No oracle beyond hand-written tests.** Fuzzing, model comparison against a reference parser, or property-based testing would be the real oracle. Phase 1 D.6 / D.7 / D.9 surfaces apply here.
5. **No "Concrete catches this" negative pair.** Phase 1 D.4 surface absent. A companion file demonstrating what Concrete rejects (e.g. mutating `data` through a borrow without capability, allocating inside a `predictable` function, returning a borrow that outlives the source) would back the thesis claim.
6. **No release evidence bundle on file.** `capture_wrong_code_bundle.sh` was built for wrong-code cases; an equivalent for showcase/release evidence is not yet wired. Phase 8 surface.

**Missing-but-not-blocking (graduation can proceed without these, but they're needed for full F.21):**

7. No semantic-diff baseline (Phase 2 E.23).
8. No report-consistency assertion specific to this example (Phase 2.2 surface — consistency is checked globally but no per-example pin).
9. No benchmark baseline (Phase 6 surface — perf budgets are not on F's critical path).

## 7. What it takes to graduate from pilot to Phase 7 flagship

The example becomes a Phase 7 entry when each of the following is true:

| # | Bar | Currently |
|---:|---|---|
| 1 | At least one Lean-backed property, theorem checked by Lean, surfaced in `--report proof-status` as `proved`. | ✅ `validate_version` proved (commits `2673244` + `7081139`) |
| 2 | Composition property (e.g. `parse_header` returns `Ok` iff all validators pass) Lean-backed. | ❌ blocked on (1) |
| 3 | Assumption file present, machine-readable, listing target / arithmetic / alloc / stack assumptions. | ✅ `examples/parse_validate/assumptions.toml` enforced by `make test-assumptions` |
| 4 | Policy file present, with enforceable authority/alloc/stack budgets that CI checks. | ❌ partial (`predictable = true` only) |
| 5 | Oracle beyond hand-written tests — at minimum a property-based test or a reference-implementation differential. | ❌ absent (oracle is `--interp` only) |
| 6 | "Concrete catches this" negative pair on file. | ❌ absent |
| 7 | Release evidence bundle capturable in one command, producing source + reports + proof status + assumption + policy in a stable layout. | ❌ partial (bundle capture works for wrong-code, not yet for showcase) |
| 8 | Honest framing: a README explaining what is proved, what is enforced, what is reported, what is assumed, and where the trust boundary actually sits. | ❌ absent |
| 9 | Snapshot/diff coverage: a baseline of facts/reports that a regression would break. | ❌ absent |
| 10 | Listed in a curated Phase 7 showcase manifest (not just `tests/oracle/vectors.txt`). | ❌ no such manifest exists yet |

Today: **2 of 10 bars met.** Most of the remaining bars are not parse_validate-specific work — they are infrastructure that the pilot is supposed to force into existence. Closing them in order is the pilot's whole purpose.

**Closed so far:**
- Bar #1 — `validate_version` proved with Lean theorem. 5 of 8 eligible validators now extract via the new early-return-as-else rule in ProofCore.
- Bar #3 — assumption file landed with schema, CI gate, and drift detection.

## 8. Suggested first three pilot commits (per ROADMAP rule 2)

The pilot rule says: pick one bounded flagship, let its concrete gaps prioritize Phases 1-4, do not paper over gaps. Concrete next moves:

1. **Open the Phase 4 ProofCore "if without else" gap as a real ticket** with this case as the forcing repro. Extend the extractor; verify `validate_version` flips from `blocked` to `eligible`. Attach a small Lean theorem (`v == 1 → validate_version(v) == 0`). Surface as `proved` in `--report proof-status`.

2. **Land the first assumption file format and apply it here.** Define `examples/parse_validate/assumptions.toml` (or equivalent) with: target = `x86_64-linux`, arithmetic = `predictable-wrap`, alloc = `none`, max-stack = `364 bytes`. Define the schema in Phase 2 E.24 only as far as parse_validate needs.

3. **Land the first policy file format and enforce it here.** `policy.toml` with `no-alloc = true`, `no-unsafe = true`, `no-trusted = true`, `caps = []`, `max-stack-bytes = 400`. Wire a CI gate that fails if the example's reports drift outside the budget. Phase 2 E.25 minimum-viable.

Each commit closes one bar above. After all 10 bars close, parse_validate graduates from pilot to Phase 7.1.

## See also

- `ROADMAP.md` — Active Dependency Order rule 2 (the pilot rule); Phase 7 header (pilot vs flagship distinction).
- `tests/oracle/vectors.txt` — already wired.
- `scripts/tests/run_tests.sh` — `parsevalidate` section.
- `tests/wrong-code/cases/WC-0004.md` — historical reference; the dominator bug was originally found in this example's accumulator style and forced the early-return rewrite that ships today.
