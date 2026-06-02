# Proof Pressure Set

Status: active reference (Phase 2, item 1)

This document defines the narrow Concrete-to-Lean proof pressure set: 6 functions in one file that exercise every proof obligation state.

## Location

`examples/proof_pressure/src/main.con` with `examples/proof_pressure/src/proof-registry.json`.

## Functions

| # | Function | Target state | Category | Reason |
|---|----------|-------------|----------|--------|
| 1 | `check_nonce` | proved | conditional-heavy validator | Nested if-else checking nonce range. Registry fingerprint matches. |
| 2 | `validate_header` | proved | helper-composition | Calls `check_nonce`, then checks version range. Exercises proved-function dependency tracking. |
| 3 | `compute_checksum` | stale | stale-repair target | Registry has fingerprint from before `+ 1` was added to the body. Body changed, proof outdated. |
| 4 | `format_result` | ineligible | intentionally excluded | Has `with(Console)` capability. Pure functions only are proof-eligible. |
| 5 | `clamp_value` | missing | pure extractable, no spec | Uses only extractable constructs (arithmetic, if-else). No registry entry. |
| 6 | `classify_range` | blocked | unsupported construct | Pure, eligible, but uses struct field access (`b.lo`, `b.hi`) which the extraction pipeline does not support. |

## Why these 6

- **proved**: `check_nonce` is conditional-heavy (the thesis validator archetype). `validate_header` is composition-heavy (calls another proved function).
- **stale**: `compute_checksum` simulates the realistic case of a body edit after proof registration. The old fingerprint is `key * message + salt`; the current body is `key * message + salt + 1`.
- **ineligible**: `format_result` has Console capability. This is the intentional exclusion case — I/O code is outside the proof boundary by design.
- **missing**: `clamp_value` is pure and extractable but nobody wrote a spec/proof yet. This is the "next proof to write" case.
- **blocked**: `classify_range` is pure and eligible but uses struct field access, which the extraction pipeline does not yet support. This is the "extraction needs work" case — intentionally kept as a first-class pressure target, not an accident.

## Expected report output

```
concrete examples/proof_pressure/src/main.con --report proof-status
```

```
Totals: 7 functions — 2 proved, 1 stale, 1 unproved, 1 blocked, 2 ineligible, 0 trusted
```

(7 functions because `main` is also ineligible as entry point.)

## State mapping

| State | Meaning | Trigger |
|-------|---------|---------|
| proved | Spec attached, fingerprint matches, extraction succeeded | Registry entry with correct fingerprint |
| stale | Spec attached, fingerprint changed | Registry entry with old fingerprint |
| missing | Eligible, extractable, no spec attached | No registry entry for this function |
| blocked | Eligible, but extraction failed | Unsupported construct in function body |
| ineligible | Fails source or profile gates | Has capabilities, is trusted, is entry point |

## Registry structure

`proof-registry.json` contains 3 entries:
- `main.check_nonce` — correct fingerprint (proved)
- `main.validate_header` — correct fingerprint (proved)
- `main.compute_checksum` — old fingerprint (stale)

No entry for `clamp_value` (missing) or `classify_range` (blocked).

## What this enables

Phase 2 items 2-5 build on this pressure set:
- Item 2: make extraction for these functions reproducible and inspectable
- Item 3: generate Lean theorem stubs from the fingerprints
- Item 4: validate theorem identity and attachment integrity
- Item 5: wire Lean kernel checking so `proved` actually means `lake build` passes

The stale case (`compute_checksum`) specifically enables item 7: stale-proof repair pressure tests.
The blocked case (`classify_range`) tracks extraction pipeline gaps against a known target.
