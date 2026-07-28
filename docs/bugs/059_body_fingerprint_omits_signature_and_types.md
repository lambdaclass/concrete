# Bug 059: the proof body fingerprint omits types and signature facts

**Status:** Open — second of R-0004's evidence-integrity defect class.
**Discovered:** 2026-07-25, filing R-0004's reproducers before implementation.

## Symptom

A function keeps its `proved` status across a return-type change, WITH a stored
`#[proof_fingerprint]` present (so this is not bug 058):

```
baseline (fn count_up() -> i32):      -- proved
after i32 -> u32 (body untouched):    -- proved
```

`i32 -> u32` changes the value domain and the overflow behaviour of every
arithmetic step in the body. A theorem proved about the `i32` version says
nothing about the `u32` one, and nothing reports a change.

## Root cause

`Concrete/Proof/ProofCore.lean`:

```lean
def bodyFingerprint (body : List CStmt) : String :=
  fingerprintExpr.fingerprintStmts body
```

Two things follow. First, the input is only `body` — the parameter list, return
type, generics and bounds, and capabilities are never offered to the hash.
Second, the statement walker discards the types it *does* see:

```lean
| .letDecl name _ _ val => s!"(let {stripAlpha name} {fingerprintExpr val})"
| .return_ (some val) _ => s!"(ret {fingerprintExpr val})"
```

The `_` positions are the declared type and the return type. So even
`let mut acc: i32 = 0` → `let mut acc: u32 = 0` leaves the fingerprint
unchanged. The hash captures the SHAPE of the computation and none of the types
that give it meaning.

## Candidate fix

R-0004's `ProofSubjectDigest`: a versioned canonical digest over qualified
semantic identity, the full typed signature and generic constraints,
capabilities, the normalized body (types included), requires/ensures/invariants,
the normalized selected specification and claim scope/coverage, and
extraction/schema version — replacing the body-only hash. A deterministic
dependency root is a separate input to the evidence decision. The theorem,
toolchain, workspace/import closure, and replay result belong in the
`ProofEvidenceReceipt` about that subject rather than changing what the semantic
subject is.

Regression: the `i32 -> u32` edit stales; a comment/formatting/alpha-renaming
edit does NOT (the digest must stay insensitive to source noise, which is why
`stripAlpha` exists); a capability or generic-bound edit stales. A mutation that
drops any single component from the digest must be killed.

## Executable witness (R-0004 slice 1)

`scripts/tests/check_proof_freshness.sh` changes the return type, the
accumulator and the loop counter from `i32` to `u32` in a copy of the real
`examples/loop_invariant` project. Every STATEMENT is textually identical, so a
body-only hash sees nothing — measured verdict is still `proved [invariant]`.

That leg is a **tripwire**: it passes because the bug is present, and it FAILS
when the digest starts covering declared types. That failure is the signal to
convert it to a positive assertion and close this document, not a regression.
The gate pairs it with a control (a statement edit, which correctly stales), so
"still proved" cannot be an artifact of the harness never re-reading the file.
