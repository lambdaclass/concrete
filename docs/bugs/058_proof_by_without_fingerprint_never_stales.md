# Bug 058: `#[proof_by]` without `#[proof_fingerprint]` can never go stale

**Status:** Open — first of R-0004's evidence-integrity defect class.
**Discovered:** 2026-07-25, filing R-0004's reproducers before implementation.
**Severity:** a false `proved` claim. Not wrong code, but wrong *evidence*, which
is the claim this project's guarantees rest on.

## Symptom

A function carrying `#[proof_by]` but no `#[proof_fingerprint]` reports `proved`
and "proof matches current body" no matter how the body is edited.

```con
mod pf {
    #[spec(Examples.LoopInvariant.Proofs.count_upBody)]
    #[proof_by(Examples.LoopInvariant.Proofs.count_up_loop_preserves)]
    #[proof_coverage(invariant)]
    fn count_up() -> i32 {
        let mut acc: i32 = 0;
        #[invariant(0 <= i && i <= 8)]
        #[variant(8 - i)]
        for (let mut i: i32 = 0; i < 8; i = i + 1) {
            acc = acc + i;          // <-- change this to `acc + i + 1000`
        }
        return acc;
    }
}
```

`concrete src/main.con --report proof-status`, before AND after that edit:

```
-- proved [invariant] ---------------------------------- src/main.con:5
  ✓ `pf.count_up` — proof matches current body.
  trust: linked + fingerprint-fresh — kernel replay via `--report check-proofs`
```

The theorem was proved about the original body. After the edit the claim is
false, and the report still asserts freshness in as many words.

**Control** — the same function WITH a `#[proof_fingerprint]` behaves correctly:

```
warning: stale fingerprint for 'pf.count_up' (#[proof_fingerprint] "40b9…" ≠
current "9278…" — body changed since the proof was linked)
-- proof stale ----------------------------------------- src/main.con:6
```

So staleness detection works; it is the missing-fingerprint path that cannot
fire.

## Root cause

`Concrete/Proof/ProofCore.lean`, the staleness filter, and the code says it:

```lean
-- ... this is how source-linked functions get staleness detection without a full
-- fingerprint in source (their synthesized bodyFingerprint always equals the
-- recomputed one, so the string compare below can never fire for them).
match re.expectedHash with
| some h => if shortHash currentFp != h then some (.staleFingerprint re currentFp) else none
| none   => if re.bodyFingerprint != currentFp then some (.staleFingerprint re currentFp) else none
```

For an in-source link with no `#[proof_fingerprint]`, `expectedHash` is `none`
and `re.bodyFingerprint` was *synthesized from the current body*. The `none`
branch therefore compares the current fingerprint with itself: always equal,
never stale. The comment describes the mechanism accurately but treats it as an
implementation note rather than the hole it is.

## Candidate fix

Per R-0004's containment rule: an in-source proof link with no stored, validated
proof-subject digest must be `missing`/`unbound` (or `needs_recheck`) — never
`proved` — and release/verified profiles must fail closed on it. A comparison
whose two sides are derived from the same input is not a check and must not be
reported as one.

Regression: the program above reports a non-`proved` state before any edit
(because there is nothing to compare against), and the control case with a
stored fingerprint keeps its current proved → stale behaviour. A mutation that
restores the self-comparison must be killed.
