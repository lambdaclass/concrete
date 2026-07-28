# Bug 058: `#[proof_by]` without `#[proof_fingerprint]` can never go stale

**Status:** CONTAINED (2026-07-25) — such a link now reports the distinct state
`unbound` ("proof link unbound: no stored proof-subject digest"), never `proved`
and never `stale`, and fails closed under `[policy] require-proofs` with E0612.
The full fix — a versioned `ProofSubjectDigest` — remains R-0004's next phase.
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

## Why the state is `unbound` and not `stale`

`stale` asserts that a recorded subject and the current body disagree — the
renderer says "the body changed". For these links nothing changed and nothing was
ever recorded, so reporting `stale` would state a fact not in evidence. `unbound`
is its own `ObligationStatus`/`ProofState`, its own ledger kind
(`unbound_proof_link`), and its own report block. Ordering matters: unbound is
decided BEFORE staleness, because with no stored subject the staleness
comparison is the body against itself.

Kernel replay accepts unbound links deliberately (`check-proofs` used to skip
anything not proved-or-stale). Replay is how such a link would earn a subject;
excluding it would leave no path from unbound to bound.

## A trap this bug set for its own investigation

`--report check-proofs` is invocation-sensitive. Run from inside an example's
directory it reported `0 verified, 11 failed` with `theorem_lookup` errors for
`examples/hmac_sha256`; run from the repository root, `11 verified, 0 failed`.
The first result nearly became a report that the flagship's proofs do not
verify. All twelve named theorems exist in
`proofs/Examples/HmacSha256/Proofs.lean`.

The lesson generalizes to the fix: a verdict that depends on the working
directory and leaves no artifact is an observation, not a receipt — and a
fingerprint recorded from an observation is the same unfounded claim this bug is
about, one level up. Backfilling therefore waits on R-0004's split artifacts: a
semantic subject digest plus dependency root, and a receipt that binds them to
the theorem artifact, toolchain, workspace/import closure, and kernel replay
result.

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

## Executable witness (R-0004 slice 1)

`scripts/tests/check_proof_freshness.sh` now drives this from a copy of the real
`examples/loop_invariant` project: removing the stored `#[proof_fingerprint]`
must yield `proof link unbound`, never `proved`, and must carry the exact wording
`proof link unbound: no stored proof-subject digest`. This bug is CONTAINED
(R-0004 slice 2), so the leg is a positive assertion guarding the containment —
not a tripwire.
