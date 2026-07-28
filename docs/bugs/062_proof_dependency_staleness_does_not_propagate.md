# Bug 062: a stale proof does not invalidate the proofs that depend on it

**Status:** Open — fourth of R-0004's evidence-integrity defect class, and the
one the task describes as "stale dependency propagation considers direct callees
rather than the transitive proof dependency closure". The reproduction shows the
defect is broader than that: propagation does not happen at ALL, not even one hop.
**Discovered:** 2026-07-25, completing R-0004's reproducer set.

## Symptom

A three-function chain `top → mid → leaf`, each carrying an in-source proof link
with a stored fingerprint. Only `leaf` is made stale (its fingerprint no longer
matches its body); `mid` and `top` are untouched and correctly bound.

`--report proof-deps`:

```text
  chain.mid [proved]
    → chain.leaf (stale)

  chain.top [proved]
    → chain.mid (proved)

Summary: 2 proved functions, 3 with dependencies, 1 with stale dependencies
```

Two distinct problems, in increasing severity:

1. **Transitive**: `top` shows nothing at all. Its chain rests on a stale proof
   two hops down and neither its status nor its dependency line records that.
2. **Direct**: `mid` depends *immediately* on a stale proof and is still reported
   **proved**. The stale dependency is listed, but it does not affect the status.

The roadmap anticipated (1). (2) means the closure is not the only missing
piece — even a correct closure would report `proved` for every function in it,
because a stale dependency currently has no effect on the dependent's status.

## Root cause

`buildCallGraphModule` (`Concrete/Proof/ProofCore.lean`) maps each function to
its DIRECT callees and never closes over them:

```lean
let callees := collectCallsStmts f.body |>.eraseDups |>.map resolveCallee
```

The second pass then filters those direct callees against the stale set:

```lean
let staleCallees := allCallees.filter fun c => staleNames.contains c
{ o with dependencies := provedCallees, staleDeps := staleCallees }
```

`staleDeps` is recorded on the obligation but is never consulted by
`deriveObligationStatus`, so it is presentational only. Hence both symptoms: no
closure (problem 1) and no effect on status (problem 2).

## Candidate fix

Per R-0004: compute dependency freshness transitively with a deterministic
SCC/Merkle root, so recursion terminates and a deep callee edit stales every
dependent claim. The root must be part of the `ProofSubjectDigest`, which is what
makes the dependent's own digest change when a dependency moves — that is the
mechanism, rather than a separate propagation pass that could drift from it.

Both problems must be gated: a direct stale dependency downgrades its dependent,
and a two-hop one does too, with a recursive SCC case proving termination and an
alpha-renaming case proving the root is insensitive to source noise.

Regression: in the chain above, staling `leaf` alone must leave neither `mid` nor
`top` reported `proved`.

## Executable witness (R-0004 slice 1) — and the number is now stable

The roadmap held this number provisional until "its document and executable
control land". The control has landed, so **062 is now a stable number**.

`scripts/tests/check_proof_freshness.sh` uses the real `examples/crypto_verify`
chain — `verify_message -> verify_tag -> compute_tag` — and edits ONLY the leaf.
Measured on 2026-07-28:

| function | role | status |
| --- | --- | --- |
| `compute_tag` | leaf, edited | `proof stale` — correct |
| `verify_tag` | DIRECT dependent | `proved [one_direction]` — wrong |
| `verify_message` | TWO HOPS up | `proved [iff]` — wrong |

The gate additionally asserts that the stale EDGE *is* recorded in
`--report proof-deps` (so this is not "the graph cannot see it"), and that
`verify_message`'s dependency block mentions no stale dependency at all — the
transitive half, which is worse than showing it and ignoring it because a reader
of that line sees an all-proved chain.

Both legs are **tripwires**, and there is a control: the unedited chain must
report no stale edge, so the witness responds to the edit rather than always
firing.
