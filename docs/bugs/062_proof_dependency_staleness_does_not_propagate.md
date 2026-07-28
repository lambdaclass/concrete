# Bug 062: a stale proof does not invalidate the proofs that depend on it

**Status:** Fixed (2026-07-28, R-0004 slice 3) — fourth of R-0004's evidence-integrity defect class, and the
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

## Fix as shipped (R-0004 slice 3)

Both halves are closed, and the two problems the document separates needed two
different things.

**No effect on status.** `notCurrentDeps` (renamed from `staleDeps` — see below)
is now consulted. A new `ObligationStatus.depsNotCurrent` says what is actually
true: this function's OWN subject is fresh, and something it reaches is not.
Reporting `stale` here would have asserted a body change that did not happen —
the same distinction that made `unbound` its own status.

**No closure.** Dependency currency is computed over the reachable closure by a
worklist bounded by the node count, so a recursive or mutually-recursive chain
terminates instead of diverging (verified against self-, mutual- and
three-cycles). `self` is excluded from its own closure, so recursion is not
self-blame.

One pass reaches a fixpoint, and that is a property rather than luck: if X
reaches Y and Y is downgraded because Y reaches a non-current Z, then X reaches
Z as well, so X is downgraded by Z directly.

Which statuses block evidence is decided in ONE place,
`ObligationStatus.isCurrentForDependents`: `proved` and `trusted` are current
(a trusted boundary is a declared, audited escape hatch); everything else is
not. `verified`/`release` profiles fail closed on the new status, and the ledger
maps it to a distinct kind rather than `proved_by_lean`.

### Measured effect on the corpus

Only `examples/parse_validate` changes: `validate_header_fields` and
`parse_header` go from `proved` to `deps_not_current`, because each reaches four
or five `missing` callees. That is the honest reading — their Lean theorems
evaluate those callees through the FnTable, but nothing pins those modelled
bodies to the source, so the caller's claim can drift silently when a callee
changes. Every other example is unaffected, including the unedited
`crypto_verify` chain.

### Two follow-on corrections this surfaced

- `staleDeps` was renamed **`notCurrentDeps`**. The field's meaning widened to
  the whole not-current closure while its name still said "stale", and INV-14
  fired on correct output (a `missing` callee "is not actually stale"). The
  report label `stale deps:` became `not current:` for the same reason.
- `dependencies` ("proved callees") and the `proof-deps` edge labels were being
  computed from the PRE-downgrade proved set, so a callee this pass had just
  contained was still printed `(proved)` and tripped INV-9. Both now read the
  final status.

Slice 6 replaces this conservative closure with a deterministic SCC/Merkle root
that makes the dependency root part of the subject digest; this slice is the
containment that must not wait for it.

## Regression

`scripts/tests/check_proof_freshness.sh` — the 062 legs are now positive
assertions: the direct dependent and the two-hop dependent are both contained,
the two-hop dependency block NAMES the stale leaf, and an unrelated function in
the same module stays `proved` so blanket over-firing cannot pass. Mutations
#34-#36 (containment removed, closure reduced to one hop, and the currency
policy admitting `stale`) are each killed.
