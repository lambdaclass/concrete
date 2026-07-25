+++
title = "Provable Subset"
+++

# Provable Subset

Status: standing architecture reference

The exact admitted surface is maintained in
[PROVABLE_V1](https://github.com/unbalancedparentheses/concrete2/blob/main/docs/PROVABLE_V1.md).
This page explains the boundary rather than copying a constructor list that can
drift.

## What It Is

The provable subset is the fragment of validated Concrete programs that the
current ProofCore extractor can represent and the Lean evaluator can execute:

1. eligibility excludes entry points, capability-bearing functions, trusted
   origins, and other declaration-level boundaries;
2. extraction translates every supported node and fails closed if any node is
   unsupported;
3. an evidence attachment binds a theorem and coverage class to a stored proof
   subject and is replayed in Lean.

ProofCore is a semantic projection of validated Core, not a replacement
whole-language IR.

## Current Shape

A current proof target is capability-free, non-trusted, non-entry,
non-recursive, free of FFI/raw-pointer/allocation behavior, and fully
expressible in `ProvableV1`.

Selected structs, enums, matches, fixed arrays, bounded loops, functional state,
and fixed-width operations are supported. Old blanket rules such as “no loops,”
“no mutation,” or “no aggregates” are obsolete. Strings/text, references,
allocation, FFI, trusted code, recursion, and arbitrary unmodeled
state/control remain outside the subset.

Capability-free means authority-free, not total: a function can still reach a
checked terminal failure unless its theorem and assumptions exclude it.

## Where It Sits

```text
Source
  → Resolve / Check / Elaborate / CoreCanonicalize / CoreCheck
  → Validated Core
  → ProofCore extraction
  → PExpr evaluation and Lean theorems
```

Extraction occurs before whole-program monomorphization, so general
per-instantiation proof identity remains future work. Runtime compilation
continues independently through Mono, lowering, SSA, and a backend; there is no
verified compiler-preservation theorem connecting that chain to `PExpr`.

## Calls and State

Non-recursive direct calls are represented when the proof function table is
complete. That lets the evaluator execute the callee expression; it does not
automatically compose separate correctness theorems.

ProofCore also models the bounded functional state used by the graduated
examples: loop-carried scalar rebinding and functional array updates. It does
not model general heap aliasing.

Callable identity is still incomplete in the proof model under bug 061 and
ROADMAP R-0442. Source-level shadowing restrictions never substitute for
compiler-owned identity.

## Attachment Integrity

A supportable Lean-backed claim has:

- a stored proof-subject fingerprint;
- a fresh attachment with an explicit coverage class; and
- successful `--report check-proofs` kernel replay.

A source link with no fingerprint is `unbound`, never `proved`. The current
fingerprint is body-oriented and does not yet cover every signature/type fact,
contract, toolchain context, or transitive dependency. ROADMAP R-0004 owns the
full versioned subject digest and dependency roots.

## Growth Rule

Adding a construct requires a forcing workload, ProofCore semantics,
extraction/negative tests, Lean evidence at the claimed coverage, and updates
to the V1 contract, proof-story matrix, and obligation register. Removing or
weakening an admitted V1 construct is a compatibility change.

For the full user contract, see
[PROOF_CONTRACT](https://github.com/unbalancedparentheses/concrete2/blob/main/docs/PROOF_CONTRACT.md).
