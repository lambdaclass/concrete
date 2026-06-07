# ObligationCore Pipeline

Concrete should have one typed evidence ledger for proof, contract,
runtime-safety, SMT, policy, audit, and proof-authoring surfaces.

The problem this fixes is simple: the compiler has grown several correct but
separate paths for closely related facts. Call-site preconditions, array
bounds, div/mod checks, overflow checks, assertions, loop VCs, proof links, and
SMT queries all need the same information: source span, scoped hypotheses,
typed conclusion, allowed engines, status, and evidence class. Keeping those
facts in parallel walkers makes the system harder to audit and easier to make
inconsistent.

The target is one flow:

```text
Concrete source
  -> parse / resolve / typecheck / capability check
  -> typed Core plus contract and proof metadata
  -> ObligationCore records
  -> discharge adapters
  -> evidence ledger
  -> reports / policies / proof workspaces / audit bundles / codegen gates
```

This document is **canonical for the obligation and evidence model** (the
`ObligationCore records -> evidence ledger` middle). It does not re-describe the
front end: the `parse / resolve / typecheck / capability check -> typed Core`
passes are owned by [ARCHITECTURE.md](ARCHITECTURE.md) and
[PASSES.md](PASSES.md), and this ledger generalizes the report-only "shared fact
layer" in [PROOF_AUDIT_PIPELINE.md](PROOF_AUDIT_PIPELINE.md). The migration is
tracked as ROADMAP Phase 3 (ObligationCore Pipeline Consolidation).

## Non-Goals

This is not a new proof language, a new solver, a new macro system, or a
replacement for Lean. It is a compiler architecture cleanup. Existing evidence
classes keep their meaning:

- Lean theorem checks stay `proved_by_lean`.
- `omega` and `bv_decide` stay `proved_by_kernel_decision`.
- External SMT stays `solver_trusted` unless separately replayed by Lean.
- Runtime checks, oracles, assumptions, and trusted boundaries stay distinct.

The refactor must not turn several honest statuses into one green badge.

## ObligationCore Record

Each obligation should carry the data needed by every downstream consumer:

```text
id
kind
function
source span
originating source construct
typed variables
scoped hypotheses
conclusion
semantic profile
dependencies
allowed engines
discharge attempts
status
evidence class
counterexample, if any
replay command, if any
policy impact
```

The `id` must be stable across harmless formatting and local refactors. The
source span is for humans. The id is for tools.

## Obligation Kinds

The first ledger should cover the obligation families Concrete already has:

- function preconditions at entry
- postconditions
- call-site preconditions
- array bounds
- div/mod nonzero
- opt-in overflow
- `assert`
- `assume`
- loop invariant initialization
- loop invariant preservation
- loop variant/decrease
- invalid contract expression
- impure contract/spec call
- vacuity
- source proof link
- proof fingerprint freshness
- spec drift
- missing theorem
- blocked proof
- proof-ineligible construct
- SMT query and replay
- oracle/test evidence
- runtime-enforced check
- trusted boundary

New obligation kinds should be added to this list, not to a private report
path.

## Scoped Context

There should be one scoped context collector. It supplies facts to every
obligation kind instead of each report walking statements independently.

Facts may come from:

- function `#[requires]`
- branch guards
- negated guards after early return or in an `else` branch
- loop invariants
- already-proved assertions
- local constants
- ghost bindings
- let substitutions
- type-derived bounds

The collector must also have one shared invalidation rule. If a variable is
assigned, facts mentioning that variable are stale after the assignment. Array
element and field updates should invalidate only what they actually touch once
the language has enough alias/frame information to say that precisely.

## Expression Lowering

Obligation expressions should be typed once and lowered many ways:

- human report text
- JSON
- Lean propositions
- SMT-LIB
- counterexample source-variable mapping
- proof-stub comments
- policy summaries

This is where semantic-profile decisions belong. For example, Concrete integer
division truncates toward zero while Lean `Int` division floors. Lowering a
division expression to Lean is sound only when the dividend is known
non-negative and the divisor is a positive literal, or when a later proof
bridge accounts for the semantic difference explicitly.

No backend should silently drop a hypothesis it cannot encode. If a lowering
cannot represent the full obligation soundly, that backend must decline the
obligation.

## Discharge Adapters

Each adapter consumes `ObligationCore` and returns an evidence-class result.

The intended adapters are:

- constant folding
- `omega`
- `bv_decide`
- linked Lean theorem
- Lean replay
- external SMT
- oracle/test runner
- runtime enforcement
- assumption
- trusted boundary declaration

Adapters must be monotonic in trust. An external solver cannot produce
`proved_by_kernel_decision`; an oracle cannot produce `proved_by_lean`; an
`assume` cannot produce a proof.

## Evidence Ledger

The evidence ledger is the result after discharge. It is the source of truth
for every user-facing surface:

- `--report contracts`
- `--report vcs`
- `--report proof-status`
- `--report check-proofs`
- `concrete audit`
- `concrete diff`
- release bundles
- policy gates
- snapshots
- evidence corpus gates
- `concrete prove --json`
- `concrete prove --workspace`
- generated Lean stubs
- SMT replay artifacts

Reports are views. Policies are checks over the same records. Proof workspaces
are exported slices of the same records.

## Migration Rule

Migrate one obligation family at a time.

For each family:

1. Generate new `ObligationCore` records.
2. Compare old and new report output on the existing fixtures.
3. Compare JSON, policy behavior, stable ids, counterexamples, and replay data.
4. Switch that family's reports and proof-authoring surfaces to the ledger.
5. Delete the old family-specific path.

No family should keep two live truth sources after migration.

## Why This Matters

Concrete's claim is evidence accounting. That claim is strongest when the
compiler has one place where evidence is classified. A single ledger makes it
harder for one report to say "proved" while another says "unproven", harder for
an agent workspace to miss a hypothesis, and harder for release policy to check
a different fact than the audit report showed.

This is the architecture that lets later phases scale without making every new
language feature reimplement proof, audit, policy, and agent plumbing by hand.
