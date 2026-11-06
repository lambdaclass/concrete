# Verification Product Model

Status: open

This note records the strongest product lessons Concrete should take from:

- Lean's "perfectable" model: the language/toolchain can state and check facts about itself
- MoonBit's built-in verification workflow: proof-oriented checking is part of the normal developer loop
- SPARK's high-integrity verification model: contracts, invariants, and proof reports are presented as an engineering product, not only a research artifact

Concrete should learn from those models without copying their entire surface area.

## The Core Product Question

Concrete is not only designing a language.
It is also designing the user experience of:

- building
- checking
- reporting
- proving
- detecting drift
- repairing stale proof state

If those workflows are scattered, the proof story will remain technically true but operationally weak.

## What Lean Contributes

Lean's strongest lesson is not "dependent types everywhere".
It is that a system becomes unusually powerful when it is **perfectable**:

- properties can be stated precisely
- properties can be checked mechanically
- the system can reason about its own artifacts and semantics

For Concrete, this supports:

- Lean-attached specs and theorems
- proof-backed artifacts instead of prose claims
- a toolchain whose trust claims can become stronger over time

Concrete should copy:

- explicit theorem/evidence attachment
- proof as part of the normal engineering workflow
- the idea that semantic claims should become machine-checkable

Concrete should not copy:

- open-ended syntax extensibility as a core direction
- a surface language that grows theorem-prover complexity everywhere

Concrete still needs:

- a small language surface
- an LL(1) parser
- explicit capability/trust boundaries
- artifact-first evidence

## What MoonBit Contributes

MoonBit's strongest lesson is product integration:

- `build`, `test`, and `prove` should feel like one toolchain story
- proof failures should be normal diagnostics, not special research output
- verification should look built-in, not bolted on

For Concrete, that suggests:

- proof/evidence status should be normal report and CI surfaces
- stale or blocked proof state should be obvious and actionable
- proof artifacts should be part of the normal developer loop

Concrete should copy:

- built-in proof workflow as a product goal
- proof diagnostics with actionable status
- one coherent toolchain story for evidence

Concrete should not copy prematurely:

- source-level contracts and loop invariants before the current ProofCore and artifact boundary are mature enough
- a broader proof surface than the current semantics and diagnostics can support honestly

## What SPARK Contributes

SPARK's strongest lesson is discipline:

- a high-integrity subset is explicit
- proof boundaries are explicit
- reports matter as much as the prover

For Concrete, that supports:

- named profiles such as safe / predictable / provable / high-integrity
- explicit trusted-computing-base accounting
- release criteria that are about what users may rely on, not what the implementation aspires to

## Concrete's Product Direction

Concrete should position verification as:

- Lean-backed
- artifact-first
- profile-aware
- audit-friendly

The right near-term product is not "the whole language is formally verified".
The right product is:

- some code is proved
- some code is enforced
- some code is only reported
- some boundaries are explicitly trusted
- the toolchain makes those distinctions visible and actionable

## Product Requirements Concrete Should Meet

### 1. Built-In Evidence Workflow

The user should experience:

- build
- check
- report
- prove
- stale detection
- repair

as one coherent workflow.

### 2. Actionable Proof Status

The toolchain should make clear:

- proved
- stale
- blocked
- missing
- ineligible
- trusted boundary reached

without forcing users to read compiler internals.

### 3. Honest Narrowness

Concrete should not widen proof claims faster than:

- ProofCore semantics
- diagnostics
- artifact stability
- trust accounting

can support honestly.

### 4. AI-Assisted, Kernel-Checked Workflow

AI may suggest:

- proof repairs
- stale-proof fixes
- attachment updates
- tactic skeletons

but the final trust anchor remains:

- Lean kernel checking
- compiler artifact validation
- explicit consistency gates

## What This Means For Roadmap Priority

Concrete should prioritize:

1. proof workflow clarity
2. proof diagnostics quality
3. artifact-backed stale/blocked/proved status
4. one coherent build/check/report/prove story

before:

- broad source-level contracts
- loop invariants
- ghost code
- ambitious proof syntax

## Related Notes

- [Proof UX And Verification Influences](./proof-ux-and-verification-influences.md)
- [Verification Surface](./verification-surface.md)
- [Spec Attachment](./spec-attachment.md)
- [Proof Evidence Artifacts](./proof-evidence-artifacts.md)
