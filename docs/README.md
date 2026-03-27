# Documentation Guide

Status: stable reference

This directory holds the stable reference docs for Concrete's implementation and language model.

Use these files as the primary reference once a design has moved out of exploration:

- [IDENTITY.md](IDENTITY.md) — what Concrete is optimizing for, where it intends to differentiate, and what it is not trying to be
- [SAFETY.md](SAFETY.md) — capabilities, `trusted`, `Unsafe`, proof boundary, and high-integrity direction
- [EXECUTION_MODEL.md](EXECUTION_MODEL.md) — runtime boundary, allocation model, FFI/runtime contract, and execution profiles
- [ARCHITECTURE.md](ARCHITECTURE.md) — compiler pipeline, artifact flow, pass boundaries, and architecture phase reference
- [PASSES.md](PASSES.md) — pass-by-pass contracts, ownership boundaries, and what each phase may assume
- [ABI.md](ABI.md) — layout, enum representation, FFI-safety, and ABI boundary rules
- [DIAGNOSTICS.md](DIAGNOSTICS.md) — diagnostics model, current status, and staged diagnostics work
- [FFI.md](FFI.md) — externs, raw pointers, and the `Unsafe` boundary
- [DESIGN_POLICY.md](DESIGN_POLICY.md) — feature-admission criteria and design filter
- [DECISIONS.md](DECISIONS.md) — first-class "no" and "not yet" decisions
- [LANGUAGE_SHAPE.md](LANGUAGE_SHAPE.md) — long-term structural commitments for the language
- [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md) — current proof-eligible subset and its boundary
- [LANGUAGE_INVARIANTS.md](LANGUAGE_INVARIANTS.md) — the language rules that must hold across every phase
- [STDLIB.md](STDLIB.md) — current stable stdlib direction and module priorities
- [TESTING.md](TESTING.md) — current test structure and what each suite is for
- [VALUE_MODEL.md](VALUE_MODEL.md) — value, borrow, ownership, and resource-model rules

The `book/` subdirectory is for tutorial-style and user-facing structured documentation.

## How To Read The Docs

- Read [../README.md](../README.md) first for project overview, current status, and build/test instructions.
- Read [../ROADMAP.md](../ROADMAP.md) for active and future work.
- Read [../CHANGELOG.md](../CHANGELOG.md) for completed milestones.
- Read [PASSES.md](PASSES.md) early if you want to understand where compiler responsibility lives today.
- Use this `docs/` directory for stable reference material, not exploratory design notes.

## Scope Boundary

- `docs/` = stable reference and implementation contracts
- `research/` = exploratory notes, possible future work, and design investigations

If a topic is still being explored or debated, it belongs in `research/` first. Once it becomes a stable project rule or compiler boundary, it should move into `docs/`.

## Where New Docs Go

- stable rule/reference -> `docs/`
- active plan/sequencing -> `ROADMAP.md`
- landed milestone/history -> `CHANGELOG.md`
- exploratory note -> `research/`
