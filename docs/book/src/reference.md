# Reference Docs

This book is the main narrative guide, but Concrete also has stable reference documents in the repository root `docs/` directory.

Use them when you need exact current direction rather than the book's higher-level walkthrough.

## Core References

- [`docs/IDENTITY.md`](../../IDENTITY.md)
  Project identity, differentiators, non-goals, and what Concrete must eventually be able to show.

- [`docs/ARCHITECTURE.md`](../../ARCHITECTURE.md)
  Compiler pipeline, pass boundaries, artifact flow, and subsystem direction.

- [`docs/PASSES.md`](../../PASSES.md)
  Pass-by-pass ownership and responsibility breakdown.

- [`docs/STDLIB.md`](../../STDLIB.md)
  Stable stdlib direction, systems-module conventions, and collection priorities.

- [`docs/TESTING.md`](../../TESTING.md)
  Test surfaces, fast/full workflows, targeted modes, fuzzing, differential tests, and regression strategy.

## Language And Runtime References

- [`docs/VALUE_MODEL.md`](../../VALUE_MODEL.md)
  The current value and ownership model.

- [`docs/LANGUAGE_INVARIANTS.md`](../../LANGUAGE_INVARIANTS.md)
  The invariants the language and compiler are trying to preserve.

- [`docs/FFI.md`](../../FFI.md)
  FFI direction, `extern fn`, `trusted extern fn`, and boundary rules.

- [`docs/ABI.md`](../../ABI.md)
  Layout and ABI-facing rules.

- [`docs/DIAGNOSTICS.md`](../../DIAGNOSTICS.md)
  Diagnostic direction and quality expectations.

## Planning And History

- [`ROADMAP.md`](../../../ROADMAP.md)
  Forward-looking execution plan.

- [`CHANGELOG.md`](../../../CHANGELOG.md)
  Landed milestones and completed work.

- [`research/README.md`](../../../research/README.md)
  Exploratory design notes and longer-horizon research direction.

## Focused Design Notes

- [`research/language/high-integrity-profile.md`](../../../research/language/high-integrity-profile.md)
  The long-term high-integrity profile direction.

- [`research/workloads/high-integrity-examples.md`](../../../research/workloads/high-integrity-examples.md)
  Concrete examples of what the high-integrity profile would restrict.

- [`research/packages-tooling/authority-budgets.md`](../../../research/packages-tooling/authority-budgets.md)
  Package/subsystem authority budgets and dependency policy.

- [`research/packages-tooling/package-model.md`](../../../research/packages-tooling/package-model.md)
  What the future package/project model needs to decide.

- [`research/proof-evidence/proof-evidence-artifacts.md`](../../../research/proof-evidence/proof-evidence-artifacts.md)
  How reports, proofs, reproducibility, and build artifacts could fit together.
