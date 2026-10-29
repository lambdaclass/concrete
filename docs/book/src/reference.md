# Reference Docs

This book is the main narrative guide, but Concrete also has stable reference documents in the repository root `docs/` directory.

Use them when you need exact current direction rather than the book's higher-level walkthrough.

## Core References

- [`docs/project/PROJECT.md`](../../project/PROJECT.md#identity)
  Project identity, differentiators, non-goals, and what Concrete must eventually be able to show.

- [`docs/compiler/ARCHITECTURE.md`](../../compiler/ARCHITECTURE.md)
  Compiler pipeline, pass boundaries, artifact flow, and subsystem direction.

- [`docs/compiler/PASSES.md`](../../compiler/PASSES.md)
  Pass-by-pass ownership and responsibility breakdown.

- [`docs/stdlib/STDLIB.md`](../../stdlib/STDLIB.md)
  Stable stdlib direction, systems-module conventions, and collection priorities.

- [`docs/compiler/TESTING.md`](../../compiler/TESTING.md)
  Test surfaces, fast/full workflows, targeted modes, fuzzing, differential tests, and regression strategy.

## Language And Runtime References

- [`docs/language/VALUE_MODEL.md`](../../language/VALUE_MODEL.md)
  The current value and ownership model.

- [`docs/spec/overview.md`](../../spec/overview.md)
  The invariants the language and compiler are trying to preserve.

- [`docs/platform/FFI.md`](../../platform/FFI.md)
  FFI direction, `extern fn`, `trusted extern fn`, and boundary rules.

- [`docs/platform/ABI.md`](../../platform/ABI.md)
  Layout and ABI-facing rules.

- [`docs/compiler/DIAGNOSTICS.md`](../../compiler/DIAGNOSTICS.md)
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
