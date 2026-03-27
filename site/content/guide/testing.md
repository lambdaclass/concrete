+++
title = "Testing"
+++

# Testing Concrete

Concrete treats testing as part of the compiler architecture, not only as project hygiene.

The current testing surface is intentionally layered:

- the main end-to-end suite
- the SSA-specific suite
- the in-language `--test` runner
- codegen differential tests
- parser fuzzing
- property and trace tests

## Main Entry Points

Fast local loop:

```bash
./run_tests.sh
```

Full suite:

```bash
./run_tests.sh --full
```

Targeted runs:

```bash
./run_tests.sh --filter struct_loop
./run_tests.sh --stdlib
./run_tests.sh --O2
./run_tests.sh --codegen
./run_tests.sh --report
```

In-language tests:

```bash
./lake/build/bin/concrete file.con --test
```

SSA-specific coverage:

```bash
bash test_ssa.sh
```

## Why This Matters

Concrete's testing strategy is meant to support the same project goals as the compiler architecture:

- fast iteration without losing confidence
- explicit coverage around lowering and SSA invariants
- direct regression tests for real bugs
- better visibility into compiler artifacts and reports

The fast path is now explicit, but the longer-term goal is even better: stdlib-aware and module-aware targeted testing, clearer partial-run coverage reporting, and later artifact-driven reuse.

## LL(1) Grammar Guardrail

Concrete's syntax is intended to stay LL(1). The repository now includes external LL(1) checkers that validate the reference grammar:

- Python checker
- C checker
- Rust checker

They currently agree on the same result for the reference grammar: 56 grammar rules, 0 conflicts.

Relevant files:

- [`grammar/concrete.ebnf`](../../../grammar/concrete.ebnf)
- [`scripts/check_ll1.py`](../../../scripts/check_ll1.py)
- [`scripts/check_ll1.c`](../../../scripts/check_ll1.c)
- [`scripts/check_ll1.rs`](../../../scripts/check_ll1.rs)

## Where To Go Deeper

For the stable testing reference, read [`docs/TESTING.md`](@/reference/TESTING.md).
