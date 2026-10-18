# Compiler Documentation

Status: stable reference index

Compiler architecture, phase ownership, pass contracts, diagnostics, determinism, testing, and
transformation registers.

Start with [ARCHITECTURE.md](ARCHITECTURE.md), [PASSES.md](PASSES.md), and
[PIPELINE_CONTRACTS.md](PIPELINE_CONTRACTS.md). Language semantics belong in
[`../language/`](../language/); proof and evidence architecture belongs in
[`../verification/`](../verification/).

The planned structural refactor that retires the 2026-10-18 defect classes (duplicated semantics,
name-joined evidence, non-unique names, uncited enforcement claims) is
[STRUCTURAL_SOUNDNESS_REFACTOR.md](STRUCTURAL_SOUNDNESS_REFACTOR.md).
