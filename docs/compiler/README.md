# Compiler Documentation

Status: stable reference index

Compiler architecture, phase ownership, pass contracts, diagnostics, determinism, testing, and
transformation registers.

Start with [ARCHITECTURE.md](ARCHITECTURE.md), [PASSES.md](PASSES.md), and
[PIPELINE_CONTRACTS.md](PIPELINE_CONTRACTS.md). Language semantics belong in
[the language specification](../spec/README.md); proof and evidence architecture belongs in
[`../verification/`](../verification/).

The plan for an end-to-end verified compiler — the target IR chain, semantics contract, checks
on Core, owned backend, and the staged migration that retires the 2026-10-18 defect classes — is
[VERIFIED_PIPELINE.md](VERIFIED_PIPELINE.md).

The declaration identity for module-level types and constants (R-0492 slice 6: one type
environment per module, Core spellings apart from the builtin enums, then `TypeId` in Check and
`ConstId` through imports) is [DECLARATION_IDENTITY.md](DECLARATION_IDENTITY.md).
