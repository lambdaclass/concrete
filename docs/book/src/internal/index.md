# Internal Details

This section is a high-level guide to the compiler's internal structure.

Concrete no longer has a "just lower the AST directly" design. The implementation is organized around explicit intermediate representations and explicit pass boundaries so the compiler is easier to understand, verify, and audit.

Use this section if you want to understand:

- how the compiler is structured internally
- why Core and SSA matter
- why the roadmap emphasizes pass boundaries and explicit artifacts
- what is already true in the compiler versus what is still planned

For the more current stable reference, also see:

- `docs/ARCHITECTURE.md`
- `docs/PASSES.md`
- `docs/ABI.md`

Good starting pages in this section:

- [Architecture](../architecture.md)
- [Current Architecture Truth](../architecture_truth.md)
