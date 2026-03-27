# Internal IRs

The current compiler architecture is built around explicit intermediate representations, not direct AST-to-codegen translation.

The important modern pipeline is:

`Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA`

## Why This Matters

Concrete wants:

- explicit semantic boundaries
- a backend contract that can be checked
- better audit/report surfaces
- eventual proof work against a reduced, explicit core

That is why the compiler architecture is such a large part of the roadmap.

## Core vs SSA

Two especially important IR boundaries are:

- **Core**: the main post-elaboration semantic representation
- **SSA**: the backend-facing representation consumed by code generation

Concrete's long-term shape depends on keeping those boundaries clear and boring.

## Control Flow In SSA

SSA represents control flow with:

- basic blocks
- terminators
- explicit values/registers

Recent compiler work has specifically hardened aggregate lowering so mutable aggregate state is kept in stable storage instead of being transported through fragile aggregate `phi` nodes.

## Where To Read More

- `docs/ARCHITECTURE.md` for the full pass structure
- `docs/PASSES.md` for pass-by-pass contracts
- `docs/ABI.md` for type/layout rules
