+++
title = "Builder"
+++

# Builders And Lowering

The compiler uses builder-style structures while lowering and assembling internal representations.

The exact implementation has evolved, but the important idea is stable:

- resolution, elaboration, monomorphization, and lowering each own different responsibilities
- temporary builder state should support those responsibilities without smearing semantics across phases

## What Builder State Usually Holds

Builder/lowering state commonly tracks things like:

- symbol/name mappings
- current module/function context
- current `Self` type or impl context
- temporary locals/register mappings
- transient structures needed only during lowering

## Why This Matters

Concrete's roadmap strongly prefers:

- explicit pass ownership
- fewer hidden cross-phase assumptions
- fewer "special cases because the builder happened to know something"

That is why so much work has gone into clarifying pass contracts, lowering boundaries, and semantic language items.

## Modern Direction

The important modern direction is not the exact historical builder shape. It is:

- Resolve owns name resolution
- Check / Elab / CoreCheck own semantics
- Mono owns monomorphization
- Lower owns semantic-to-SSA lowering
- SSAVerify and SSACleanup defend the backend boundary

For the current stable view, prefer:

- `docs/ARCHITECTURE.md`
- `docs/PASSES.md`
