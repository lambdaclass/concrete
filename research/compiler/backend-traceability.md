# Backend Traceability

**Status:** Open

This note defines the source-to-machine trust story for Concrete.

## Why This Matters

Concrete wants stronger audit and evidence claims than typical systems languages.

That raises a hard question:

1. what does a source-level report actually claim?
2. how far does that claim survive lowering and code generation?
3. where do backend assumptions begin?

Without a clear answer, report-driven trust claims become ambiguous.

## The Layers

Concrete already has multiple semantic layers:

1. source language
2. checked/elaborated language
3. SSA
4. LLVM IR
5. machine code

Some claims are naturally source-level. Others depend on lower layers.

## The Main Distinction

Concrete should explicitly separate:

1. properties intended to be preserved by the compiler pipeline
2. properties reported only at a chosen intermediate layer
3. properties that require backend or target assumptions

Examples:

### Likely source-to-SSA claims

1. capability/authority requirements
2. explicit FFI boundaries
3. explicit trusted boundaries
4. absence of hidden allocation in the source language model
5. call-graph cycle information after monomorphization

### Likely backend-sensitive claims

1. precise stack usage
2. exact instruction counts
3. target-specific timing bounds
4. final machine-level memory layout effects from optimization choices

## Questions This Note Should Answer

1. which report modes are source-level claims?
2. which report modes are SSA-level claims?
3. which claims need preservation arguments across lowering?
4. what is trusted about LLVM and the backend pipeline?
5. what traceability should exist from a source function to emitted IR/code?

## Why This Connects To Predictable Execution

Predictable execution cannot stop at source syntax.

If Concrete eventually claims boundedness or stronger execution evidence, it must say:

1. what was proved or reported at source level
2. what remains true after lowering
3. what requires target-specific validation

This is not optional if the project wants evidence to be taken seriously.

## Relationship To Other Notes

1. [../proof-evidence/formalization-breakdown.md](../proof-evidence/formalization-breakdown.md)
2. [../proof-evidence/proof-addon-architecture.md](../proof-evidence/proof-addon-architecture.md)
3. [../predictable-execution/predictable-execution.md](../predictable-execution/predictable-execution.md)
4. [../predictable-execution/effect-taxonomy.md](../predictable-execution/effect-taxonomy.md)
5. [../../docs/PASSES.md](../../docs/PASSES.md)

## Bottom Line

Concrete needs an explicit trust story for the gap between source reports and emitted programs.

The immediate goal is not full compiler verification. It is a clear account of:

1. where each claim is anchored
2. which layers must preserve it
3. where backend assumptions begin
