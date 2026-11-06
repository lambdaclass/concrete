# Why Lean 4?

Concrete uses Lean 4 because Lean gives the project a serious proof environment, not because Concrete is trying to turn into a proof assistant.

## The Split

The intended split is:

- **Concrete** is the low-level implementation language
- **Lean 4** is the theorem and proof environment
- **validated Core** is the bridge between them

That means the project can aim for two different kinds of trust:

- trust in the language/compiler itself
- trust in selected Concrete programs

## Why Not Just Write Everything In Lean?

Lean 4 is powerful, but it is not designed as the ordinary low-level systems language for this project.

It has:

- a runtime
- a garbage collector
- a proof-oriented programming model

Concrete exists because the implementation language still needs to be:

- explicit about ownership and destruction
- explicit about FFI and layout
- explicit about authority and trust boundaries
- suitable for low-level systems work without leaning on a GC-oriented runtime model

## Two Lean 4 Goals

Concrete's Lean 4 story has two layers.

### 1. Prove The Language And Compiler

Examples:

- type soundness
- ownership and linearity coherence
- capability and trust preservation
- Core -> SSA preservation

This is about the compiler and the language design itself.

### 2. Prove Selected Concrete Functions

Examples:

- a parser round-trips with a formatter
- a collection operation preserves an invariant
- a critical routine satisfies a specification

This is about particular user programs.

## Why Selected Function Proofs Come Earlier

Proving selected Concrete functions is much smaller in scope than proving the whole compiler.

The intended proof boundary sits:

- after `CoreCheck`
- before `Mono`

At that point the code is already:

- explicit typed Core
- semantically validated
- still close to source meaning

That makes it a much better first proof object than the full compiler pipeline.

## The Milestone

One of the most important planned breakthroughs for Concrete is:

1. write a selected Concrete function
2. expose its validated Core / `ProofCore` representation
3. write Lean 4 proof code about that function

That is narrower than "prove the whole compiler," but it is also much more achievable earlier.

## Where To Go Deeper

- [Project Direction](./direction.md)
- [Architecture](./architecture.md)
- [research/proof-evidence/proving-concrete-functions-in-lean.md](../../../research/proof-evidence/proving-concrete-functions-in-lean.md)
- [research/proof-evidence/formalization-roi.md](../../../research/proof-evidence/formalization-roi.md)
