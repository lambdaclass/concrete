# MLIR Backend Shape

**Status:** Open research direction
**Affects:** Backend architecture, backend plurality, tooling, MLIR integration
**Date:** 2026-03-12

## Purpose

This note describes what an MLIR backend should look like in Concrete, where it should sit in the pipeline, and what implementation strategy best fits the language's goals.

The main question is not "should Concrete use MLIR?" in the abstract.

The real questions are:

- where should MLIR enter the pipeline?
- what should MLIR consume?
- how much of the compiler should depend on MLIR?
- what integration strategy preserves simplicity and reliability?

## Short Answer

MLIR is only worth making a real backend target if Concrete wants a genuinely richer
multi-stage, multi-backend compiler.

If that happens, the safest architecture is:

- keep **SSA** as the backend boundary
- first replace raw LLVM string emission with a structured LLVM backend
- add an explicit backend abstraction over verified/cleaned SSA
- make MLIR a **consumer of validated/cleaned SSA**
- keep language semantics out of the MLIR layer
- use MLIR for backend lowering and optimization, not frontend meaning

This keeps the current architecture legible and avoids introducing a second semantic compiler pipeline.

If Concrete only wants to stop hand-emitting LLVM text and keep one native backend,
MLIR is probably more machinery than it is worth. In that case, the better path is:

- typed backend construction over LLVM
- SSA remains the only backend boundary
- no extra middle/backend layer until there is a concrete multi-backend need

## When MLIR Is Worth It

MLIR becomes a good fit when Concrete wants one or more of these:

- multiple serious backend targets (for example native LLVM, C, Wasm)
- a richer middle/backend optimization story than direct SSA -> LLVM permits
- more than one useful lowering level below SSA
- a backend architecture that can branch after SSA without duplicating semantics

MLIR is **not** automatically the right end goal just because raw LLVM strings are brittle.
The immediate problem there is better solved by a structured LLVM backend.

## Where MLIR Should Sit

Concrete's existing backend shape is:

```
Source -> Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA -> clang
```

The MLIR direction should be:

```
Source -> Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSAVerify -> SSACleanup -> backend abstraction -> {LLVM backend, MLIR backend, ...} -> binary
```

The key point is:

**MLIR should replace or sit beside the current target-emission backend, not become a new semantic middle-end.**

That means:

- no parsing/type-checking semantics should move into MLIR
- no surface-language constructs should survive to MLIR
- no second lowering path should bypass SSA

## Why SSA Should Remain The Boundary

Concrete already has a strong architectural direction:

- Core is the semantic IR
- SSA is the backend IR
- codegen should consume SSA, not surface syntax

If MLIR consumes SSA, several good things happen.

### 1. There is only one semantic pipeline

The language's meaning is decided before MLIR appears.

This keeps:

- pass ownership clear
- proof targets clear
- backend responsibilities narrow

### 2. MLIR stays replaceable

If MLIR is just a backend consumer of SSA, Concrete can still keep:

- a textual LLVM backend
- an MLIR backend
- future alternative backends

without multiplying semantic lowering paths.

### 3. Verification boundaries stay cleaner

Concrete's long-term verification story should target:

- validated Core for language semantics
- validated/cleaned SSA for backend preconditions

MLIR should not become part of the semantic trust story more than necessary.

## What The MLIR Layer Should Do

The MLIR backend should be responsible for:

- translating SSA control flow to MLIR control flow
- translating Concrete data layout decisions into target operations
- representing calls, memory operations, and aggregates
- handing off to LLVM-oriented lowering and optimization passes

It should not be responsible for:

- resolving names
- type checking the source language
- borrow checking
- capability validation
- trait/method semantics
- monomorphization

Those responsibilities belong earlier in the compiler.

## Candidate Implementation Strategies

There are three realistic implementation shapes.

### Option A: Thin Lean binding to the MLIR C API

Shape:

- Lean code calls into a small MLIR binding layer
- the backend constructs MLIR modules/programmatically via the C API

Pros:

- keeps the compiler in one overall architecture
- avoids text-format brittleness
- keeps the backend explicit and structured
- allows direct use of MLIR verification APIs

Cons:

- requires FFI/binding work
- MLIR APIs are not tiny
- Lean-to-C integration becomes part of the backend engineering burden

This is probably the best long-term fit if Concrete wants a serious MLIR backend without splitting the compiler in two.

### Option B: External SSA-to-MLIR lowering tool

Shape:

- Lean emits a serialized SSA artifact
- a separate tool in C++/Rust consumes it and builds MLIR

Pros:

- easier access to MLIR ecosystem tooling
- potentially faster initial development
- simpler than writing broad Lean bindings up front

Cons:

- weakens the "compiler in Lean" story
- introduces another implementation boundary and another trust boundary
- makes backend debugging and invariants easier to scatter

This may be acceptable as a bootstrap path, but it is not the cleanest long-term architecture.

### Option C: Textual MLIR emission

Shape:

- Lean prints MLIR text directly, similar to the current LLVM text backend

Pros:

- conceptually simple
- small implementation surface at first
- fast to prototype

Cons:

- brittle
- weaker structural guarantees
- harder to use MLIR APIs cleanly
- easier to generate malformed or awkward IR

This is acceptable for experimentation, but it is probably not the right final shape if MLIR becomes a real backend.

## Recommended Strategy

The best staged plan is:

### Stage 1: Stop hand-emitting LLVM as raw strings

Before MLIR becomes urgent, Concrete should first solve the immediate backend problem:

- replace textual LLVM concatenation with a structured LLVM backend
- keep SSA as the backend boundary
- avoid introducing another IR layer unless there is a concrete need

This can be done through:

- LLVM C API bindings
- or a small typed LLVM builder layer

### Stage 2: Preserve SSA as the backend boundary

Do not let MLIR affect frontend or Core architecture.

### Stage 3: Define an explicit backend abstraction over SSA

Before adopting MLIR, make the backend contract explicit:

- verified/cleaned SSA is the only backend input
- any backend must consume the same SSA boundary
- target-specific work begins only after SSA

### Stage 4: Define an explicit SSA-to-MLIR mapping

Write down how each SSA concept maps to MLIR concepts:

- modules
- functions
- blocks
- phi-like structure / block arguments
- aggregates
- memory operations
- calls
- control flow

### Stage 5: Start with a minimal backend path

Either:

- a very small textual MLIR prototype, or
- a minimal external lowering tool

This stage is for validating the mapping, not for finalizing architecture.

### Stage 6: Move to a real backend library

Once the mapping is stable, build a more principled MLIR backend layer, ideally through a thin binding approach.

### Stage 7: Add optimization passes only after the mapping is stable

Do not start by "using MLIR because optimization exists."

First ensure:

- the backend boundary is clean
- the mapping is correct
- verification/debugging of generated MLIR is straightforward

Then optimization becomes a reward for good architecture rather than a source of architectural pressure.

## Recommended Roadmap Placement

MLIR should not be in Concrete's immediate implementation queue.

The better order is:

1. finish the current compiler cleanup and stdlib deepening work
2. replace raw LLVM string emission with a structured LLVM backend
3. make backend plurality over SSA explicit
4. only then prototype MLIR if C/Wasm/additional backends are still a serious goal

So in roadmap terms:

- **Now:** not MLIR
- **Next:** maybe backend abstraction work, if backend plurality becomes active
- **Later:** MLIR as one possible backend family over SSA

## Which Dialects To Target

Concrete should be conservative here.

The likely progression is:

### 1. LLVM-oriented MLIR lowering

Use MLIR mainly as a structured path toward LLVM.

This is the simplest conceptual fit if Concrete still expects LLVM-ish codegen in the near term.

### 2. Minimal control-flow + memory representation first

Prioritize:

- functions
- control flow
- loads/stores
- calls
- aggregates

Avoid trying to express high-level language semantics in custom dialects too early.

### 3. Add custom dialects only if they buy something concrete

A custom Concrete dialect may sound appealing, but it is risky if it just recreates Core or SSA in another system.

A custom dialect only makes sense if it provides:

- real optimization value
- better tooling
- or cleaner lowering structure

## Bottom Line

MLIR is useful if Concrete eventually wants a richer, multi-stage, multi-backend compiler.

It is **not** the best immediate answer to the current backend problem.

The immediate answer is:

- keep SSA as the backend boundary
- stop emitting LLVM as raw strings
- add backend abstraction only when it earns its place

Then, if Concrete still wants C/Wasm/native backend plurality, MLIR becomes a strong later option.

Otherwise it is just another IR to maintain.

## Risks To Avoid

### 1. MLIR becoming a second semantic compiler

This is the biggest risk.

If language meaning has to be re-explained in MLIR, the architecture has failed.

### 2. Parallel backend paths with different semantics

If `EmitSSA` and the MLIR backend stop agreeing about layout, calls, or control flow assumptions, the compiler becomes harder to trust.

### 3. Adopting MLIR before backend invariants are stable

MLIR should not be used as a substitute for deciding Concrete's own backend invariants.

### 4. Building too much custom dialect surface too early

That risks reintroducing complexity under a different name.

## Relation To The Roadmap

MLIR belongs after the current architecture cleanup, not before it.

Concrete should first finish:

- summary-based frontend work
- Core semantic authority cleanup
- ABI/layout subsystem cleanup
- stable SSA/backend boundary

Only after that should MLIR become a serious implementation effort.

This matches the roadmap's basic ordering: architecture first, backend expansion after.

## Recommendation

If Concrete does MLIR, the rule should be:

**MLIR is a backend library over SSA, not a semantic layer of the compiler.**

That keeps the architecture simple, preserves the proof story, and avoids turning MLIR adoption into a rewrite of the compiler's core meaning pipeline.

## Related Notes

- [file-summary-frontend.md](file-summary-frontend.md)
- [candidate-ideas.md](candidate-ideas.md)
- [design-filters.md](design-filters.md)
