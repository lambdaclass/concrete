# Concrete Influences

Status: stable reference

This document tracks the external languages and systems that Concrete learns
from.

It is not a claim that Concrete should become a blend of all of them. The point
is the opposite: copy the constraints and workflow lessons that strengthen a
small, auditable, no-GC systems language, and reject the machinery that would
make Concrete less clear.

The synthesis Concrete is aiming for is roughly:

- Zig/Austral-style small explicit systems programming
- SPARK/Dafny/Why3-style assurance workflow and proof discipline
- Lean 4 as implementation language and proof environment
- explicit capabilities, predictable-execution boundaries, and artifact-first auditability as the distinctive Concrete layer

That synthesis only works if the result stays minimalist, linear/resource-aware,
no-GC, and honest about what is enforced, reported, proved, or trusted.

## Positioning Summary

The surrounding tools each cover part of the space:

- Rust, Zig, C, and C++ are strong systems languages, but proofs usually live
  outside the compiler workflow.
- SPARK/Ada, Dafny, F*, and Why3 have strong verification workflows, but are not
  trying to be a small C/Rust/Zig-style systems language with Lean as the
  compiler/proof substrate.
- Lean, Coq, and Isabelle are excellent proof systems, but ordinary low-level
  systems programming is not their primary path.
- Austral is close on linear safety, but not on Lean-backed proof artifacts and
  drift-gated evidence.

Concrete's niche is the intersection: no-GC systems code, explicit authority,
linear/resource discipline, selected Lean theorems, and machine-readable
evidence tying the theorem back to the current source.

## How To Read This

Each influence is classified as:

- **Copied** — already clearly present in Concrete
- **Adapted** — an idea Concrete wants, but in a smaller or different form
- **Rejected** — an idea Concrete intentionally does not want

## Core Influences

### Lean 4

**Status:** Copied + adapted

Concrete takes:

- implementation in Lean 4
- theorem-proving environment for compiler and user-program proofs
- explicitness over hidden behavior
- the expectation that important claims should eventually be machine-checked

Concrete does not take:

- GC as the execution model
- proof assistant ergonomics as the default programming style
- dependent types as the default surface of the systems language

The key distinction is that Concrete is Lean-implemented and Lean-provable in
parts, but it is not trying to become "Lean without GC."

### Rust

**Status:** Adapted

Concrete takes:

- serious compiler-enforced safety culture
- explicit trust/unsafe boundary thinking
- the idea that guarantees should be compiler facts, not conventions
- engineering rigor around diagnostics, testing, and review discipline

Concrete does not take:

- borrow-checker and lifetime surface complexity
- trait system scale
- macro-heavy language growth

### Zig

**Status:** Adapted

Concrete takes:

- small-language pressure
- explicit low-level control
- no hidden GC
- straightforward systems programming feel
- preference for visible boundaries over language magic
- the recent I/O design lesson that runtime choice should belong to the
  application rather than splitting libraries into sync and async ecosystems

Concrete is researching a related async/concurrency direction, but adapting it
into capabilities, structured scopes, linear handles, and evidence reports
rather than a pervasive `io` parameter or generic async/await color. See
[../research/stdlib/async-concurrency-evidence.md](../research/stdlib/async-concurrency-evidence.md).

Concrete does not take:

- "manual control first, proof/evidence later" as the whole philosophy
- async/evented I/O as an implemented feature today

### Austral

**Status:** Copied + adapted

Concrete takes:

- visible capabilities in interfaces
- linear ownership without a giant abstraction tower
- auditability-first design
- language-size discipline

Concrete differs by pushing harder on:

- proof/evidence artifacts
- predictable execution reporting and enforcement
- Lean-backed proof direction

### SPARK / Ada

**Status:** Adapted

Concrete already takes:

- restricted analyzable profiles
- assurance workflow as a first-class concern
- explicit trust and review discipline
- the idea that operational restrictions should be user-visible architectural tools

Concrete still wants to add carefully:

- contracts
- loop invariants
- ghost code

Concrete does not want:

- a large language/kernel burden just to support proof-oriented features

### Dafny

**Status:** Adapted

Concrete wants to copy:

- proof usability
- actionable proof diagnostics
- proof maintenance ergonomics
- a normal-feeling function/spec workflow

Concrete does not want:

- a source language that feels like a proof script by default

### Why3 / WhyML

**Status:** Adapted

Concrete wants to copy:

- explicit proof-obligation artifacts
- inspectable separation between code, specs, obligations, and proof results
- proof workflow that feels like a real tool pipeline, not hidden magic
- machine-consumable proof/audit artifacts rather than only human prose

### F*

**Status:** Adapted

Concrete wants to copy:

- clear effect/proof boundary thinking
- explicit extraction/trust-boundary methodology
- honesty about what is proved at source/proof-IR level versus what is trusted
  in runtime, backend, FFI, or target

Concrete does not want:

- to become a full effect-typed proof language first and a systems language
  second

### Frama-C

**Status:** Adapted lightly

Concrete can learn from Frama-C:

- practical contracts/invariants over low-level code
- analyzable restricted subsets rather than "verify everything at once"
- tool-assisted assurance workflow around ordinary systems programming

Concrete should not copy:

- a plugin-heavy or analysis-fragmented workflow as its primary UX

### ATS

**Status:** Adapted

Concrete takes:

- serious resource-aware low-level programming
- linearity as a practical systems tool

Concrete does not want:

- proof-term density in ordinary source code
- a language surface that is too dense for audit-first use

### Liquid Haskell

**Status:** Adapted lightly

Concrete can learn from Liquid Haskell:

- lightweight contract/refinement ideas for API-level properties
- proving useful bounds and shape properties without turning the entire
  language into a dependent-type system

Concrete should not copy:

- SMT/refinement machinery as the default explanation surface for ordinary
  systems code

### Vale

**Status:** Adapted narrowly

Concrete can learn from Vale:

- proof-aware low-level boundary discipline
- strong separation between trusted low-level envelopes and proved higher-level
  properties
- serious handling of cryptographic and security-sensitive code

Vale matters more as a lesson in boundary discipline than as a surface-language
model for Concrete.

### CompCert

**Status:** Adapted as compiler-trust influence

Concrete can learn from CompCert:

- explicit compiler-correctness mindset
- honesty about backend trust boundaries
- the value of proving selected compiler-preservation properties where they
  protect real evidence claims

CompCert is not a surface-language influence. It is a reminder that if
Concrete's thesis depends on compiler-reported evidence, the compiler and
backend contracts themselves eventually matter.

## Secondary Influences

### Cyclone

**Status:** Adapted lightly

Concrete takes:

- checked low-level subsets
- the idea that dangerous operations should be visible and boring

### Idris / Agda

**Status:** Rejected as main surface, adapted as a lesson

Concrete keeps only:

- the lesson that proof/spec attachment should become more direct when users
  choose it

Concrete rejects:

- making a full dependent-type surface the normal way to write systems code

## What Concrete Is Not Copying

Concrete should continue to reject:

- trait/typeclass scale as a default language-expansion path
- macros as the main extensibility story
- hidden dynamic dispatch
- proof-heavy source code as the ordinary programming style
- adding verification features before the artifact/workflow path is usable
- hidden runtime models that undermine systems-level auditability
- turning the language into a theorem prover instead of a systems language

## Current Priority From These Influences

The highest-value still-missing ideas are:

1. a small explicit spec surface
2. loop invariants for real bounded examples
3. proof UX and proof diagnostics
4. stronger assurance workflow polish
5. ghost code only if a real proof-backed example forces it

Those fit Concrete's philosophy best because they strengthen:

- explicit authority
- explicit operational behavior
- explicit evidence
- explicit trust boundaries
- an artifact-first review and CI workflow
