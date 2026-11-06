# Runtime Collections

Status: open

Phase H showed that “collection exists” is not the same as “collection is mature enough for interpreter/runtime workloads.”

## Problem

Interpreter-style and runtime-heavy programs want:

- maps/dictionaries with predictable ownership and mutation patterns
- nested mutable structures that do not feel unnatural
- frame-friendly environment representations
- text/token/runtime helper structures that fit explicit ownership

The current collection story is already much stronger than before, but still thin for these workloads.

## Focus Areas

### Maps and dictionaries

- API ergonomics
- update/lookup patterns
- performance confidence
- ownership patterns under sustained mutation

### Nested runtime structures

- vectors of frames
- frame-linked environments
- stacks of scopes
- runtime object graphs expressed with explicit ownership

### Runtime-friendly patterns

- examples and idioms for interpreters, analyzers, and schedulers
- what belongs in stdlib versus example code

## Deliverable

Define what “runtime-oriented collection maturity” should concretely mean for Concrete, then identify which gaps are:

- missing APIs
- bad ergonomics
- missing examples/patterns
- broader runtime/tooling issues
