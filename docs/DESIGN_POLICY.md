# Design Policy

Status: standing policy

This document defines the admission criteria for language features, compiler features, and borrowed ideas. It is the gate that every proposed change must pass through.

For recorded decisions (permanent "no" and deferred "not yet"), see [DECISIONS.md](DECISIONS.md).
For the language identity, see [IDENTITY.md](IDENTITY.md).
For the original research note, see [../research/design-filters.md](../research/design-filters.md).

## Admission Principle

Concrete is not trying to maximize shorthand or track trends. The standard for admission is:

- preserves simplicity
- improves reliability
- fits the verification story
- keeps semantics explicit

If an idea improves ergonomics while making the compiler harder to explain, it is a bad fit.

## Feature Admission Checklist

Every proposed feature must pass all ten checks. Failing any one is grounds for rejection.

### 1. Can it be explained as a simple invariant?

If the rule needs many exceptions, fallback cases, or "except when..." clauses, reject it.

### 2. Does it make behavior more visible or less visible?

Prefer features that expose effects, ownership, dispatch, layout, or control flow. Reject features that hide work behind familiar syntax.

### 3. Does it reduce compiler phase coupling?

Prefer ideas that keep parse, resolve, check, and elab boundaries clean. Reject ideas that make early phases depend on later semantic information.

### 4. Are cross-file dependencies declaration-level only?

Prefer summaries, signatures, layouts, and explicit imports. Reject anything that makes one file depend on another file's bodies.

### 5. Is dispatch still statically known or explicitly indirect?

Prefer monomorphization, named functions, and explicit function pointers. Reject hidden dynamic dispatch, hidden captures, or broad implicit lookup.

### 6. Does it preserve predictable code generation?

The user should be able to form a rough mental model of runtime behavior from the source. If the compiler may insert surprising work, be very skeptical.

### 7. Does it improve diagnostics or make them murkier?

A good feature should have obvious ownership for error reporting. If failures become "somewhere in inference/magic," reject it.

### 8. Can the compiler own it with one clear pass?

Each rule should have an obvious home. If multiple passes must partially own it, that is a warning sign.

### 9. Does it help or hurt the proof story?

Prefer rules that elaborate into simpler core forms. Reject rules that require semantic duplication, hidden state, or many meta-level exceptions.

### 10. Is the benefit real for audited low-level code?

Concrete is not trying to maximize shorthand. If the gain is mainly convenience for writing code faster, that is not enough.

## Quick Decision Rule

**Adopt** ideas that are: explicit, local, phase-separated, summary-friendly, easy to lower away.

**Reject** ideas that are: implicit, global, inference-heavy, body-dependent, hard to model formally.

## One-Line Test

A feature is promising if it makes the compiler and the language easier to explain at the same time.

## Main Rule for Borrowed Ideas

**Copy constraints before copying features.**

Zig, Austral, SPARK, and Odin are often most useful because they say "no" in structurally helpful places. Concrete should borrow their disciplined constraints, not their convenience features.

## High-Leverage Priorities

The highest-leverage improvements for Concrete are:

1. Summary-based frontend (declaration-level cross-file info)
2. Core as semantic authority (earlier, not just after elaboration)
3. ABI/layout subsystem clarity
4. Audit-focused tooling and compiler outputs
5. Small but excellent standard library
6. Explicit project/build model
7. Proof-driven narrowing

These are high leverage because they improve compiler structure, user trust, auditability, proof tractability, and future tooling options simultaneously.

## Ordering Principle

- Architecture before ornament
- Tooling visibility before convenience syntax
- ABI/layout credibility before feature expansion
- Proof-friendly boundaries before richer abstractions

Concrete gets stronger by becoming sharper, not merely bigger.
