# Design Filters

**Status:** Process guideline
**Affects:** Language design, compiler architecture, roadmap decisions
**Date:** 2026-03-09

## Purpose

Use this note as the gate before adopting any language feature, compiler feature, or borrowed idea.

Concrete is not trying to maximize shorthand or track trends. The standard should be higher:

- preserve simplicity
- improve reliability
- fit the verification story
- keep semantics explicit

If an idea improves ergonomics while making the compiler harder to explain, it is probably a bad fit.

## Feature Admission Checklist

### 1. Can it be explained as a simple invariant?

If the rule needs many exceptions, fallback cases, or "except when..." clauses, reject it.

### 2. Does it make behavior more visible or less visible?

Prefer features that expose effects, ownership, dispatch, layout, or control flow.

Reject features that hide work behind familiar syntax.

### 3. Does it reduce compiler phase coupling?

Prefer ideas that keep parse, resolve, check, and elab boundaries clean.

Reject ideas that make early phases depend on later semantic information.

### 4. Are cross-file dependencies declaration-level only?

Prefer summaries, signatures, layouts, and explicit imports.

Reject anything that makes one file depend on another file's bodies.

### 5. Is dispatch still statically known or explicitly indirect?

Prefer monomorphization, named functions, and explicit function pointers.

Reject hidden dynamic dispatch, hidden captures, or broad implicit lookup.

### 6. Does it preserve predictable code generation?

The user should be able to form a rough mental model of runtime behavior from the source.

If the compiler may insert surprising work, be very skeptical.

### 7. Does it improve diagnostics or make them murkier?

A good feature should have obvious ownership for error reporting.

If failures become "somewhere in inference/magic," reject it.

### 8. Can the compiler own it with one clear pass?

Each rule should have an obvious home.

If multiple passes must partially own it, that is a warning sign.

### 9. Does it help or hurt the proof story?

Prefer rules that elaborate into simpler core forms.

Reject rules that require semantic duplication, hidden state, or many meta-level exceptions.

### 10. Is the benefit real for audited low-level code?

Concrete is not trying to maximize shorthand.

If the gain is mainly convenience for writing code faster, that is not enough.

## Quick Decision Rule

Adopt ideas that are:

- explicit
- local
- phase-separated
- summary-friendly
- easy to lower away

Reject ideas that are:

- implicit
- global
- inference-heavy
- body-dependent
- hard to model formally

## One-Line Test

A feature is promising if it makes the compiler and the language easier to explain at the same time.

## High-Leverage Priorities

The highest-leverage improvements for Concrete are likely to be:

1. summary-based frontend
2. Core as semantic authority
3. ABI/layout subsystem clarity
4. audit-focused tooling and compiler outputs
5. small but excellent standard library
6. explicit project/build model
7. proof-driven narrowing

These are high leverage because they improve several things at once:

- compiler structure
- user trust
- auditability
- proof tractability
- future tooling/performance options

## Main Rule For Borrowed Ideas

**Copy constraints before copying features.**

Zig, Austral, SPARK, and Odin are often most useful because they say "no" in structurally helpful places.

Concrete should borrow:

- explicit compiler boundaries
- summary-based interfaces
- proof-oriented design discipline
- simple whole-program lowering constraints

Concrete should be conservative about:

- ergonomic sugar
- inference-heavy abstraction systems
- hidden dispatch or allocation
- features that broaden cross-file coupling

## Recommendation

Concrete should spend effort first on improvements that multiply clarity across the whole project:

- architecture before ornament
- tooling visibility before convenience syntax
- ABI/layout credibility before feature expansion
- proof-friendly boundaries before richer abstractions

Concrete gets stronger by becoming sharper, not merely bigger.

## Related Notes

- [candidate-ideas.md](candidate-ideas.md)
- [external-ideas.md](external-ideas.md)
- [file-summary-frontend.md](file-summary-frontend.md)
- [mlir-backend-shape.md](mlir-backend-shape.md)
