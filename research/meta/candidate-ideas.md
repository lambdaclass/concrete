# Candidate Ideas

**Status:** Open research direction
**Affects:** Compiler architecture, language design, tooling
**Date:** 2026-03-09

## Purpose

This note turns broad inspirations into actual candidate ideas for Concrete.

The goal is to identify:

- concrete compiler changes
- concrete language features
- concrete tooling ideas
- ideas that should probably be rejected

Everything here should still be filtered through [design-filters.md](design-filters.md).

## Best Candidate Compiler Changes

### 1. Add a `FileSummary` pass

```
ParsedFile -> FileSummary
```

`FileSummary` should contain declaration-level information needed across files:

- module identity
- imports
- exported names
- type declarations
- function signatures
- impl headers
- trait headers
- trait-impl headers

Why it is strong:

- makes cross-file dependencies explicit
- supports parallel and incremental compilation
- reduces whole-program frontend coupling
- fits the explicit import/module model

### 2. Split `Resolve` into shallow resolution and body resolution

Concrete direction:

- `ResolveShallow`: declarations, imports, interfaces
- `ResolveBodies`: names inside expressions/statements using imported summaries

Why it is strong:

- gives clearer phase ownership
- aligns the compiler with the summary-based frontend goal

### 3. Make Core IR the semantic authority earlier

Concrete direction:

- move semantic ownership out of `Check` and into elaborated Core
- simplify `Check` into a frontend validation step
- make correctness arguments primarily about Core

Why it is strong:

- fewer duplicated semantic rules
- clearer proof target
- easier pass contracts

### 4. Enforce a hard monomorphization boundary

Concrete direction:

- guarantee that no generics survive past a specific boundary before SSA/lowering
- document this as a compiler invariant

Why it is strong:

- simplifies backend passes
- improves codegen predictability

### 5. Use linearity and ownership information in lowering/optimization

Concrete direction:

- optimize moves, destruction ordering, borrow handling, and storage reuse using linearity facts

Why it is strong:

- makes semantic guarantees operationally useful
- fits the language instead of bolting optimization on later

### 6. Introduce a more declarative internal engine for coverage/capabilities

Concrete direction:

- represent match coverage, capability propagation, or borrow-region checks in a more logic-like internal form

Why it is plausible:

- may reduce ad hoc checker logic
- may improve reliability

Risk:

- only worth it if it truly simplifies implementation

## Best Candidate Language Features

### 7. Narrow layout-control features for ABI-sensitive code

Concrete direction:

- extend or refine `#[repr(C)]`
- possibly add a very small explicit layout/alignment control surface

Why it might fit:

- low-level systems code needs representation control
- explicit layout is better than implicit compiler choice at FFI boundaries

Risk:

- layout features create complexity fast

### 8. Derived structural equality for predictable types

Concrete direction:

- allow `==` for structs/enums when semantics are compiler-derived, structural, and non-overridable

Why it might fit:

- removes boilerplate
- keeps behavior predictable

Risk:

- convenience features in this category must stay rare

### 9. Sharper explicit interface around modules/imports

Concrete direction:

- strengthen import/export rules so cross-file dependencies are even more declaration-driven
- possibly add explicit interface artifacts later

Why it might fit:

- improves auditability
- supports the summary-based frontend

## Best Candidate Tooling Ideas

### 10. Explicit project/build model in the main tool

Concrete direction:

- make project structure, target config, and FFI configuration explicit in the main `concrete` tool
- keep it small and boring

Why it might fit:

- reduces external build complexity
- makes compilation easier to understand

### 11. Compiler output modes focused on auditability

Concrete direction:

- better `--emit-core`, `--emit-ssa`
- layout inspection
- capability summaries
- import summaries
- pass-boundary debugging modes

Why it fits:

- directly serves the auditability story
- improves visibility without changing semantics

## Ideas That Are Probably Bad Fits

### 12. Source-generating macro systems

Why probably reject:

- destroys file-local parsing
- couples early phases tightly
- weakens the summary-based frontend direction

### 13. Hidden dispatch features

Examples:

- trait objects
- closures with hidden captures
- broad implicit method lookup

Why probably reject:

- weakens static call-graph clarity
- hides behavior and state

### 14. Inference-heavy abstraction layers

Examples:

- rich implicit effect inference
- complicated trait-resolution search behavior
- features whose meaning depends on broad global context

Why probably reject:

- makes diagnostics murkier
- increases phase coupling

### 15. Convenience sugar that inserts non-obvious work

Why probably reject:

- Concrete should not trade reliability for shorthand
- repeated small sugars can blur the language model

## Ranking By Actionability

Most actionable now:

1. `FileSummary` pass
2. split `Resolve`
3. move semantic authority toward Core
4. document/enforce hard monomorphization boundary
5. auditability-focused compiler output modes

Promising but should wait:

6. ownership-informed optimization
7. narrow layout-control features
8. explicit build/project model
9. declarative internal coverage/capability engine
10. derived structural equality

## Related Notes

- [design-filters.md](design-filters.md)
- [file-summary-frontend.md](file-summary-frontend.md)
- [derived-equality-design.md](derived-equality-design.md)
- [mlir-backend-shape.md](mlir-backend-shape.md)
