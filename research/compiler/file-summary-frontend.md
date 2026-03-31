# File-Summary Frontend Architecture

**Status:** Implemented — summary frontend is complete
**Affects:** Frontend architecture, modules/imports, method resolution, future incremental compilation
**Date:** 2026-03-09 (design), 2026-03-10 (completed)

## Context

Concrete already leans toward a language design that should make the frontend simpler than Rust's:

- LL(1) grammar
- explicit modules and imports
- no macros today
- no hidden control flow
- explicit capabilities, borrows, destruction, and allocation

The current compiler pipeline is still largely whole-program and batch-oriented:

```
Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSA
```

This is the right shape for clarity, but it still processes `List Module` at several stages and builds combined scopes/export tables over the whole program.

The question is whether Concrete should push further toward a Zig-like frontend architecture: parse each file independently, build file-local semantic summaries, and defer whole-program work until later phases where it is actually needed.

## Reference: matklad on Query-Based Compilers

In [Against Query Based Compilers](https://matklad.github.io/2026/02/25/against-query-based-compilers.html) (February 25, 2026), matklad argues that language design strongly determines compiler architecture.

The key comparison in the article is:

- **Zig** can parse files independently because macros do not generate source code and the grammar is designed to stay file-local.
- **Rust** cannot fully parse independently at the file level because macro expansion and crate-wide name resolution feed back into parsing and early semantic stages.

The useful takeaway for Concrete is not "never use queries" as a slogan. The useful takeaway is:

**If the language avoids early cross-file semantic dependencies, the compiler can use coarse-grained passes with cheap summaries rather than fine-grained dependency tracking from the start of the frontend.**

That is already close to Concrete's stated philosophy.

## Historical Context

Concrete's pass contracts already separate syntax from semantics:

- `Parse` produces a syntactically valid AST.
- `Resolve` validates many names and imports.
- `Check` handles type checking, linearity, capabilities, and some resolution that depends on types.
- `Elab` lowers to fully typed Core IR.

Relevant current properties:

1. Parsing is independent of semantic reasoning.
Concrete's parser is LL(1) recursive descent and does not depend on macro expansion or type information.

2. Module file loading is separate from parsing.
`mod X;` currently triggers recursive file loading before the semantic passes.

3. Resolution and checking are still whole-program in shape.
`Resolve` builds a combined global scope across modules.
`Check` builds an export table across modules and then checks each module using imported signatures.

4. Instance method resolution still depends on type information.
This is explicitly documented as a boundary between `Resolve` and `Check`.

This is the direction the compiler was moving toward before the summary frontend landed.

## Implementation Status (2026-03-10)

The summary-based frontend is now implemented. Here is what landed:

### Artifacts

| Artifact | Location | Consumers |
|----------|----------|-----------|
| `FileSummary` | `Concrete/FileSummary.lean` | Resolve (shallow phase), Check, Elab |
| `ResolvedImports` | `Concrete/FileSummary.lean` | Check, Elab |
| `FnSummary` | `Concrete/FileSummary.lean` | All passes via FileSummary |

### Pass responsibilities

| Pass | Role |
|------|------|
| **Resolve** | Purely shallow/interface: module existence, import validity, top-level name visibility, deep type name validation. Consumes `FileSummary`. Does NOT own trait impl completeness or any post-elaboration semantic rule. |
| **Check** | Surface/inference-specific: linearity/borrow tracking, type inference, cap-polymorphic call resolution, surface-syntax-dependent checks. Consumes `ResolvedImports`. |
| **Elab** | Elaboration to Core IR. Consumes `ResolvedImports`. |
| **CoreCanonicalize** | Pure Core→Core normalization. Recurses through submodules. |
| **CoreCheck** | Post-elaboration semantic authority: capability discipline, match completeness, operator/return-type legality, declaration-level trait/FFI/repr checks. Recurses through submodules. |

### What moved where

- Trait impl completeness: Resolve → CoreCheck (operates on Core IR after elaboration)
- Copy/Destroy conflict, Copy field validation: Check → CoreCheck
- repr(C)/packed/align validation: Check → CoreCheck
- FFI safety (extern fn params/return): Check → CoreCheck
- Builtin trait redeclaration: Check → CoreCheck
- Reserved function names: duplicated in both Check (early gate) and CoreCheck
- Submodule processing: CoreCheck and CoreCanonicalize now recurse into `mod X { ... }` blocks

### "Done enough" criteria met

1. `FileSummary` is the single cross-file interface artifact — all passes consume it rather than rebuilding views from raw ASTs. **Caveat:** FileSummary and ResolvedImports carry full impl/trait-impl blocks with method bodies (not just signatures) because Check and Elab need them for cross-module method type-checking and elaboration. Splitting into interface-only and body portions is a future incremental-compilation concern.
2. `ResolvedImports` is an explicit stable artifact (documented, not ad hoc)
3. No early pass rebuilds import/export/signature views ad hoc
4. Resolve is purely shallow/interface-oriented
5. Check is clearly surface/inference-specific
6. CoreCheck owns all post-elaboration legality rules
7. The artifact flow is explicit in the pipeline

### What remains for future incremental compilation

- Split `FileSummary` into interface-only and body portions (currently carries `implBlocks`/`traitImpls` for Elab)
- Cache summaries by file content hash
- Parallel per-file parsing and summary construction
- Incremental rebuild keyed off file hash + imported summary hashes

---

## Original Design Direction (preserved for reference)

## Goal

Move the frontend toward **file summaries as the main cross-file interface**.

Instead of feeding full ASTs from all files into every early pass, each file should produce a summary that contains exactly the information other files need:

- declared module name
- imports
- public items
- type declarations
- struct fields and enum variants
- function signatures and capability sets
- impl headers
- method signatures
- trait declarations
- trait-impl headers

Other files should depend on these summaries, not on the original AST bodies.

## Proposed Architecture

### 1. Parse files independently

Introduce a per-file frontend unit:

```
SourceText -> ParsedFile
```

`ParsedFile` should preserve:

- top-level declarations
- function bodies
- spans/diagnostics
- unresolved `mod` declarations

This step should remain purely syntactic.

### 2. Build a shallow file summary

Add a new pass:

```
ParsedFile -> FileSummary
```

`FileSummary` was the crucial new abstraction. In the implemented version it became the declaration-level interface artifact, but it still carries impl/trait-impl bodies because imported method checking and elaboration need them today.

Suggested contents:

- file path / module path
- imported module names and imported symbols
- public and private top-level names
- function signatures
- extern signatures
- constants and type aliases
- struct/enum/newtype declarations
- trait headers and method signatures
- impl headers (`impl T`, `impl Trait for T`)
- method signatures from impl blocks

This pass should not inspect function bodies beyond what is needed to collect local declaration metadata.

### 3. Resolve imports from summaries only

Replace ad hoc cross-module export construction with:

```
List FileSummary -> ImportIndex
```

This pass answers:

- which modules exist
- which symbols are exported
- whether imports are valid
- whether there are circular module dependencies

At this stage, no function bodies need to be loaded from imported files.

### 4. Resolve file bodies against imported summaries

Split name resolution into two layers:

- **Shallow resolution**: declarations, imports, top-level names
- **Body resolution**: names inside expressions/statements

Body resolution for file `A` should have access to:

- `A`'s own full AST
- `A`'s local declarations
- imported `FileSummary` data from other files

This avoids using a whole-program merged scope when only summary data is required.

### 5. Type-check per file using summaries

Refactor checking so that a file is checked from:

```
CheckedFile = check(ParsedFile, LocalSummary, ImportedSummaries)
```

This should be enough for:

- expression typing
- function-call validation
- capability propagation
- linearity and borrow checks
- FFI validation

The main discipline is that imported files contribute signatures and type declarations, not executable body semantics.

### 6. Elaborate per file to Core IR

Once a file is checked, elaboration can also be per-file:

```
CheckedFile -> CoreFile
```

Whole-program work can then happen later, at the Core level, where the IR is more explicit and dependencies are narrower and easier to reason about.

## The Hard Part: Method and Trait Resolution

This is the main architectural constraint.

Concrete currently documents that instance method resolution cannot happen in `Resolve` because the receiver type is only known after type checking. That is fine, but it creates an important design question:

**Can method lookup be driven entirely from imported summaries once the receiver type is known?**

For the file-summary architecture to work cleanly, the answer should be "yes".

That suggests the following rule:

- imported files expose impl headers and method signatures
- body checking may use those summaries to determine method candidates
- body checking should not need to inspect imported function bodies

This is still compatible with a later type-directed method lookup step, as long as the lookup surface is summary-only.

### Implication for language design

If Concrete adds features that make method lookup depend on arbitrary body-level reasoning, the summary-based frontend becomes much harder.

The architecture therefore favors:

- explicit imports
- explicit trait bounds
- no source-generating macros
- no body-dependent name lookup
- no hidden capture environments

These are already aligned with existing design decisions.

## Why This Is Better Than a Query-First Frontend

For Concrete, a file-summary architecture has three advantages.

### 1. It matches the language's explicitness goals

Concrete is deliberately trying to make semantics visible in the source. A summary-based frontend is the compiler analogue of that same principle: expose declaration interfaces early, avoid hidden cross-file dependencies, and make phase boundaries explicit.

### 2. It preserves a simple pass pipeline

Concrete's compiler documentation already emphasizes explicit pass contracts. File summaries strengthen this model instead of replacing it with a large fine-grained dependency graph.

### 3. It creates a clean path to incremental and parallel compilation

Once `FileSummary` exists:

- parsing can run per file in parallel
- summary construction can run per file in parallel
- import validation can run from summaries
- unchanged files can reuse cached summaries
- checked/elaborated outputs can be cached by file hash plus imported-summary hashes

This gets many of the practical benefits people want from query systems without introducing query machinery at the front of the compiler.

## Why Not Salsa-Style Queries?

For Concrete, a Salsa-style or broadly query-first frontend is probably the wrong default architecture.

Query systems are strongest when the language itself creates many fine-grained, cyclic, demand-driven dependencies:

- source-generating macros
- complex name lookup rules
- body-sensitive interface semantics
- heavy IDE-first incremental demands from the beginning
- semantic questions that naturally recurse across many files and phases

That is much closer to Rust's problem shape than Concrete's.

Concrete is deliberately trying to stay in a better position:

- LL(1) parsing
- explicit imports
- no source-generating macros
- explicit capabilities and effects
- no hidden lookup rules
- fewer surface forms that require global semantic feedback

Because of that, Concrete should benefit more from a **summary-based pass pipeline** than from a query framework.

The main reasons are:

1. **Simpler compiler architecture**
   File summaries preserve a clear pass structure and keep the frontend understandable without a large dependency engine.

2. **Better fit with the language philosophy**
   Concrete wants explicit boundaries in the language; file summaries are the compiler analogue of the same idea.

3. **Easier long-term specification and proof work**
   A coarse-grained, summary-based frontend is easier to describe, test, and eventually reason about mechanically than a fine-grained query graph.

4. **Probably enough incremental behavior without the framework cost**
   If file summaries and their hashes become the unit of reuse, Concrete can still get useful caching and parallelism without adopting query machinery as the primary frontend model.

This does not mean query systems are bad. It means Concrete should first exploit the advantages created by its own language design before reaching for a more complex compiler framework.

If Concrete later adds features that destroy file-local boundaries or introduce much more dynamic cross-file semantics, that conclusion could change. But with the language as currently designed, a summary-based frontend is the better architectural default.

## What Must Not Be Added Casually

If Concrete wants this architecture, the following features should be treated as high-risk:

### 1. Source-generating macros

Rust-style macros are the clearest way to destroy file-local parsing and early summary construction. If macros are ever added, they should be phase-separated and hygienic, or Concrete should continue to reject them entirely.

### 2. Implicit cross-file lookup rules

If a use site can trigger broad crate-wide search without explicit imports or explicit summary-visible declarations, the file-summary design starts to collapse.

### 3. Body-sensitive interface semantics

Cross-file compilation should depend on declarations, signatures, and type layout, not on the executable content of other files' function bodies.

## Migration Plan for This Repository

The practical path is incremental.

### Phase 1: Introduce `FileSummary`

Extract the existing export-table logic into an explicit data structure and builder pass.

Likely sources:

- `Concrete/Resolve.lean`
- `Concrete/Check.lean`
- `Main.lean` module-loading logic

The first version can be minimal and coexist with the current whole-program pipeline.

### Phase 2: Separate shallow and body resolution

Refactor `Resolve` so declaration/interface resolution is summary-based while body validation remains in a second step.

This is a structural change, not a language change.

### Phase 3: Check files against imported summaries

Refactor `Check` so imported information flows through `FileSummary` rather than through whole-program merged structures.

### Phase 4: Elaborate per file

Once checking is file-scoped, elaboration can follow the same shape.

### Phase 5: Add caching and parallelism

After the semantic interfaces are stable, parallel parsing/indexing/checking becomes straightforward, and incremental rebuilds can key off file content plus imported summary hashes.

## Open Questions

1. Should the summary layer include private declarations, or only exported ones plus enough local metadata for checking the file itself?
2. How should nested modules and `mod X;` file loading map onto a stable file/module identity model?
3. Should trait impl summaries include only method signatures, or also normalized receiver/type information for faster method lookup?
4. Which validations are truly file-local, and which should remain crate-level checks even after this refactor?
5. Should Core IR become the first phase with any whole-program semantic authority, leaving the surface frontend strictly file-scoped?

## Recommendation

Concrete should move toward a **summary-based frontend, not a query-first frontend**.

This is the architecture most consistent with:

- the language's LL(1) and explicit-design goals
- the current pass-contract documentation
- the absence of macros
- the long-term verification goal

That part is now implemented. The remaining future refinement is narrower:

- if incremental compilation becomes a priority, split interface-only and body-bearing portions of `FileSummary` / `ResolvedImports`
- otherwise keep the current coarse-grained artifact boundary and move on to the next architecture item

That single move would turn the current whole-program frontend from an implementation convenience into a more principled architecture, while keeping the existing compiler understandable.

## References

- matklad, [Against Query Based Compilers](https://matklad.github.io/2026/02/25/against-query-based-compilers.html), February 25, 2026.
- [README.md](../README.md)
- [docs/PASSES.md](../docs/PASSES.md)
- [ROADMAP.md](../ROADMAP.md)
