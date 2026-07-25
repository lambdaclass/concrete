+++
title = "Decisions"
+++

# Language Decisions

Status: standing reference

This document records first-class "no" and "not yet" decisions for Concrete. These are not gaps or missing features. They are deliberate choices that shape the language.

Every entry records what was decided, why, and what Concrete does instead. Permanent decisions are load-bearing constraints the verification and auditability stories depend on. Deferred decisions are not rejected, just sequenced.

For the admission criteria applied to new features, see [DESIGN_POLICY](@/reference/DESIGN_POLICY.md).
For the language identity and positioning, see [IDENTITY](@/reference/IDENTITY.md).
For the safety model, see [SAFETY](@/reference/SAFETY.md).

## Permanent Decisions

These are structural constraints. Reversing any of them would require rethinking the verification story.

### No closures

**Status:** Decided (2026-03-08)
**Detail:** [../research/language/no-closures](https://github.com/unbalancedparentheses/concrete2/blob/main/research/language/no-closures.md)

Closures bundle code and hidden captured data into a single opaque value. This violates three invariants: all code paths known at compile time, no hidden control flow, no hidden data flow.

**What Concrete does instead:**
- Function pointers (no capture, C-style) for callbacks
- Monomorphized generics for map/filter/fold patterns
- Struct of function pointers for multi-method pluggable interfaces
- Enums for closed dispatch

**What you lose:** Inline anonymous logic. You must define a named function.

### No trait objects

**Status:** Decided (2026-03-08)
**Detail:** [../research/language/no-trait-objects](https://github.com/unbalancedparentheses/concrete2/blob/main/research/language/no-trait-objects.md)

Trait objects (`dyn Trait`) hide dispatch behind an opaque vtable at runtime. This violates: all code paths known at compile time, no hidden control flow.

**What Concrete does instead:**
- Enums for closed dispatch (exhaustiveness checked)
- Monomorphized generics for compile-time dispatch
- Function pointers for explicit indirect dispatch
- Struct of function pointers for manual vtable (C pattern)

**What you lose:** Open extension by downstream code. Users cannot add new variants to your enum without modifying the definition.

### No source-generating macros

**Status:** Permanent
**Detail:** [../research/meta/candidate-ideas](https://github.com/unbalancedparentheses/concrete2/blob/main/research/meta/candidate-ideas.md) (rejected candidates section)

Source-generating macros destroy file-local parsing, couple early phases to late semantic information, and make audit output unreliable. They violate phase separation and locality.

**What Concrete does instead:** Monomorphized generics, ordinary functions,
explicit stdlib APIs, and external code generation as a build step whose output
is audited as ordinary source. There are no derive helpers, proc macros,
syntax macros, or compile-time source generation inside the language.

### No hidden dynamic dispatch

**Status:** Decided (2026-03-09)

All dispatch in Concrete is either statically resolved (monomorphization) or explicitly indirect (typed function pointers). There is no mechanism where `x.method()` can silently become a runtime vtable lookup.

### No inference-heavy abstraction layers

**Status:** Decided (2026-03-09)

Concrete does not have type inference beyond local let-binding inference. No HM-style global inference, no implicit conversions, no implicit trait resolution beyond monomorphization. The cost: more type annotations. The benefit: every type is visible where it matters, diagnostics have clear ownership, and the compiler frontend stays phase-separated.

### No lexical value shadowing

**Status:** Decided (2026-07-25); enforcement pending under ROADMAP R-0435.

A new local value binding may not reuse the spelling of another local value
binding visible along its lexical scope chain. The rule covers parameters,
`let` declarations, pattern and loop binders, and borrow-block value/region
binders. It rejects `let s = transform(s)` as a second binding even when the RHS
consumes the old value.

Assignment is different: it evolves one existing place rather than introducing
a binding. A `mut` linear place may be rebound after its old value is consumed,
so `acc = f(acc, x)` remains the ordinary fold/accumulate form. Assigning while
the old value is live remains E0219. The invariant is:

> One lexical name, one binding; at most one live resource in a linear place.

Sibling match arms may use the same binder spelling because neither is visible
from the other. A declaration executed repeatedly by a loop remains one lexical
binding. Field labels, type names, traits, and modules are separate namespaces;
top-level functions, imports, and intrinsics require a separate namespace
decision rather than an accidental expansion of this one.

The ban is an auditability and human-identity choice, not a claim that shadowing
is inherently memory-unsafe. Elab alpha-renaming remains mandatory under
[PRINCIPLES #12](https://github.com/unbalancedparentheses/concrete2/blob/main/docs/PRINCIPLES.md);
rejecting the source spelling does not authorize later passes to recover
identity from strings. Pattern-binder
renaming must land before enforcement so nested `Option`/`Result` patterns
remain expressible.

### Evidence is multidimensional, not a total ladder

**Status:** Decided (2026-07-25); implementation pending under ROADMAP R-0440.

Evidence is represented by orthogonal facts: subject, claim kind, semantic
scope, method, status, assumptions, producer, validator, trusted dependencies,
freshness, and replay receipt. A broad native oracle test and a narrow
kernel-checked ProofCore theorem are not globally rankable because they
establish different claims over different scopes.

A policy may prefer one discharge method over another only after subject and
scope are comparable. Friendly composite labels may not erase the underlying
dimensions or imply that a narrower claim dominates broader evidence merely
because its method is called a proof.

### Predictable generated-code performance is an affirmative project claim

**Status:** Decided (2026-07-25); evidence machinery pending under ROADMAP
R-0416–R-0419.

Concrete intends to make workload-scoped, replayable claims about generated-code
time, size, memory, and optimization stability. That commitment does not
authorize an unqualified “fast” badge or a comparison without a matched workload,
input, backend, target, toolchain, metric, uncertainty, raw samples, and replay
command. Before the evidence machinery lands, the honest status of an individual
comparison remains `performance_not_claimed`.

Optimization remains pull-gated by a reproduced gap or regression. Retiring the
project-level performance claim later would require retiring R-0416–R-0419 and
the corresponding public language together.

### Trusted means pointer containment only

**Status:** Decided (2026-03-15)

`trusted` permits exactly four pointer-level operations (arithmetic, deref, assign, cast) without `with(Unsafe)`. It does not suppress capabilities, does not permit extern calls, does not relax linearity. See [SAFETY](@/reference/SAFETY.md).

## Deferred Decisions

These are not rejected. They are explicitly sequenced as "not yet" to avoid premature complexity.

### `with(Std)` in high-integrity profiles — pending

**Status:** Pending; owned by ROADMAP R-0441.

Current profiles accept `with(Std)`. Rejecting the broad alias in a
high-integrity profile is the preferred proposal, but it is not yet a language
or profile guarantee. R-0441 must ratify either that restriction or an exact
transitive-use budget before changing accepted programs.

### Freestanding / no-std mode — not yet

**Status:** Deferred
**Detail:** [../research/stdlib/no-std-freestanding](https://github.com/unbalancedparentheses/concrete2/blob/main/research/stdlib/no-std-freestanding.md)

A freestanding mode (no libc, no hosted stdlib, explicit allocator) would be valuable for embedded, kernel, and audit-critical targets. Deferred because the hosted stdlib boundary is not yet stable enough. The capability system and explicit allocation model make this easier to add later.

**Prerequisite:** Stable hosted stdlib/runtime boundary, clear core-vs-hosted module split.

### Capability hiding in trusted wrappers — not yet

**Status:** Deferred

A mechanism where a trusted wrapper could absorb `with(Unsafe)` and expose only `with(Alloc)` to callers. Currently, capabilities are always visible in signatures. This would reduce signature noise for stdlib consumers but adds semantic complexity (callers cannot see what authority the wrapper actually uses).

**Prerequisite:** Sustained real-program pressure testing to determine if the ergonomic cost is real.

### Concurrency — not yet

**Status:** Deferred
**Detail:** [../research/stdlib/concurrency](https://github.com/unbalancedparentheses/concrete2/blob/main/research/stdlib/concurrency.md)

Concrete should only broaden concurrency once it can do so without importing async fragmentation and hidden runtime culture. The current implementation is single-threaded; the planned direction is explicit, threads-first concurrency with structured long-term evolution. No async/await, no goroutines, no thread pool runtime.

**Prerequisite:** Language surface and safety model stable enough that concurrency does not distort them.

### Pre/post conditions — not yet

**Status:** Deferred
**Detail:** [../research/language/pre-post-conditions](https://github.com/unbalancedparentheses/concrete2/blob/main/research/language/pre-post-conditions.md)

Contract annotations on functions (requires/ensures) would strengthen the proof story. Deferred because the ProofCore extraction and Lean proof story must mature first. Adding contracts before the proof pipeline is ready creates unverified annotations that look like guarantees.

**Prerequisite:** ProofCore extraction working, at least one proven function in Lean.

### Derived structural equality — not planned

**Status:** Not planned as a language feature
**Detail:** [../research/language/derived-equality-design](https://github.com/unbalancedparentheses/concrete2/blob/main/research/language/derived-equality-design.md)

Concrete has no derive mechanism. Equality for structs is written explicitly or
provided by ordinary library functions. External generators may produce source
files, but their output is reviewed and gated like any other source.

### Package/dependency model — not yet

**Status:** Deferred
**Detail:** [../research/packages-tooling/package-model](https://github.com/unbalancedparentheses/concrete2/blob/main/research/packages-tooling/package-model.md)

A real package manager and dependency system. Deferred until the language surface is stable and real programs have been written to validate the model.

## How to add a new decision

1. Write a research note in `research/` with the full analysis (what, why, alternatives, precedent).
2. Add an entry to this file with status, rationale summary, and link to the research note.
3. If permanent: explain what invariant it preserves and what Concrete does instead.
4. If deferred: explain why "not yet" and what the prerequisite is.
