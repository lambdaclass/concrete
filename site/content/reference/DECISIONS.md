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

**Status:** Decided (2026-03-09)
**Detail:** [../research/meta/candidate-ideas](https://github.com/unbalancedparentheses/concrete2/blob/main/research/meta/candidate-ideas.md) (rejected candidates section)

Source-generating macros destroy file-local parsing, couple early phases to late semantic information, and make audit output unreliable. They violate phase separation and locality.

**What Concrete does instead:** Monomorphized generics, explicit code generation as a build step (not a language feature), and a small surface of compiler-recognized attributes.

### No hidden dynamic dispatch

**Status:** Decided (2026-03-09)

All dispatch in Concrete is either statically resolved (monomorphization) or explicitly indirect (typed function pointers). There is no mechanism where `x.method()` can silently become a runtime vtable lookup.

### No inference-heavy abstraction layers

**Status:** Decided (2026-03-09)

Concrete does not have type inference beyond local let-binding inference. No HM-style global inference, no implicit conversions, no implicit trait resolution beyond monomorphization. The cost: more type annotations. The benefit: every type is visible where it matters, diagnostics have clear ownership, and the compiler frontend stays phase-separated.

### Trusted means pointer containment only

**Status:** Decided (2026-03-15)

`trusted` permits exactly four pointer-level operations (arithmetic, deref, assign, cast) without `with(Unsafe)`. It does not suppress capabilities, does not permit extern calls, does not relax linearity. See [SAFETY](@/reference/SAFETY.md).

## Deferred Decisions

These are not rejected. They are explicitly sequenced as "not yet" to avoid premature complexity.

### Freestanding / no-std mode — not yet

**Status:** Deferred
**Detail:** [../research/stdlib-runtime/no-std-freestanding](https://github.com/unbalancedparentheses/concrete2/blob/main/research/stdlib-runtime/no-std-freestanding.md)

A freestanding mode (no libc, no hosted stdlib, explicit allocator) would be valuable for embedded, kernel, and audit-critical targets. Deferred because the hosted stdlib boundary is not yet stable enough. The capability system and explicit allocation model make this easier to add later.

**Prerequisite:** Stable hosted stdlib/runtime boundary, clear core-vs-hosted module split.

### Capability hiding in trusted wrappers — not yet

**Status:** Deferred

A mechanism where a trusted wrapper could absorb `with(Unsafe)` and expose only `with(Alloc)` to callers. Currently, capabilities are always visible in signatures. This would reduce signature noise for stdlib consumers but adds semantic complexity (callers cannot see what authority the wrapper actually uses).

**Prerequisite:** Sustained real-program pressure testing to determine if the ergonomic cost is real.

### Concurrency — not yet

**Status:** Deferred
**Detail:** [../research/stdlib-runtime/concurrency](https://github.com/unbalancedparentheses/concrete2/blob/main/research/stdlib-runtime/concurrency.md)

Concrete should only broaden concurrency once it can do so without importing async fragmentation and hidden runtime culture. The current implementation is single-threaded; the planned direction is explicit, threads-first concurrency with structured long-term evolution. No async/await, no goroutines, no thread pool runtime.

**Prerequisite:** Language surface and safety model stable enough that concurrency does not distort them.

### Pre/post conditions — not yet

**Status:** Deferred
**Detail:** [../research/language/pre-post-conditions](https://github.com/unbalancedparentheses/concrete2/blob/main/research/language/pre-post-conditions.md)

Contract annotations on functions (requires/ensures) would strengthen the proof story. Deferred because the ProofCore extraction and Lean proof story must mature first. Adding contracts before the proof pipeline is ready creates unverified annotations that look like guarantees.

**Prerequisite:** ProofCore extraction working, at least one proven function in Lean.

### Derived structural equality — not yet

**Status:** Deferred
**Detail:** [../research/language/derived-equality-design](https://github.com/unbalancedparentheses/concrete2/blob/main/research/language/derived-equality-design.md)

Auto-deriving `==` for structs with all-Copy fields. Deferred because it requires deciding the trait/derive mechanism more carefully.

### Package/dependency model — not yet

**Status:** Deferred
**Detail:** [../research/packages-tooling/package-model](https://github.com/unbalancedparentheses/concrete2/blob/main/research/packages-tooling/package-model.md)

A real package manager and dependency system. Deferred until the language surface is stable and real programs have been written to validate the model.

## How to add a new decision

1. Write a research note in `research/` with the full analysis (what, why, alternatives, precedent).
2. Add an entry to this file with status, rationale summary, and link to the research note.
3. If permanent: explain what invariant it preserves and what Concrete does instead.
4. If deferred: explain why "not yet" and what the prerequisite is.
