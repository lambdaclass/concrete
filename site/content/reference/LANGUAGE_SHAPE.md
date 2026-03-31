+++
title = "Language Shape"
+++

# Language Shape

Status: standing reference

This document makes Concrete's long-term language shape decisions explicit instead of letting them emerge from local convenience. It synthesizes commitments from [IDENTITY](@/reference/IDENTITY.md), [DESIGN_POLICY](@/reference/DESIGN_POLICY.md), [DECISIONS](@/reference/DECISIONS.md), and [SAFETY](@/reference/SAFETY.md) into a single picture of what Concrete is becoming.

## The Language Concrete Is Trying To Be

A small, low-level language where:

- Every function's authority is visible in its signature
- Every resource has a clear owner and destruction point
- Every call target is either statically known or explicitly indirect
- Every trust boundary is marked and auditable
- The compiler is structured enough to eventually prove properties about programs

The target is not "systems programming for everything." It is software that must be small, explicit, reviewable, and honest about power.

## Structural Commitments

These are not aspirations. They are load-bearing constraints that current code depends on.

### Dispatch model: static or explicitly indirect

All dispatch is monomorphized (compile-time) or through typed function pointers (runtime, visible). No closures, no trait objects, no implicit vtables. The call graph is either fully known at compile time or the indirection is visible at every call site.

**Depends on:** No closures, no trait objects decisions.
**Enables:** Static analysis, formal verification, predictable codegen.

### Authority model: capabilities in signatures

Semantic effects (file, network, allocation, etc.) are declared in function signatures via `with(Cap)`. A function can only call functions whose capabilities are a subset of its own. This is checked at both surface and Core IR levels.

**Depends on:** Capability checking in Check and CoreCheck.
**Enables:** Authority reports, proof eligibility analysis, high-integrity profiles.

### Trust model: three-way split

- `with(Cap)` — semantic effects visible to callers
- `trusted` — audited pointer-level containment behind safe APIs
- `with(Unsafe)` — foreign boundary authority

Each concept does one thing. They do not overlap. See [SAFETY](@/reference/SAFETY.md).

### Ownership model: linear by default

All structs and enums are linear (must be consumed exactly once) unless marked `Copy`. Branches must agree on consumption. Loops cannot consume linear variables from outer scope. Resources have deterministic destruction.

**Depends on:** Linearity checking in Check.
**Enables:** Resource safety without GC, deterministic cleanup.

### Compilation model: whole-program monomorphization

Generics are resolved at compile time. No separate compilation of generic code. The output is concrete, specialized functions with no runtime type information.

**Depends on:** Monomorphization pass.
**Enables:** Predictable codegen, no hidden runtime, proof-friendly Core IR.

### Phase separation: parse → resolve → check → elab → core → SSA → LLVM

Each pass has a clear input type, output type, and set of invariants. Early passes do not depend on late semantic information. Cross-file dependencies are declaration-level only (summaries and signatures).

**Depends on:** Pipeline architecture.
**Enables:** Incremental compilation (future), clear diagnostics ownership, proof extraction at Core level.

## What Concrete Will Not Become

These are standing constraints, not temporary limitations.

- **Not a convenience-first language.** Verbosity is acceptable when it buys visibility.
- **Not an inference-heavy language.** Types are written where they matter. No HM-style global inference.
- **Not a metaprogramming language.** No source-generating macros. No comptime evaluation (may revisit for constants only).
- **Not an ecosystem-first language.** Language clarity comes before ecosystem breadth.
- **Not a runtime-dependent language.** No GC, no async runtime, no thread pool. The runtime is minimal (startup, abort, optional allocator).

## What May Change (With Evidence)

These are areas where the current design may evolve, but only under explicit criteria.

| Area | Current state | What would trigger change |
|------|--------------|--------------------------|
| Concurrency | Single-threaded | Real-program pressure showing structured concurrency is needed |
| Freestanding mode | Hosted only | Stable stdlib boundary, clear core/hosted split |
| Capability hiding | Caps always visible | Sustained ergonomic pain in real programs |
| Pre/post conditions | None | ProofCore extraction mature, at least one Lean proof |
| Derived equality | Manual only | Trait/derive mechanism design settled |
| Package model | Single-file/lakefile | Real multi-package projects written |

## Shape Principles

1. **Features must earn admission.** The default answer to "should we add X?" is "not yet." See [DESIGN_POLICY](@/reference/DESIGN_POLICY.md).
2. **Removals are improvements.** Making the language smaller (removing `main!()`, narrowing `trusted`) makes it more coherent.
3. **Constraints compound.** Each "no" decision makes the remaining language easier to verify, teach, and audit.
4. **Real programs test shape.** The language shape should be validated by writing serious programs (Phase H), not by thought experiments alone.
5. **Document "no" decisions.** A rejected feature with a recorded rationale is more valuable than a feature added without one. See [DECISIONS](@/reference/DECISIONS.md).

## Related Documents

- [IDENTITY](@/reference/IDENTITY.md) — what Concrete is and is not
- [DESIGN_POLICY](@/reference/DESIGN_POLICY.md) — feature admission criteria
- [DECISIONS](@/reference/DECISIONS.md) — recorded "no" and "not yet" decisions
- [SAFETY](@/reference/SAFETY.md) — the three-way trust/capability/unsafe model
- [PROVABLE_SUBSET](@/reference/PROVABLE_SUBSET.md) — proof-eligible subset definition
- [../research/language/high-integrity-profile](https://github.com/unbalancedparentheses/concrete2/blob/main/research/language/high-integrity-profile.md) — high-integrity profile direction
