# What Still Makes A Complete Language System

**Status:** Open

This note captures the larger things a language still needs even after the compiler architecture, core language, and first standard-library wave are in place.

Concrete is already becoming a serious language design and compiler project. What remains after the current roadmap items is the work that turns a strong language into a complete language system.

## Big Missing Areas

### 1. Concurrency and runtime model

Even with a strong low-level stdlib, a modern systems language still needs a coherent answer for:

- concurrency structure
- blocking vs non-blocking execution
- runtime authority
- cancellation and cleanup
- scheduling boundaries

Concrete should treat this as a first-class design problem, not as “add async later.”

See [concurrency.md](concurrency.md).

### 2. Tooling maturity

Strong languages need more than a compiler.

Important missing tooling includes:

- formatter
- better diagnostics presentation
- editor/LSP support
- better inspection/debugging workflows
- stable machine-readable report surfaces
- migration/deprecation assistance for changing language and tooling behavior

These are the pieces that make a language pleasant and sustainable in real projects.

### 3. Backend and optimization maturity

The current SSA pipeline is already strong, but a complete language system often still wants:

- an explicit optimization policy, not only opportunistic codegen improvements
- profiling methodology and performance regression discipline
- deeper optimization work
- possibly additional backends (for example MLIR)
- stronger backend/debug info maturity
- eventually incremental/disk caching if the artifact boundaries are ready
- a clear target/platform support policy instead of implicit "whatever LLVM happens to tolerate"

Concrete should be especially careful here:

- performance work should not quietly outrun the audit/proof story
- incremental compilation should sit on explicit artifacts and boring pass contracts, not on hidden compiler coupling
- debug-info and observability quality should be treated as product surfaces, not accidental backend side effects

### 4. Formalization

Concrete’s long-term differentiator is not only explicit design, but proof-backed trust.

The important missing pieces here are:

- mechanized Core soundness work
- proof-backed lowering claims
- a smaller/frozen kernel story if the project still wants one

See [../docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) and [../docs/LANGUAGE_INVARIANTS.md](../docs/LANGUAGE_INVARIANTS.md).

### 5. Ecosystem and package model

A language becomes a system when people can build on it repeatedly.

That eventually means:

- a cleaner package/dependency workflow
- shared library conventions
- examples and reference applications
- guidance for “idiomatic Concrete”
- dependency trust / supply-chain expectations
- evidence-aware package metadata where it actually earns its cost

Concrete should be careful here: ecosystem growth should preserve explicitness rather than importing hidden-magic culture.

### 6. Compatibility and migration discipline

A complete language system eventually needs more than a vague statement that breaking changes are allowed or not allowed.

It needs:

- deprecation rules
- migration guidance
- explicit versioning expectations for reports and IR-facing tooling
- a story for how users move across language/compiler changes without relying on tribal knowledge

### 7. Trust and evidence operations

Concrete's audit/proof identity eventually pushes beyond "compiler outputs are nice" toward:

- reproducible trust bundles
- evidence authenticity / signing questions if the trust bundle story becomes operational
- build/dependency trust expectations
- reviewable links between source, reports, proofs, and produced artifacts

This is not just a proof problem and not just a CI problem.
It is part of what would make Concrete feel like a complete trust-oriented system instead of an interesting compiler.

### 8. Bootstrap and self-hosting stance

A complete language system also needs an explicit answer to whether it should:

- remain implemented in Lean long term
- become partially self-hosted in carefully chosen layers
- or eventually try to self-host broadly

This is not only an implementation question. It affects:

- trust and bootstrap policy
- proof leverage
- maintenance cost
- operational complexity
- what the project is actually optimizing for

Concrete should treat this as an explicit strategic choice, not as ambient pressure from "real languages self-host."

## Why This Matters

These areas are not “extra polish.” They are the bridge between:

- a strong language/compiler
and
- a complete usable platform

Concrete is already well past the “toy language” phase in architecture and direction. The question for the future is whether it can become a complete, disciplined low-level system without losing its clarity.

## Recommended Order

After the current near-term roadmap work, the likely larger system order is:

1. deepen the stdlib further
2. push formalization
3. develop the concurrency/runtime story
4. improve tooling
5. mature backend/optimization/caching work
6. grow ecosystem/package conventions carefully
7. add compatibility/migration and trust/evidence operations as product surfaces

## What To Avoid

Concrete should avoid becoming “complete” by just accreting whatever other languages already do.

In particular, avoid:

- convenience features that blur effect/resource boundaries
- runtime assumptions leaking into every library
- huge standard-library breadth without coherence
- ecosystem pressure toward hidden allocation, hidden dispatch, or hidden concurrency

The point is not to become feature-rich in the abstract. The point is to become a complete system while staying explicit, inspectable, and proof-friendly.
