# Verification Surface For Concrete

**Status:** Open

This note answers one question:

which ideas from verification-oriented languages actually fit Concrete's
philosophy, and in what order?

Concrete is not trying to become Dafny, SPARK, F*, or Why3 in systems-language
clothing. It is trying to remain a small no-GC systems language where
authority, execution risk, trust boundaries, and proof evidence are explicit
enough to audit, restrict, and sometimes prove.

That means verification features must earn their complexity.

## The Filter

A verification feature fits Concrete only if it strengthens at least one of:

1. visible authority
2. visible operational behavior
3. visible trust boundaries
4. visible proof/evidence workflow
5. artifact-first review and CI

It does **not** fit if it mainly adds:

- proof-language complexity to ordinary systems code
- a second hidden semantics layer
- large kernel/language machinery before examples need it
- proof syntax without a usable artifact workflow underneath it

## Strong Fits

### 1. Small explicit spec surface

This fits Concrete very well.

Why:

- proof evidence needs a named property
- reviewers need to know what claim a proof is attached to
- contracts/specs extend the same "make the boundary explicit" philosophy that
  already motivates capabilities and predictable profiles

What fits:

- Lean-attached specs
- named spec identities in artifacts
- later, small source-visible markers if needed

What does not fit yet:

- a broad source-level contract language as a first move

### 2. Loop invariants

This also fits Concrete strongly.

Concrete can already classify loops as bounded or not. That is not enough to
prove semantic properties *through* loops.

Loop invariants are the natural next bridge between:

- predictable/bounded execution
- proof-backed semantic claims

They should be added only when real examples need them.

### 3. Proof UX and proof diagnostics

This is a strong fit and should be treated as early workflow work, not late
polish.

If Concrete wants proofs to be part of normal engineering, users must be able
to tell:

- why a proof is stale
- why a function is not proof-eligible
- what obligation is missing
- what unsupported construct blocked extraction
- what changed between two proof-attached revisions

This is directly aligned with Concrete's clarity and auditability goals.

### 4. Assurance workflow

This is not extra philosophy; it is core philosophy.

Concrete should feel like an assurance workflow made out of compiler artifacts:

- facts
- proof status
- obligations
- extraction
- traceability
- semantic diff
- policy checks
- CI gates

This is how the project becomes useful to reviewers, teams, and AI tools rather
than just interesting to compiler authors.

### 5. Restricted analyzable subsets / profiles

This fits strongly and is already part of Concrete's direction.

The language should continue to push:

- predictable profiles
- no-alloc / bounded-allocation profiles
- no-FFI / trust-budget policies
- explicit package/module budgets

This is the SPARK-like part of Concrete that makes the systems language story
more operationally honest.

## Fits, But Carefully

### 6. Ghost code

Ghost code can fit, but only under tight discipline.

Why it helps:

- some proofs need proof-only state or proof-only bookkeeping
- it can make imperative proofs much more manageable

Why it is risky:

- it can make source code look more like a verification language than a systems
  language
- it introduces an erasure/trust story that must stay explicit

So the right rule is:

- add ghost code only when a real proof-backed example cannot be expressed
  cleanly without it
- keep it explicit and erasable
- tie it to the artifact/proof boundary story

### 7. Explicit proof-obligation / VC workflow

This fits, but mostly as tooling/artifact work rather than language syntax.

Concrete should copy the Why3 lesson:

- obligations should be generated explicitly
- obligations should be inspectable
- proof results should be replayable / explainable

But it should not become a giant theorem-management framework before the
artifact path is stable.

## Important Later, Not First

### 8. Stronger effectful-proof architecture

This matters long term, but it is not the first proof priority.

Concrete's current honest strength is:

- prove the pure core
- make effect boundaries explicit
- show where proof stops

The near-term job is not "prove arbitrary effectful code." It is:

- prove bounded core functions
- make shell/core splits explicit
- say exactly where capabilities, `trusted`, FFI, blocking, allocation, and
  backend assumptions stop the proof

That is the right path toward a stronger effectful-proof story later.

## Recommended Order

The best order for Concrete is:

1. proof diagnostics / proof UX
2. artifact-backed obligations and extraction workflow
3. small explicit spec surface
4. loop invariants when a flagship example needs them
5. ghost code only if a flagship example still cannot be expressed
6. stronger effectful-proof boundary model

## What Concrete Should Not Do

Concrete should not:

- add broad proof syntax before the artifact workflow is usable
- add a large verification language inside the source language
- copy dependent-type surfaces as the default way to write systems code
- let proof features weaken the small-language / audit-first philosophy

## Rule

Add verification features only if they make Concrete's existing thesis clearer:

- authority is visible
- operational behavior is reportable / enforceable
- proof evidence attaches to extracted Concrete semantics
- trust assumptions stay explicit
- the resulting workflow is understandable by someone other than the compiler
  author
