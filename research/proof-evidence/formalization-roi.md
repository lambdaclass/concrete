# Formalization ROI

Status: open

This note prioritizes formalization work for Concrete by **return on investment**, not by theoretical completeness.

The goal is to answer:

- what proof work gives the most trust per unit of effort?
- what order best matches the current compiler architecture?
- what should be proved first if we want Concrete to become unusually trustworthy, not just formally ambitious?

This is intentionally a **pragmatic sequencing note**. It is not a full metatheory plan.

## Core Principle

The highest-ROI proof work is the proof work that:

1. attaches to a **stable artifact boundary**
2. validates a **language claim Concrete already makes**
3. reduces the amount of **informal trust** users must place in the compiler

The practical rule is:

- **prove user-facing guarantees before proving compiler niceties**

In Concrete, that means proving:

- effect/capability discipline
- ownership / linearity guarantees
- the honesty of the trusted / `Unsafe` split
- lowering preservation across the major semantic boundary

before proving:

- parser theory
- SSA cleanup correctness in full detail
- a giant end-to-end theorem over every compiler stage at once

For Concrete, that means:

- prove things over **Core** before proving things over the surface AST
- prove things over the **Core -> SSA** boundary before worrying about secondary backends
- prefer properties that justify the language's value proposition:
  - ownership safety
  - capability discipline
  - safe/unsafe boundary honesty
  - lowering preservation

## What Not To Do First

Do **not** start with:

- a full formal semantics of every surface-syntax form
- proof work over parser details
- optimizer correctness for every SSA cleanup rule
- a giant end-to-end theorem covering the whole compiler at once
- a proof-heavy contract/refinement system for user code

Those may be worth doing later, but they are not the best first leverage.

## Best ROI Order

### 1. Nail down the Core semantic object

Before proving much, make sure Core is the thing worth proving over.

This means:

- Core syntax stable enough to last
- CoreCheck is the semantic authority
- surface sugar elaborates away aggressively
- effect/capability information is explicit in Core
- ownership/borrows/moves are explicit enough to state invariants cleanly

Why this is first:

- proof work over the wrong object is expensive waste
- this is the boundary the rest of the proof story depends on

For user-program proofs, the practical refinement is:

- validated Core is the semantic authority
- an early **ProofCore** should be a restricted, proof-oriented view of validated Core rather than a separate rival IR

That keeps the proof story attached to the real compiler boundary instead of inventing a second semantic center.

### 2. Prove Core well-formedness invariants are preserved by CoreCheck

First high-value theorem family:

- a Core program accepted by CoreCheck satisfies the invariants the rest of the compiler assumes

At minimum:

- type consistency
- match legality / coverage
- capability requirements satisfied
- declaration-level legality (traits, repr, FFI-safe checks) satisfied

Why this has high ROI:

- CoreCheck is already the semantic authority in the implementation
- this proves the checker is not merely “best effort”

### 3. Prove capability/effect discipline

This is one of Concrete's strongest differentiators.

Important theorem:

- if a function is checked without some capability, then execution of the checked Core does not perform that capability-governed effect

Examples:

- no `File` capability -> no file effects
- no `Network` capability -> no network effects
- no `Process` capability -> no process effects
- no `Alloc` capability -> no allocation effects

Why this is high ROI:

- it directly validates the `with(...)` design
- it strengthens the audit story
- it is much more distinctive than proving ordinary typing alone

### 4. Prove ownership / linearity soundness for safe code

Important theorem family:

- no use-after-move
- no double-consume
- no silent leak of linear values in safe code
- borrow discipline is preserved by the checked Core model

Why this is high ROI:

- ownership is one of the central reasons Concrete exists
- users need to trust this much more than parser or syntax details

### 5. Prove the safe/unsafe / trusted split is honest

Concrete now has:

- semantic capabilities in signatures
- `trusted` for internal implementation unsafety
- `Unsafe` for foreign / semantically dangerous authority

Important proof targets:

- trusted code does not suppress ordinary capabilities
- `extern fn` still requires `Unsafe`
- safe APIs can be implemented with trusted internals without leaking `Unsafe`

Why this matters:

- this is one of the most distinctive parts of Concrete's safety model
- it keeps the audit story sharp

This proof family should be treated as higher ROI than parser-theoretic or optimizer-theoretic work, because it validates a central language promise directly visible to users.

### 6. Prove elaboration preserves meaning

Important theorem:

- elaborating surface code to Core preserves the meaning relevant to the checked semantics

At minimum:

- method desugaring correctness
- `?` lowering correctness
- `!` / capability sugar correctness
- pattern/match lowering correctness

Why this is next:

- Core only matters if the path into Core is trustworthy
- this bridges the user-facing language and the proof target

### 7. Prove lowering from Core to SSA preserves semantics

This is the next really high-value theorem after Core soundness.

Important targets:

- structured control flow -> SSA control flow preservation
- correct variable/state handling across branches and loops
- match lowering correctness
- memory/layout operations remain semantically aligned

Why this matters:

- the backend story is one of Concrete's strongest engineering decisions
- once Core is trusted, this is the next big informal trust boundary

### 8. Prove SSA verification assumptions match codegen assumptions

This is smaller, but still valuable.

Important theorem:

- `EmitSSA` only relies on invariants that `SSAVerify` and `SSACleanup` actually establish

Why this matters:

- prevents “verified” and “emitted” from drifting apart
- keeps the backend honest without proving all LLVM emission details immediately

## Medium-ROI Later Work

These are worth doing later, but not first:

### Layout/ABI formalization

Very valuable for FFI credibility, but probably after the basic Core and lowering theorems:

- struct size/alignment/offset correctness
- enum layout correctness
- `repr(C)` agreement

### Optimizer/cleanup correctness

Useful later for SSA cleanup passes:

- constant folding preservation
- CFG cleanup preservation
- trivial phi elimination preservation

### Parser / grammar formalization

Useful, especially because Concrete cares about LL(1), but lower ROI than the semantic core.

The better first step there is:

- external LL(1) grammar checker

not a full proof of parser correctness.

### Full end-to-end compiler theorem

Valuable eventually, but not the right first proof target.

Better to accumulate:

- Core soundness
- elaboration preservation
- lowering preservation
- backend assumption alignment

then decide whether a larger end-to-end theorem is worth the cost.

## ROI Heuristic

When choosing between two plausible proof tasks, prefer the one that:

1. validates a language guarantee users already rely on
2. sits on a boundary the compiler already treats as authoritative
3. shrinks the trusted computing base most directly
4. is likely to survive future refactors without being thrown away

This strongly biases the project toward:

- CoreCheck
- effect/capability discipline
- ownership/linearity
- trusted / `Unsafe` honesty
- Core -> SSA preservation

and away from:

- parser proofs first
- cleanup/optimizer proofs first
- large end-to-end theorems too early

## Suggested Concrete Proof Roadmap

If I compress the highest-ROI formalization order:

1. stabilize Core as proof target
2. prove CoreCheck establishes Core invariants
3. prove capability/effect discipline
4. prove ownership / linearity soundness
5. prove trusted / Unsafe / capability split is honest
6. prove elaboration preserves meaning
7. prove Core -> SSA lowering preserves meaning
8. prove SSA verification assumptions match EmitSSA assumptions
9. later: layout/ABI proofs
10. later: SSA cleanup correctness

## Why This Order Is Best

Because it matches Concrete's actual value proposition:

- explicit effects
- explicit ownership
- explicit trust boundaries
- explicit lowering boundaries

If those become proven, Concrete becomes much more than a well-designed language.

It becomes a low-level language whose main claims are backed by machine-checked reasoning, starting with the parts users most need to trust.
