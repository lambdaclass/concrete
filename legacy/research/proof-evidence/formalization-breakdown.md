# Formalization Breakdown

**Status:** Open
**Affects:** Core semantics, proof workflow, CoreCheck, lowering, SSA contract, trust model
**Date:** 2026-03-14

## Purpose

This note breaks "full formalization" into smaller proof tracks, dependency layers, and realistic milestones.

It is intentionally more concrete than:

- [formalization-roi.md](formalization-roi.md)
- [proving-concrete-functions-in-lean.md](proving-concrete-functions-in-lean.md)

Those notes explain **why** and **in what order** proof work matters.
This note explains **what the work actually breaks into**.

## Short Answer

"Full formalization" is not one task.

It is a stack of at least these major efforts:

1. formalize the semantic object to prove over
2. formalize validated Core and its invariants
3. formalize a proof-eligible program fragment
4. prove selected user-program properties
5. prove key language guarantees
6. prove elaboration preserves meaning
7. prove Core -> SSA preservation
8. prove backend-contract alignment
9. only later consider layout/ABI, cleanup correctness, and broader end-to-end arguments

If treated as one monolith, it is overwhelming.
If treated as staged proof tracks, it becomes a plausible long-horizon program.

## Scope Levels

The phrase "formalization" can mean several different things.

### Level 0: Proof-Friendly Architecture

Not a proof itself.

This is the work that makes proofs realistic:

- explicit Core artifact boundary
- explicit `ValidatedCore`
- explicit `ProofCore`
- explicit SSA contract
- explicit trust and capability boundaries

Concrete already has early versions of this.

### Level 1: Formal Semantics For A Small Core Fragment

This is the current state:

- pure Core fragment
- evaluation semantics in Lean
- selected proved examples

This is necessary, but still the easy part.

### Level 2: Language-Guarantee Proofs

These prove that Concrete's main claims are not just design intentions:

- capability honesty
- linearity/resource soundness
- trusted/unsafe boundary honesty

### Level 3: Compiler Preservation Proofs

These prove passes preserve meaning across major compiler boundaries:

- surface -> Core
- Core -> SSA
- SSA contract -> codegen assumptions

### Level 4: Operational/Boundary Proofs

These are harder and more target-sensitive:

- layout correctness
- ABI/FFI correctness
- cleanup/optimization correctness

### Level 5: Broad End-To-End Trust Story

This is the long-horizon version people often imagine when they say "full formalization":

- rich Core semantics
- strong language guarantee proofs
- preservation across major compiler boundaries
- selected boundary proofs
- traceability from source to proof artifacts

This should be treated as a destination, not an early milestone.

## Main Proof Tracks

The work breaks most cleanly into parallel-but-dependent tracks.

## Track A: Semantic Object Stabilization

Goal:
- make sure the thing being proved is the right thing

Subtasks:
1. define what validated Core contains and excludes
2. document which semantic facts are established by `CoreCheck`
3. separate proof-relevant Core semantics from compiler convenience structure
4. decide what belongs in `ProofCore` versus validated Core proper
5. make source-to-Core traceability explicit enough for proof workflows

Completion signal:
- proofs can refer to validated Core as a stable semantic object rather than a moving implementation detail

Dependencies:
- architecture stability
- `CoreCheck` as semantic authority

## Track B: Richer Core Semantics

Goal:
- grow the current tiny pure fragment into a semantics worth proving interesting programs over

Subtasks:
1. literals, variables, arithmetic, conditionals
   - already started
2. tuples/records/structs
3. enums and constructors
4. pattern matching
5. recursion and fuel/termination discipline
6. function environments and module-level definitions
7. error/failure model where relevant

Completion signal:
- a meaningful pure subset of real Concrete programs can be re-expressed in the formal Core semantics

Main difficulty:
- recursion and pattern matching increase semantic complexity quickly

## Track C: ProofCore Definition

Goal:
- define the proof-eligible subset precisely

Subtasks:
1. specify inclusion rules
   - pure functions only?
   - no `trusted`?
   - no FFI?
   - no capabilities?
2. specify excluded constructs explicitly
3. formalize extraction from `ValidatedCore`
4. prove extraction sound enough for proof use
5. make extraction diagnostics/reporting useful

Completion signal:
- there is a principled, explicit boundary for "functions we can prove today"

Main difficulty:
- avoiding a second rival semantic language

## Track D: User-Program Proof Workflow

Goal:
- prove selected Concrete functions, not just semantics lemmas

Subtasks:
1. embed selected programs manually
2. prove simple arithmetic/ordering properties
3. prove simple data-structure invariants
4. connect extracted `ProofCore` functions to Lean statements
5. define what proof obligations look like for users
6. later, add export/tooling rather than manual embedding

Completion signal:
- real Concrete functions can be connected to Lean theorems with a repeatable workflow

Main difficulty:
- keeping the workflow narrow and honest instead of trying to automate everything too early

## Track E: CoreCheck Soundness / Accepted-Core Invariants

Goal:
- prove that accepted Core satisfies the invariants downstream passes rely on

Subtasks:
1. typing consistency
2. declaration legality
3. match legality / coverage assumptions
4. capability metadata consistency
5. trait/impl legality where represented in Core
6. FFI-safe / repr / declaration checks where CoreCheck claims them

Completion signal:
- `ValidatedCore` is not only a naming convention; it has formal meaning

Main difficulty:
- aligning proof statements with actual implementation checks

## Track F: Capability / Effect Discipline

Goal:
- prove Concrete's capability model is honest

Subtasks:
1. define effectful semantic actions
2. prove capability absence prevents those actions
3. prove capability propagation is sound
4. account for `trusted` and foreign boundaries honestly
5. connect this to report claims later

Completion signal:
- statements like "no `File` capability implies no file effect" are formal theorems, not only report claims

Main difficulty:
- effect semantics and trusted boundaries complicate what "honesty" means

## Track G: Ownership / Linearity / Resource Soundness

Goal:
- prove the central ownership claims for safe code

Subtasks:
1. define resource values and consume/move semantics
2. define borrow semantics
3. prove no use-after-move
4. prove no double-consume
5. prove safe code respects borrow discipline
6. reason about destruction / cleanup obligations where feasible

Completion signal:
- Concrete's ownership story becomes mechanized rather than only enforced-by-checker

Main difficulty:
- resource semantics are much more subtle than pure expression semantics

## Track H: Trusted / Unsafe Boundary Honesty

Goal:
- prove the language's explicit trust split is meaningful

Subtasks:
1. formalize what safe code may rely on
2. formalize what `trusted` is allowed to encapsulate
3. show `Unsafe`/foreign authority cannot silently disappear from the model
4. prove trusted wrappers can preserve safe external signatures without erasing the trust boundary

Completion signal:
- the trust model can be explained formally, not only operationally

Main difficulty:
- by definition, trusted code introduces assumptions, so the theorem boundaries must be phrased carefully

## Track I: Surface -> Core Preservation

Goal:
- prove elaboration and desugaring preserve meaning

Subtasks:
1. method desugaring
2. match desugaring
3. `?` / result-propagation desugaring
4. capability sugar / call rewriting
5. type-alias and name-resolution preservation where proof-relevant

Completion signal:
- proving over Core is justified as a proof boundary for surface Concrete

Main difficulty:
- surface syntax and elaboration details change more often than Core

## Track J: Core -> SSA Preservation

Goal:
- prove lowering preserves meaning across the compiler's main backend boundary

Subtasks:
1. control-flow preservation
2. variable/state preservation
3. branch/merge preservation
4. loop preservation
5. match lowering preservation
6. aggregate-storage promotion correctness

Completion signal:
- the main semantic-to-backend boundary has a formal preservation story

Main difficulty:
- mutable-state lowering and storage identity are the hardest parts

## Track K: SSA Contract / Codegen Alignment

Goal:
- prove codegen relies only on invariants that verification/cleanup actually establish

Subtasks:
1. state SSA invariants formally
2. state cleanup postconditions formally
3. state EmitSSA assumptions formally
4. prove the assumption chain lines up

Completion signal:
- the backend contract is not only documented, but mechanically aligned

Main difficulty:
- avoiding accidental mismatch between implementation details and proof statements

## Track L: Layout / ABI / FFI Formalization

Goal:
- formalize the lowest-level representation claims that are tractable

Subtasks:
1. struct size/alignment/offset model
2. enum layout model
3. `repr(C)` claims
4. pass-by-pointer / calling convention claims
5. selected FFI envelope properties

Completion signal:
- at least some layout/ABI claims become formal rather than only documented

Main difficulty:
- target dependence and runtime/ABI unsettledness make this poor early proof work

## Track M: Cleanup / Optimization Preservation

Goal:
- prove selected SSA cleanup rules preserve semantics

Subtasks:
1. trivial phi elimination
2. dead block cleanup
3. constant folding
4. simple strength reduction

Completion signal:
- at least the nontrivial cleanup rules used routinely have a preservation argument

Main difficulty:
- optimizer proofs expand quickly if attempted too broadly

## Track N: Evidence / Traceability Integration

Goal:
- connect proofs to compiler artifacts and reports

Subtasks:
1. proof references tied to validated Core identities
2. traceability from source function to proof artifact
3. later, proof-backed report integration
4. operational evidence packaging

Completion signal:
- proof work becomes part of the compiler/evidence story, not a disconnected Lean experiment

Main difficulty:
- this depends on both proof maturity and artifact identity maturity

## Dependency Order

The likely dependency order is:

1. Track A: semantic object stabilization
2. Track B: richer Core semantics
3. Track C: ProofCore definition
4. Track D: early user-program proof workflow
5. Track E: CoreCheck invariants
6. Track F: capability discipline
7. Track G: ownership/linearity soundness
8. Track H: trusted/unsafe honesty
9. Track I: surface -> Core preservation
10. Track J: Core -> SSA preservation
11. Track K: SSA contract / codegen alignment
12. Track L: layout/ABI/FFI formalization
13. Track M: cleanup preservation
14. Track N: evidence integration

Not all of these need to be strictly serialized, but this is the most plausible trust-building order.

## Practical Milestone Plan

### Milestone 1: Mature Pure ProofCore

Includes:
- richer pure Core semantics
- structs
- enums
- match
- recursion
- more selected proved examples

Does not include:
- effects
- ownership
- lowering preservation

### Milestone 2: Honest ValidatedCore Boundary

Includes:
- formal statement of accepted-Core invariants
- ProofCore extraction rules
- better traceability from compiler artifacts to proof objects

### Milestone 3: Language Guarantee Proofs

Includes:
- capability discipline
- ownership / linearity soundness
- trusted/unsafe honesty

This is where Concrete starts to validate its strongest unique claims.

### Milestone 4: Surface And Lowering Preservation

Includes:
- elaboration preservation
- Core -> SSA preservation
- SSA contract alignment

This is where the compiler, not just the language design, starts to become formally trusted.

### Milestone 5: Selected Low-Level Boundary Proofs

Includes:
- layout/ABI claims where stable
- selected FFI envelope claims
- selected cleanup/optimization proofs

### Milestone 6: Evidence Integration

Includes:
- proof references in reports
- source-to-proof traceability
- later, evidence bundles

## What Should Not Be In The First Full-Proof Target

Do not make the first "full" target include all of:

- parser correctness
- formatter correctness
- every SSA cleanup rule
- every report mode
- all FFI/ABI details
- concurrency
- package/dependency semantics
- end-to-end LLVM correctness

That would turn the effort into a research sink before it proves the core value of the language.

## Hardest Parts

The hardest proof areas are likely:

1. ownership / linearity / destruction semantics
2. trusted/unsafe boundary honesty
3. Core -> SSA preservation for mutable-state lowering
4. layout/ABI/FFI correctness
5. keeping proof statements aligned with moving compiler implementation

The easiest high-value parts are likely:

1. richer pure Core semantics
2. ProofCore extraction discipline
3. more selected function proofs
4. accepted-Core invariants
5. SSA contract alignment

## Recommended Research Questions

Before trying to formalize everything, the project should answer:

1. what exact semantic object does `ValidatedCore` denote?
2. what exact subset does `ProofCore` contain?
3. what theorem statement best captures capability honesty?
4. what theorem statement best captures ownership soundness without over-promising?
5. what is the intended semantic model of destruction and cleanup?
6. how much source-to-Core traceability is needed for proof workflows?
7. which Core -> SSA preservation theorem is small enough to prove first?

## Bottom Line

Full formalization is a program of work, not a single milestone.

The right way to make it real is:

- treat it as tracks, not one theorem
- prove the language's distinctive claims before low-leverage formal niceties
- keep validated Core as the semantic center
- keep ProofCore as a restricted view, not a rival language
- stage compiler-preservation proofs only after the language-side boundary is strong enough

If Concrete does this well, "formalization" becomes a credible long-term trust story rather than an aspirational slogan.
