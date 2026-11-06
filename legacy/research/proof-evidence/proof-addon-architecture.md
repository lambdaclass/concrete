# Artifact-Driven Proof Addon Architecture

Status: open

This note sharpens one important architectural decision in Concrete's proof story:

- the core compiler should stay proof-friendly
- proof tooling should become first-class
- but the proof engine should not be fused into ordinary compilation

The right direction is:

- compiler produces stable proof-facing artifacts
- optional proof tooling consumes those artifacts
- CI and review workflows consume the resulting proof/evidence outputs

That is a better fit for Concrete than turning `concrete build` into an always-on theorem-prover workflow.

## Why This Matters

Concrete already has the right ingredients:

- `ValidatedCore` as a real proof boundary
- `ProofCore` as a filtered proof-eligible subset
- a growing evidence/report story
- an artifact-driven compiler direction

What is still easy to misunderstand is where stronger proof automation should live.

The wrong direction would be:

- SMT solving inside ordinary compile paths
- symbolic execution entangled with semantic passes
- proof failure as a normal compile failure for all users
- growing the core compiler into a large proof/search engine

That would raise:

- build latency
- implementation complexity
- debugging difficulty
- maintenance cost
- risk of turning proof work into a second semantic authority

## The Right Split

Concrete should separate:

1. semantic compilation
2. proof-facing artifact production
3. optional proof/evidence consumers

In practice:

- `concrete build`
  - parses, resolves, checks, elaborates, validates, lowers, emits
- compiler artifacts
  - include stable proof-facing subjects rooted in `ValidatedCore`
- `concrete prove` or equivalent addon workflow
  - reads those artifacts
  - runs SMT, symbolic execution, Lean export/orchestration, or proof caching
  - emits proof obligations, proof references, and evidence outputs

## Architectural Shape

Long-term shape:

```text
source
  -> parse / resolve / check / elab / core-check
  -> ValidatedCore
  -> ProofCore or other proof-facing export subjects
  -> normal lowering / codegen

ValidatedCore / proof-facing artifacts
  -> proof addon
     -> solver results
     -> proof obligations
     -> Lean-facing exports
     -> evidence bundle additions
```

The proof addon is downstream of the compiler's semantic authority.
It must not become a second place where language meaning is decided.

## Why Addon-Style Is Better

### Keeps the compiler small

Concrete's core compiler should remain:

- explicit
- inspectable
- mechanically understandable

An always-on proof engine pushes in the opposite direction.

### Preserves artifact discipline

Concrete already wants:

- stable artifacts
- stable IDs
- source-to-Core traceability
- reusable reports
- evidence bundles

Proof tooling should reinforce that architecture by consuming compiler artifacts, not bypassing them.

### Makes proof cost explicit

Proof search is expensive and failure-prone in ways that ordinary compilation should not be.

Users should be able to choose:

- normal build
- proof-oriented build
- CI policy gate over proof coverage or selected obligations

### Fits Concrete's trust model

Concrete already separates:

- capabilities
- `trusted`
- `Unsafe`

The proof workflow should show a similar separation:

- compiler establishes semantic facts
- proof tool attempts stronger derived claims
- policy layer decides what must hold

That is cleaner than pretending every build is a proof run.

## What The Compiler Should Provide

To support strong addon tooling, the compiler should eventually provide:

- stable proof-facing artifact IDs
- source-to-`ValidatedCore` traceability
- source-to-`ProofCore` traceability
- machine-readable proof-subject exports
- report/evidence hooks tied to the same identities
- deterministic artifact serialization where justified

This is the high-value work.
The multiplier is not "put an SMT solver everywhere."
It is "make proof-facing artifacts strong enough that external tools can be excellent."

## What The Addon Should Do

An optional proof tool could later provide:

- SMT-backed discharge of simple obligations
- symbolic execution for bounded properties
- Lean export and proof-subject references
- proof cache reuse
- coverage summaries
- CI-oriented failure thresholds
- evidence bundle augmentation

## Layered Proof Stack

Concrete should treat proof tooling as a layered stack, not a single prover choice.

The most plausible shape is:

1. compiler artifacts
   - `ValidatedCore`
   - `ProofCore`
   - stable proof-facing subject identities
   - source-to-Core traceability
2. fast automatic reasoning
   - SMT for arithmetic, bounds, reachability, and simple invariants
   - symbolic execution for path exploration, panic-freedom checks, and bounded behavioral properties
3. Lean-backed deeper proofs
   - selected semantic properties
   - inductive arguments
   - durable proof references worth carrying into evidence bundles

This is better than either extreme:

- "everything goes through Lean"
- "SMT replaces the proof story"

The right interaction is:

- SMT and symbolic execution discharge easy obligations quickly
- they emit residual obligations or derived facts when automation stops
- Lean handles the harder semantic claims and the proof artifacts worth keeping

That keeps:

- fast automation available
- the deeper proof story honest
- the semantic authority anchored in the existing Lean-facing proof architecture

## Why This Split Is Strong

SMT and symbolic execution are good at:

- cheap automation
- broad shallow coverage
- CI-friendly checks
- finding obvious proof obligations early

Lean is good at:

- precise semantics
- harder inductive arguments
- long-lived proof objects
- reviewable references that can become part of a trust/evidence story

So the long-term goal should be:

- automation where it is cheap
- Lean where rigor and durable proof artifacts matter

not:

- one proof engine forced to do every job

## Useful Ideas From Other Verification Systems

Concrete does not need to copy another language's full verification stack.
But several systems have ideas worth borrowing.

### SPARK / GNATprove

Useful idea:

- proof sessions should be replayable and shareable

That is a strong fit for Concrete's evidence/review direction.

Concrete should borrow:

- stable proof session artifacts
- replay/recheck workflows in CI
- proof results that can be shared alongside source and reports

### Why3

Useful ideas:

- multiple prover backends behind one workflow
- explicit verification-condition splitting
- persistent proof sessions

Why3 is a good model for the layered workflow itself:

- generate obligations
- try automated provers
- preserve sessions and results
- escalate harder goals to stronger interactive workflows

Concrete should borrow:

- explicit proof-obligation artifacts
- solver/result/session persistence
- good subgoal splitting instead of treating every failure as one monolith

### Kani

Useful ideas:

- harness-oriented verification entry points
- a separate verification driver
- modeling or overriding operations that are irrelevant or hostile to verification

Kani reinforces:

- keep verification as a tool-oriented workflow
- use dedicated proof/verification entry points
- do not force all runtime behavior into the proof engine unchanged

### Creusot

Useful ideas:

- a pure logical/specification fragment
- contracts as a function's logical interface
- explicit variants for termination when automation is not enough

Concrete already has a promising shape here with `ProofCore`.
Creusot is a reminder that the proof-facing subset should stay:

- explicit
- pure
- separate from ordinary effectful program execution

### Prusti

Useful ideas:

- default checking of a small set of basic safety properties
- counterexample reporting on failure
- external specifications or models for code that is hard to verify directly

Concrete should be careful not to grow a large annotation system too early, but two ideas are strong:

- proof failures should produce useful counterexample-style output where possible
- proof tooling should be able to attach external specs/models to selected low-level or foreign boundaries

### F*

Useful idea:

- automation and interactive proof should reinforce each other rather than compete

Concrete should not become a proof language in F*'s mold.
But F* is still a good reminder that:

- SMT can handle a lot of fast proof discharge
- harder semantic proofs still need a stronger theorem-proving layer

## Concrete-Specific Takeaways

The strongest external ideas to adopt are:

- proof sessions that can be replayed and shared
- explicit proof-obligation artifacts and subgoal splitting
- harness-oriented verification entry points
- counterexample output for failed automatic checks
- a small explicit proof-facing fragment instead of "verify all source code directly"
- layered automation first, deeper Lean proofs second

The strongest ideas to avoid are:

- turning the main compiler into a universal verifier
- forcing every proof through one backend
- growing a large new specification language before the current artifact/traceability story is mature

## Narrow SMT Export Direction

One plausible extension of the addon workflow is a narrow SMT-export mode.

The important word is narrow.

Concrete should not aim for:

- whole-language SMT export
- a second semantic authority beside the compiler and Lean formalization
- an SMT-driven redesign of the language surface

The useful direction is:

- export selected proof subjects or proof obligations
- export only proof-facing fragments grounded in compiler artifacts
- use SMT export as one backend for fast automatic checks

That could support checks such as:

- panic-freedom for selected functions
- simple bounds or reachability obligations
- arithmetic side conditions

This fits the addon architecture because:

- the compiler remains the producer of semantic artifacts
- SMT tooling remains an optional consumer
- Lean remains the place for the deeper semantic proofs and durable proof references

## Philosophy Check

The proof-addon direction remains compatible with Concrete only if it stays:

- artifact-driven
- explicit
- optional
- subordinate to the existing semantic and proof boundaries

It becomes a bad fit if it turns into:

- hidden proof work in ordinary builds
- language-surface pressure for solver convenience
- a growing second semantics around annotations or proof-only syntax

Example surface:

```text
concrete prove
concrete prove --auto
concrete prove --lean
concrete prove --bundle out/
concrete prove --min-coverage 90
concrete prove --emit-obligations
```

These are workflow ideas, not yet a commitment to exact CLI shape.

## What Not To Do

Concrete should avoid:

- making proof search part of every normal build
- coupling pass correctness to solver availability
- treating proof automation as a second checker for ordinary language semantics
- promising "automatic proofs everywhere" before the artifact and traceability story is mature
- hiding proof cost or proof failure behind ordinary compile UX

## Roadmap Implication

This direction mainly affects:

- **Phase I**:
  - proof-facing artifacts
  - source-to-Core traceability
  - user-program proof workflow
- **Phase J**:
  - artifact identity and graph discipline strong enough for proof consumers
- **Phase L**:
  - operational proof/evidence workflows
  - machine-readable outputs
  - evidence bundles and review tooling

The main strategic point is:

- keep the compiler proof-friendly
- keep proof tooling first-class
- keep heavy proof automation as an artifact consumer, not as the semantic center of the compiler

## Bottom Line

Concrete does not need "compiler as theorem prover" in the ordinary build path.

It needs:

- a strong semantic core
- stable proof-facing artifacts
- traceability
- optional proof tooling that can become powerful without distorting the compiler itself

That is the more disciplined architecture, and it fits the rest of Concrete's roadmap far better than an always-on integrated proof engine.
