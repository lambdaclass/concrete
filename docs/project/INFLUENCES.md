# Concrete Influences

Status: stable reference

This document tracks the external languages and systems that Concrete learns
from.

It is not a claim that Concrete should become a blend of all of them. The point
is the opposite: copy the constraints and workflow lessons that strengthen a
small, auditable, no-GC systems language, and reject the machinery that would
make Concrete less clear.

The synthesis Concrete is aiming for is roughly:

- Zig/Austral-style small explicit systems programming
- Hylo/Val-style mutable value semantics: mutation through scoped access,
  without returned/stored safe references
- SPARK/Dafny/Why3-style assurance workflow and proof discipline
- Lean 4 as implementation language and proof environment
- explicit capabilities, predictable-execution boundaries, and artifact-first auditability as the distinctive Concrete layer

That synthesis only works if the result stays minimalist, linear/resource-aware,
no-GC, and honest about what is enforced, reported, proved, or trusted.

## Positioning Summary

The surrounding tools each cover part of the space:

- Rust, Zig, C, and C++ are strong systems languages, but proofs usually live
  outside the compiler workflow.
- SPARK/Ada, Dafny, F*, and Why3 have strong verification workflows, but are not
  trying to be a small C/Rust/Zig-style systems language with Lean as the
  compiler/proof substrate.
- Lean, Coq, and Isabelle are excellent proof systems, but ordinary low-level
  systems programming is not their primary path.
- Austral is close on linear safety, but not on Lean-backed proof artifacts and
  drift-gated evidence.

Concrete's niche is the intersection: no-GC systems code, explicit authority,
linear/resource discipline, selected Lean theorems, and machine-readable
evidence tying the theorem back to the current source.

## How To Read This

Each influence is classified as:

- **Copied** — already clearly present in Concrete
- **Adapted** — an idea Concrete wants, but in a smaller or different form
- **Rejected** — an idea Concrete intentionally does not want

## Core Influences

### Lean 4

**Status:** Copied + adapted

Concrete takes:

- implementation in Lean 4
- theorem-proving environment for compiler and user-program proofs
- explicitness over hidden behavior
- the expectation that important claims should eventually be machine-checked

Concrete does not take:

- GC as the execution model
- proof assistant ergonomics as the default programming style
- dependent types as the default surface of the systems language

The key distinction is that Concrete is Lean-implemented and Lean-provable in
parts, but it is not trying to become "Lean without GC."

### Rust

**Status:** Adapted

Concrete takes:

- serious compiler-enforced safety culture
- explicit trust/unsafe boundary thinking
- the idea that guarantees should be compiler facts, not conventions
- engineering rigor around diagnostics, testing, and review discipline

Concrete does not take:

- borrow-checker and lifetime surface complexity
- trait system scale
- macro-heavy language growth

### Zig

**Status:** Adapted

Concrete takes:

- small-language pressure
- explicit low-level control
- no hidden GC
- straightforward systems programming feel
- preference for visible boundaries over language magic
- the recent I/O design lesson that runtime choice should belong to the
  application rather than splitting libraries into sync and async ecosystems

Concrete is researching a related async/concurrency direction, but adapting it
into capabilities, structured scopes, linear handles, and evidence reports
rather than a pervasive `io` parameter or generic async/await color. See
[../research/stdlib/async-concurrency-evidence.md](../../research/stdlib/async-concurrency-evidence.md).

Concrete does not take:

- "manual control first, proof/evidence later" as the whole philosophy
- async/evented I/O as an implemented feature today

### Hylo / Val

**Status:** Adapted

Concrete takes:

- mutable value semantics as a design reference: mutation is allowed, but safe
  references are scoped access paths, not ordinary returned or stored values
- the "references flow down, not up" discipline for local reasoning
- accessor APIs shaped as scoped callbacks, projections, owned views, or value
  returns instead of lifetime-bearing `&T` returns
- the idea that avoiding first-class safe references can avoid a Rust-style
  lifetime surface while still permitting efficient mutation

Concrete differs by combining this with:

- linear ownership for non-`Copy` resources
- explicit capability headers for authority and effects
- Lean-backed evidence artifacts and pipeline gates
- trusted/raw-pointer escape hatches for low-level boundaries, kept outside the
  safe reference model

See [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md) for Concrete's rule and
[../research/compiler/pipeline-lessons-2026-07.md](../../research/compiler/pipeline-lessons-2026-07.md)
for the research notes.

Because Concrete never returns a reference, a safe function can be modelled as a pure function that
takes its `&mut` arguments in and hands them back; this is what makes simple functional models
possible (see Aeneas below).

**References:** [Hylo](https://www.hylo-lang.org);
[Racordon et al., Native Implementation of Mutable Value Semantics](https://arxiv.org/abs/2106.12678);
[Hylo specification](https://github.com/hylo-lang/specification).

### Austral

**Status:** Copied + adapted

Concrete takes:

- visible capabilities in interfaces
- linear ownership without a giant abstraction tower
- auditability-first design
- language-size discipline

Concrete differs by pushing harder on:

- proof/evidence artifacts
- predictable execution reporting and enforcement
- Lean-backed proof direction

Austral is the default reference for language decisions: a Concrete rule is as strict as Austral's or
stricter, and every deviation is written down. One deliberate deviation is authority: Austral passes
capabilities as linear values minted from a `RootCapability`, while Concrete separates permission from
resources and has no root value ([the authority rules](../spec/decisions.md#decided-2026-11-02-authority)).

**References:** [Austral](https://austral-lang.org);
[Austral specification](https://austral-lang.org/spec/spec.html);
[LambdaClass interview with Fernando Borretti](https://lambdaclass.github.io/lambdaclass_blog/tags/programming-languages).

### Wirth Languages / Oberon

**Status:** Adapted

Concrete takes:

- small-language discipline: a compact grammar, few concepts, and little syntax
  that exists only for convenience
- modules as the main scaling mechanism, not macros or pervasive language
  extension
- the habit of growing capability through libraries and tooling before adding
  surface syntax
- implementation simplicity as an assurance feature, not just an aesthetic

Concrete does not take:

- GC as the default systems-programming model
- unsafe escapes hidden behind implementation convention
- a minimalism that avoids explicit authority, ownership, or proof artifacts

The lesson for Concrete is especially relevant to Phase 6D and the stdlib:
remove special cases that do not earn their keep, but do not remove ergonomic
forms that keep ordinary programs clear.

### Hare

**Status:** Adapted

Concrete takes:

- small, predictable systems programming as a first-order goal
- simple modules and explicit OS-facing APIs
- a bias toward boring syntax and straightforward control flow

Concrete does not take:

- C-like memory unsafety as the ordinary programming model
- a language surface that leaves authority and trust boundaries implicit

Hare is useful as a reminder that a systems language can stay direct without
turning its standard library into a framework.

### Pony

**Status:** Adapted

Concrete takes:

- capability thinking as a way to make authority explicit
- the idea that aliasing/authority restrictions can be part of a practical
  programming model, not only a theorem

Concrete does not take:

- actor concurrency as the core execution model
- a large reference-capability surface

Pony is a reference for authority discipline, not for Concrete's concurrency or
object model.

### SML / OCaml

**Status:** Adapted

Concrete takes:

- module signatures as a discipline for stable, inspectable APIs
- a clear separation between implementation and public interface
- small, principled libraries with predictable naming
- the value of a real language specification rather than only tests and prose

Concrete does not take:

- GC as the runtime assumption
- pervasive higher-order functional style as the systems-programming default
- module/functor power that would make the surface harder to audit

The concrete stdlib lesson is that every module should have a signature-like
contract: exported surface, ownership behavior, capabilities, allocation,
failure modes, and examples.

### Gleam / Roc

**Status:** Adapted cautiously

Concrete takes:

- friendly error messages and approachable documentation style
- simple data-first APIs
- effect visibility as a user-facing design concern
- example-driven testing as part of the library contract

Concrete does not take:

- managed runtimes as the base assumption
- fast-moving language features as a substitute for a small stable core

These are references for stdlib documentation and workflow polish, not for
Concrete's runtime model.

### Lua / Wren / Janet

**Status:** Adapted cautiously

Concrete takes:

- compact language references
- embeddability and host-boundary clarity
- small standard libraries that are easy to learn as a whole

Concrete does not take:

- dynamic typing
- metatables, macros, or runtime reflection as the main extension mechanism
- implicit authority through host APIs

The useful lesson is documentation and host-boundary clarity: if a feature
touches the host, the boundary should be visible and easy to audit.

### Koka / Flix

**Status:** Internal influence, not surface-language target

Concrete takes:

- effect/capability facts as compiler-owned decisions
- callback capability propagation as a real semantic axis
- query/fact-store ideas for future relational facts, provenance, and
  invalidation

Concrete does not take:

- user-facing row-polymorphic effects for v1
- algebraic effect handlers as a language feature
- Datalog as a user-facing programming model

The lesson is internal: `CapabilityJudgment`-style decision records and future
CompilerDB facts should be precise, but the surface language should remain
ordinary and explicit.

### Cyclone / ATS

**Status:** Adapted cautiously

Concrete takes:

- region/escape-discipline lessons for safe low-level references
- proof/resource-safety ambition for systems code
- the warning that powerful static systems can become too hard for daily
  programming if exposed directly

Concrete does not take:

- a full region/lifetime surface
- dependent/resource proof terms in ordinary code

These languages support Concrete's "no returned safe references" and
proof-boundary direction: keep the invariant strong, but keep the daily surface
small.

### SPARK / Ada

**Status:** Adapted

Concrete already takes:

- restricted analyzable profiles
- assurance workflow as a first-class concern
- explicit trust and review discipline
- the idea that operational restrictions should be user-visible architectural tools

Concrete still wants to add carefully:

- contracts
- loop invariants
- ghost code

Concrete does not want:

- a large language/kernel burden just to support proof-oriented features

### Dafny

**Status:** Adapted

Concrete wants to copy:

- proof usability
- actionable proof diagnostics
- proof maintenance ergonomics
- a normal-feeling function/spec workflow

Concrete does not want:

- a source language that feels like a proof script by default

### Why3 / WhyML

**Status:** Adapted (workflow), reference architecture (multi-prover dispatch)

Why3 is the closest existing system to Concrete's proof-backend ambition: it is a
production platform (the engine under Frama-C and SPARK) that generates proof
obligations and dispatches each to *many* provers — SMT solvers *and* ITPs (Coq,
Isabelle, PVS) — through a shared neutral layer. It is worth studying carefully.

Concrete wants to copy (workflow):

- explicit proof-obligation artifacts
- inspectable separation between code, specs, obligations, and proof results
- proof workflow that feels like a real tool pipeline, not hidden magic
- machine-consumable proof/audit artifacts rather than only human prose

Concrete wants to copy (architecture — see `research/proof-evidence/why3-architecture-and-positioning.md`):

- **Task = obligation.** Why3's "task" (a self-contained context + one goal) is the
  right shape for Concrete's prover-neutral obligation unit.
- **Composable transformation pipeline.** Lowering is a list of small, named,
  logic-to-logic transforms (`split_goal`, inline, compute, eliminate-algebraic,
  encode-polymorphism), *per-backend selected* — so one obligation serves both a
  first-order SMT solver and an ITP. Concrete should generalize its current fixed
  `toLeanProp`/`exprToSmt` lowering into this shape.
- **Drivers as declarative data.** A per-prover config (symbol printing, selected
  transforms, built-in theories, command line, result-parsing regexp) means adding a
  prover is a *data file*, not a new module. Concrete's `obBinOp` operator table is a
  proto-driver; push it all the way.
- **Session shape+checksum staleness.** A structural goal *shape* stored separately from
  an exact checksum lets an edit re-attach a proof to a moved-but-unchanged goal instead
  of marking everything stale — a refinement of Concrete's `ProofSubjectDigest` /
  `stale`/`unbound`/`depsNotCurrent` machinery.
- **Realization** as a semantics bridge: prove inside each ITP that the shared
  obligation-language axioms hold in that system's model (stronger than conformance
  testing alone).

Concrete does not want (and this is why it copies the ideas rather than building *on*
Why3):

- **WhyML as an execution substrate.** WhyML is an ML that extracts to GC'd OCaml — the
  wrong target for a no-GC systems language. Concrete owns its LLVM/QBE codegen instead.
- **Why3's trust posture.** Why3's transformations, drivers, and printers are a large,
  largely-*unverified* trusted surface (a printer bug can make a false goal look proved).
  That is the opposite of Concrete's shrinking-TCB thesis. Concrete copies the
  architecture but diverges on trust: prefer **certificate replay** (LRAT/Alethe checked
  in a kernel — the `solver_checked` evidence class) over trusting a driver, and keep the
  transform set small and, where possible, itself proven sound.

The distinction that matters: multi-prover dispatch is *not* a reason Concrete exists —
Why3 already does it better today. Concrete's reason to exist is the two axes Why3 does
not occupy (no-GC systems substrate; a graded, audit-visible evidence ledger), and it can
reuse Why3's dispatch *design* — or even Why3 itself as a temporary scaffold/oracle while
building its own — to get there without adopting Why3's TCB.

### F*

**Status:** Adapted

Concrete wants to copy:

- clear effect/proof boundary thinking
- explicit extraction/trust-boundary methodology
- honesty about what is proved at source/proof-IR level versus what is trusted
  in runtime, backend, FFI, or target

Concrete does not want:

- to become a full effect-typed proof language first and a systems language
  second

### Frama-C

**Status:** Adapted lightly

Concrete can learn from Frama-C:

- practical contracts/invariants over low-level code
- analyzable restricted subsets rather than "verify everything at once"
- tool-assisted assurance workflow around ordinary systems programming

Concrete should not copy:

- a plugin-heavy or analysis-fragmented workflow as its primary UX

### ATS

**Status:** Adapted

Concrete takes:

- serious resource-aware low-level programming
- linearity as a practical systems tool

Concrete does not want:

- proof-term density in ordinary source code
- a language surface that is too dense for audit-first use

### Liquid Haskell

**Status:** Adapted lightly

Concrete can learn from Liquid Haskell:

- lightweight contract/refinement ideas for API-level properties
- proving useful bounds and shape properties without turning the entire
  language into a dependent-type system

Concrete should not copy:

- SMT/refinement machinery as the default explanation surface for ordinary
  systems code

### Vale

**Status:** Adapted narrowly

Concrete can learn from Vale:

- proof-aware low-level boundary discipline
- strong separation between trusted low-level envelopes and proved higher-level
  properties
- serious handling of cryptographic and security-sensitive code

Vale matters more as a lesson in boundary discipline than as a surface-language
model for Concrete.

### CompCert

**Status:** Adapted as compiler-trust influence

Concrete can learn from CompCert:

- explicit compiler-correctness mindset
- honesty about backend trust boundaries
- the value of proving selected compiler-preservation properties where they
  protect real evidence claims

CompCert is not a surface-language influence. It is a reminder that if
Concrete's thesis depends on compiler-reported evidence, the compiler and
backend contracts themselves eventually matter.

**References:** [CompCert](https://compcert.org);
[Leroy, A formally verified compiler back-end](https://arxiv.org/abs/0902.2137).

## Verification and Compiler Influences (research of 2026-11-01)

These shape the new compiler written from the Lean definition (ROADMAP, owner decision of
2026-11-01) and the order of its proof. Each is a source of technique, not of surface syntax.

### Aeneas

**Status:** Adapted

Aeneas translates safe Rust into pure functions in a proof assistant, using Rust's borrow discipline
to avoid memory reasoning. It models how borrows end across calls with "backward functions", which
carry updated values back to the lender. Concrete takes the functional-translation idea; its
second-class references (never returned or stored) simplify it: a `&mut` argument goes in and comes
back out, and the supported subset may need no backward functions. That each model is exact is still a
theorem to prove. These functional models are the planned basis for proofs about Concrete programs,
replacing ProofCore.

**References:** [Ho and Protzenko, Aeneas: Rust Verification by Functional Translation (ICFP 2022)](https://arxiv.org/abs/2206.07185).

### Effekt

**Status:** Adapted

Effekt reads effect types as the capabilities a computation needs from its context, keeps functions
second-class by default (they can be boxed into first-class values) so capabilities do not escape
unnoticed, and links capability scopes to regions for compilation. Concrete takes the
effects-as-capabilities idea with one difference: Effekt's capabilities are values passed to
computations, while Concrete's `with(...)` is compile-time permission and the resources it governs are
separate linear values.

**References:** [Brachthäuser et al., Effects as Capabilities (OOPSLA 2020)](https://se.cs.uni-tuebingen.de/publications/brachthaeuser20effects/);
[Lexical effect handlers](https://se.cs.uni-tuebingen.de/research/handlers/);
[Müller et al., From Capabilities to Regions (OOPSLA 2023)](https://se.informatik.uni-tuebingen.de/publications/mueller23capabilities-preprint.pdf).

### Scala capture checking (System Capless)

**Status:** Reference for the metatheory

Scala 3's capture checking tracks which capabilities a value may hold, including capabilities held
inside generic containers ("reach capabilities"); its calculus, System Capless, has type soundness
and scope safety mechanized in Lean. Concrete uses it as the metatheory reference for its capability
variables ([authority rule 3](../spec/decisions.md#decided-2026-11-02-authority)) and, once foreign
function values are admitted, for callbacks that carry capabilities.

**References:** [Xu, Bračevac, Pham and Odersky, What's in the Box (OOPSLA 2025)](https://www.arxiv.org/abs/2509.07609).

### Linear Core

**Status:** Adapted as a compiler lesson

Linear Core shows that optimising transformations can break syntactic linearity while preserving
semantic linearity, and gives an IR type system that accepts them. Concrete takes the lesson that
every pass must re-establish the next IR's ownership and authority invariants, not only preserve
behaviour.

**References:** [Mesquita and Toninho, Lazy Linearity for a Core Functional Language (POPL 2026)](https://arxiv.org/abs/2511.10361).

### Owlang

**Status:** Adapted as a proof structure

Owlang is a Rust-inspired ownership language compiled through CompCertO, in Rocq. Its ownership
checker is proved to rule out temporal memory errors (partial safety), and the verified compiler
carries that safety to assembly. Concrete takes the split: the checker proves the ownership
invariant and compilation preserves it, the ownership conditions at module boundaries are stated as
interface contracts, and every check runs before optimization.

**References:** [End-to-end Compositional Verification of Program Safety through Verified and Verifying Compilation](https://arxiv.org/abs/2510.10015).

### CompCertO

**Status:** Adapted

CompCertO extends CompCert's theorem to open components that interact through explicit calling
conventions, so separately compiled modules and C code compose. Concrete takes it as the model for
per-module correctness that composes into a whole-program theorem, with explicit contracts at the C
boundary.

**References:** [Koenig and Shao, CompCertO: Compiling Certified Open C Components (PLDI 2021)](https://flint.cs.yale.edu/flint/publications/compcerto.pdf).

### CakeML and Pancake

**Status:** Adapted as a compiler-trust influence

CakeML is an end-to-end verified compiler through several intermediate languages to machine code;
Pancake is a verification-friendly systems language built on CakeML's verified backend, whose
verified device driver performs close to C. Concrete takes the proof-first compiler structure and the
evidence, on its device-driver workload, that a verification-friendly systems language can perform
close to C.

**References:** [CakeML](https://cakeml.org); [Pancake](https://cakeml.org/pancake);
[Verifying Device Drivers with Pancake](https://arxiv.org/abs/2501.08249).

### Verus

**Status:** Adapted, later

Verus verifies Rust with specifications and proofs written in Rust, and uses linear ghost types
(ghost permissions) that the borrow checker tracks. Concrete plans ghost permissions to discharge the
trust assumptions of selected `trusted` functions instead of carrying them.

**References:** [Lattuada et al., Verus: Verifying Rust Programs using Linear Ghost Types (OOPSLA 2023)](https://arxiv.org/abs/2303.05491).

### WebAssembly SpecTec

**Status:** Adapted

SpecTec writes the WebAssembly semantics once and generates the specification document and an
interpreter from it; the Wasm community adopted it in 2025. Concrete takes the single-source idea and
writes the source directly in Lean, which already gives the proof assistant, instead of a separate
specification language.

**References:** [SpecTec has been adopted](https://webassembly.org/news/2025-03-27-spectec/);
[Engineering a Formal Language Specification (ECOOP 2025)](https://2025.ecoop.org/details/plss-2025-papers/1/Engineering-a-Formal-Language-Specification).

### Vermillion (verified LL(1) parsing)

**Status:** Planned

Vermillion is an LL(1) parser generator in Coq whose table generator and parser are proved sound,
complete and terminating. Concrete's grammar is to be Lean data that must pass the LL(1) check, so
the same approach can generate its parser; the lexer, the semantic actions and running the generated
parser are further obligations before the parser leaves the trusted base.

**References:** [Lasser et al., A Verified LL(1) Parser Generator (ITP 2019)](https://drops.dagstuhl.de/storage/00lipics/lipics-vol141-itp2019/LIPIcs.ITP.2019.24/LIPIcs.ITP.2019.24.pdf);
[Isabelle AFP LL1_Parser](https://isa-afp.org/entries/LL1_Parser.html).

### Lean program verification (`Std.Do`, `mvcgen`, `grind`)

**Status:** Planned

Lean 4.22 added `grind`, an SMT-style tactic, and `Std.Do`, a Hoare logic for monadic programs with
the `mvcgen` verification-condition generator. Concrete plans to state contracts over its functional
models and discharge them with these, instead of maintaining its own verification-condition generator.

**References:** [Lean 4.22 release notes](https://lean-lang.org/doc/reference/stable/releases/v4.22.0/).

### Equality saturation with proved rules (egg, AMO-Lean)

**Status:** Later

An e-graph searches for rewrites whose rules are proved theorems. For the search and the extraction
to stay outside the trusted base, each result also needs a kernel-checked proof that the output equals
the input: the Lean egg tactic reconstructs such proofs, and AMO-Lean reports e-graph compositional
correctness as work in progress. Concrete takes this as the shape of a later optimizer.

**References:** [egg tactic in Lean](https://www.arxiv.org/abs/2405.10188);
[AMO-Lean (LambdaClass)](https://blog.lambdaclass.com/amo-lean-towards-formally-verified-optimization-via-equality-saturation-in-lean-4/).

### Axon and LLM-assisted verification

**Status:** Workflow reference

Axon reports a verified compiler built by a coding agent with Lean 4, combining testing, translation
validation and verification; Trivet uses LLMs with Lean for LLVM translation validation. Concrete takes
the working mode: agents write proofs and the Lean kernel checks them. Trivet validates LLVM's own
optimization passes; a similar agent-written validation could cover a named edge of Concrete's
unverified LLVM path, such as the BIR → LLVM printer.

**References:** [Axon](https://arxiv.org/abs/2605.01660);
[LLVM Translation Validation Automated with Large Language Models and Lean](https://arxiv.org/abs/2609.19583).

## Secondary Influences

### Cyclone

**Status:** Adapted lightly

Concrete takes:

- checked low-level subsets
- the idea that dangerous operations should be visible and boring

### Idris / Agda

**Status:** Rejected as main surface, adapted as a lesson

Concrete keeps only:

- the lesson that proof/spec attachment should become more direct when users
  choose it

Concrete rejects:

- making a full dependent-type surface the normal way to write systems code

## What Concrete Is Not Copying

Concrete should continue to reject:

- trait/typeclass scale as a default language-expansion path
- macros as the main extensibility story
- hidden dynamic dispatch
- proof-heavy source code as the ordinary programming style
- adding verification features before the artifact/workflow path is usable
- hidden runtime models that undermine systems-level auditability
- turning the language into a theorem prover instead of a systems language

## Current Priority From These Influences

The highest-value still-missing ideas are:

1. a small explicit spec surface
2. loop invariants for real bounded examples
3. proof UX and proof diagnostics
4. stronger assurance workflow polish
5. ghost code only if a real proof-backed example forces it

Those fit Concrete's philosophy best because they strengthen:

- explicit authority
- explicit operational behavior
- explicit evidence
- explicit trust boundaries
- an artifact-first review and CI workflow
