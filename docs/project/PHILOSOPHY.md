# Concrete Philosophy

Status: standing policy, the one home of the project's design rules. Every other document links
here instead of restating them. The language rules themselves are the
[specification](../spec/README.md) and the owner's [decision record](../spec/decisions.md); where this
document cites one, those are authoritative, and nothing here overrides a decided entry.

It absorbs, as of 2026-11-04, the principles and design policy of [PROJECT.md](PROJECT.md), the
refusal list of `ANTI_FEATURES.md` and the "no" and "not yet" entries of PROJECT.md's language
decisions. The per-language comparisons that used to sit beside each refusal are in
[INFLUENCES.md](INFLUENCES.md#how-other-languages-answer-concretes-refusals).

Contents:

1. [What Concrete is for](#what-concrete-is-for)
2. [The rules](#the-rules)
3. [When rules conflict](#when-rules-conflict)
4. [Admission test](#admission-test)
5. [What Concrete refuses](#what-concrete-refuses)

## What Concrete is for

Concrete is a small systems language for code that must be reviewed, audited, bounded and proved:
no garbage collector and no hidden runtime, linear ownership, second-class references, and
authority that is visible in every signature. Its aim is that a reader can answer "what does this
code do, what may it touch, and what is actually proved about it" from the source, the compiler's
reports and the Lean theorems, without trusting anything that is not named. Lean 4 is the language
the semantics and proofs are written in and the language the compiler is built in; Concrete itself
stays a systems language, not a proof assistant.

## The rules

Each rule states what holds and why. Its history is in the changelog and the decision record.

1. **Pick the best long-term structural solution.** A design question is answered by the structure
   that removes the whole class of problem, not by the smallest local patch, and once that answer is
   known it is chosen rather than offered as one option among several. *Why:* a patch fixes one
   instance and leaves the class to recur; the language is still small enough to change shape.

2. **One form per situation.** There are never two ways to write one thing. A second spelling, an
   alias or a convenience form beside an existing one is removed, not added. *Why:* every extra form
   is one more thing a reader, a checker and a proof must cover, and two forms drift apart. The
   decided removals are listed in [OD61](../spec/decisions.md#od61) and the entries it names.

3. **The most transparent long-term option wins.** Between two designs, prefer the one that leaves
   nothing for the reader to reconstruct: no hidden control flow, data flow, allocation, dispatch,
   authority or cleanup. *Why:* Concrete's value is that behaviour can be read off the source.

4. **Verbosity never argues against a clearer or safer design.** Writing more is acceptable when it
   makes behaviour, authority or failure visible. "It is longer" is not a reason to reject a
   design; "it hides something" is. *Why:* most code is now written by tools, where writing explicit
   forms is cheap and reviewing them is the expensive part.

5. **Every fact once.** Every fact that affects behavior or authority is written exactly once, at a
   place the reader can find from the use site. A restated fact (a size copied into a constant, a
   rule copied into a second document, a capability implied rather than declared) is a defect
   waiting to drift. This document is itself an instance: other documents link here.

6. **Lean is the only normative source.** The language's syntax, typing, ownership, effects and
   semantics are defined in Lean; the specification chapters, the rule registry and the reference
   interpreter are generated from those definitions, never the reverse. Until a rule lands in Lean
   its chapter states the decided rule
   ([Lean-source decision](ROADMAP_DECISIONS.md#2026-11-01--lean-as-the-only-source)). *Why:* one
   definition that machines check cannot silently disagree with itself.

7. **Systems first, not theorem-prover first.** Source code reads as systems programming. Proofs
   attach to real Concrete functions through the semantics, not to a parallel reimplementation.
   *Why:* the point is evidence about the code that runs.

8. **No hidden runtime.** No garbage collector, no hidden reference counting, no hidden scheduler or
   executor, no life before `main`. What the target does at startup, at shutdown and on a trap is a
   named entry in the target's table. *Why:* memory, time and authority behaviour must be auditable
   and boundable.

9. **Ownership is linear, not affine.** A non-Copy value is used exactly once; it is never dropped
   silently at the end of a scope. Copy means "may be duplicated", nothing more. `_` names an unused
   binding and never discards a linear value, a `Result` or an `Option`
   ([OD62](../spec/decisions.md#od62), [OWN-5](../spec/ownership.md#OWN-5)). Cleanup is an explicit
   consuming operation. *Why:* every resource has one visible owner and one visible end.

10. **References are second-class.** A reference may be passed down into a call or held inside a
    borrow block; no function, trusted or not, returns one and no value stores one
    ([REF-1](../spec/references.md#REF-1), [REF-3](../spec/references.md#REF-3)). Raw pointers in
    trusted code are the only escape, and they carry stated validity premises. *Why:* scoped access
    needs no lifetime language, and the checker and the proofs stay local.

11. **Authority and trust are visible.** A function's permissions are declared in `with(...)`; a
    resource is a value it holds; an operation on a resource needs both. Nothing discharges or hides
    an operational capability, `trusted` and `with(Unsafe)` included, and there are no umbrella
    capabilities ([authority rules](../spec/decisions.md#decided-2026-11-02-authority)). *Why:* a
    reviewer reads what a function may do from its signature alone.

12. **Trust boundaries are honest.** Every claim says which of these it is: enforced by the compiler,
    reported by analysis, proved in Lean, or assumed at a named source, backend, toolchain or target
    boundary. Being listed in the trusted base proves nothing; an unknown is never reported as a
    reassuring default ([ABSENCE_IS_NOT_A_FACT.md](ABSENCE_IS_NOT_A_FACT.md)). *Why:* evidence that
    overstates itself is worse than none.

13. **The compiler owns semantic identity.** A name, a link symbol or a restated constant is a
    representation of an entity, never the entity. Every pass preserves an entity's identity or
    creates a checked derivation from it, and identity claims are tested structurally, not by
    observed values. *Why:* using a representation as identity, and re-interpreting it in a later
    pass, is the single largest class of past compiler defects. (Older documents and comments
    call this "principle 12".)

14. **Artifact-first auditability.** The compiler emits facts, evidence and replay commands that
    people, CI and tools consume directly. Auditing never means reading the compiler's source.
    *Why:* an audit that depends on the auditor's reading of the implementation is not repeatable.

15. **A small analyzable core over feature growth.** A smaller language with strong boundaries
    beats a larger one with weaker analysis. Removing a feature is an improvement when what remains
    is more coherent; each refusal makes the rest easier to check, teach and prove. *Why:* every
    feature is paid for in grammar, checker, semantics, proofs and review.

16. **Predictable execution and a separable core.** Recursion, loop bounds, allocation, blocking and
    foreign calls stay visible, so a bounded, analyzable core can be told apart from an effectful
    shell, and writing code that way is natural. *Why:* resource bounds and proofs need a core they
    can reach.

17. **Real programs test the shape.** A design is validated by serious workloads, not by thought
    experiments alone, and a refused or deferred feature reopens only on such evidence. *Why:*
    hypothetical convenience is not a cost; measured pain is.

## When rules conflict

**Proposed, pending owner approval.** When two rules pull in different directions, the earlier
item in this order wins:

1. soundness and provability;
2. transparency: nothing hidden;
3. a single source of truth: every fact once;
4. explicitness at the use site;
5. brevity.

The third item is read with rule 5's definition: every fact that affects behavior or authority is
written exactly once, at a place the reader can find from the use site. So "write it once" and
"show it at the use site" agree whenever the use site can reach the one place the fact is written;
a fact that can be found only by searching the whole program fails both.

## Admission test

A proposed language feature, compiler feature or borrowed idea is admitted only if every question
below has a good answer. Each question is a rule above applied to the proposal; failing one is
grounds for rejection.

1. **One form?** Does it add a second way to do something already expressible? (rule 2)
2. **Visible?** Does it expose ownership, effects, dispatch, layout, allocation or control flow, or
   hide work behind familiar syntax? (rules 3, 8)
3. **One fact, one place?** Is every fact it introduces written once and findable from the use site?
   (rule 5)
4. **A simple invariant?** Can it be stated as one Lean judgment without "except when" clauses, and
   does it elaborate into simpler Core forms? (rules 6, 15)
5. **Declaration-level and phase-separated?** Does it keep cross-module dependence on declarations
   and summaries, never on another module's bodies, and early phases independent of later semantic
   information? (rule 15)
6. **Static or explicitly indirect dispatch?** Is every call target known at compile time or visibly
   indirect at the call site? (rule 3)
7. **Predictable code?** Can a reader form a rough model of the generated work from the source?
   (rule 16)
8. **One owner?** Does exactly one pass or judgment own it, with one diagnostic when it fails? (rules
   5, 13)
9. **Authority intact?** Does every effect still appear in `with(...)` and every resource as a
   value, with no way to hide either? (rule 11)
10. **Through the contract boundary?** Do its safety, behaviour, effects, failure modes and resource
    consequences appear in summaries and obligations, rather than disappear at a function boundary?
    (rules 12, 14)
11. **Fails visibly?** Does an unsupported use produce a precise diagnostic or a named hole, never
    silence? (rule 12)
12. **Identity kept?** Does every entity, claim and piece of evidence it introduces have a typed
    identity of its own, never a string, a display name or one status standing for several claims?
    (rule 13)
13. **Honest under hostile input?** For an evidence or tooling feature: does it define limits,
    malformed and duplicate input behaviour, negative controls, and how a revoked assumption
    degrades only the claims that depend on it? (rule 12)
14. **A real benefit for audited code?** Is the gain more than writing speed? Convenience alone is
    not enough, and brevity alone never outweighs a visibility loss. (rules 4, 17)

**In one line.** A feature is promising when it makes the language and the compiler easier to
explain at the same time: explicit, local, phase-separated, summary-friendly and easy to lower
away, never implicit, global, inference-heavy, body-dependent or hard to model formally.

**Ordering.** Architecture before ornament; visibility in tooling before convenience syntax;
layout and ABI credibility before feature expansion; proof-friendly boundaries before richer
abstractions; replayable evidence alongside each new contract family. The roadmap applies this
order; it does not restate it.

**Borrowed ideas.** Copy constraints before copying features: Zig, Austral, SPARK and Odin are
most useful where they say no. [INFLUENCES.md](INFLUENCES.md) records what is copied, adapted and
rejected.

**Recording a refusal.** A rejected feature with its reason is worth more than a feature added
without one. A new refusal gets an entry in the next section, with its research note when there is
one. A refusal that is a language rule is an owner decision first ([decisions.md](../spec/decisions.md)).

## What Concrete refuses

Each refusal is either **Permanent**, with the reason, or **Reopens when** a named roadmap condition
holds. "Pull rules" means the [backlog pull rules](ROADMAP_BACKLOG.md#pull-rules): a workload, bug,
pilot or user need hits it. Reopening a refusal that is a decided language rule is an owner decision.

**Hidden runtime and hidden work**

- **Garbage collection and hidden reference counting.** Permanent. They hide when memory is
  reclaimed and whether a call allocates; linear ownership and explicit cleanup replace them.
- **A hidden async runtime, executor or scheduler, and function colouring.** Permanent. They hide
  concurrency and scheduling behind calls that look ordinary.
- **Explicit structured concurrency.** Reopens when [R-0399](ROADMAP_BACKLOG.md#task-r-0399)'s pull
  trigger holds: a workload needs concurrency after the new compiler's v1 surface is frozen. Any
  design keeps authority in `with(...)` and handles as linear values; single-threaded execution
  until then.
- **Exceptions and unwinding.** Permanent. They turn every call into a hidden exit. Recoverable
  failure is a `Result` handled by an exhaustive `match`, running out of memory included
  ([allocation decision](../spec/decisions.md#decided-2026-11-02-allocation)); an abort is terminal
  and runs no deferred call ([OD10](../spec/decisions.md#od10)).
- **Postfix `?` and any implicit error propagation or conversion.** Permanent. It hides a function
  exit, a cleanup boundary and a conversion behind one character; the error arm is written.
- **Mutable global state of any kind.** Permanent ([OD8](../spec/decisions.md#od8)). Constants are
  truly immutable; resources reach code only as values handed to it.

**Implicit conversions and inference**

- **Implicit numeric conversions.** Permanent. They hide data loss. `as` converts only when no
  value can be lost ([OD11](../spec/decisions.md#od11)); every other conversion is a named function
  ([OD99](../spec/decisions.md#od99)), and a checked one reports failure as a `Result`
  ([OD106](../spec/decisions.md#od106)).
- **Truthiness.** Permanent. A condition is a `bool`; no other value stands in for one.
- **Implicit string conversion** (`Display`/`ToString`-style hooks). Permanent. It hides
  allocation and dispatch. The reserved output and append forms ([OD133](../spec/decisions.md#od133))
  accept a closed list of types, expand to typed calls and keep their authority visible.
- **Global (Hindley–Milner) inference and inference from the expected result type.** Permanent.
  Signatures are written in full. A generic call's type arguments are inferred only from the checked
  types of its arguments ([OD46](../spec/decisions.md#od46), [OD129](../spec/decisions.md#od129)) and
  capability variables the same way ([OD45](../spec/decisions.md#od45)); what no argument determines
  is written explicitly, as is any capability argument in a turbofish
  ([OD87](../spec/decisions.md#od87)). Choosing among several consistent capability solutions is
  undecided: [Core-session question 5](../../ROADMAP.md#questions-for-the-core-session).

**Dispatch and abstraction**

- **Closures and anonymous functions with capture.** Permanent. Captured data and control flow are
  invisible at the call site. Named functions, capture-free function values, monomorphized generics
  and an explicit context parameter replace them ([detail](../../research/language/no-closures.md)).
- **Trait objects and hidden dynamic dispatch.** Permanent. An opaque vtable hides the call target
  from the reader, the analyses and the prover. Enums, monomorphized generics and explicit function
  values replace them ([detail](../../research/language/no-trait-objects.md)).
- **Implicit trait resolution beyond monomorphization.** Permanent. Traits are coherent, an
  ambiguous call is qualified, and resolution is by typed identity ([OD6](../spec/decisions.md#od6)),
  so adding an impl elsewhere cannot change what a call means.
- **Class inheritance.** Permanent. It makes the call target depend on a hierarchy instead of the
  source; structs, enums, traits and composition replace it.
- **User-defined operator overloading.** Permanent. An operator would hide allocation or effects
  behind arithmetic syntax; operators take primitive operands only
  ([OD26](../spec/decisions.md#od26), [OD42](../spec/decisions.md#od42)), and user types use named
  functions.
- **Hiding an operational capability**, for example a trusted wrapper that uses `Fs` or `Alloc`
  without declaring it. Permanent ([authority rule 5](../spec/decisions.md#decided-2026-11-02-authority)).
  A caller must see what authority a call uses. A `trusted fn` absorbs `Unsafe` only, which is a
  caller safety obligation, not authority ([TRUST-5](../spec/trusted.md#TRUST-5)).
- **Umbrella capabilities and capability aliases** (`Std`, user `cap X = ...`). Permanent
  ([authority rule 6](../spec/decisions.md#decided-2026-11-02-authority),
  [audit decisions](../spec/decisions.md#decided-2026-11-02-audit)). A signature lists its real
  capabilities.
- **An effect system beyond `with(...)`** (algebraic effects and handlers). Permanent. Permissions
  are compile-time sets over a fixed, reserved list of capabilities and resources are linear values;
  that needs no runtime support and keeps signatures readable.
- **Foreign function values.** Refused ([OD4](../spec/decisions.md#od4)). Reopens when the owner
  admits typed `extern "C" fn` types, which OD4 leaves for later, with the permissions of foreign
  callbacks decided at that point ([authority rule 4](../spec/decisions.md#decided-2026-11-02-authority));
  [R-0283](ROADMAP_BACKLOG.md#task-r-0283) reviews the C interop requirements when phase C becomes
  active or next.

**Generation and metaprogramming**

- **Source-generating macros, derive helpers and in-language source generation.** Permanent. They
  break file-local parsing and phase separation and make audit output describe code nobody wrote.
  Generics, ordinary functions and external generators whose output is reviewed as ordinary source
  replace them; attributes classify, never generate.
- **Derived structural equality.** Permanent as language machinery, for the same reason; equality
  is written or generated as reviewable source.
- **General compile-time evaluation** (`comptime`, `constexpr`-style). Reopens when a workload under
  the pull rules needs compile-time values beyond constant declarations; any design stays
  constants-only and states its interaction with the semantics.
- **Broad in-source conditional compilation** (`#[cfg]` throughout files). Reopens when a workload
  under the pull rules shows that selecting modules and files by target profile in `Concrete.toml` is
  not enough; any form is narrow and appears in audit output. It hides control flow, authority and
  target assumptions behind build configuration.

**Calls and declarations**

- **User-defined and C-style variadic functions.** Permanent. They are unchecked or complicate
  every signature. The reserved output and append forms are the only multi-argument forms, and they
  are not a function type and cannot be passed as values.
- **Default arguments.** Reopens when a workload under the pull rules shows the cost of writing
  every argument, and any design shows defaults in signatures and reports. A call shows every
  argument it passes.
- **Lexical shadowing of a visible local, parameter or top-level name.** Permanent
  ([SCOPE-1](../spec/scope.md#SCOPE-1), [OD67](../spec/decisions.md#od67)). One name, one binding
  in a scope; the compiler still never recovers identity from spellings (rule 13).

**Contracts and ghost code**

- **Decorative contracts.** Permanent. A contract is a claim that lowers to an obligation and is
  reported with its evidence; contracts never change what a program does at run time
  ([OD125](../spec/decisions.md#od125)), and a vacuous one is reported as such
  ([OD120](../spec/decisions.md#od120)).
- **`old`, quantifiers and implication in contracts.** Permanent ([OD117](../spec/decisions.md#od117)).
  Such properties are stated as Lean theorems, keeping the contract language small.
- **Ghost code that affects execution.** Permanent. Ghost code itself is admitted as decided:
  `ghost let` with `ghost` reserved ([OD81](../spec/decisions.md#od81)) and specification
  functions whose body is required and total ([OD115](../spec/decisions.md#od115)). How ghost state
  is represented in Core and erased is not decided: it is
  [Core-session question 9](../../ROADMAP.md#questions-for-the-core-session), and the
  owner decides it with the target Core proposal.
