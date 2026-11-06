# Proving Concrete Functions In Lean

Status: Open

This note sketches how Concrete programs could eventually be proved in Lean, and what architecture choices make that realistic or unrealistic.

Concrete was created to close a gap between low-level programming and mechanized reasoning. Systems languages usually give low-level power. Proof systems usually give reasoning power. Concrete is trying to make those two meet cleanly by keeping authority, resource, trust, and compiler-meaning boundaries explicit enough that Lean can reason about them.

## Short Answer

Yes, Concrete functions should be provable in Lean, but the realistic path is not "prove arbitrary surface syntax directly".

The realistic path is:

1. formalize a small Core language in Lean
2. give that Core language an executable or relational semantics
3. elaborate Concrete source into Core
4. connect selected Concrete functions to their Core meaning
5. prove properties about that Core meaning in Lean

That makes function proofs a downstream consequence of the compiler architecture instead of a separate ad-hoc feature.

## Why This Fits Concrete

Concrete is already being shaped around:

- explicit Core semantics
- explicit pass boundaries
- SSA as a backend boundary
- a small semantic surface
- explicit trust/capability/resource boundaries

Those choices are exactly what make a Lean proof story plausible.

There are really two proof layers in play:

- language/compiler proofs ask whether the language rules and compiler pipeline are trustworthy
- program proofs ask whether a specific Concrete function satisfies its specification

Both matter. The first gives language and compiler trust. The second gives program trust.

## Why This Is Useful

This matters because it lets Concrete justify a stronger claim than "the compiler seems well-designed".

It would mean:

- not only can you reason about the language in Lean
- you can reason about actual Concrete programs in Lean
- you can use Lean's theorem library and proof environment while still implementing the executable code in Concrete

That last point is one of the most practical reasons this idea is interesting.

If the bridge is done well, the workflow is not "replace Lean with Concrete" or "replace Concrete with Lean". It is:

- write low-level executable code in Concrete
- connect that code to formal Core semantics
- prove properties about it in Lean using existing Lean definitions, lemmas, and theorems

That matters because Concrete is meant to be a real low-level implementation language:

- no GC assumptions
- explicit ownership and resources
- explicit layout and FFI boundaries
- explicit authority and trust boundaries

Lean is excellent for proofs, but it is not the language most people would choose for ordinary systems programming. Concrete can potentially make a different tradeoff: write the executable systems code in a low-level language, but reason about it in Lean.

That is useful for several reasons.

### 1. It Validates The Language Design

If small real functions can be proved against formal Core semantics, that is strong evidence the language is actually as explicit and proof-friendly as intended.

### 2. It Tests Whether The Compiler Architecture Choice Was Correct

If Core really works as a proof boundary, then the effort around:

- explicit Core semantics
- semantic cleanup
- small language surface
- explicit trust/effect boundaries

was not only aesthetically good; it was functionally correct for the project's proof goals.

### 3. It Is A Real Differentiator

Many languages can talk about safety or correctness.

Far fewer can realistically say:

- here is a source function
- here is its formal Core meaning
- here is a Lean proof of a property about it

That is a strong identity marker for Concrete.

### 4. It Helps Define What Should Remain Simple

Once the project cares about proving actual functions, it becomes easier to reject language features that would damage that property or make the proof boundary less explicit.

### 5. It Creates A Path From Language Trust To Program Trust

Compiler proofs and program proofs are different:

- compiler proofs say "the compiler preserves semantics"
- program proofs say "this program satisfies its spec"

Concrete is unusually well-positioned to care about both.

### 6. It Is Especially Valuable For Security / Audit-Sensitive Code

If Concrete is used for low-level code where authority, resources, and trust boundaries matter, then proving selected critical functions is much more valuable than generic language marketing.

### 7. It Could Make Verified Low-Level Code More Practical

One of the strongest long-term possibilities is this:

- implementation language = Concrete
- proof language = Lean
- bridge = formalized Core semantics

That would let users write real low-level executable code in Concrete and still prove properties about it using Lean's theorem ecosystem.

This is especially appealing for code that would be awkward to write directly in Lean for practical systems reasons, but that still benefits from mechanized proofs.

## Why This Could Be Distinctive

Program verification is not new, and some other systems-oriented languages or verification ecosystems already support related work.

What would still be relatively unusual here is the combination of:

- a low-level language
- explicit trust/capability/resource boundaries
- compiler architecture intentionally aligned with a formal Core semantics
- and a realistic path to proving selected user programs in Lean

It would also create a more practical split than "just write everything in a proof assistant":

- Lean remains the theorem and proof environment
- Concrete remains the executable low-level language
- the bridge between them is intentional instead of accidental

That is not the same thing as "verification exists somewhere". It would make this proof story part of Concrete's architectural identity rather than an external afterthought.

## Levels Of Proof

There are several different goals that can all be called "proving Concrete functions":

### 1. Compiler / Language Soundness

Examples:

- progress and preservation
- ownership / linearity soundness
- capability / trust honesty
- Core -> SSA preservation

This proves that the language and compiler behave coherently.

### 2. Program Property Proofs Over Core

Examples:

- a pure function returns sorted output
- a parser/formatter pair round-trips
- a transformation preserves a structural invariant

This is the most realistic early form of proving Concrete programs.

### 3. Program Refinement Against Specifications

Examples:

- a Concrete function refines a mathematical spec
- an effectful function respects a capability-aware contract
- a data-structure operation preserves representation invariants

This is possible, but depends on stronger semantic/spec infrastructure.

## Likely Technical Approach

### Deep Embedding First

The most realistic initial model is a deep embedding:

- represent Core syntax as Lean data
- define evaluation / typing / effect rules in Lean
- prove properties over that representation

This is the right place to start because it aligns with compiler formalization work already on the roadmap.

### Surface Syntax Is Not The First Target

Trying to prove arbitrary surface Concrete code directly is the wrong first move.

Surface syntax includes:

- parser details
- elaboration details
- naming and sugar
- diagnostics-facing structure

Those are valuable, but they are not the cleanest proof interface.

Core is the right proof boundary because it is:

- smaller
- more explicit
- semantically authoritative

## What "Core" Should Mean For Proofs

The proof target should not be raw surface syntax, and it probably should not be a totally separate verification IR either.

The best design is:

- **Validated Core is the semantic authority**
- **ProofCore is a restricted, proof-oriented view of validated Core**

That means the proof story should attach after:

```text
Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck
```

and before:

```text
Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA
```

In other words:

- `Elab` produces typed Core
- `CoreCanonicalize` normalizes it
- `CoreCheck` validates it
- **that validated Core is the right source for proving Concrete functions**

## ProofCore Design

The most useful near-term design is **not** "invent a brand-new proof IR."

Instead:

- keep Core as the main semantic object
- define a **ProofCore fragment** as the subset of validated Core intended for early user-program proofs
- make that fragment explicit and stable enough to export or re-encode in Lean

ProofCore should initially include:

- literals and locals
- algebraic data construction and projection
- explicit calls
- structured branching and pattern matching
- simple recursion
- typed values and explicit return structure

ProofCore should initially exclude or fence off:

- FFI
- `Unsafe`
- `trusted`
- host-environment-dependent capabilities
- raw pointer operations
- concurrency/runtime-dependent operations

That gives the project a proof target that is:

- small enough to formalize
- close enough to real user code to matter
- still clearly connected to the real compiler pipeline

## Pipeline Placement

If proving Concrete functions is a real goal, the compiler architecture should make one thing explicit:

- there is a **proof-oriented artifact boundary after `CoreCheck`**

This does **not** mean every compile goes through a separate proof pass.
It means the pipeline should be able to expose, for selected functions:

- validated Core term
- source-to-Core traceability
- explicit trusted/unsafe/FFI boundary markers
- enough identity information that Lean proofs do not depend on ad hoc names

The simplest future shape is something like:

```text
Source
  -> Parse
  -> Resolve
  -> Check
  -> Elab
  -> CoreCanonicalize
  -> CoreCheck
  -> ValidatedCore artifact
  -> optional ProofCore/Lean export for selected functions
  -> Mono
  -> Lower
  -> SSA ...
```

## Architecture Changes Suggested By This

If the project wants this proof story to be practical, the compiler should evolve in a few specific ways:

1. **Treat validated Core as a first-class artifact**
   - not just a pass-local internal value
   - something selected functions can be exported from later

2. **Preserve source-to-Core traceability**
   - enough span/name/origin metadata that proofs remain understandable to humans

3. **Keep trusted/unsafe/FFI boundaries explicit in Core**
   - these boundaries should be machine-visible in the proof story, not reconstructed later

4. **Avoid creating a second semantic authority**
   - ProofCore should be a restricted view of validated Core, not a rival IR with its own meaning

5. **Stage proof export after the semantics settle**
   - manual embedding first
   - compiler-supported export later

This is a refinement of the current architecture, not a demand for a whole new compiler pipeline.

## Architecture Summary

### Keep As-Is

These are already the right architectural choices:

- `CoreCheck` as the semantic authority
- validated Core as the main proof boundary
- `Mono` after the proof boundary
- SSA as backend-only territory, mainly for compiler-preservation proofs
- explicit `Unsafe` / `trusted` / FFI boundaries in the language and reports

### Should Change

The main architecture work still needed is:

1. add a first-class `ValidatedCore` artifact in the pipeline
   - right now it is implicit in pass flow
   - it should become explicit after `CoreCheck`
2. define `ProofCore` as a restricted view of `ValidatedCore`
   - not a new semantic IR
   - just the proof-friendly subset/filter for early Lean work
3. preserve source-to-Core traceability
   - function origin
   - spans or source mapping
   - stable names/ids through `Elab -> CoreCheck`
4. add export support later
   - manual embedding first
   - later compiler export of selected `ValidatedCore` / `ProofCore` terms to Lean
5. be explicit about proof scopes
   - early: pure / no FFI / no `Unsafe` / no `trusted`
   - later: effects, resources, capabilities
   - much later: runtime and concurrency

### Probably Not A Goal

The following are not just "later." They are probably the wrong default architecture:

- no separate verification compiler
- no proof pass inserted into ordinary compilation
- no surface-AST proof target

These all risk splitting the semantic authority, making the ordinary compiler path heavier than it should be, or attaching proofs to a noisier object than validated Core.

### Not A Near-Term Goal, But Potential Later Work

The following are real later-stage possibilities, but only after the narrow Core-based proof story works:

- no MLIR/backend involvement in the proof story yet
- no attempt to prove arbitrary programs end to end first

Why they could make sense later:

- backend or MLIR-layer reasoning could eventually matter for compiler-preservation proofs, backend rewrite validation, or richer multi-backend translation contracts
- broader end-to-end program proofs could eventually expand from pure fragments into effects, resources, capabilities, runtime interaction, and later concurrency

These are legitimate downstream proof problems. They are just not the first proof problems Concrete should try to solve.

### One Important Caution

Do not let `ProofCore` become a second semantic authority.

If `ValidatedCore` says one thing and `ProofCore` says another, the design is wrong. `ProofCore` should stay a restricted view of validated Core, not a new compiler world.

## What Should Be Provable First

The first realistic target subset is:

- pure functions
- no FFI
- no `Unsafe`
- no `trusted`
- no environment interaction
- simple recursion / structured control flow
- algebraic data types and pattern matching

Examples of good first proofs:

- arithmetic helpers
- structural recursive functions
- formatter/parser properties on limited domains
- collection operations over abstract specs

## What Gets Harder

These cases need stronger models or proof boundaries:

- FFI
- `Unsafe`
- `trusted`
- mutable heap/stateful code
- capabilities tied to the host environment
- concurrency/runtime-dependent behavior

That does not make them impossible. It means they likely need:

- relational specs
- effect models
- trusted assumptions at boundaries
- explicit proof scopes instead of fully total-function reasoning

## Compiler Support Needed

To make function proofs practical, Concrete should eventually be able to:

1. expose a stable Lean-side representation of validated Core / ProofCore terms for selected functions
2. preserve source-to-Core traceability well enough that proofs remain understandable
3. identify trusted/unsafe/FFI boundaries explicitly in the proof story
4. keep language-item identity explicit so proofs do not depend on ad-hoc names

This means the formalization roadmap and the semantic-cleanup roadmap directly support function proving.

## Example Workflow

The intended long-term workflow is:

1. a user writes a Concrete function
2. the compiler exposes or exports a validated Core / ProofCore representation for that function
3. the user writes Lean proof code against that exported meaning

A tiny conceptual example:

Concrete source:

```con
fn add1(x: Int) -> Int {
    return x + 1;
}
```

The compiler would eventually make available something like:

- a validated Core / ProofCore term for `add1`
- or a generated Lean module containing that term and its semantics hook

Then the user could write Lean along these lines:

```lean
-- Conceptual example: exported from Concrete, not current syntax
def add1_core : ProofCoreFn := ...

def add1_sem (x : Int) : Int := ...

theorem add1_correct (x : Int) :
  add1_sem x = x + 1 := by
  rfl
```

A slightly more structured example:

Concrete source:

```con
enum Option<T> {
    Some { value: T },
    None,
}

fn unwrap_or_zero(x: Option<Int>) -> Int {
    match x {
        Option#Some { value } => return value,
        Option#None => return 0,
    }
}
```

Lean-side proof shape:

```lean
-- Conceptual example only
def unwrap_or_zero_core : ProofCoreFn := ...

def unwrap_or_zero_spec : Option Int -> Int
| some v => v
| none   => 0

theorem unwrap_or_zero_correct (x : Option Int) :
  unwrap_or_zero_sem x = unwrap_or_zero_spec x := by
  cases x <;> rfl
```

The important point is not the exact Lean API yet. The important point is the split:

- Concrete remains the implementation language
- Lean remains the proof language
- validated Core / ProofCore is the bridge between them

## A More Interesting Example

The real value of this idea is not proving `add1`.
It is proving selected low-level routines that are still practical executable code.

For example, imagine a small parser/formatter round-trip property over a bounded domain.

Concrete source:

```con
fn format_then_parse(x: Int) -> Result<Int, ParseError> with(Alloc) {
    let s: String = fmt::format_int(x);
    return parse::parse_int(s);
}
```

A useful Lean-side theorem shape would be:

```lean
-- Conceptual only
theorem format_then_parse_roundtrip (x : Int) :
  within_domain x ->
  format_then_parse_sem x = Result.ok x := by
  ...
```

This is much more interesting than a toy arithmetic proof because it starts to look like a real library guarantee:

- the implementation is ordinary Concrete code
- the theorem uses Lean
- the property is about actual executable behavior, not a separate handwritten model

Another good class of example is a low-level data-structure invariant.

Concrete source:

```con
fn push_preserves_len<T>(v: Vec<T>, x: T) with(Alloc) -> Vec<T> {
    let before: Int = v.len();
    v.push(x);
    assert(v.len() == before + 1);
    return v;
}
```

The interesting theorem is not the assertion itself. It is the semantic property:

```lean
-- Conceptual only
theorem vec_push_len (v : VecModel α) (x : α) :
  len (push_sem v x) = len v + 1 := by
  ...
```

That kind of result is where the bridge starts to justify itself:

- real low-level implementation language
- real theorem-prover proof environment
- properties about code you would actually want to run

## A Concrete-Specific Example: Capability Discipline

One of the most interesting longer-horizon examples would not be pure arithmetic or containers. It would be a proof about authority.

Concrete source:

```con
fn read_config(path: String) with(File, Alloc) -> Result<String, FsError> {
    return fs::read_to_string(path);
}
```

The interesting Lean-side theorem shape is not merely "this returns a string."
It is something closer to:

```lean
-- Conceptual only
theorem read_config_no_network_or_process_effects (path : String) :
  effects_of (read_config_sem path) ⊆ {Effect.file, Effect.alloc} := by
  ...
```

Or, phrased more generally:

- if a function is validated without `Network`
- and validated without `Process`
- then its semantics cannot perform network or process effects

This is especially interesting because it is not just a normal program-correctness theorem.
It is a theorem about one of Concrete's central design promises:

- authority is explicit
- capabilities matter semantically
- the proof story can talk about those authority boundaries directly

That makes capability/effect proofs one of the most Concrete-specific examples the Lean bridge could eventually support.

## Good First Milestones

1. Formalize a small pure Core fragment in Lean.
2. Prove basic typing and evaluation properties for that fragment.
3. Select a few representative Concrete functions and connect them to their elaborated Core form.
4. Prove simple correctness properties about those functions in Lean.
5. Later, extend the proof boundary toward effects, resources, and capabilities.

## What Not To Do First

- do not start by trying to prove arbitrary source programs end to end
- do not start with FFI-heavy or `Unsafe`-heavy examples
- do not make proof tooling depend on unstable surface syntax details
- do not bolt on a separate "verification language" before the Core semantics are strong

## Roadmap Connection

This topic belongs downstream of:

- semantic cleanup
- stronger Core semantics/formalization work
- explicit trust/capability boundaries
- stable artifact boundaries

It is best treated as a later formalization/proof track, not as an early-language feature.
