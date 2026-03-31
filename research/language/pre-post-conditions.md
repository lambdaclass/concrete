# Pre/Post Conditions: Should Concrete Add Refinement Types or Contracts?

**Status:** Decided — not on the roadmap. Revisit at Phase 12+ only if the simpler system proves insufficient.
**Affects:** Language design, Phase 11 (kernel formalization), verification scope
**Date:** 2026-03-07

## Context

Concrete's type system currently guarantees three things:
1. **Memory safety** — no use-after-free, no double-free, no dangling references (linearity + borrow checker)
2. **Resource safety** — linear values consumed exactly once, no leaks
3. **Effect correctness** — declared capabilities match actual effects

These are *structural* guarantees: they say how resources flow, not what values they hold. The type system cannot express "this function returns a sorted list" or "this index is within bounds" or "this balance never goes negative."

Pre/post conditions (Hoare-style contracts, refinement types) would add *functional correctness* guarantees: proving that programs compute the right answer, not just that they handle resources correctly.

This document examines four approaches to pre/post conditions, evaluates whether they fit Concrete's design thesis, and recommends a course of action.

---

## How Other Languages Handle This

### F*/Low* — Hoare-style refinement types

F* is a dependently typed functional language designed for program verification. Low* is a subset of F* that compiles to C via the KaReMinern toolchain.

**Mechanism:** Refinement types. A type like `x:Int{x > 0}` describes positive integers. Function signatures carry pre/post conditions as refinements:

```fstar
val factorial: n:nat -> Tot (r:nat{r >= 1})
let rec factorial n =
  if n = 0 then 1
  else n * factorial (n - 1)
```

The `nat` type is itself a refinement: `x:int{x >= 0}`. The return type `r:nat{r >= 1}` is a postcondition. F* uses an SMT solver (Z3) to discharge proof obligations automatically.

**Key properties:**
- Full dependent types — types can contain arbitrary terms
- SMT-backed — Z3 handles most proofs automatically, but can time out or fail unpredictably
- Effectful — F* has an effect system (Tot, ML, ST, etc.) that tracks side effects in types
- Low* extracts to C — verified low-level code with memory safety proofs
- The proof burden is high: real Low* programs (miTLS, HACL*) require significant verification effort, often more lines of proof than code

**What Concrete can learn:** F*/Low* proves that refinement types *work* for systems code. The miTLS and HACL* projects demonstrate verified cryptographic implementations. But the cost is a full dependent type theory plus an SMT solver — both are complex and can produce unpredictable proof failures.

### Ada/SPARK — Contracts via pragmas

Ada/SPARK takes a pragmatic approach: contracts are annotations on subprogram declarations, proved by a dedicated tool (GNATprove) rather than the type system.

```ada
function Increment (X : Integer) return Integer
  with Pre  => X < Integer'Last,
       Post => Increment'Result = X + 1;
```

**Mechanism:** Pre/post conditions are boolean expressions written in a subset of Ada. SPARK's prover (based on Why3 + SMT solvers) attempts to verify them statically. What it cannot prove, it reports as unproved — the programmer can add loop invariants, ghost code, or accept the gap.

**Key properties:**
- No dependent types — contracts are separate annotations, not part of the type
- Incremental adoption — you can add contracts to some functions and not others
- The SPARK subset restricts Ada (no pointers, no exceptions, no tasking in verified code) to keep proofs tractable
- Industrial track record — used in avionics (DO-178C), railway signaling, defense
- Proof failures are explicit — the tool tells you what it could not prove, and you decide whether to add more annotations or accept the risk
- Contracts can also be checked at runtime (assertion mode) when static proof is not attempted

**What Concrete can learn:** SPARK's pragmatism is instructive. Contracts are *separate* from the type system, so they don't infect every type signature. The SPARK subset restricts the language specifically to make proofs tractable — this mirrors Concrete's "simple enough to verify" thesis. But SPARK required decades of investment in the prover toolchain.

## What SPARK / Ada Suggest Beyond Contracts

The most useful SPARK / Ada lessons are not only "add pre/post conditions."
For high-integrity and critical systems, the bigger lessons are about subsets, profiles, analyzability, and certification-friendly discipline.

### 1. A clearly defined critical subset

SPARK succeeds partly because it is not "all of Ada with proofs sprinkled on top." It is a stricter analyzable subset.

For Concrete, the analogous long-horizon idea is:

- define a **proof-critical** or **high-integrity** subset
- keep it smaller than the full language
- make the restrictions explicit and tool-enforced

That subset could later rule out or tightly constrain things like:

- FFI-heavy code
- `Unsafe`
- `trusted`
- host-dependent effects
- unconstrained dynamic allocation
- concurrency features that do not yet have a clear analyzable model

This is one of the strongest ideas Concrete could borrow for critical systems.

### 2. Bounded-allocation and determinism profiles

Critical software often needs more than "memory safe." It needs behavior that is analyzable and predictable.

Concrete could eventually support profiles such as:

- no allocation
- bounded allocation
- no FFI
- no `Unsafe`
- no hidden authority escalation
- no recursion, or recursion only with explicit bounds

That would fit Concrete better than trying to imitate Ada syntax directly. The value is the profile discipline, not the surface language.

### 3. Stronger contract and invariant surfaces

If Concrete eventually adds richer proof-oriented contracts, the most useful forms for critical code would be:

- preconditions
- postconditions
- data-structure invariants
- loop invariants
- explicit termination / variant measures

The key SPARK lesson is not "make everything dependently typed." It is that a language becomes much more useful for critical systems when these obligations are explicit and reviewable.

### 4. Better initialization and definite-assignment discipline

Ada and SPARK are strong at keeping partially initialized or semantically suspect states out of ordinary code.

Concrete should keep pushing in the same direction:

- explicit initialization
- making uninitialized states hard to express
- keeping ownership/resource state transitions visible

This matters especially for critical systems because bugs at initialization boundaries are common and expensive.

### 5. Module and interface contracts for proofs and audits

Critical systems need more than local function reasoning. They need strong boundaries between components.

Concrete could borrow this lesson as:

- module-level contracts
- capability/authority contracts
- resource/lifetime invariants at module boundaries
- proof-oriented interface artifacts

This aligns well with Concrete's audit and report story.

### 6. Certification-friendly artifact and traceability discipline

Another major SPARK / Ada strength is not just proof power. It is process discipline:

- reviewable artifacts
- explicit assumptions
- traceability from source to analysis result
- tooling that supports certification evidence

For Concrete, the comparable opportunity is:

- reproducible compiler outputs
- report artifacts tied to code and build state
- proof artifacts tied to compiler artifacts
- explicit trust boundaries and assumptions

This is a better fit than trying to copy Ada's ecosystem directly. The real win is stronger engineering evidence.

### 7. Restricted, analyzable concurrency

SPARK's restrictions are a reminder that concurrency is one of the fastest ways to make proofs and certification harder.

If Concrete eventually adds concurrency, the SPARK/Ada lesson is:

- prefer an analyzable concurrency model
- keep authority and ownership explicit across concurrent boundaries
- do not make "threads exist" the same thing as "concurrency is ready for critical systems"

### What this means for Concrete

The highest-value Ada/SPARK-inspired ideas for Concrete are probably:

1. a clearly defined critical/provable subset
2. bounded-allocation and determinism profiles
3. stronger contracts and invariants where they earn their complexity
4. certification-friendly artifacts, reports, and traceability

These ideas strengthen Concrete for serious high-integrity systems without requiring Concrete to become "Ada syntax with Lean attached."

### ATS — Linear proofs interleaved with code

ATS (Applied Type System) combines dependent types with linear types in a unique way: proofs are values that must be consumed linearly, just like resources.

```ats
fun {a:t@ype}
array_get {n:int} {i:nat | i < n}
  (A: &array(a, n), i: int(i)): a
```

**Mechanism:** Types carry index terms (like `n` and `i` above). Proof obligations are generated as linear "proof values" — the programmer must construct and consume them. The `{i:nat | i < n}` constraint is a static assertion that `i` is a valid index.

**Key properties:**
- Dependent types with linear proofs — the programmer manually manages proof terms alongside data
- No SMT solver — proofs are constructed manually by the programmer (or by the constraint solver for simple cases)
- Extremely powerful — can express precise memory layout invariants, pointer arithmetic safety, array bounds
- Extremely difficult to use — the learning curve is steep, and proof terms clutter the code significantly
- Limited adoption — ATS has a small user community, partly due to the difficulty of writing proofs

**What Concrete can learn:** ATS demonstrates that linear types and dependent types can coexist — linear proofs are an elegant concept. But the practical cost is high. ATS code is dense with proof terms, which contradicts Concrete's goal of readability and LLM-friendliness. The lack of automation (no SMT solver) means every proof is manual.

### Liquid Haskell — SMT-backed refinement types

Liquid Haskell adds refinement types to Haskell as a layer on top of the existing type system. Refinements are predicates drawn from a decidable logic (quantifier-free linear arithmetic + uninterpreted functions), discharged by an SMT solver.

```haskell
{-@ type Pos = {v:Int | v > 0} @-}

{-@ abs :: Int -> Pos @-}
abs :: Int -> Int
abs x = if x >= 0 then x else negate x
```

**Key properties:**
- Refinements are annotations, not changes to the type system — existing Haskell code continues to work
- Decidable logic — the refinement language is deliberately restricted to keep SMT queries decidable (no arbitrary quantifiers)
- Automatic — the SMT solver handles most proofs without programmer intervention
- Inference — Liquid Haskell infers refinement types for local variables via liquid type inference (abstract interpretation + SMT)
- Limited expressiveness — you cannot express arbitrary properties; the logic is restricted to what SMT can handle efficiently
- Separate tool — `liquidhaskell` is a GHC plugin / standalone checker, not part of GHC itself

**What Concrete can learn:** Liquid Haskell shows that refinement types can be added *after* a language ships, as a non-breaking extension. The decidable logic restriction is key — by limiting what you can express, you guarantee that proofs always terminate. But even with the restriction, integrating with GHC's type inference has been a multi-year research effort.

---

## Comparison Table

| Dimension | F*/Low* | Ada/SPARK | ATS | Liquid Haskell |
|-----------|---------|-----------|-----|----------------|
| **Mechanism** | Dependent refinement types | Contract annotations | Dependent types + linear proofs | Refinement types (annotations) |
| **Proof engine** | Z3 (SMT) | Why3 + CVC4/Z3 | Manual + constraint solver | Z3 (SMT) |
| **Expressiveness** | Arbitrary properties | Boolean expressions over Ada subset | Arbitrary properties | Decidable logic subset |
| **Automation** | High (SMT) | High (SMT) | Low (mostly manual) | High (SMT) |
| **Predictability** | Low (SMT timeouts) | Medium (explicit unproved items) | High (manual = no surprises) | Medium (SMT timeouts rare due to decidable logic) |
| **Integration** | Built into language | Separate annotations | Built into language | Separate annotations/plugin |
| **Learning curve** | Very high | Moderate | Very high | Moderate |
| **Industrial use** | miTLS, HACL*, EverParse | Avionics, defense, railway | Research/academic | Research, some production |
| **Added after v1?** | No (fundamental) | Yes (incremental adoption) | No (fundamental) | Yes (GHC plugin) |
| **Requires dependent types?** | Yes | No | Yes | No (restricted refinements) |
| **Requires SMT solver?** | Yes | Yes | No | Yes |

---

## The Dependent Types vs Refinement Types Spectrum

There is a spectrum of approaches to expressing program properties in types:

```
Simple types ──── Refinement types ──── Full dependent types
  (Concrete)      (Liquid Haskell)       (F*, ATS, Idris)
```

**Simple types** (what Concrete has): Types classify values by structure — `Int`, `Bool`, `List<T>`, `Heap<T>`. The type system ensures structural properties (linearity, borrowing, effects) but says nothing about values. You cannot express "this list is sorted" or "this integer is positive."

**Refinement types**: Types are simple types plus predicates — `{x:Int | x > 0}`. The predicates live in a restricted logic (typically quantifier-free linear arithmetic) that an SMT solver can decide. You get some value-level reasoning without full dependent types. The restriction to decidable logic means proofs always terminate, but you cannot express arbitrary properties.

**Full dependent types**: Types can contain arbitrary terms — `Vec n a` where `n` is a value-level natural number. You can express any property, but proofs may require manual effort, and type checking may be undecidable in general. The type system and the proof system are unified — there is no separation between "types the compiler checks" and "properties the prover checks."

**The key tradeoff:**

| Property | Simple types | Refinement types | Full dependent types |
|----------|-------------|-----------------|---------------------|
| What you can express | Structure only | Structure + decidable predicates | Anything |
| Type checking decidable? | Yes | Yes (by construction) | No (in general) |
| Proof automation | N/A | High (SMT) | Low to medium |
| Lean formalization difficulty | Tractable | Hard (must model SMT interaction) | Very hard |
| Compiler complexity | Low | Medium-high | Very high |
| LLM comprehension | Easy | Moderate | Hard |

Concrete currently sits at the left end. The question is whether to move rightward, and how far.

---

## What Pre/Post Conditions Would Add to Concrete

### Gains

1. **Functional correctness proofs.** The type system could guarantee that a sort function actually sorts, that a balanced tree stays balanced, that an encoder/decoder round-trips correctly. Currently, these are tested but not proven.

2. **Bounds checking elimination.** Refinement types like `{i:Nat | i < len}` could eliminate runtime bounds checks when the compiler can prove the index is valid. This is a real performance win for tight loops.

3. **Richer API contracts.** Function signatures could express preconditions ("this buffer has at least N bytes") and postconditions ("the returned list has the same length as the input") that callers must satisfy and callees must guarantee.

4. **Domain-specific invariants.** Financial code could track "balance >= 0" in the type. Cryptographic code could track "key has been initialized." Protocol code could track state machine transitions more precisely than the current type system allows.

5. **Closing the verification gap.** Phase 11 verifies that the type system is sound. Pre/post conditions would let the type system verify that user programs are correct. This extends the chain of trust from "the compiler is correct" to "your program is correct."

### Costs

1. **Dependent types or SMT solver — pick one.** Refinement types require an SMT solver (Z3, CVC4) as a dependency. Full dependent types require a proof language embedded in the type system. Neither is simple. Both add thousands of lines of compiler code and new failure modes.

2. **Complexity explosion in the type system.** Every type signature potentially carries predicates. Type inference must account for refinements. Error messages must explain why a refinement was not satisfied ("Z3 could not prove that x + 1 > 0 given that x >= 0" is not a helpful error message for most programmers). The "simple enough to verify" thesis is directly threatened.

3. **Harder Lean formalization.** Phase 11 aims to prove the type system sound in Lean. Adding refinement types means proving that the SMT interaction is sound — that the solver's "proved" answer actually implies the property holds. This is a major formalization challenge. F*'s soundness relative to Z3 is an open research problem (the "SMT oracle assumption"). Full dependent types would require formalizing a much richer type theory.

4. **Unpredictable proof failures.** SMT solvers can time out. Small code changes can cause previously-passing proofs to fail. This is a known problem in F* and Dafny — "proof instability" makes CI unreliable. ATS avoids this by requiring manual proofs, but at the cost of usability.

5. **LLM unfriendliness.** Concrete's core thesis is "can a machine reason about this?" Refinement types and dependent types are among the *hardest* things for LLMs to generate correctly. Writing proofs, satisfying refinement predicates, constructing proof terms — these are tasks where current LLMs struggle significantly. Adding pre/post conditions could make Concrete *harder* for machines to write, contradicting the LLM-first design.

6. **Annotation burden.** Even with good inference, refinement types require annotations on function signatures and data types. The ratio of proof annotations to actual code can be high — in verified HACL* code, proof annotations often exceed the implementation code. This contradicts Concrete's goal of minimal ceremony.

---

## Does It Conflict With "Simple Enough to Verify"?

Yes.

Concrete's kernel (Phase 11) is designed to be small: ~15 term constructors, ~10 type constructors. The proofs target linearity soundness, progress/preservation, and effect soundness. These are well-understood PL theory results with established proof techniques.

Adding refinement types would require:
- Extending the kernel with refinement predicates on types
- Formalizing the interaction with an SMT solver (or embedding a proof language)
- Proving that refinement checking is sound (the solver does not accept invalid programs)
- Proving that refinement checking is decidable (the solver always terminates)

The SPARK approach (contracts as separate annotations, proved by an external tool) avoids infecting the kernel, but creates a *second* verification system that must be maintained alongside the Lean formalization. You now have two provers (Lean for the type system, SMT for contracts) and must argue that they are consistent.

The Liquid Haskell approach (restricting to decidable logic) keeps proofs terminating but still requires SMT integration and significantly complicates the formalization.

In all cases, the "simple enough to verify" property is weakened. The kernel grows, the proof obligations multiply, and the trusted computing base expands.

---

## Is the Simpler Approach Sufficient?

For Concrete's target use cases — systems programming where memory safety, resource safety, and effect correctness matter — the answer is likely **yes, for now.**

### What assertions + testing provide

- **Runtime bounds checks** — array access is bounds-checked at runtime (abort on violation). This catches the most common class of "functional correctness" bugs.
- **Debug assertions** — `assert(x > 0)` in debug builds catches violated assumptions during development.
- **Property-based testing** — tools like QuickCheck-style testing can check properties across random inputs. Not a proof, but high confidence.
- **The type system already handles the hardest bugs.** Use-after-free, resource leaks, data races, unauthorized effects — these are the bugs that refinement types *cannot* catch (they are structural, not value-level). Concrete already prevents them.

### What assertions + testing do NOT provide

- **Static guarantees about values** — you cannot prove at compile time that an index is in bounds, that a list is sorted, or that a balance is non-negative.
- **Bounds check elimination** — without static proof, runtime bounds checks remain (performance cost in tight loops).
- **API contract enforcement** — callers can violate preconditions that are only documented, not enforced by the type system.

### The pragmatic argument

Most production systems programming (operating systems, databases, network stacks, game engines) is done in C, C++, Zig, or Rust — none of which have refinement types. The bugs that kill these systems are memory safety bugs (use-after-free, buffer overflows, data races) and resource management bugs (leaks, double-close). Concrete already prevents these statically.

The functional correctness bugs (wrong sort order, off-by-one, incorrect state machine transition) are real, but they are caught effectively by testing. The cost/benefit ratio of adding refinement types to catch them statically is unfavorable — high implementation and formalization cost for modest marginal safety improvement over testing.

---

## Can This Be Added Later as a Non-Breaking Extension?

**Yes, with care.** The key insight is that refinement types *refine* existing types — they add information, they don't change the underlying type. This means:

1. **Existing code continues to work.** A function `fn add(a: Int, b: Int) -> Int` remains valid. Refinements are opt-in annotations.

2. **The SPARK model works.** Contracts as annotations (`pre`, `post`) on existing function signatures. No change to the core type system. An external tool (or compiler plugin) checks the contracts.

3. **The Liquid Haskell model works.** Refinement annotations in comments or a separate file. The core compiler ignores them. A separate checker verifies them.

4. **The kernel need not change.** If contracts are checked by an external tool (not the core type checker), the Phase 11 formalization remains intact. The contract checker is a separate system with its own soundness argument.

Potential extension syntax (speculative, not proposed):

```
// Option 1: SPARK-style annotations
fn binary_search(arr: &[Int], target: Int) -> Option<Uint>
    pre(is_sorted(arr))
    post(result.is_some() => arr[result.unwrap()] == target)
{
    ...
}

// Option 2: Refinement type aliases
type SortedArray<T> = {arr: &[T] | is_sorted(arr)}
type BoundedIndex<N> = {i: Uint | i < N}

fn get<T>(arr: &[T; N], i: BoundedIndex<N>) -> &T {
    // No runtime bounds check needed — the type guarantees i < N
    ...
}
```

Both could be added without breaking existing code or the existing type system. The compiler would treat un-annotated code as having trivially-true pre/post conditions.

**Requirements for a clean extension:**
- Contracts must not change the runtime semantics of existing code
- The core type checker must not depend on the contract checker
- The contract checker must be a separate, optional pass
- The kernel formalization must remain valid with or without contracts
- Error messages from contract failures must be clearly separated from type errors

---

## Recommendation

**Keep pre/post conditions off the roadmap.** This is a conscious design decision, not an oversight.

### Rationale

1. **Concrete's type system already provides the guarantees that matter most.** Memory safety, resource safety, and effect correctness prevent the most damaging classes of bugs in systems code. These are the guarantees that C, C++, and Zig lack and that Rust provides only partially (unsafe blocks, no effect tracking).

2. **The cost is disproportionate to the benefit.** Adding refinement types or dependent types would significantly complicate the compiler, the kernel formalization, and the user experience. The marginal safety improvement (proving functional correctness, not just structural correctness) does not justify this cost for Concrete's current target use cases.

3. **It conflicts with the "simple enough to verify" thesis.** The Phase 11 formalization is tractable *because* the type system is simple. Adding refinement types or SMT interaction would make the formalization significantly harder — potentially intractable within reasonable effort.

4. **It conflicts with the LLM-first design.** LLMs are reasonably good at generating code that satisfies structural type constraints (linearity, borrowing, effects). They are significantly worse at generating code with correct proof annotations or refinement predicates. Adding pre/post conditions makes Concrete harder for machines to write.

5. **Assertions + testing are sufficient for now.** The functional correctness bugs that refinement types would catch are effectively caught by runtime assertions and property-based testing. This is the approach that works in practice for C, C++, Go, Zig, and Rust.

6. **It can be added later without breaking anything.** The SPARK and Liquid Haskell models both demonstrate that contracts/refinements can be layered onto an existing language. If Concrete's simpler system proves insufficient for some use cases, pre/post conditions can be added as a Phase 12+ extension — an optional, external checker that does not affect the core language or its formalization.

### When to revisit

Revisit this decision if:
- Users report that the lack of static value-level guarantees is a blocker for critical use cases (e.g., certified avionics, formally verified cryptography)
- LLM capabilities advance to the point where generating proof annotations is reliable
- The Phase 11 formalization is complete and the team has bandwidth for a second verification system
- SMT solver integration becomes significantly simpler and more predictable (e.g., stable proof caching, guaranteed termination)

Until then, the priority is completing the current type system (Phases 1-6), proving it sound (Phase 11), and building the standard library and tooling (Phases 9-10, 12). These deliver concrete value to users. Pre/post conditions are a Phase 12+ consideration at the earliest.

---

## Open Questions (for future exploration)

1. **Could Concrete support a `debug_assert` that is erased in release builds?** This is lighter than refinement types but stronger than nothing — it documents assumptions even if it doesn't prove them.
2. **Could capability-bounded assertions work?** E.g., `assert(x > 0) with(Debug)` — only checked when `Debug` capability is active. This keeps assertions visible in the capability system.
3. **If contracts are added later, should they use SPARK-style annotations (pre/post on functions) or Liquid Haskell-style refinement types (`{x:Int | x > 0}`)?** The SPARK model is more pragmatic; the Liquid Haskell model is more composable.
4. **Could the Lean formalization itself serve as the "proof" layer?** Instead of adding an SMT solver to Concrete, could users write proofs about their Concrete programs directly in Lean? This leverages the existing tool (Lean 4) rather than adding a new one. However, it requires bridging the gap between Concrete source and Lean terms, which is nontrivial.
