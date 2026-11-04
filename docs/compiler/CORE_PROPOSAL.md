# Core proposal (draft for A2)

**Precedence.** This draft is background. The governing proposal is the set of 88 Core
decision rows (Q1.1–Q9.19) that Claude and Codex settled on 2026-11-04 (three debate
rounds, then a resolution round that gave each open point a concrete rule) and put
before the owner on the decision page. Six of them were added by the resolution round:
Q2.8 (separate Copy and Destroy impls per container), Q3.12 (the `kept` extern marker
and its `Kept<H>` receipt), Q4.7 (the `slice … else` spelling), Q7.10 (the hosted grant
table), Q7.11 (`Device` or `Console` for the standard streams) and Q9.19 (approval,
formalization, freeze). Where this draft and a row differ, the row governs, and the
draft's recommendation is superseded; the passages below that a row overrides are marked
*Superseded* where they stand. Neither is decided until the owner answers. Rows that
revise a recorded rule say so: Q5.1 and Q5.4 revise OD45 and OD87; Q6.4 revises
TYPE-22's two examples; Q2.8 revises TRAIT-1's coherence rule; Q9.15 replaces the
generated-chapter requirement; Q7.11 revises authority rule 13 only if `Console` is
chosen. The rows are committed beside this draft when the owner
answers them. Until then they live on the decision page, and each section below names
its governing rows.

Status: draft for the owner's Core session. Nothing here is decided. The owner
answers in the stages of roadmap rows A2a–A2c (provisional approval, a proposed
`Spec.lean` with four checked design theorems, freeze; row Q9.19); until then no work
encodes an answer. The questions are the nine in
[ROADMAP.md](../../ROADMAP.md#questions-for-the-core-session). The proposal is drafted
against the [A1 boundary artifacts](A1_BOUNDARIES.md), and its sections 1–4 are cited
as "A1 §n". Lean stays the only normative source. An approved answer is formalized
in a proposed `Spec.lean` during A2b, becomes normative at the A2c freeze, and then
receives A3's one-main-file layout gate; it never becomes prose here.

Each question gives the decided rules it must respect, a recommendation and what the
recommendation leaves open. Where alternatives were weighed, it also gives the options
and the counterexamples that ruled them out. Where a question carries an A2b design
proof, it states which.

The recommendations come from a Claude draft that Codex reviewed against the
decision record on 2026-11-04. Codex agreed with Q8 and asked for changes everywhere
else. Its corrections are adopted below; where they reversed the draft (Q2, Q5, Q6),
the draft's answer is listed as a rejected option with the reason. On Q5 the owner
then leaned towards never inferring; that option revises the decided OD45 and OD87, so it is
set out next to the recommendation for the owner to choose.

Fixed constraints:
- the decided rules in [decisions.md](../spec/decisions.md);
- references are second-class;
- ownership is linear, not affine;
- one form per situation;
- the most transparent long-term option, with verbosity accepted.

Code below is schematic: the spellings are the owner's to choose.

## Q1. Allocator discipline

*Governing rows: Q1.1–Q1.13. Where this section differs from them, it is superseded.*

**Decided.** Allocation and reset decisions (decisions.md, allocation section): an
allocator is an explicit value, a reset invalidates every allocation, and giving up an
allocator requires no live allocation from it.

**Options.**
- **(a) Brand occurrence.** A value depends on allocator `A` when its type mentions
  `A`. Rejected:
  - It misses hidden backing. `new_arena(&mut parent)` that takes its storage from
    `parent` returns `Allocator<B>`, whose type does not mention `A`. Consuming
    `parent` and then destroying a `Heap<T, B>` frees through a dead release context.
  - It over-rejects. An allocation-free `Tag<A>`, or a function pointer taking
    `Heap<T, A>`, mentions `A` without owning anything.
- **(b) Structural dependency judgment (recommended).**
- **(c) Runtime reference counting of allocator users.** Rejected: hidden cost and a
  second ownership mechanism.

**Recommendation.**
1. **Brands.** A brand is its own kind (`Brand`), not a type. It appears only as the
   brand argument of `Allocator<A>`, `Heap<T, A>`, `HeapArray<T, A>`, `Vec<T, A>`,
   `String<A>` and user types with a brand parameter. Brands are erased at run time
   and never cause specialization. `Sys` is a brand constant that the hosted target
   mints once.
2. **Fresh identities.** Creating an allocator (`new_arena`, `reset`, a target grant)
   returns a scoped existential package. Opening it introduces a fresh brand variable
   scoped to the block. A value depending on that brand leaves the block only inside a
   package together with its allocator.
3. **Reset in a loop.** Each iteration's `reset` opens a fresh brand in that
   iteration's scope, so generations are never equated across the back-edge. State
   carried across iterations carries the package, never a bare brand.
4. **Dependency is a structural judgment**, defined once and covering:
   - fields;
   - enum payloads;
   - container elements;
   - existential packages;
   - returned values;
   - pending deferred calls;
   - loans;
   - allocator backing.

   An allocator that takes its backing from a parent either shows the parent's brand
   in its type (`Allocator<B, backed_by A>`, spelling open) or is restricted to backing
   it owns independently. The draft prefers exposing the parent. A type parameter that
   only tags (`Tag<A>`) carries no dependency unless the judgment derives one. Giving up
   or resetting `Allocator<A>` is accepted only when nothing live in scope depends on
   `A`. Across calls the judgment is compositional through signatures.
5. **Release context.** `Allocator<A>` is a linear handle to state that never moves.
   Every heap stores a raw release context that points at that state. The context is
   never a safe reference. It is valid by the dependency judgment: one proof covers
   every container. The release code's identity is part of that state, together with a
   construction and failure protocol for the state itself.
6. **`AllocFailure<T>`** holds exactly the rejected `T`. An operation that can fail
   part-way has its own failure type and Lean contract (`ReserveFailure` holds nothing
   because the container is unchanged; a building operation returns what it built).
7. **"Unchanged"** means every observable of the container (contents, length,
   capacity, ownership obligations) is as before, and the allocator holds no allocation
   from the failed operation. Hiding allocator statistics is not enough: an
   **allocation-oracle and progress contract** fixes whether a failure may change later
   allocation success ([VERIFIED_PIPELINE.md](VERIFIED_PIPELINE.md)). *Superseded* by row
   Q1.11: an availability predicate per allocator kind and four laws (availability gives
   success, a failure leaves the state unchanged, a release never reduces availability, a
   concrete success witness), with a failure-then-retry example.

**Still open:**
- how packages are formed and opened, and the legal unpacking order;
- brand equality, and explicit brand arguments;
- who owns an allocator's backing;
- what happens when allocating the allocator's own state fails;
- whether capacity and addresses are observable;
- rollback around callbacks.

*Superseded:* rows Q1.3–Q1.12 answer each of these.

**Proof impact.** A2b design proof: reset generativity plus allocator cleanup. The
release-validity lemma follows from the dependency judgment.

## Q2. `destroy` bounds

*Governing rows: Q2.1–Q2.7. Where this section differs from them, it is superseded.*

**Decided.**
- OD59: the only ways to consume a heap are `*h` (move the payload out and release the
  storage) and `destroy(h)`; `free` is removed.
- OD28: `Destroy` is satisfied only by an impl, and a Copy type is never `Destroy`.
- Arrays have their own language impl.
- OD113 removed the unchecked empty-vector release exceptions.
- Resource cleanup is explicit and fallible (authority rule 9).

**Options.**
- **(a) Structural automatic `Destroy` plus `free`** (the draft). Rejected:
  - it brings back `free`;
  - it makes Copy types destroyable;
  - it rejects a valid user impl, such as a wrapper whose `destroy` calls a linear
    `Ticket`'s consuming `finish`;
  - draining a `Vec<File, A>` does not change its type, so the drained vector still
    has no `Destroy`.
- **(b) Explicit impls plus specified container impls plus a consuming drain
  (recommended).**

**Recommendation.** Three verbs, never merged:
- **Release storage:** `*h`. It consumes the heap and returns the payload; it needs
  the allocator context only.
- **Destroy:** an explicit `Destroy` impl. Its body is ordinary checked code whose
  capabilities are at most `Alloc`. Built-in containers have specified conditional
  impls (`Heap<T, A>: Destroy` when `T: Destroy`, and so on), written in the Lean
  prelude. *Superseded* by rows Q2.2 and Q2.8: exactly sixteen named impls, with a
  separate impl for each admitted Copy or Destroy payload combination, and `HashMap`
  excluded until it has a stated order. A container of Copy elements is handled by those impls without making Copy
  satisfy `Destroy`.
- **Resource cleanup:** `close(f)` and similar. It is fallible and effectful, and it
  returns what survives.

A container of resources is emptied by a **consuming drain** that hands back each
element and releases the emptied storage itself. No operation releases an "empty"
container without a check.

**Still open:**
- the exact container impl conditions;
- the order in which container elements are destroyed (fixed arrays are already
  decided: index order, OD35);
- the contracts of the drain when it fails and of its callbacks;
- whether `destroy` must terminate and must not trap. Returning Unit alone guarantees
  neither.

*Superseded:* rows Q2.2–Q2.6 and Q2.8 answer each of these (destruction order is Q2.3;
the drain's result fields are Q2.4 and Q2.5).

**Proof impact.** A2b design proof: allocator cleanup across deferred destroy calls.

## Q3. `HeapArray` element view and raw memory

*Governing rows: Q3.1–Q3.11. Where this section differs from them, it is superseded.*

**Decided.**
- Opening goes one layer deep.
- OD64: a pointer derived from a borrow is never cast to an integer and never escapes.
- A pointer derived from a shared borrow is never written through.

**Recommendation.**

**View.** Opening a `HeapArray<T, A>` in a borrow block yields a second-class view:
`ArrayView<T>` for shared access, `ArrayViewMut<T>` for exclusive access. The view
follows the reference rules: never stored, returned, captured or put in an aggregate,
and it ends with its block. Operations:
- `len`;
- for Copy `T`: `get(i) -> T` and, on a mutable view, `set(i, v)`;
- for any `T`, on a mutable view: `replace(i, v) -> T` and `swap(i, j)`.

A bounds violation is a defined trap. No operation yields an element reference.

**Raw memory.**
- CompCert-style blocks with offsets.
- Explicit address exposure, which OD64 forbids for pointers derived from a borrow.
- Provenance on every pointer.
- No aliasing guarantee for raw pointers.

Every unsafe primitive's contract fills in one fixed list of premises:
- initialized;
- in bounds;
- aligned;
- live;
- provenance-valid;
- exclusive;
- ownership transfer: `read_owned` moves out and marks the slot uninitialized, and a
  raw store over a live linear value violates the contract;
- frame;
- foreign mutation.

**Stated limitation.** An element that is itself a heap is reached by `replace`-ing it
out, which needs a substitute value; if building one needs an allocation that fails,
there is no route. This limits what can be expressed; it is not a safety hole. If a
workload needs traversal, add a consuming traversal or a scoped transformation of the
owned element, with conservation on every exit. *Superseded* by row Q3.6: `drain` and
`map_into` are part of the proposal, not deferred to a workload, and `swap_at` replaces an
element in place.

**Still open:**
- passing and lending views;
- root identity;
- the index type;
- alias conflicts between views;
- zero-sized elements;
- pointer equality and integer round trips;
- running out of address space;
- the typed and raw representations.

*Superseded:* rows Q3.2–Q3.11 answer each of these, and row Q3.12 adds the `kept` extern
marker.

**Proof impact.** A2b design proofs: heap opening and mutable lending, plus one
values-to-bytes lemma (A1 §1).

## Q4. Static text

*Governing rows: Q4.1–Q4.6. Where this section differs from them, it is superseded.*

**Decided.** `String<A>` carries an allocator; literals must not allocate.

**Options.**
- **(a) A separate static text type (recommended).**
- **(b) Literals as `String<Sys>` allocated at startup.** Rejected: it allocates and
  ties every literal to one allocator.
- **(c) One string-view type for literals and borrowed dynamic strings.** Rejected.
  If the type were returnable, `borrow s as v { return v; }` would return "static"
  text that dangles once `s` is destroyed. To be safe, the shared type must be
  second-class, and then a literal could not be stored in a struct or returned. The
  static/dynamic distinction keeps literals first-class and views second-class.

**Recommendation.**
- Add a built-in Copy type for immutable static text, preferably spelled `StaticStr`
  (the spelling is the owner's choice).
- It owns nothing, needs no allocator, is never destroyed, and is never a view into a
  dynamic string.
- A string literal has this type.
- Conversion is explicit and fallible:
  `String::from_static(a: &mut Allocator<A>, s: StaticStr) with(Alloc) -> Result<String<A>, AllocFailure<StaticStr>>`.
- Views of a dynamic string follow the second-class borrow rules.

**Still open:**
- the encoding, and whether embedded NULs are allowed;
- length units and comparison;
- whether a literal's address is observable, and whether identical literals may share
  storage;
- the FFI representation;
- the type and operations of dynamic string views.

*Superseded:* rows Q4.2–Q4.7 answer each of these (byte offsets, scalar-boundary
subviews, `byte_at` traps).

## Q5. Capability inference with several solutions

*Governing rows: Q5.1–Q5.4. Where this section differs from them, it is superseded.*

**Decided.**
- OD45 (owner, 2026-11-01): a capability variable is bound from the checked type of
  every argument, whatever the argument's form. Unresolved or inconsistent bindings
  are refused, and there is no inference from the result context.
- Capability sets are exact.
- OD46 binds generic type arguments the same way, from checked argument types only.
- OD87 (owner, 2026-11-01): capability arguments may be written explicitly in a
  turbofish, and inference fills the omitted ones. Option (d) below would also make
  them mandatory, revising OD87 as well.
- Open: only choosing among several consistent solutions ([decisions.md](../spec/decisions.md#core-capability-inference)).

**Options.**
- **(a) Syntactic:** infer a variable only where it stands alone as a whole set.
  Rejected: `{}` against `C + D` has the single solution `C = D = {}` but is refused,
  and `C` and `C + {}` would be treated differently.
- **(b) Unique solution, else explicit (the draft's original recommendation; superseded).**
- **(c) Pick the least solution.** Rejected: the compiler would choose among answers.
- **(d) Never infer.** Every call writes its capability arguments. This is the owner's
  stated leaning on 2026-11-04, and it **revises OD45 and OD87**, so it needs the
  owner's explicit approval as a change to decided rules.
  - For it: the call site shows the capability instantiation without the reader
    solving anything, and the checker needs no solver.
  - Against it: the argument function's capability set is already written once, in its
    own signature, and writing it again at every call states the same fact twice.
  - Capabilities would also no longer bind the way type arguments do (OD46).

> **Superseded** by rows Q5.1 and Q5.4. Those rows recommend option (d): explicit
> capability arguments on every capability-polymorphic call, checked against the
> argument types with no inference solver, and `{}` written for an empty set. This
> revises OD45 and OD87 and is pending the owner. A written capability argument is
> a checked restatement, not a second authority. Type arguments stay inferred
> (OD46). The text below is the draft's original recommendation, kept for its
> reasoning.

**Recommendation (superseded): (b).** Keep OD45. Infer exactly when the equations from all checked
argument types, together with any explicit arguments, have one solution. With no
solution the call is inconsistent; with several, explicit arguments are required. For
example, `C + {Fs} = {}` is inconsistent and `C + {Fs} = {Fs}` is ambiguous. This is
decidable because the set of capabilities is finite. The specification is the
brute-force definition, and any optimized solver is proved equivalent to it.

The authority a higher-order call passes on is the argument's own declared set, which
is visible where that function is declared. The call adds no authority of its own.

**Still open:**
- passing the caller's own capability variables;
- the normal form of capability sets, so that `C` and `C + {}` compare equal.

## Q6. Static refusal and traps

*Governing rows: Q6.1–Q6.7. Where this section differs from them, it is superseded.*

**Decided.**
- OD118: a proven violation is refused everywhere, trusted code included.
- [arithmetic.md](../spec/arithmetic.md) already refuses a known zero divisor and keeps
  its policy for violations in unreachable code.

**Options.**
- **(a) Refuse only operations whose operands are all constants** (the draft).
  Rejected: it accepts `fn bad(x: i64) -> i64 { return x / 0; }` and `a[5]` on
  `a: [i64; 3]`.
- **(b) Whatever the optimizer proves.** Rejected: the accepted language would change
  as the compiler improves.
- **(c) A fixed local check per kind of violation (recommended).**

**Recommendation.** Source acceptance runs a small, fixed, local check that looks only
at the facts each violation needs:
- a constant zero divisor, whatever the numerator;
- an invalid constant shift count;
- a constant index against a fixed-size array length;
- overflow of an operation whose operands are all constants.

The check uses the same checked-operation definitions as the runtime semantics. It
never depends on optimizer strength. Every other checked violation is a defined trap.
A failed unsafe premise is not a trap; it stays distinct stuckness
([VERIFIED_PIPELINE.md](VERIFIED_PIPELINE.md)). Core has only defined traps. Slice 1's
witness `fn entry(x: u8) -> u8 { return x + 1; }` is accepted and traps at 255.

*Superseded* by rows Q6.1–Q6.7. The known facts are a fixed table, not only all-constant
operands: literals, immutable `let`s, module constants, checked operations, six named
wrapping and saturating operations and lossless `as` (Q6.2). Every written body is
checked, unreachable branches, generic bodies and every module-constant initializer
included, used or not (Q6.4, which revises TYPE-22's two examples). The witness above is
unaffected: a parameter is not a known value.

**Still open:**
- local constant propagation;
- substitution of generics;
- unreachable code, keeping the existing policy;
- short-circuit operators;
- errors during constant evaluation.

*Superseded:* rows Q6.2–Q6.7 answer each of these.

## Q7. Target grant schemas and narrowing

*Governing rows: Q7.1–Q7.9. Where this section differs from them, it is superseded.*

**Decided.**
- Authority rules 11–12.
- Selective minting.
- Confinement.
- Refusal to mount.
- Separate DNS authority.
- Diagnostics allowed by the target for terminal output.

**Recommendation.**
- **One Lean table per target.** Each entry lists the grant type, how many the target
  offers (each minted at most once), its rights and scope, and its consuming close
  operation. Hosted entries:
  - `Allocator<Sys>`;
  - the named `Directory` roots given at launch;
  - `Environment`;
  - `Clock`;
  - the standard streams;
  - network grants from the launch configuration;
  - a grant to spawn processes. *Superseded* by rows Q7.4 and Q7.10: the hosted target
    (`linux-x86_64-hosted`) offers no process spawning, and an absent service is absent
    from the table. The standard streams use `Device` (row Q7.11, pending).
- **One policy order over rights and scope.** Every acquisition (opening a child
  directory), split, narrowing and process inheritance must stay within the source's
  provenance and below it in that order. A read-only directory therefore cannot open a
  writable child. Primitive contracts enforce the same order, so there is no second,
  informal authority mechanism.
- **Narrowing** consumes the grant and returns one with a subset of rights.
- **Splitting** is an explicit operation with its own contract, never implicit
  duplication.

**Still open:**
- the full schemas and how many of each grant a target offers;
- the close and error contracts;
- process inheritance;
- choosing the environment;
- clock precision;
- network scopes;
- whether policy is checked at run time or compile time.

*Superseded:* row Q7.10 gives one complete table for `linux-x86_64-hosted` (grant types,
launch syntax, rights, narrowing and splitting, ending contracts, the resolver grant,
startup exit 125 and trap exit 134); rows Q7.1–Q7.9 answer the rest.

## Q8. Slice 1's assurance scope

*Governing rows: Q8.1–Q8.7. Where this section differs from them, it is superseded.*

**Recommendation** (Codex agrees).
- Prove source → BIR.
- Qualify the native binary by differential testing against the Lean semantics.
- State separately, and label as conditional, the source → image theorem that assumes
  every LLVM and native edge (A1 §2–3). LLVM execution is never called proved.
- Discharge the named edges one at a time later. Adopting a Lean backend does not
  discharge the adapter, runtime, linking or image-binding obligations by itself.

**Counterexample kept in view.** A correct BIR with a faulty LLVM printer can pass
native tests at input 0 and miscompile at 255. Differential testing does not carry the
BIR theorem to the image.

**Still open:**
- the exact source subset;
- frontend correspondence;
- component versus complete executable;
- the launcher ABI and observations;
- exact artifact binding.

*Superseded:* rows Q8.1–Q8.7 answer each of these.

Acceptance, coverage and nonvacuity obligations are required (A1 §2), not refinement
of successful compilations alone.

## Q9. The whole Core interface

*Governing rows: Q9.1–Q9.19. Where this section differs from them, it is superseded.*

**Decided.**
- OD59: no `free`.
- OD140: `defer` takes a Unit call.
- A deferred call's arguments are read when it runs, not when it is registered.
- Cleanup skips abort and stops after a trapping deferred call.

**Recommendation.**
- **Form.** A-normal form. Operands are atoms: a literal, `copy x` for Copy values,
  or `move x` for linear values. Places (locals and projection paths) are separate from
  operands.
- **Types and kinds.** Types:
  - scalars;
  - fixed arrays;
  - nominal structs and enums;
  - `Heap<T, A>` and `HeapArray<T, A>`;
  - `Allocator<A>`;
  - existential packages;
  - reference and view types (second-class);
  - function pointers with an exact capability set;
  - raw pointers;
  - static text.

  Kinds: `Type`, `Brand` and `CapSet`.
- **Binders.** `let`, opening an existential, function parameters and match-arm
  binders. Each name is bound once; there is no shadowing in Core.
- **Data.** Construct; flat destructure; flat `match` on an enum (nested patterns and
  `if` are elaborated to it); store to a place.
- **Control.** `return`, `abort`, and checked primitive operations whose failure is a
  defined trap. *Superseded:* this draft's labelled `loop` is replaced by row Q9.3,
  which proposes OD47's `while` as the single Core loop, written
  `while L { C } do { B }`: the condition region `C` runs again on every iteration, and
  `break L` and `continue L` have resolved targets. In a counted loop a hidden counter
  advances before the body, the body sees the value before the advance, and `continue`
  advances it exactly once. That proposal is pending the owner.
- **Borrowing.** A scoped `borrow` of a place (shared or exclusive) and a scoped
  heap or array `open`, each binding a second-class reference or view for its block.
- **Allocation.** Allocator operations and `*h` are typed primitives with contracts,
  not syntax. *Superseded:* this draft also listed `destroy` as a primitive. Row Q9.6
  proposes primitives only at irreducible boundaries (`*h`, raw allocation and
  release, I/O). Under it, `destroy` resolves to the type's impl and runs that checked
  body. That proposal is pending the owner.
- **Calls.** Direct calls, function-pointer calls and primitive calls. Primitive calls
  come from one table covering externs, intrinsics and generated operations. Every call
  is checked against its capability set.
- **Cleanup.** `defer` survives in Core as **nested registration scopes**. Each
  `defer` opens a scope whose cleanup runs on every exit from the rest of that scope,
  so an early return runs only the cleanups registered so far:
  `defer a(); if stop { return; } defer b();` runs only `a` on that return.

  Deferred arguments keep their place identity and are read when the cleanup runs:
  `let mut n = 1; defer show(n); n = 2;` shows 2. The semantics is defined once,
  including:
  - result temporaries;
  - releasing a deferred call's loan after that call;
  - skipping cleanup on abort;
  - stopping after a trapping cleanup.
- **User destruction.** Ordinary checked impl bodies; there is no parallel destructor
  semantics.
- **Contracts and ghost state.** On function declarations and loops. Ghost bindings
  are marked and erased by a proved step. Contracts never affect execution.

**Still open:**
- the evaluator state;
- ownership instrumentation and its erasure;
- the inventory of primitive contracts;
- declaration and interface evidence;
- elaboration judgments independent of the checker.

*Superseded:* rows Q9.9–Q9.13 answer each of these, and Q9.19 orders approval,
formalization and freeze.

A count of constructors is a size goal, never evidence of completeness.

**Proof impact.** A2b design proof: deferred cleanup with allocators and loans.

## Decision order

1. **Pin slice 1:** Q8, Q6, and the parts of Q7 and Q9 that slice 1 needs (its input,
   launcher and observation boundary).
2. **Q1 and Q2 together:** dependency and destruction share one judgment.
3. **Q3, Q4, Q5 and the rest of Q7.**
4. **Q9 last,** combining the other answers.

Draft the semantic objects and theorem assumptions alongside each answer. Freezing
Core (row A2c) requires the four Lean design theorems of row A2b (row Q9.14), proved
against a `Spec.lean` headed PROPOSED:
- `reset_fresh`: reset generativity with allocator dependencies;
- `defer_cleanup_releases`: deferred cleanup with allocators and loans;
- `open_mut_freezes_root`: heap opening and mutable lending;
- `rep_roundtrip`: one values-to-bytes lemma.

A prose inventory alone cannot freeze Core.
