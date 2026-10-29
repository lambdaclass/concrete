# Verified-pipeline design examples

Status: design models, 2026-10-20. Owner: R-0511 (queue row 3 in [ROADMAP.md](../../ROADMAP.md)).
Specification exercised: [VERIFIED_PIPELINE.md](../../docs/compiler/VERIFIED_PIPELINE.md),
"Early semantic risk retirement" and the semantic contract it relies on.

The plan asks for "reviewed machine-checked design examples [for] a linear move and destroy, a
downward exclusive borrow across a call, shared borrowing, permitted reborrowing, structured
cleanup, and typed-value representation in byte/provenance memory" before Core's interfaces
freeze. These files are those examples. They are deliberately **not** Core: each is a few hundred
lines, imports nothing from the compiler, and models one decision small enough to review by eye.
Their job is to test the plan's intended design and to force the decisions it leaves open. Those
decisions are listed below, each with a recommendation.

Build only this library: `lake build DesignExamples` (about 5 seconds; it is also in
`defaultTargets`, so plain `lake build` and CI check it). Every theorem is fully proved. The
package sets `warningAsError`, so a `sorry` fails the build, and
[`Axioms.lean`](Axioms.lean) pins each theorem's `#print axioms` output with `#guard_msgs`.
Every footprint is a subset of `propext`, `Classical.choice` and `Quot.sound`. To add a theorem,
add its name to `Axioms.lean` with the output `#print axioms` gives. The library does not
participate in `#[proof_by]` evidence; `check_axiom_inventory.sh` covers only linked theorems,
so the pin in `Axioms.lean` is this library's axiom check.

## What each example exercises

| example | file | plan section | what is proved |
|---|---|---|---|
| 1. Move and destroy | [`Linear.lean`](Linear.lean), [`LinearSound.lean`](LinearSound.lean) | Semantic contract, "Resources and failure"; "No implicit destruction"; Core's "ownership instrumentation with a proved erasure theorem"; S8 use-once and conservation at reachable exits; S14 | For **every** program the model checker accepts and every input: no stuck state, each resource destroyed exactly once at fall-through and `return` exits, at most once on `abort`, and the erased machine's trace equals the instrumented trace. Examples cover branches, early return and abort. Each rejected program (leak on return, use after move, branch disagreement, rebinding a live owner) is shown to leak or double-free in the erased machine |
| 2. Borrows and reborrowing | [`Borrow.lean`](Borrow.lean) | "Language decisions" (second-class references); S8 borrow validity and "no escaping or stored reference"; Core's "places … allocation identities" and loan instrumentation; V2/V4 | A program with an exclusive borrow, an exclusive reborrow, the borrow passed down to a callee three times, and a shared reborrow of the exclusive reference is accepted and prints `[11, 22, 11]`. Shared borrows coexist with an owner read (a proposed relaxation, B7). Rejected: storing a reference into a longer-lived slot and releasing the owner while borrowed, both real dangling reads of freed storage, each accepted by a checker that drops only that rule; a callee storing its parameter; a reference slot shadowing a live reference; and, rejected and stuck in the instrumented semantics, using the parent during a reborrow, two exclusive borrows, reading a frozen owner, writing through a shared reference |
| 3. Structured cleanup | [`Cleanup.lean`](Cleanup.lean) | IR chain: Core's "scoped `defer`" and Place MIR's "cleanup/`defer` edges on every exit"; S10's exit "cleanup expansion is proved for one representative function with early exits and `defer`" | LIFO cleanup on fall-through, early `return` from a nested scope, `break` and `continue` (per-iteration cleanup in a loop body), no cleanup on `abort`, and a cleanup that traps during a return ends the run with earlier cleanup effects kept and later ones skipped. A static expansion (`lower`) into a defer-free, scope-free form agrees with the scoped meaning on every example and input |
| 4. Typed-value representation | [`Repr.lean`](Repr.lean) | IR chain: Core's "byte-and-provenance memory component … connected to typed values by a representation relation" and Memory CFG's "block–offset memory (bytes, pointer fragments, undef, permissions)"; S7's "representative typed-value/raw-memory abstraction lemma"; S11's single `Layout` authority | For **every** well-typed value: scalar round trip (`u8`, `bool`, `i32`, `i64`, `u64`, pointers), round trip for every struct of scalar fields, and load-after-store in block memory. `S { u8, i64, bool, i32 }` gets offsets 0/8/16/20 and size 24, as `Layout.lean` computes. The model's `alignUp` agrees with `Layout.alignUp` on all offsets below 256. The examples pin the open choices: padding is undef, and typed decoding ignores it (shown for one alternate padding image); pointer bytes are not an integer and integer bytes are not a pointer; a torn pointer is no pointer; `bool` byte 2 and fresh memory decode to nothing; misaligned and out-of-bounds accesses fail |

## Theorems

- `Linear`: `straight_accepted`, `branches_accepted`, `earlyReturn_accepted`, `straight_safe`,
  `branches_safe`, `earlyReturn_safe`, `earlyReturn_paths`, `earlyReturn_erasure`,
  `branches_erasure`, `leakOnReturn_rejected`, `leakOnReturn_leaks`, `useAfterMove_rejected`,
  `useAfterMove_double_free`, `disagree_rejected`, `disagree_double_free`, `shadowLive_rejected`,
  `shadowLive_leaks`.
- `LinearSound` (general): `checkFn_sound`, `exec_sound`, `erasure`, `checkFn_erasure`, and
  the lemmas they use (`inv_acquire`, `inv_move`, `inv_destroy`, `inv_setEq`, `chk_nodup`,
  `inv_noDoubleDestroy`, `inv_nil_noLeak`, list lemmas).
- `Borrow`: `reborrowEx_accepted`, `reborrowEx_runs`, `sharedEx_accepted`, `sharedEx_runs`,
  `escapeEx_rejected`, `escapeEx_dangles`, `escapeEx_needs_second_class`,
  `storeParamInCallee_rejected`, `storeParamInCallee_harmless`,
  `releaseWhileBorrowed_rejected`, `releaseWhileBorrowed_dangles`,
  `releaseWhileBorrowed_needs_liveness`, `slotShadowsRef_rejected`/`_stuck`,
  `parentDuringReborrow_rejected`/`_stuck`, `twoExclusive_rejected`/`_stuck`,
  `ownerReadWhileExcl_rejected`/`_stuck`, `writeThroughShared_rejected`/`_stuck`,
  `writeParentDuringSharedReborrow_rejected`/`_stuck`.
- `Cleanup`: `normalEx_order`, `earlyReturnEx_paths`, `loopEx_paths`, `abortEx_no_cleanup`,
  `deferPrint_exit_time`, `trapCleanupEx_fatal`, and the six `*_lowered` agreement theorems.
- `Repr` (general): `le_length`, `unle_le`, `encode_length`, `decode_encode`,
  `decodeInt_le_of_lt`, `le_alignUp`, `le_fieldsEnd`, `encFields_length`,
  `decFields_encFields`, `decodeStruct_encodeStruct`, `load_store`; (examples)
  `alignUp_agrees_with_Layout`, `S_layout`, `S_bytes`, `S_padding_unobservable`,
  `S_padding_undef`, `ptr_bytes_not_int`, `int_bytes_not_ptr`, `torn_pointer`,
  `bool_invalid_byte`, `fresh_is_undef`, `access_rules`.

## Why the theorems are not vacuous

- Properties are stated over **semantics** (trace counts, printed values, decoded values), not
  over the checker. The examples check that the accepted programs take the interesting paths:
  `earlyReturn_paths` shows the return path really returns and the abort path really aborts
  holding both resources, and `reborrowEx_runs` gives the exact output.
- Each checker rule has a rejected program, and the semantics shows what the rule prevents: a
  leak or a double free in `Linear`'s erased machine, a read of freed storage in `Borrow`. For
  `Linear` the general theorem covers every rule at once. For `Borrow`'s two memory-safety rules
  (second-class storage, owner liveness) the isolation is proved: a checker variant that drops
  exactly that rule accepts the program (`escapeEx_needs_second_class`,
  `releaseWhileBorrowed_needs_liveness`). `storeParamInCallee` is a blanket-rule rejection: the
  model has no slot that outlives a call, so even the stored copy is harmless.
- `Borrow`'s aliasing rejections (`*_stuck`) are stuck only because the instrumented semantics
  says so; they demonstrate agreement between checker and instrumentation, not a memory error.
  Question B4 is about exactly this.

## Design questions found, with recommendations

### Ownership (example 1)

**L1. Are destruction events observable?** The model keeps acquire/destroy as ghost events and
states conservation over them. *Recommendation:* conservation is a property of Core's instrumented
semantics, stated over ghost acquire/destroy events. The observable trace contains only the
external calls a destructor makes. A conservation claim transports to a binary only through a
lemma that says so, as the plan already requires for ghost trap kinds.

**L2. What exactly is promised on a fatal exit?** The plan says fatal traps "promise no cleanup or
reclamation". The model proves more on `abort`: no resource is destroyed twice. The model's
"exactly once at normal exits" holds only because it is closed: nothing is returned or handed
outward. In the language an owned value may instead move into a return value, an aggregate, a
loop result or a callee, and a linear token may have no `Destroy` at all (OWNERSHIP_MODEL).
*Recommendation:* destruction is at most once along every execution, fatal ones included; at
normal and structured exits each ownership obligation is discharged by an explicit consuming
operation or transferred exactly once. Both hold in safe code under satisfied trusted and FFI
contracts. The same invariant gives both halves, and the first excludes double free before an
abort.

**L3. Which semantics is normative?** Use after move gets stuck in the instrumented semantics and
double-frees in the erased one. `erasure` holds for every non-stuck instrumented run, with no
checker premise. *Recommendation:* Core's meaning for checked programs is the instrumented
semantics. S14 proves checked programs never get stuck, and the erasure theorem hands lowering
the uninstrumented semantics to refine. Ownership violations never become defined behaviour.

**L4. Next increment.** The model has no loops, no `defer`, no parameters and no outward
transfer. OWNERSHIP_MODEL's loop rule (E0207 and the exit-branch exemption), "pending defers
count as consumption", "params are owned locals" and returning or storing an owned value are
where the next design example belongs (V3). *Recommendation:*
extend `LinearSound` to them before V3's checker freezes. The invariant style here scales; the
loop rule needs an invariant that holds at the loop head.

### Borrows (example 2)

**B1. Does passing a borrow-block `&mut` to a call consume it?** Before 2026-10-23 it did, while
parameter references were implicitly reborrowed; since then every call argument is lent in one mode
(REF-10's rationale, the former MUT_REF_SEMANTICS §3 and §4). The model
treats every call argument as an implicit reborrow scoped to the call. Because references are
second-class, the callee cannot retain it, and `reborrowEx` passes the same block reference to
`inc` twice with no unsafety. *Recommendation:* decide this under exit 2i (the plan says the
verified pipeline never models looser rules than the compiler enforces). Adopt implicit reborrow
for every `&mut` argument. It removes the two-kind distinction from Core; `reborrowEx`
demonstrates it on one program. That is an example, not a proof: single-argument calls, no
callbacks, and a callee that cannot see the caller by construction. Two obligations remain for
the real rule: alias conflicts between several arguments of one call (E0293 already rejects
overlapping borrows within a call), and loan propagation when a reference is transferred or
reborrowed.

**B2. Is `&mut T` linear or affine?** OWNERSHIP_MODEL says `&mut T` is linear "anyway". Its
control-flow section and REF-10's rationale (the former MUT_REF_SEMANTICS §7) say an unconsumed borrow may lapse. In the model a
reference is non-duplicable but carries no consumption obligation. *Recommendation:* Core has
three usage modes: Copy (duplicable, droppable), owned linear (neither), and exclusive
reference (droppable but not duplicable). Conservation applies to owned values only. This is a
coordinated change to OWNERSHIP_MODEL, MEMORY_SEMANTICS, REF-10's rationale (the former MUT_REF_SEMANTICS) and VALUE_MODEL, not a
wording fix in one document.

**B3. Is a borrow an alias of the owner's place or a copy?** Today's codegen copies the owner into
a temporary and writes it back at block exit (REF-10's rationale (the former MUT_REF_SEMANTICS §8)). The model, like the plan's
"stable storage identities", aliases the owner's storage. The two agree only while the frozen-owner
rule holds. Once a raw pointer derived from the borrow exposes its address, they differ
observably: the address is the temporary's, not the owner's. *Recommendation:* Core's semantics
says a borrow denotes the owner's place. Preserving that is then a Place MIR obligation:
copy-in/copy-out is allowed only where proved equivalent, and never where the address can be
exposed.

**B4. May lower passes exploit exclusivity?** Loans are instrumentation. Aliasing violations are
stuck only because the instrumented semantics says so. *Recommendation:* until an aliasing model
covering raw pointers exists (V6), no pass or adapter may assume `&mut` exclusivity, for example
by emitting LLVM `noalias`. Loans are ghost state for checker soundness only. Admitting such an
optimization later requires that model and a proof against it.

**B5. Reborrow needs a Core form.** The surface language has no reborrow syntax (`&mut *r` does
not parse; REF-10's rationale, the former MUT_REF_SEMANTICS §10), but the plan requires "permitted reborrowing". The model's
`reborrow` is a scoped form: the parent is frozen under an exclusive reborrow and read-only
under a shared one. *Recommendation:* Core has an explicit scoped reborrow. Elaboration produces
it for implicit call-argument reborrows (B1). Surface syntax waits for a workload.

**B6. Escape through assignment, not just aggregates. Found while writing the model; filed as
[bug 104](../../docs/bugs/104_assigned_reference_outlives_block_local.md).** Second-class must
also forbid storing a reference into a *longer-lived local*. The model's `stashRef` is that case,
and it dangles. Bug 092's fix (E0239, now on `main`) covers aggregates and leaves "bare reference
bindings … unchanged".
Today's checker catches only a bare borrow-block identifier (E0234, `Check.lean` `.assign`). The
following compiles with the main checkout's binary (built 2026-10-18; `Concrete/Check` and
`Concrete/Elab` are unchanged on `origin/main` since), and `f` returns 0 instead of 77: it reads
freed heap.

```
struct Copy B { v: Int }
fn f(init: &Heap<B>) with(Alloc) -> Int {
    let mut leaked: &Heap<B> = init;
    if true {
        let h: Heap<B> = alloc(B { v: 77 });
        leaked = &h;          // assignment records no borrow of h
        free(h);              // accepted
    }
    let h2: Heap<B> = alloc(B { v: 99 });
    let out: Int = leaked.v;  // use after free
    free(h2);
    return out;
}
fn main() with(Alloc) -> Int {
    let h0: Heap<B> = alloc(B { v: 1 });
    let r: Int = f(&h0);
    free(h0);
    return r;
}
```

The control (read before `free(h)`) returns 77. The checker source agrees: `borrowedFrom` is
recorded only for a direct `let x = &y`, and the `.assign` case neither records it for
`leaked = &h` nor propagates it from aliases. The reproducer and its control are not checked in;
the native result was observed once with that binary. *Recommendation:* triage into row 0 as a
memory-safety defect, with assignment and alias-transfer rejection fixtures. For Core and exit
2i, single-assignment reference locals are not enough on their own while `let s = r` can create
an alias without carrying `r`'s loan. Either create and transfer references only through the
scoped forms the model has (borrow, reborrow, call), so escape is a lexical property, or
propagate the root loan through every alias and transfer, enforce owner liveness against it
(S8), and check that the source's scope encloses the reference's.

**B7. Shared borrows: a proposed relaxation.** The model lets shared borrows of one owner
overlap and lets the owner be read during them (`sharedEx`). Today's rule freezes the owner
against reads and further borrows during any borrow block (MEMORY_SEMANTICS.md, borrow blocks).
*Recommendation:* adopt the relaxation as a language decision under exit 2i/S4, with migration
fixtures: shared access cannot invalidate anything a shared reference reads, and it is what
"shared borrowing" in the plan means. Until then the model is looser than the compiler here and
must not be cited as describing current rules.

### Cleanup (example 3)

**C1. `defer` is block-scoped and statically registered.** A `defer` registers in its enclosing
block, so the pending cleanups at every program point follow from the syntax. `lower` exploits
this: it needs no runtime defer stack. A `defer` in a loop body runs every iteration, and one in
an `if` branch runs when that branch ends, unlike Go's function-scoped `defer`.
*Recommendation:* fix this in Core and have Place MIR's cleanup edges be exactly `lower`'s static
expansion. The S10 proof then generalizes the five `*_lowered` theorems.

**C2. When are a deferred call's arguments evaluated?** `Lower.lean` re-lowers the deferred
expression at each exit, so arguments are read at exit: `deferPrint` prints 2, not 0. This must
be exit time for `let mut v = …; defer v.drop(); v.push(1);` to drop the final vector.
*Recommendation:* Core evaluates a deferred call at scope exit against the current values of its
argument places, and restricts deferred-call arguments to places and constants. Expanding the
call onto several exit edges then cannot duplicate an effect or a trap. This is in addition to,
not instead of, the registration-time reservation OWN-12 specifies: a linear argument is
reserved when the `defer` is registered and cannot be moved afterwards. `deferPrint` shows only
the read timing. Write both into OWN-12.

**C3. Failure during cleanup** (open in OWN-12's rationale). *Recommendation:* a trap inside a deferred call
is fatal like any other. Effects of the cleanups that already ran remain, and the remaining
cleanups, outer scopes' included, do not run. `trapCleanupEx_fatal` shows this on a return
path, and `trapCleanupEx_lowered` shows the static expansion agrees. This is the plan's existing
fatal-trap rule, so it needs a sentence, not a mechanism.

### Representation (example 4)

**R1. Data layout is a profile parameter.** The model fixes little-endian, 8-byte pointers and
`Layout.lean`'s sizes. *Recommendation:* the representation relation takes a data-layout record
(endianness, pointer size, scalar sizes and alignments) emitted by the single `Layout` authority
and fixed per target profile. Safe code observes it only through `sizeof::<T>()` and
`alignof::<T>()` (VALUE_MODEL.md), which Core defines from the same record; raw-memory operations
and FFI observe the rest.

**R2. Padding is undef, and the relation is many-to-one.** `S_padding_unobservable`: an image with
different padding decodes to the same value, because typed decoding skips padding (shown for one
alternate image; the quantified lemma is a follow-up). Raw byte loads still see the padding.
*Recommendation:* padding is undef in the semantics. Byte copies
preserve undef. A typed read of undef is stuck. FFI contracts state that padding is unspecified.
Refinement proofs relate a value to the set of its byte images, not to one image.

**R3. Pointer ↔ integer only through explicit operations.** Pointer fragments do not decode as
integers, and integer bytes do not decode as pointers. Type punning through memory therefore
fails even in `Unsafe` code. *Recommendation:* this matches the plan's quasi-concrete addresses
("address exposure … is not an erasable cast"). Make `expose`/`from_exposed` the only
conversions, and make an integer load of pointer fragments stuck rather than an implicit
exposure.

**R4. Validity invariants.** `bool` byte 2 is not a `bool`. *Recommendation:* each scalar type has
a validity invariant: `bool` ∈ {0, 1}, `char` a Unicode scalar value, an enum tag one of its
declared discriminants. Loading invalid bytes in `Unsafe` code is stuck. FFI contracts state
what foreign code must produce.

**R5. Uninitialized memory.** Safe code cannot create an uninitialized local (`let x: T;` is a
parse error), but fresh raw allocations are undef (`fresh_is_undef`), and trusted collections
store into uninitialized slots (OWNERSHIP_MODEL). *Recommendation:* fresh allocations are undef.
A store into an uninitialized slot is a distinct raw operation with no "overwrite a live value"
obligation.

**R6. Alignment and packed layouts.** Typed loads and stores require alignment; misaligned access
is stuck. Concrete has `#[repr(packed)]` (VALUE_MODEL.md, `Layout.lean`'s `isPacked`), whose
fields can be misaligned. *Recommendation:* packed-struct field access is a distinct unaligned
memory operation, or packed layouts stay outside the verified profile until specified. The plan
does not mention packed layouts (gap G4 below).

**R7. Enums are not modelled.** Tag width (`Layout.lean` uses a 4-byte tag), payload offset and
any niche optimization are the next representation example (V2 adds match).

**R8. Outside this example.** The round trips do not cover permissions, allocation freshness and
lifetime across operations, memory injections, address exposure, or foreign mutation. These are
the remaining decisions the plan fixes at this boundary (S7 with R-0502, S11).

## Gaps in the plan, with proposed wording

None of these is a factual error, so VERIFIED_PIPELINE.md is unchanged. Each is proposed wording
for the plan's owner.

- **G1 (B2).** In "Language decisions", after the second-class-references bullet, add: "References
  are not subject to linear conservation: an exclusive reference may not be duplicated but may
  lapse unused; conservation applies to owned values." This needs the coordinated language-doc
  change B2 describes.
- **G2 (L2).** In "Resources and failure", after "promise no cleanup or reclamation", add:
  "Destruction is at most once along every execution, fatal ones included; at normal and
  structured exits each ownership obligation is discharged by an explicit consuming operation or
  transferred exactly once (in safe code, under satisfied trusted and FFI contracts)."
- **G3 (B4).** In "Language decisions", add: "Loans are ghost instrumentation. No pass or adapter
  may assume `&mut` exclusivity (for example LLVM `noalias`) until an aliasing model covering raw
  pointers is specified and the exploiting pass is proved against it."
- **G4 (R6).** In S11, after "`Layout` as the single proved authority", add: "packed layouts are
  either outside the verified profile or accessed only through distinct unaligned memory
  operations".
- **G5 (B3).** In the IR chain's Core row, add "a borrow denotes the owner's place"; in the Place
  MIR row, add: "a borrow denotes the owner's place; the
  existing copy-in/copy-out borrow lowering is retired or proved equivalent, and is never used
  where the borrow's address can be exposed".
- **G6 (C2).** In the IR chain's Core row, replace "scoped `defer`" with "scoped `defer`, whose
  linear arguments are reserved at registration and whose call runs at scope exit against the
  current values of its argument places".
- **G7 (B5).** In the IR chain's Core row, replace "borrows" with "borrows and scoped reborrows
  (elaboration makes implicit call-argument reborrows explicit)".
- **G8 (B6).** In ROADMAP exit 2i, replace "strictly second-class references (none stored in an
  aggregate)" with "strictly second-class references (none stored in an aggregate, and none
  assigned or aliased into a binding that outlives the reference's source)". In S4's list,
  replace "and second-class references" with "and second-class references (never returned,
  stored in an aggregate, or assigned or aliased into a binding that outlives their source;
  enforced by S8's owner-liveness rule)".
