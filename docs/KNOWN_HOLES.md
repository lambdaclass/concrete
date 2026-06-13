# Known Holes and Tracked Soundness Gaps

Status: canonical index — the single place that lists every known soundness or
"semantically dark construct" gap, its honest current state, the gate that
keeps it from drifting, and where its fix is scheduled.

Why this file exists: holes were previously scattered across `CLAIMS_TODAY.md`
disclosures, `AXIOMS.md`, individual gate scripts, and ROADMAP items, so no
single page showed the whole picture. This is that page. Every entry must name
(1) what the gap is, (2) whether it is OPEN/CLOSED, (3) the reproducing
fixture, (4) the gate that locks it, and (5) the roadmap item that fixes it.

Governing rule (from ROADMAP): no construct may be **semantically dark** — a
construct that looks meaningful but is silently ignored, or whose unsoundness
is undisclosed, is a bug. A hole is acceptable only while it is *tracked,
gated, and disclosed*; it is never acceptable while *silent*.

---

## OPEN holes (tracked, gated, disclosed — not yet fixed)

### H1. Returned-reference provenance — CLOSED 2026-06-13

**RESOLVED by the language invariant "references are second-class — never
returned"** (`docs/VALUE_MODEL.md`): the checker rejects any safe-callable
function or function *type* that returns a reference — directly, nested in an
aggregate, or via generic instantiation. The accessor surface was migrated to
the value model: `get -> Option<V>` (Copy cell, `V: Copy`); `with_value` /
`with_at` to borrow (Borrow cell, scoped — the `&V` never escapes the callback);
`remove`/`pop` to move out (Move cell); raw pointers (`*const`/`*mut`) for
low-level/unsafe access. No lifetimes, regions, or `from()`. Locked by
`scripts/tests/check_returned_ref_provenance.sh` (now asserts ref-returns are
rejected) + the blanket signature rule in `Concrete/Check.lean` (`checkFn`) +
the fn-type / generic-instantiation rules in `resolveType` / call sites. The
`from(param)` escape valve remains deferred and evidence-gated (#8a1).
Deferred follow-on: `with_value_mut`/`modify` (separate container-not-in-context
obligation).

ORIGINAL HOLE (for history): stdlib `get`/`get_mut`-style APIs returned
references inside aggregates (`Option<&T>`, `Option<&mut V>`). The owner was
**not** frozen while the returned reference lived, so a saved reference could
survive a mutation that reallocated/removed/reused storage — a use-after-realloc
that compiled in safe code. Affected (all now migrated): `HashMap::get`/`get_mut`,
`OrderedMap::get`/`get_mut`, `OrderedMap::min_key`/`max_key`, `OrderedSet::min`/
`max`, `Vec::get`, `Slice::get`/`MutSlice::get`, `Deque::get`, `BinaryHeap::peek`.

- **State:** OPEN. Blast radius FROZEN (no new aggregate-ref public API may
  land).
- **Reproduce:** `examples/known_holes/returned_ref_provenance_{map,vec}/`
  (build today = hole present).
- **Gate:** `scripts/tests/check_returned_ref_provenance.sh` — reproduces the
  hole, freezes the existing public aggregate-ref baseline
  (`scripts/tests/returned_ref_aggregate_baseline.txt`), rejects new ones, and
  keeps a positive case proving bare scalar `-> &T` is not the banned shape.
- **Disclosed:** `CLAIMS_TODAY.md` (§1, "No dangling safe reference" narrowed
  to borrow-block refs only).
- **State (updated 2026-06-13): RESOLUTION is now a language invariant —
  "references are second-class, never returned".** A safe-callable function or
  function *type* may not return a reference (directly, nested in an aggregate,
  or via generic instantiation). This subsumes the no-aggregate-ref ban and
  retires the returned-reference-provenance question entirely — no lifetimes,
  regions, or `from()`. See `docs/VALUE_MODEL.md`. Status:
  - MUTABLE half: FIXED earlier (`get_mut` withdrawn → `update`).
  - Enforcement LANDED (2026-06-13): the checker rejects (a) function-pointer
    types returning a reference and (b) generic type params instantiated to a
    reference in return position — closing the `with_value` `R=&V` backdoor and
    the `wrap<R>->Option<R>` backdoor. Immutable `HashMap::with_value` landed
    (sound replacement for reading a non-Copy value). Gate
    `scripts/tests/check_callable_values.sh`.
  - STAGED (V1.1 window): blanket rejection of bare `-> &T` *definition
    signatures* (switches on only AFTER the existing accessors are migrated, else
    the stdlib build breaks); migrate `get -> Option<&V>` / `peek` / `min` /
    `max` / `get_unchecked` / `get_mut` to value / `with_value` / raw-pointer
    (`*const T`/`*mut T`); `with_value_mut`/`modify` (separate
    container-not-in-context obligation). Until migrated, the grandfathered
    accessors remain CONTAINED (they instantiate `V` to a non-ref; the `&` is
    declared, so checks (a)/(b) do not fire on them).
  - Related codegen bug (defense-in-depth, now unreachable from safe code):
    returning a reference computed from a ref *identifier*/`&place` miscompiles
    (spurious load → segfault). Tracked; low priority since the invariant makes
    the shape ill-formed in safe code.
- **Fix (decided 2026-06-11): by SUBTRACTION, staged by danger, Clone-free.**
  ROADMAP Phase 6 #8a. **Mutable half (DONE):** withdraw
  `get_mut` (the actual use-after-realloc write vector) and replace with
  operation APIs `contains` / `insert` / `remove -> Option<V>` / `replace` /
  `update(k, fn(V) -> V)` (moves, works for non-Copy via today's fn-pointers)
  plus value-`get -> Option<V>` for Copy. This eliminates the memory-corrupting
  half as a pure stdlib refactor. **Contained until V1.1:** the immutable
  `get -> Option<&V>` (also unsound in principle but its real uses are scoped
  non-escaping reads, and there is no Clone-free/callback-free replacement for
  reading a non-Copy value out) — kept disclosed and frozen, withdrawn when
  V1.1 scoped callbacks (`with_value`, callable-values doc #24) land.
  `ByteView` (#5a) for stored zero-copy. **`Clone` is NOT part of the H1 fix**
  — it is a separate value-model design item (#8a2, VALUE_MODEL.md), not
  rushed to make map reads convenient. Scalar `from(param)` is DEFERRED (#8a1).
  Validation (2026-06-11): lox uses no map accessors; kvstore is 100% tier-1
  (zero migration); integrity's single read site migrates to `with_value` at
  V1.1. The flat no-aggregate ban (refs never inside Option/Result/struct/
  array/container/callback-context) is permanent; unfreezing it is a
  thesis-level decision.

### C9. Address-taken loop variable → lost condition / infinite loop — OPEN

A loop variable that is **both** loop-carried and address-taken (e.g. `&i`
inside a `while i < n { … ; i = i + 1 }`) miscompiles: the variable is promoted
to an alloca (because of `&i`, per C8) **and** kept as an SSA phi, the two
diverge, the init `store 0` lands inside the loop body (resetting the counter
every iteration), and the loop condition disappears entirely — producing a
silent infinite loop (wrong answer / hang, no diagnostic). Root cause is the
interaction between alloca-promotion (C8) and the loop SSA/phi construction: a
promoted variable that is loop-carried must be driven entirely through memory
(load/condition/increment/store via the alloca), not also through a phi.
- **Repro:** `tests/known_bugs/loop_var_borrow.con` (currently hangs; lives
  outside `tests/programs/` so no glob runs it — see roadmap).
- **Scope:** does NOT affect the stdlib or the callable-values work — stdlib
  loops use a plain counter and never borrow it; container callbacks borrow
  *elements* via pointer walks. Surfaced while implementing #24 step 1.
- **Disposition:** fail-closed candidate — until the loop-SSA/promotion
  interaction is fixed, the checker should *reject* borrowing a loop-carried
  mutable variable rather than miscompile. Tracked at ROADMAP Phase 5 #6c.

### C10. Indexing an array behind a reference yields `&<unknown>` — OPEN

Borrowing an element of an array reached through a `&[T; N]` (`&arr[i]` where
`arr: &[T; N]`) fails type resolution with `&<unknown>` (E0220 at the use site),
because the array-index element type is not resolved when the array operand is
itself a reference. This is fail-closed (it rejects, never miscompiles) but
blocks the ergonomic `&arr[i]` element-borrow loop form.
- **Repro:** `tests/known_bugs/index_through_ref.con` (currently rejected with
  E0220).
- **Scope:** does NOT block the callable-values work — combinators iterate
  container internals via pointer walks. Surfaced while implementing #24 step 1.
- **Disposition:** resolve array-index element type through `&`/`&mut`/`*` in the
  type checker (sibling of the #6b `peekExprType` fix). Tracked at ROADMAP
  Phase 5 #6c.

---

## CLOSED this session (kept here so the fix can't silently regress)

### C8. Address-of-local did not alias the local — CLOSED 2026-06-11 (was H5)

`&mut x as *mut i64` (and `&mut x` / `&x`) materialized a pointer to a **copy**
of the local, because local scalars were lowered as SSA register values, not
addressable stack slots — a store through the pointer did not reach `x`. This
was the architectural root that the nested-place fix (C5) worked around and the
last manifestation of the addressability problem. Fixed: `addrOfLocal`
(`Concrete/Lower.lean`) promotes a local to a stable stack alloca on first
address-take, so the pointer aliases the variable; `lookupVar`/`setVar` route
all reads/writes through the alloca, including writes before and after the
address-take.
- **Locked by:** `scripts/tests/check_raw_ptr_to_local.sh` (6 oracles: raw
  `*mut` store, `&mut` via fn, repeated mutate, writes around the address-of,
  deref consistency). Full suite 1548/0; codegen/nested-write/struct-layout
  gates unaffected.

### C7. Proven safety violations not enforced — CLOSED 2026-06-11

A runtime-safety obligation the compiler discharges to `violation` is a
compile-time **proof** the access is wrong. Previously `violation` was only
reported, so safe code with `a[5]` on `[i64; 3]` or `10 / 0` still built and
shipped UB. Fixed: safe code with a proven runtime-safety violation now fails
the build with E0900; `trusted` / `with(Unsafe)` code remains an explicit
audit-responsibility escape hatch; `unproven` obligations remain reportable
and are NOT swept into the hard-error path.
- **Locked by:** `scripts/tests/check_proven_violation_enforcement.sh` —
  rejects constant OOB and literal div-zero, confirms `trusted` and
  `with(Unsafe)` exemptions, and confirms a variable-index `unproven`
  obligation still builds.
- **Fixtures:** `examples/known_holes/proven_{oob_index,div_zero}/` are now
  expected-error regression fixtures.

### C6. Struct mixed-width field-layout miscompile — CLOSED 2026-06-10

The struct-literal store packed fields **tightly** (summing `computeTySize`)
while field reads used `Layout.fieldOffset` (**aligned**). Any struct with a
sub-word field followed by a wider one read garbage — `{a: u8, b: i64}` stored
`b` at offset 1 but read it from offset 8. A silent miscompile in one of the
most common constructs; only all-same-width or single-field structs were
unaffected, which is why it survived. Fixed: `.structLit` lowering now stores
each field at the same aligned `Layout.fieldOffset` that reads use.
- **Locked by:** `scripts/tests/check_struct_field_layout.sh` (6 execution
  oracles: u8+i64, three mixed, middle/trailing field, nested mixed-width,
  all-i64 no-regression). Full suite 1548/0.

### C5. Nested place-write miscompile — CLOSED 2026-06-10

`o.inner.v = x`, `a[i].x = x`, `m[i][j] = x`, `b.data[i] = x`, triple-nesting,
and nested writes through a `&mut` parameter were all silently dropped: Lower
handled only single-level assignment targets, and a compound base was lowered
as a value copy whose mutation was discarded. Deeper root: locals are SSA
register values, not addressable slots, so single-level workarounds (struct
copy-writeback; arrays happen to be alloca-backed) did not compose. Fixed by a
unified `storeToPlace` (`Concrete/Lower.lean`) that writes compound places in
place by value-writeback, terminating at a root variable or a reference/deref
base. `.fieldAssign` and `.arrayIndexAssign` now delegate to it.
- **Locked by:** `scripts/tests/check_nested_field_write.sh` (9 execution
  oracles: nested field, array-elem-field, struct-array, triple-nest, 2D
  array, nested-via-&mut, plus single-level no-regression). Full suite 1548/0.
- **Related:** the addressability root this worked around is now fixed
  outright — see C8 (address-of-local), which promotes address-taken locals
  to stack allocas.

### C4. Monomorphization name collision — CLOSED 2026-06-10 (was the most severe)

Mono mangled a specialization by the **head constructor** of the type argument
and discarded nested args, so `tag<Hold<Pair<i64>>>` and `tag<Hold<Pair<bool>>>`
collapsed into one `tag_for_Pair` / one `%Hold_Pair` despite different layouts
(inner 16 bytes vs 2 bytes) — a silent miscompile (ABI corruption on field
access). Arrays/refs/pointers/fn-types fell through to `"unknown"`, collapsing
even more. Fixed: `tyToSuffix` (`Concrete/Mono.lean`) is now total and keys on
the FULL type with bracketed nested args (`Hold_T_Pair_T_Int_E_E`), so distinct
instantiations get distinct symbols and struct types. Both the function-name
(`monoNameFor`) and struct-name manglers route through it, staying consistent.
- **Locked by:** `scripts/tests/check_mono_name_collision.sh` — now a
  regression gate: two same-head/different-arg instantiations emit two distinct
  functions, array type-args specialize separately, and an execution oracle
  (field-touching body over both layouts) returns the correct value.
- **Adjacent, still open:** the `mod`-wrapped form of the fixture trips E0602
  in nested-generic struct lowering — a separate, fail-closed bug (rejects, no
  miscompile). Needs its own fixture; surfaced by the codegen sweep.

### C1. Function-pointer capability escalation — CLOSED 2026-06-09

A function with no `with(...)` could accept and call `f: fn(i32) with(Network)
-> i32` — authority smuggling through a callback. Now calling through a
function pointer requires the fn type's capability set, enforced in both Check
(E0240) and CoreCheck (E0520).
- **Locked by:** `tests/programs/adversarial_neg_cap_fnptr_smuggle.con`
  (rejected) + `cap_fnptr_declared.con` (positive), and
  `scripts/tests/check_capability_polymorphism_design.sh`, which also freezes
  the stdlib HOF surface until the callable-values design doc exists.

### C2. Explicit enum discriminants silently discarded — CLOSED 2026-06-10

`enum Op { Get = 0x01, Set = 0x02 }` parsed the values and **threw them away**,
assigning positional tags 0/1 — a semantically dark construct that would
corrupt any FFI/protocol/serialization enum (and made duplicate discriminants
`A = 1, B = 1` "compile" because both were discarded). Now rejected at parse
time (E0001) with a hint pointing at the planned feature.
- **Locked by:** `tests/programs/error_enum_explicit_discriminant.con`.
- **Feature:** ROADMAP Phase 12 #7a — honor the value at the repr/ABI
  boundary and reject duplicate discriminants.

### C3. Unknown attributes silently ignored — CLOSED 2026-06-10

`#[notreal]`, `#[trustedz(foo)]`, and any other unrecognized attribute were
parsed and silently dropped — so a typo in a proof/capability/test attribute
(`#[overflow_checkd]`, `#[tes]`, `#[proof_b]`) silently lost its meaning, and
several such losses fail *open* (a typo'd `#[test]` silently doesn't run; a
typo'd `#[overflow_checked]` silently drops overflow obligations). Now
`parseAttribute` validates the key against a complete allowlist (repr, test,
overflow_checked, spec, proof_by, ensures_proof, proof_coverage,
proof_fingerprint, requires, ensures, invariant, variant, intrinsic, langitem)
and rejects unknowns (E0001) with the known list as a hint.
- **Locked by:** `tests/programs/error_unknown_attribute.con`.
- **Maintenance:** a new attribute must be added to the `knownAttrs` list in
  `Concrete/Parser.lean` as well as wired into its consumer, or it will be
  rejected.

---

## Trust ledger (not a hole, but a tracked trust boundary)

### T1. Native-code trust under `bv_decide`

The six HMAC/SHA-256 flagship theorems depend on `Lean.ofReduceBool` /
`Lean.trustCompiler` because `bv_decide`'s LRAT certificate checker runs as
compiled Lean — so `proved_by_kernel_decision (bv_decide)` is kernel-checked
reflection over a *natively executed* certificate check, a larger TCB than
`omega`. This is honest, declared, and gated.
- **Gate:** `scripts/tests/check_axiom_inventory.sh` (`#print axioms` over
  every `#[proof_by]` theorem; `sorryAx` fails hard; native trust must be
  declared in `scripts/tests/axiom_native_trust.txt`).
- **Documented:** `docs/AXIOMS.md` (kernel allowlist, native tier, and the
  unproven links no axiom check can see: extraction preservation, PExpr eval,
  BitVec↔LLVM, unbounded-Int model).

---

## Open design decisions that gate the Phase 5/6 freeze

These are not holes (no current unsoundness) but are unmade decisions that must
land before the relevant freeze. Full text in ROADMAP; listed here so the
whole picture is in one place.

- **Callable values + capability-polymorphic callbacks** — ROADMAP Phase 5
  #24/#24a. DESIGN DONE (2026-06-12): `docs/CALLABLE_VALUES_AND_CAPABILITIES.md`
  now exists and records the decided model; the HOF freeze is lifted (doc
  governs). What remains is implementation — and in particular the scoped
  collection callbacks (`with_value`/`with_value_mut`/`modify`) that the H1
  immutable-half withdrawal below depends on. `from(param)` stays deferred
  (#8a1) and is explicitly NOT the H1 fix.
- **Owned `ByteView` zero-copy stored idiom** — Phase 5 #5a.
- **Narrow const generics** (`[T; N]`) — Phase 5 #6a.
- **Pattern completeness** (ranges/guards/or/nested) — Phase 5 #11.
- **Explicit-dictionary coherence** — Phase 6 #8c.
- **Arena/index safety** (stale-index use-after-remove) — Phase 6 #8b.
- **Interpreter structured diagnostics** (prereq for the differential harness)
  — Phase 4 #18a.
- **Declaration-span remainders** (extern-fn, module-file-not-found) —
  Phase 4 #13e.

---

## How to use this file

- Adding a hole: add an OPEN entry with all five required fields, wire a
  reproduce-and-freeze gate, add a `CLAIMS_TODAY.md` disclosure if it touches a
  public claim, and a ROADMAP fix item.
- Fixing a hole: flip its gate to expected-reject, move the entry to CLOSED,
  remove the `CLAIMS_TODAY.md` disclosure, and update the ROADMAP item.
- The `check_docs_drift` gate (ROADMAP Phase 4 #44) should treat this file as
  claim-bearing: an OPEN entry whose gate no longer reproduces the hole, or a
  CLOSED entry whose regression fixture is missing, is drift.
