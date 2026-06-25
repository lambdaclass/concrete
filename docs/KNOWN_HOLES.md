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

One soundness hole is open: **H2 (float→int cast overflow)**, below. The H1 /
C9 / C10 entries are recently closed and retained here (with `CLOSED` markers
and regression gates) so the thread is legible and the fixes cannot silently
regress; the longer-standing closed set is under "CLOSED this session" further
down. Deferred *design* items that are not holes are listed under "Open design
decisions" near the end.

### H2. Float→int cast overflow is unchecked — OPEN (filed 2026-06-25)

`f as iN` / `f as uN` lowers to a raw LLVM `fptosi`/`fptoui`. When the float
value is outside the integer type's range the result is LLVM **poison** — the
compiled binary silently produces garbage rather than trapping (e.g.
`9999999999.0 as i32` yields `8501526768`, not a trap; i32's max is
`2147483647`). In-range casts are correct (`100.5 as i32 == 100`).

This is the one remaining "semantically dark" arithmetic construct after the
ROADMAP #10 checked-integer flip (which made `+ - * / % << >>` and unary `-`
trap on overflow/UB). It is scoped out of that flip because (a) it is a distinct
subsystem from integer arithmetic, and (b) the **interpreter does not support
float literals yet** (`interp: float literals not yet supported`), so there is
no interp==compiled differential oracle to verify a fix against — shipping a
compiled-only checked-cast would be unverifiable by the project's standard.

**Intended fix:** make float→int casts checked (abort on out-of-range) or
saturating, decided alongside the rest of the float story, once the interpreter
gains float support so the fix can be differential-tested. Until then this is
disclosed, not silently dark.

**Gate:** `scripts/tests/check_float_cast_hole.sh` pins the current behavior
(in-range correct; out-of-range emits a raw `fptosi` and does not trap). When
the cast is made checked/saturating the gate fails by design — the signal to
move this entry to CLOSED and replace the pin with a real trap/agreement check.

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
`from(param)` escape valve remains deeply deferred and evidence-gated (ROADMAP
Phase 7 #8e).
Deferred follow-on: `with_value_mut`/`modify` (separate container-not-in-context
obligation).

ORIGINAL HOLE (for history): stdlib `get`/`get_mut`-style APIs returned
references inside aggregates (`Option<&T>`, `Option<&mut V>`). The owner was
**not** frozen while the returned reference lived, so a saved reference could
survive a mutation that reallocated/removed/reused storage — a use-after-realloc
that compiled in safe code. Affected (all now migrated): `HashMap::get`/`get_mut`,
`OrderedMap::get`/`get_mut`, `OrderedMap::min_key`/`max_key`, `OrderedSet::min`/
`max`, `Vec::get`, `Slice::get`/`MutSlice::get`, `Deque::get`, `BinaryHeap::peek`.

- **Disclosed:** `CLAIMS_TODAY.md` — "No dangling safe reference" now covers the
  whole safe surface (borrow-block refs *and* the absence of any returned ref).
- **Deferred (evidence-gated):** `from(param)` returned references (ROADMAP
  Phase 7 #8e); the
  mutable scoped callback `with_value_mut`/`modify` — parked, nothing pulls it
  (the surface is covered by `update` for single-key mutation, `for_each_ctx` for
  mutable-context traversal, `with_value`/`with_at` for borrowed reads).
- **History (resolved by subtraction, not patched):** the fix was staged by
  danger — mutable half first (`get_mut` → `update`, the use-after-realloc write
  vector), then the immutable read accessors migrated to the value model
  (`get -> Option<V>` for Copy; `with_value`/`with_at` to borrow), then the
  blanket "no returned references" rule turned on once the surface was migrated.
  `Clone` was deliberately NOT used as the patch (separate value-model item,
  #8a2). No lifetimes, regions, or `from()`. The flat no-aggregate-ref ban is now
  a corollary of the broader invariant.

### C9. Address-taken loop variable → lost condition / infinite loop — CLOSED 2026-06-13

A loop variable that was **both** loop-carried and address-taken (e.g. `&i`
inside a `while i < n { … ; i = i + 1 }`) miscompiled: the variable got both a
promoted alloca (from `&i`, per C8) **and** an SSA phi, the two diverged, the
init `store 0` landed inside the loop body (resetting the counter every
iteration), and the loop condition disappeared — a silent infinite loop.
**Fixed** (`Concrete/Lower.lean`): a scalar whose address is taken anywhere in
the loop body is now promoted to a stable alloca BEFORE the loop (memory-backed,
single source of truth) rather than phi-carried — so it is driven entirely
through memory, like aggregates. Promoted scalars are excluded from loop / `if` /
`match` value reconciliation (else the merge re-stores a stale snapshot), and a
`&mut promotedVar` call argument passes the alloca directly (no copy/write-back
that would desync from the alloca).
- **Locked by:** `tests/programs/regress_loop_addr_taken_var.con` (= 3) in the
  main suite; broader loop edge cases (single `&i`, `&mut i`, nested `&i`/`&j`)
  verified. Full suite 1553/0; examples 123/0.

### C10. Indexing an array behind a reference yields `<unknown>` — CLOSED 2026-06-14

Indexing an array reached through a `&[T; N]` / `&mut [T; N]` (`arr[i]`,
`&arr[i]`, `arr[i] = v`) used to resolve the element type to `<unknown>`
(E0220 / E0552 / E0501) — indexing did not auto-deref a reference to the array.
Fail-closed (it rejected, never miscompiled), but it blocked the ergonomic
`&arr[i]` element-borrow form. **Fixed** by resolving the array-index element
type through one ref/ptr/heap layer in all three places that compute it:
`Check` (`.arrayIndex`), `CoreCheck` (`.arrayIndex` / `.arrayIndexAssign`), and
`Elab` (`.arrayIndex`). Lowering needed no change — a `&[T; N]` already *is* the
array base pointer. Sibling of the #6b `peekExprType` fix.
- **Locked by:** `tests/programs/regress_index_through_ref.con` (= 78: read by
  value, read by `&`, and index-assign through `&mut`). Full suite 1557/0;
  examples 123/0.

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

## Open design decisions that gate the Phase 5/6/7 freeze

These are not holes (no current unsoundness) but are unmade decisions that must
land before the relevant freeze. Full text in ROADMAP; listed here so the
whole picture is in one place.

- **Callable values + capability-polymorphic callbacks** — ROADMAP Phase 6
  #18. DESIGN DONE (2026-06-12): `docs/CALLABLE_VALUES_AND_CAPABILITIES.md`
  now exists and records the decided model; the HOF freeze is lifted (doc
  governs). What remains is implementation only when workloads pull it:
  `with_value_mut`/`modify` need the container-not-in-context gate, stored
  `BoundFn` values need a storage workload, and `from(param)` stays deferred
  (Phase 7 #8e) and is explicitly NOT the H1 fix.
- **Owned `ByteView` zero-copy stored idiom** — Phase 5 #5a. DONE (2026-06-21):
  `docs/BYTE_VIEW.md` design + `std.numeric` `ByteView { off, len, buf_len }`
  (reference-free Copy, checked Option-returning access, overflow / bounds /
  wrong-buffer-length brand) + the explicit UTF-8-validated raw→`Text` step
  (`std.text` `try_from_raw`/`validate_utf8`, `ByteView::try_text`) +
  `examples/byte_view/*` + gated by `scripts/tests/check_byte_view.sh`. No open
  hole.
- **Narrow const generics** (`[T; N]`) — Phase 7 #8f. DESIGN DECIDED, BUILD
  DEFERRED (2026-06-21): `docs/CONST_GENERICS_V1.md` fixes the V1 boundary
  (`struct Buf<T, const N: u64>`, integer params, literal/const-foldable args,
  per-N monomorphization recording N in name/layout/obligations; type-level
  computation / reflection / comptime / runtime-bound params rejected). A forcing
  probe found no current workload needs it — every fixed array in `examples/` is
  a single-use domain constant, none instantiate one container at multiple
  capacities, and the single-fixed-capacity workaround is clean. Workload-driven:
  build only when the doc's forcing conditions appear. No open soundness hole.
- **Pattern completeness** (ranges/guards/or/nested) — Phase 6 #5.
- **Explicit-dictionary coherence** — Phase 7 #8c.
- **Arena/index safety** (stale-index use-after-remove) — Phase 7 #8b.
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
