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

### H1. Returned-reference provenance — aggregate-wrapped refs are unsound

Stdlib `get`/`get_mut`-style APIs return references inside aggregates
(`Option<&T>`, `Option<&mut V>`). The owner is **not** frozen while the
returned reference lives, so a saved reference can survive a mutation that
reallocates/removes/reuses storage — a use-after-realloc that compiles in safe
code. Affected: `HashMap::get`/`get_mut`, `OrderedMap::get`/`get_mut`,
`OrderedMap::min_key`/`max_key`, `OrderedSet::min`/`max`, `Vec::get`,
`Slice::get`/`MutSlice::get`, `Deque::get`, `BinaryHeap::peek`.

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
- **Fix:** ROADMAP Phase 5 #24 — scalar `from(param)` returned references
  (flat no-aggregate rule, partiality via `#[requires]`); Phase 6 #8a is the
  collections-freeze blocker that forces the fix before the stdlib freezes.

### H2. Proven safety violations are not enforced by default

A runtime-safety obligation the compiler discharges to `violation` is a
compile-time **proof** the access is wrong — but `violation` is currently only
reported, not enforced, so safe code still compiles and ships UB. This is
distinct from `unproven` (an undischarged obligation, reasonably policy-gated).

- **State:** OPEN. Detection works; only enforcement is missing.
- **Reproduce:** `examples/known_holes/proven_oob_index/` (`a[5]` on
  `[i64; 3]` builds and does an out-of-bounds read) and
  `examples/known_holes/proven_div_zero/` (`10 / 0` builds). Both show
  `VIOLATION: …` in `concrete <file> --report contracts`.
- **Gate:** `scripts/tests/check_proven_violation_enforcement.sh` — confirms
  both still build (hole present) AND that each is classified `VIOLATION`
  (proven, not `unproven`), so only enforcement is missing; flips to
  expected-reject when the fix lands.
- **Disclosed:** `CLAIMS_TODAY.md` (§1, "what enforced does NOT cover").
- **Fix:** ROADMAP Phase 12 #0 — `status = violation` ⇒ hard error by default
  in safe code, suppressible only via `trusted`/`with(Unsafe)` or a named
  assumption; `unproven` is NOT swept into the same path.

### H4. Nested field assignment silently dropped — miscompile

`o.inner.v = x` (a field assignment whose object is itself a field access) does
not take effect. Lower's `.fieldAssign` value-struct path mutates a temporary
**copy** of `o.inner` and only writes it back when the object is a plain
`.ident` (single level); for a nested object it hits `_ => pure ()` and the
mutated copy is discarded. Single-level `o.v = x` works. Reading back returns
the stale value (sibling fields intact), so it is a fail-open miscompile.

- **State:** OPEN. Fail-open (compiles, runs, wrong value).
- **Reproduce:** `examples/known_holes/nested_field_write/` — returns 109
  (stale) instead of 7709.
- **Gate:** `scripts/tests/check_nested_field_write.sh` — asserts the fixture
  builds and returns the wrong value (109) while broken, and that single-level
  writes still work; flips to expect 7709 when fixed.
- **Disclosed:** `CLAIMS_TODAY.md` (§1, "what enforced does NOT cover").
- **Fix:** ROADMAP Phase 4 #44c — nested place/lvalue lowering: compute the
  address of `o.inner.v` by chained GEP (or recurse the value writeback up the
  field-access chain) and store in place, instead of copy-mutate-discard.

---

## CLOSED this session (kept here so the fix can't silently regress)

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

- **Callable values + `from(param)` provenance** — ROADMAP Phase 5 #24/#24a.
  THE keystone: `docs/CALLABLE_VALUES_AND_CAPABILITIES.md` does not exist yet,
  so the stdlib HOF surface is frozen, and writing it is also the fix for H1.
- **Owned `ByteView` zero-copy stored idiom** — Phase 5 #5a.
- **Narrow const generics** (`[T; N]`) — Phase 5 #6a.
- **Generic inference through references** (`id<T>(x: &T)` can't infer `T`
  from `&w` today; blocks every `&T`/`&K`/`&V` HOF) — Phase 5 #6b.
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
