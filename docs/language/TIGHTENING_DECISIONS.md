# Language Tightening Decisions

**Status:** decision record, 2026-10-23. The four questions this record raised were decided by the
user on 2026-10-23 ([§4](#4-decisions-of-2026-10-23)); the other recommendations stand as
recommendations until their owners implement them.
This document does not restate rules. Each rule is normative in its canonical owner: references and
binding in [MEMORY_SEMANTICS §15](MEMORY_SEMANTICS.md), evaluation order in
[STATEMENT_EXPRESSION_MODEL](STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment),
arithmetic and casts in [ARITHMETIC_POLICY](ARITHMETIC_POLICY.md), the C boundary in
[FFI.md](../platform/FFI.md) and `main` in [MAIN_EXIT_MODEL](../platform/MAIN_EXIT_MODEL.md).
Sequencing belongs to [ROADMAP.md](../../ROADMAP.md). An adopted item is written into its owner
and its exit, and this record then points there.

## 1. Criterion

The user approved selective tightening on 2026-10-22. A candidate rule is tightened when it does
one of two things:

- it closes a **soundness hole**: the compiler miscompiles, or the checker accepts a program the
  semantics should refuse; or
- it removes a case from the semantics the verified pipeline
  ([VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md), R-0491 – R-0509) must formalize.

A rule that is only stricter style waits for a workload, under the workload-driven rule
([DESIGN_POLICY.md](../project/DESIGN_POLICY.md)). Each candidate gets one class:

| class | meaning |
|---|---|
| **HOLE** | unsound or miscompiled, or the checker accepts what the semantics should refuse |
| **SEMANTICS** | an undecided or implementation-defined case the formal semantics would have to carry |
| **STYLE** | well-defined, but unusual or looser than it could be |

Each candidate then gets one recommendation: **now** (implement in the next train),
**decide now, implement later** (record the rule; its implementation waits for a named owner), or
**deferred**, with the condition that pulls it.

## 2. Method

**Revisions.** Every witness was compiled with three builds on the OVH server (`fede-256gb`):

- main `185d998c`;
- `wip/train1` `f92b8f32`, which holds exit 2i: E0301 – E0305, SCOPE-1, REF-6 and the renamed
  pattern binder;
- `wip/train3` `949d79e9`, which is train 2 plus train 1b (exit 2i).

For each accepted witness the binary was run and `--interp` was run. The witness files are
reproduced inline below, and the bug reproducers are in `tests/regressions/`. Unless an entry says
otherwise, train1 and train3 behave like main. "Refused" quotes the diagnostic code.

**Corpus impact.** `scripts/tools/rule_impact.sh` ([§6](#6-the-rule-impact-scanner)) compiles the
whole corpus with two compilers and lists every unit whose accept/refuse outcome or code set
changes. It measures what a rule breaks before the rule lands.

**New defects.** The probes found three defects, filed with full records:

- [bug 181](../bugs/181_generic_borrowed_scrutinee_binder_mode.md): a binder through a borrowed
  scrutinee of generic type. Check types it `&T` and Elab types it `T`, so the program segfaults in
  safe code.
- [bug 182](../bugs/182_borrowed_call_result_temporary.md): a borrowed call result. A `Heap`
  temporary segfaults, and a linear temporary leaks.
- [bug 183](../bugs/183_deref_assign_destination_first.md): the known, unfiled order disagreement
  for a dereference assignment.

## 3. Candidates

### 3.1 Summary

| id | candidate | class | today (main `185d998c` / train3 `949d79e9`) | proposed rule | recommendation |
|---|---|---|---|---|---|
| T-G2 | post-mono reference predicate weaker than the front end's (G2 remainder) | SEMANTICS | front end refuses `T := *const &Int` in a return (E0236 on train3, no code on main); a transitive generic reaches `id::<*const &Int>` and passes | one predicate (`tyContainsRef`) at both sites, E0236 `post-mono`; completeness gate scans `Verify.lean` | now (exit 2i) |
| T-G9 | `&mut` argument modes (G9, B1, B2) | SEMANTICS | a parameter `&mut` is reborrowed at each call; a borrow-block `&mut` is consumed by its first call (E0205 on the second); both may lapse unused | **decided (D1):** every `&mut` argument is an implicit reborrow; an unused `&mut` may lapse | implement with R-0496 (Core reborrow form); 0 corpus sites break |
| T-G10 | owner reads under a shared borrow block (G10, B7) | STYLE | refused (E0203) | none: keep the freeze | deferred: a workload that needs owner reads under a named shared borrow (R-0499) |
| T-REF5 | borrow of a temporary: literal, call result, aggregate literal (G12) | HOLE (bug 182) | accepted. `&alloc(..)` segfaults and `&int_to_string(..)` leaks (bug 182). `&42`, `&mk()` and `&P{..}` of Copy type give 42 in both consumers | **decided (D2):** only places are borrowable; a string literal is a static place; everything else is E0238 (REF-5@1) | now (exit 2i, closes bug 182); migration: 1 corpus site (`std/src/map.con:712`) |
| T-G13 | trait method declaration returning a reference (G13) | SEMANTICS | accepted; every implementation is refused | E0236 at trait method declarations | now (exit 2i) |
| T-G15 | `extern fn` with `*const &T` in its signature (G15) | SEMANTICS | result and parameter forms accepted; reading the inner reference is E0238 | E0575 looks through raw-pointer pointees, in parameters and results; 0 corpus sites | now (exit 2i, with R-0285) |
| T-G16 | reference-returning function type via an alias or a field (G16) | SEMANTICS | accepted (no value can inhabit it) | E0236 after alias expansion and in struct/enum field declarations; 0 corpus sites | now (exit 2i) |
| T-G17 | arm binder through `match &o` after its root moves (G17) | HOLE (not reproduced) | three variants refused, E0204 (train3: E0204, E0208) | none new | **closed 2026-10-23:** three rejection fixtures `error_g17_*.con`; §15.7 row and exit 0b clause corrected |
| T-BIND | Copy-keyed binding mode through a borrowed scrutinee | HOLE + SEMANTICS (bug 181) | non-Copy payload binds `&P`, Copy payload binds `P`. A generic payload is `&T` to Check and `T` to Elab: segfault at `T := Int` (bug 181) | **decided (D3):** every binder through a reference scrutinee is a reference (`&T`), Copy or not | now (exit 2i, closes bug 181); migration: 2 fixtures, 0 std or example sites |
| T-G19 | `noalias` / exclusivity exploitation; Core reborrow form (G19, B4, B5) | SEMANTICS | no `noalias` emitted | policy: no pass exploits `&mut` exclusivity before an aliasing model covering raw pointers exists | decide now (adopt sentence), implement later (R-0505, R-0496) |
| T-ARITH | checked arithmetic | settled | `+ - *` trap (exit 134; interp error); `/ %` by zero trap; shift count checked; `i8 100 << 1` is -56 by rule | none | nothing to tighten |
| T-CAST | lossy numeric `as` | SEMANTICS | `300 as u8` is 44 silently in both consumers | decided 2026-10-21 (ARITHMETIC_POLICY §8): `as` lossless only; lossy is a compile error after the named operations exist | decide now (done), implement later (exit 2k after 2h) |
| T-RAWDUP | trusted `*p` duplicates a non-Copy pointee (the raw-pointer generic leak, bug 046 shape) | HOLE | accepted with and without generics; binary exits 0, train3 `--interp` reports a use after free | `*p` by value of a non-Copy pointee is refused in trusted code (new code); an explicit `ptr::read_owned` marks the transfer | decide now, implement later (R-0014 → R-0514 / V6; scanner measures std first) |
| T-165 | `println` binds a user `print_int` (bug 165) | HOLE | binary SIGABRT; IR calls `@_C.m.print_int` | no language rule: generated calls bind by identity | now (exit 0b repair) |
| T-166 | foreign function value called with the Concrete ABI (bug 166) | HOLE (trusted boundary) | IR: `declare i64 @take_pair(i64)` but `call i64 %f(ptr %p)` | **decided (D4):** refuse now; long term, function types carry their calling convention (`extern "C" fn(..)`) | refusal now (exit 0b); the typed convention with the first callback workload |
| T-ORDER | evaluation-order lag: 136, 137, 183 | HOLE | functional update base runs twice; enum literal fields in declaration order; deref assignment destination first (binary 12, interp 21) | rule exists (exit 2j); implementation lags | now (exit 0b repairs) |
| T-MAIN | `main`'s `Int` result outside 0–255 | STYLE (specified) | wrapper masks with `and 255`: 256 → 0, -1 → 255 | none now | deferred: the open surface decision (MAIN_EXIT_MODEL) |
| T-H27 | ill-typed contracts (KNOWN_HOLES H27) | SEMANTICS | `#[requires(x < 9999999999)]` on `i32` and `(x > 0) + 1 > 0` accepted | contract expressions type-checked as expressions of the function's types (existing E0220 / E0228 / E0227) | decide now, implement later (R-0473) |

The 2026-10-21 decisions already close G3, G4, G14 and G18 by exit 2i (E0301 – E0305), so they
are not candidates here. Two other items are settled and are not repeated in the table:

- literal range: `let x: u8 = 300;` is E0227, and `-128` stays a valid `i8`;
- parameters on `main`: E0310 (bug 111).

### 3.2 Entries

#### T-G2. One reference predicate before and after monomorphization

- **Status.** Front-end half closed by exit 2i: E0236 at `checkFn`, `resolveType` and the instantiation checks. The
  post-mono `tyExposesRef` does not look into raw-pointer or function types (MEMORY_SEMANTICS §15.7
  G2), and the diagnostic-codes completeness gate does not scan `Verify.lean`.
- **Witness** (accepted on all three revisions, runs, exit 0):

  ```con
  fn id<T>(x: T) -> T { return x; }
  fn relay<U>(x: U, k: fn(U) -> Int) -> Int { return k(id::<U>(x)); }
  trusted fn first(p: *const &Int) -> Int { return 0; }
  trusted fn go(p: *const &Int) -> Int { return relay::<*const &Int>(p, first); }
  fn main() -> Int { return 0; }
  ```

  The direct instantiation `id::<*const &Int>(p)` is refused at the front end (E0236 on train3;
  the same message without a code on main). The transitive one reaches `id` at `*const &Int` only
  after substitution, and the post-mono check passes it.
- **Class.** SEMANTICS. Producing a `*const &Int` takes trusted code, and no unsound safe program was
  found. But REF-1's normative predicate is the front end's, so the formalized rule would have to
  carry two predicates.
- **Rule.** `verifyNoReturnedRefs` uses `tyContainsRef`: E0236 with pass `post-mono`, REF-1@1
  unchanged. The completeness gate also scans `Verify.lean`.
- **Interactions.** It finishes exit 2i's G2 row. There is no corpus migration expected: no corpus program returns a
  pointer to a reference.
- **Recommendation.** Now, as part of closing exit 2i.

#### T-G9. `&mut` argument modes

- **Status.** The modes are inconsistent (G9). Witnesses:

  ```con
  fn inc(r: &mut Int) { *r = *r + 1; }
  fn twice(r: &mut Int) { inc(r); inc(r); }   // a parameter &mut: reborrowed, accepted
  fn main() -> Int {
      let mut x: Int = 0;
      twice(&mut x);
      borrow mut x as r in R { inc(r); }       // a borrow-block &mut: one use, accepted
      borrow mut x as r2 in R2 { }              // lapses unused, accepted
      return x;                                 // 3 in both consumers
  }
  ```

  A borrow block that calls `inc(r); inc(r);` is refused (E0205 "linear variable 'r' used after
  move") on all three revisions. REF-6 (exit 2i, E0302) removes the third mode, the `let`-bound
  `&mut`.
- **Class.** SEMANTICS: the formal semantics would carry two argument-passing modes for one type.
- **Rule.** B1: every `&mut` argument is an implicit reborrow, whatever binding it comes from. B2:
  an exclusive reference may lapse unused. B2 is not implicit destruction, because the reference
  owns nothing.
- **Interactions.** Exit 2i's acceptance requires the B1/B2 decision to be recorded. B1 relaxes the
  borrow-block case, so it breaks no program. It needs Core's explicit reborrow form (G19 B5,
  R-0496), so the implicit reborrows are visible after elaboration.
- **Decided (D1, 2026-10-23):** B1 and B2 as stated. Implement with R-0496. Until then, the
  documented borrow-block consumption stays enforced.

#### T-G10. Owner reads under a shared borrow block

- **Status.** `borrow x as r in R { out = x + *r; }` is refused (E0203 "variable 'x' is frozen by
  borrow block") on all three revisions. The design model B7 would allow it.
- **Class.** STYLE. The current rule is stricter and well-defined, and B7 is a relaxation.
- **Recommendation.** Deferred. The pull condition is a workload that needs owner reads under a
  named shared borrow; it would become a new REF-8 version under R-0499. Until then the model must not cite B7 as
  the current rule (MEMORY_SEMANTICS §15.7).

#### T-REF5. Borrow of a temporary

- **Status.** G12 is wider than §15.7 records. Every non-place borrow form is accepted, and the
  interpreter no longer refuses them: `&42`, `&mk()` and `&P { n: 42 }` (Copy `P`) return 42 in
  both consumers. Two forms are defects
  ([bug 182](../bugs/182_borrowed_call_result_temporary.md)):

  ```con
  struct Copy B { v: Int }
  fn peek(b: &Heap<B>) -> Int { return b.v; }
  fn main() with(Alloc) -> Int { return peek(&alloc(B { v: 42 })); }
  // binary: SIGSEGV (reads address 0x2a); --interp: 42
  ```

  `n(&int_to_string(12345))` returns 5, but valgrind reports 32 bytes definitely lost: the
  temporary `String` is never destroyed. A string literal temporary is harmless, because a literal
  `String` has capacity 0 and owns no heap. The corpus borrows string literals at 77 sites, an
  integer literal at 2 (one in an already-refused fixture) and a call result at 2 (bug 182's
  reproducers), with no aggregate-literal borrows; this is the operand scan of
  [D2](#d2-only-places-are-borrowable-q2-ref-5-g12), which supersedes an earlier, cruder textual
  count of 79 / 1 / 0.
- **Class.** HOLE: a leak of a linear value and a miscompile, both in safe code.
- **Rule options.**
  - (A) REF-5@1 as written, string literals included. Every non-place borrow is E0238, and the 79
    literal sites (77 string literals, 2 integer literals) migrate to named locals.
  - (B) REF-5@2. A borrow of a value expression is an anonymous local of the enclosing
    statement, destroyed at its end. Linear temporaries would then be destroyed implicitly, which the 2026-10-21
    stance excludes.
  - (C) A borrow of a non-place whose type is not Copy is E0238. Copy temporaries and string
    literals, which are static storage, are anonymous locals of the enclosing statement that need no
    destruction.
    No corpus site breaks.
- **Interactions.** REF-5 is exit 2i's rule, and C is a REF-5@2 version of it. It is compatible
  with REF-6 (exit 2i): no `let` may hold a reference and none is assigned, so a borrowed temporary
  is used only where REF-5 lets a reference be used, all inside one statement: a call argument or
  receiver, the operand of `*` (`*(&42)`), a field or index base, or a `match` / `if let` /
  `while let` scrutinee (`match &mk() { .. }`). For a scrutinee the statement is the whole `match`,
  so arm binders through it stay inside the temporary's lifetime. Place MIR (R-0501) gives the anonymous local a place.
- **Decided (D2, 2026-10-23):** variant A with string literals as static places, not C: only
  places are borrowable. The options above are kept as the record of what was weighed; 1 corpus
  site migrates. It closes bug 182.

#### T-G13. Trait method declaration returning a reference

- **Witness:** `trait Get { fn get(&self) -> &Int; }` is accepted on all three revisions, and every
  `impl` of it is refused.
- **Class.** SEMANTICS: the trait admits a signature that nothing can implement.
- **Rule.** E0236 at trait method declarations (REF-1).
- **Recommendation.** Now (exit 2i). It breaks no corpus program (0 sites).

#### T-G15. Raw pointers to references at the C boundary

- **Witnesses:**
  - `extern fn f() with() -> *const &Int;` and `extern fn keep(p: *const &Int) with();` are
    accepted.
  - `let p: *const &Int = f(); return **p;` in a trusted function is E0238: the inner reference
    cannot be read out.
- **Class.** SEMANTICS. The FFI predicate decides whether a reference may cross the boundary under
  a raw pointer. The parameter form would let C retain the address of a reference slot.
- **Rule.** E0575 looks through raw-pointer pointees, in both parameters and results.
- **Interactions.** REF-1 and FFI.md (R-0285). There are no `extern` declarations with `*const &`
  or `*mut &` in the corpus.
- **Recommendation.** Now (exit 2i, with R-0285).

#### T-G16. Reference-returning function types via an alias or a field

- **Witnesses:** `type Cb = fn(&Int) -> &Int;` used as a parameter type, and
  `struct H { f: fn(&Int) -> &Int }`. Both are accepted on all three revisions.
- **Class.** SEMANTICS: these are uninhabited types.
- **Rule.** `resolveType` re-checks `.fn_` after alias expansion, and declarations use
  `tyContainsRef`. Code E0236 (REF-2).
- **Recommendation.** Now (exit 2i). It affects 0 corpus sites.

#### T-G17. Arm binder through a borrowed scrutinee whose root moves

- **Status.** Not reproduced. MEMORY_SEMANTICS §15.7 still reads "compiles on `main`", but three
  variants are refused:
  - `match &o` with `consume(o)` in the arm;
  - the `if let` form;
  - a `Heap` payload freed in the arm.

  Each is refused at the move with E0204 ("cannot move linear variable 'o': variable is borrowed")
  on main, plus E0208 on train1 and train3. The heap variant writes its inner match with a renamed
  binder, so it parses only on train1 and train3. With a Copy payload the arm binder is a copy
  (T-BIND), so reassigning the root in the arm is safe and accepted (42).
- **Closed (2026-10-23).** The three shapes are rejection fixtures (`tests/programs/error_g17_*.con`,
  wired in `run_tests.sh`; the heap form calls a releasing function instead of an inner match, so
  it parses on main), and they join `tests/rules/exit_2i.tsv` when that manifest lands. The §15.7
  row and the ROADMAP 0b clause are corrected. Reference-parameter
  scrutinees (`fn get(o: &Option<P>)`) were checked too: they cannot move the root, and they run
  (42).

#### T-BIND. One binding mode through a borrowed scrutinee

- **Status.** The payload binder of `match &o` / `match r` has two modes: `&P` for a non-Copy
  payload and `P` by value for a Copy payload (`Check.lean`, the `match` case). Elab does not follow
  Check: the Core binder always has the payload type. The difference is invisible for non-generic
  code. In generic code it is [bug 181](../bugs/181_generic_borrowed_scrutinee_binder_mode.md):

  ```con
  fn get<T>(o: &Option<T>, k: fn(&T) -> Int) -> Int {
      match o { Option::Some { value } => { return k(value); }, Option::None => { return 0; } }
  }
  fn read(p: &Int) -> Int { return *p; }
  fn main() -> Int { let o: Option<Int> = Option::Some { value: 42 }; return get::<Int>(&o, read); }
  // binary: SIGSEGV on main, train1 and train3; --interp: "deref of non-ref value"
  ```

- **Class.** HOLE (bug 181) and SEMANTICS. The mode is keyed on a property that substitution
  changes, so the formal semantics would carry two modes and a rule for choosing between them after
  monomorphization.
- **Rule options (Q3).**
  - (A) Uniform: every payload binder through a reference is `&T`, Copy or not; a Copy payload is
    read with `*value`.
  - (B) Keep two modes, decided on the checked (pre-substitution) type and carried in Core, so
    `get::<Int>` keeps `value: &Int`.
- **Interactions.** REF-8 (exit 2i) freezes the root while the binder lives. The exit 2i manifest
  row "a non-Copy payload binds as `&P`, a Copy payload by value" (MEMORY_SEMANTICS §15.3 REF-5,
  accepted examples) changes under (A). Measured under (A): 2 fixtures, no std or example site
  ([D3](#d3-a-binder-through-a-reference-is-a-reference-q3)).
- **Decided (D3, 2026-10-23):** (A), for payload and whole-value binders alike. It closes bug 181;
  Core carries the `&T` binder (R-0496).

#### T-G19. Exclusivity exploitation and the Core reborrow form

- **Status.** The backend emits no `noalias`. A witness with two `&mut` parameters shows none in its
  IR and runs (21).
- **Class.** SEMANTICS. Whether passes may assume `&mut` exclusivity is a property the backend proof
  would have to carry.
- **Rule.** A policy sentence owned by VERIFIED_PIPELINE and R-0505: no pass exploits `&mut`
  exclusivity until an aliasing model that covers raw pointers exists. B5: Core gets an explicit
  reborrow form (R-0496), with no surface syntax.
- **Recommendation.** Decide now: adopt the sentence. Implement later: the reborrow form, with
  T-G9.

#### T-ARITH. Checked arithmetic

- **Status.** Settled and enforced in both consumers:
  - `i32` `2147483647 + 1` aborts (exit 134), and `--interp` reports "arithmetic overflow (checked
    +)";
  - division and remainder by zero trap;
  - a shift count of `width` or more traps;
  - `i8` `100 << 1` is -56 (exit 200), the decided rule (ARITHMETIC_POLICY §6): a shift checks its
    count, not its result.
- **Recommendation.** Nothing to tighten. A trapping left shift would be STYLE, deferred until a
  workload shows lost-bit defects. Exit 2h (one `IntArith`) carries the semantics into V1a.

#### T-CAST. Lossy numeric `as`

- **Witness:** `fn narrow(x: Int) -> u8 { return x as u8; }` gives `narrow(300)` = 44 in both
  consumers, silently.
- **Class.** SEMANTICS: the formal semantics would have to carry a truncation for every width pair.
- **Rule.** Decided 2026-10-21, ARITHMETIC_POLICY §8 (`CAST-1@1`): `as` is lossless only. A lossy
  conversion is a compile error, with named `truncate` / `reinterpret` / `try_convert` / rounding
  operations as the migration.
- **Interactions.** Owned by exit 2k, staged after exit 2h. The estimated migration is 500 – 700
  sites, and the scanner measures it exactly at stage 2.
- **Recommendation.** Decide now (done); implement later in the staged order.

#### T-RAWDUP. Trusted dereference duplicates a non-Copy pointee

This is the "raw-pointer generic leak": the bug 046 shape, and the ROADMAP linearity decision
"Trusted is not implicit linear copy" (R-0014, merged into R-0514 / V6).

- **Witness** (accepted on all three revisions; the non-generic form behaves the same):

  ```con
  struct Copy B { v: Int }
  trusted fn dup<K>(x: &mut K) -> K { let p: *mut K = x as *mut K; return *p; }
  fn main() with(Alloc) -> Int {
      let mut h: Heap<B> = alloc(B { v: 42 });
      let h2: Heap<B> = dup::<Heap<B>>(&mut h);
      free(h2);
      free(h);
      return 0;
  }
  ```

  The binary exits 0 with a double free. On train3, `--interp` reports "read through a pointer to
  freed memory (alloc); a use after free".
- **Class.** HOLE. SAFETY.md and MEMORY_SEMANTICS §11 state that `trusted` does not relax
  linearity, and this shape contradicts them.
- **Rule.** In trusted code, `*p` by value of a non-Copy pointee is refused; a new code is
  allocated when the rule is implemented. An explicit `ptr::read_owned(p)` (std, trusted) states
  the ownership transfer. Copy pointees and stores through raw pointers are unchanged, because
  stores are the idiom for uninitialized slots.
- **Interactions.** `std`'s collections use `*p` reads, and the count is not yet known. Run the
  scanner on a candidate build before the rule lands. It belongs to V6's trusted-code proof
  obligations (R-0509), so the formalized trusted subset has one way to move out of raw memory.
- **Recommendation.** Decide now, implement later: measure, migrate std, then refuse, all in one
  change.

#### T-165, T-166. Identity and ABI at generated and foreign calls

- **Bug 165.** The witness binary aborts (SIGABRT, exit 134), because `println(7)` calls the
  module's own `print_int`. There is no language rule to add. LANGUAGE_GAPS #2's "user-defined
  functions with the same names take precedence" stays true of explicit calls, and a generated
  call binds the runtime helper by identity. **Now:** exit 0b repair; the class owner is R-0492.
- **Bug 166.** The IR shows `declare i64 @take_pair(i64)` beside `call i64 %f(ptr %p)`. It is a
  HOLE at the trusted boundary. **Now:** containment rule. Taking as a value an `extern fn` whose
  signature contains a `#[repr(C)]` aggregate is refused, with a new FFI code allocated when the
  rule is implemented. **Decided (D4, 2026-10-23):** this refusal now; the long-term design is a
  calling convention carried in the function type (`extern "C" fn`), built with the first callback
  workload.

#### T-ORDER. Evaluation order (bugs 136, 137, 183)

The rule is decided (STATEMENT_EXPRESSION_MODEL, exit 2j), and the implementation lags it:

| bug | witness | binary | `--interp` | rule |
|---|---|---|---|---|
| 137 | `E::V { b: tick(5), a: tick(6) }` | 6, 5 (exit 65) | 6, 5 | source order: 5, 6 |
| 136 | `P { a: 7, ..mk() }` | `mk` runs twice | twice | the base runs once |
| 183 | `*(&mut a[tick(&mut c, 1)]) = tick(&mut c, 2)` | 12 | 21 | right-hand side first: 21 |

All three are HOLEs (silent wrong code). **Now:** exit 0b repairs. No source migration is expected.

#### T-MAIN. `main`'s result outside the exit-status range

- **Status.** The entry wrapper masks the result (`and i64 %result, 255`), so `return 256` exits 0
  and `return -1` exits 255. `--interp` prints 256 and -1.
- **Class.** STYLE. The behaviour is specified (MAIN_EXIT_MODEL Stage 1), unusual and well-defined.
- **Recommendation.** Deferred. The narrowing to `fn main()` / `-> u8` is the open surface decision
  the user left for a later pass, with 973 `-> Int` signatures in the corpus.

#### T-H27. Ill-typed contracts

- **Witness:** `#[requires(x < 9999999999)] #[requires((x > 0) + 1 > 0)] fn f(x: i32) -> i32` is
  accepted on all three revisions and runs.
- **Class.** SEMANTICS. An obligation's meaning depends on the sorts that contract validation does
  not check (KNOWN_HOLES H27).
- **Rule.** A contract is type-checked as an expression of the function's parameter and result
  types, with the existing codes: E0227 literal range, E0228 mixed width and E0220 mismatch. Exit
  2i's E0305 already makes contracts binder- and statement-free.
- **Recommendation.** Decide now; implement later under R-0473, before V1a states claims with
  contracts.

## 4. Decisions of 2026-10-23

The user answered the four questions on 2026-10-23 with one instruction: **"do the best long term
solution for this problem and every problem"**. Each decision below is the long-term rule, not a
containment, chosen by that test: it leaves the formal semantics one case where there were two, it
removes the defect's class rather than its spelling, and its migration is measured. Where an
earlier recommendation in this record differs (Q2 recommended variant C), the decision governs.

### D1. `&mut` arguments: one mode (Q1, G9, design examples B1 and B2)

**Rule.** Every `&mut` argument is an implicit reborrow, whatever binding it comes from: a
parameter, a borrow-block binder, an arm binder. The reference stays usable after the call. An
exclusive reference that is never used may go unused: it owns nothing, so letting it lapse is not
implicit destruction.

**Rationale.** It leaves one argument-passing mode for `&mut T` in the formal semantics instead of
two (consume for borrow-block binders, reborrow for parameters). It relaxes only the borrow-block
case, so no program breaks; the refused `inc(r); inc(r);` inside a borrow block becomes legal.
After elaboration the reborrow is explicit in Core (B5), so the verified pipeline models one
operation.

**Homes.** MEMORY_SEMANTICS §15.7 G9 and MUT_REF_SEMANTICS §2 – §3 record the decision; R-0496
(Core reborrow form) implements it with the checker change in exit 2i. **Migration:** none.

### D2. Only places are borrowable (Q2, REF-5, G12)

**Rule.** A borrow `&e` / `&mut e` is accepted only when `e` is a canonical place (MEMORY_SEMANTICS
§15.1). A string literal is a place: it denotes static, read-only storage, so `&"text"` borrows that
storage, and `&mut "text"` is refused. Everything else is E0238 (REF-5@1): an integer, float, `bool`
or `char` literal, a call or method-call result, an aggregate or array literal, an operator
expression and a constant. The rewrite is an explicit `let`. There are no hidden anonymous locals
and no temporary materialization in the formal semantics.

**Rationale.** The alternatives each add a case: variant B destroys temporaries implicitly, which
the 2026-10-21 stance excludes, and variant C keeps call- or statement-scoped anonymous locals,
which the semantics would have to define (lifetime, place, which operand positions). Under D2 every
reference has a named or static root, which is INV-REF's premise. Bug 182 (a `Heap` temporary
miscompiled, a linear temporary leaked) is unwritable under it.

**Migration (measured).** A scan of every borrow operand in `std`, `examples` and `tests`, comments
excluded (`&e` where `e` is not a binding followed by field, index or `.0` projections, a
dereference of one, or a parenthesized place):

| operand | sites | disposition |
|---|---|---|
| string literal | 77 | unchanged: a static place |
| integer literal | 2 | `std/src/map.con:712` (`m.remove(&2)`, a std test) needs a `let`; `tests/programs/error_092_const_ref_array.con` is already refused (E0239) |
| call result | 2 | the two bug 182 reproducers, which become D2's rejection fixtures |
| call, aggregate or array literal elsewhere | 0 | |

So **one** corpus site needs an explicit `let`. The 13 parenthesized operands the scan also
reported (`&mut (*p).a`, `&mut (p as &mut P).v`) are places. The inline gate programs (256 heredoc
programs) have no non-place borrow. The implementing branch confirms the count with
`scripts/tools/rule_impact.sh`.

**Homes.** MEMORY_SEMANTICS §15.3 REF-5 (the string-literal place, the closed list of refusals)
and §15.7 G12; enforcement in exit 2i; bug 182 closes with it.

### D3. A binder through a reference is a reference (Q3)

**Rule.** Every pattern binder that binds through a scrutinee of reference type (`match r`,
`match &o`, `if let`, `while let`) has reference type: a payload field of type `T` binds as `&T`,
and a whole-value binder over `&T` binds as `&T`, whether or not `T` is Copy. A Copy value is read
with `*value`. Literal and range patterns still compare the referenced value.

**Rationale.** Today the mode depends on whether the payload is Copy, a property monomorphization
changes, and Check and Elab already disagree about it (bug 181: SIGSEGV in safe code at
`T := Int`). One mode, invariant under substitution, removes the class; the Core binder carries
`&T` from Check through Mono. It also keeps REF-8's freeze meaningful for every binder.

**Migration (measured).** Two measurement builds of main `185d998c` (branches
`wip/ltd-measure-q3a` and `wip/ltd-measure-q3b`, for measurement only, not for landing) made the
binder `&T` in Check, and `rule_impact.sh` compared them with main over all 1 907 units:

| build | change | newly refused |
|---|---|---|
| q3a | payload binders through a reference are always `&T` | 1: `tests/programs/patterns/match_ref_enum.con` (E0220: `return x;` with `x: &i32`) |
| q3b | q3a plus the whole-value binder (`n => n + 1000` over `&i32`) | 2: the above and `tests/programs/patterns/match_ref_scalar.con` (E0220) |

`std` and every example compile unchanged. The count is of checker refusals; a binder whose uses
also type-check at `&T` (an argument to a generic parameter, for example) changes type silently, so
the implementing branch also runs the binary-against-`--interp` legs. **Two** fixtures migrate
(`*x`, `*n`).

**Homes.** MEMORY_SEMANTICS §15.3 (REF-5's accepted examples, REF-8), with R-0496's explicit Core
binding mode; bug 181 closes with it.

### D4. Foreign function values: refuse now, typed calling convention later (Q4)

**Rule now.** Taking as a value an `extern fn` whose signature contains a `#[repr(C)]` aggregate
(parameter or result) is refused, with a new FFI code. Those are exactly the signatures where the C
ABI and the Concrete ABI differ (a small aggregate travels in registers under C and by pointer under
Concrete), so the refusal removes bug 166's wrong code; an `extern fn` over integers, floats and
raw pointers passes the same way under both conventions and stays usable as a value.

**Long-term design.** The calling convention is part of the function type. Its sketch, normative
in [FFI.md](../platform/FFI.md#calling-convention-in-function-types-decided-2026-10-23-not-implemented):

- `fn(A) -> R` is a Concrete function type; `extern "C" fn(A) -> R` is a C function type. They are
  distinct types and never convert implicitly.
- An `extern fn` taken as a value has type `extern "C" fn(..)`; a Concrete function never does.
- A call through an `extern "C" fn` value lowers with the C ABI (the same flattening a direct
  extern call uses), and needs what a direct call of that extern needs (`with(Unsafe)`, unless the
  declaration is `trusted extern`).
- A Concrete function that C must call through a pointer is exported explicitly (an adapter with
  the C ABI, `#[export_name]`); it is not a value of `extern "C" fn` type by coercion.

It is implemented when the first workload passes a callback to C or stores a foreign function
value with an aggregate signature; at that point the refusal above becomes a type error between the
two function types, so the containment is a step toward the design and nothing written under it
needs to change.

**Homes.** FFI.md (the section linked above); R-0283 / R-0287 (ABI checks); bug 166 closes with the
refusal.

### Also closed: G17

The three shapes of G17 are refused on main (E0204) and are now rejection fixtures
(`tests/programs/error_g17_match_borrow_root_moved_in_arm.con`,
`error_g17_if_let_borrow_root_moved.con`, `error_g17_match_borrow_heap_freed_in_arm.con`, wired in
`run_tests.sh`); MEMORY_SEMANTICS §15.7 and ROADMAP exit 0b no longer describe it as open.

## 5. Implementation plan (HOLE and SEMANTICS items)

The order puts silent defects first, then the decided rules that refuse code, then the rules that
need Core work or a migration. Every rule lands with a versioned migration diagnostic and manifest
rows, as MEMORY_SEMANTICS §15.5 – §15.6 require. Before it lands, its branch runs
`scripts/tools/rule_impact.sh` against main, and the report goes in the change description. The
soundness-holes agent implements these; this record is where the two coordinate.

1. **Exit 0b repairs**, independent of each other:
   - bug 183: `Lower` evaluates the right-hand side first;
   - bugs 136 and 137: Elab binds the functional-update base once and keeps enum fields in source
     order;
   - bug 166: D4's refusal of a foreign function value with a `#[repr(C)]` aggregate signature;
   - bug 165: generated calls carry the runtime callee.
2. **D2, only places are borrowable** (REF-5@1, E0238; closes bug 182 and G12): the string-literal
   place, the refusal of every other non-place operand, the one `let` in `std/src/map.con`, and
   bug 182's reproducers as rejection fixtures.
3. **D3, binders through a reference are references** (closes bug 181): Check, Elab and Mono agree
   on `&T`; the two `patterns/match_ref_*` fixtures migrate; bug 181's reproducer becomes a run
   fixture (42, binary = `--interp`).
4. **Exit 2i closure rules, no corpus migration:**
   - T-G13: E0236 on trait method declarations;
   - T-G16: E0236 after alias expansion and in field declarations;
   - T-G15: E0575 looks through pointees;
   - T-G2: one predicate post-mono, and the completeness gate scans `Verify.lean`.

   Each is a reject row plus a run row in `tests/rules/exit_2i.tsv`; G17's fixtures join that
   manifest when it lands on main.
5. **D1, one `&mut` argument mode**, with R-0496's Core reborrow form: the checker stops consuming
   a borrow-block `&mut` passed to a call; B7 stays deferred (T-G10); the G19 policy sentence goes
   into VERIFIED_PIPELINE.
6. **Contract typing** (T-H27) under R-0473, before V1a's contract claims.
7. **Trusted move-out** (T-RAWDUP): add `ptr::read_owned`, measure with the scanner, migrate std,
   refuse. This is in V6's trusted-code slice and may be pulled earlier if a trusted defect needs it.
8. **Casts** (T-CAST): the exit 2k staging after exit 2h. Named operations come first, then the
   rejection with migration diagnostics, then the migration.
9. **D4's typed calling convention**, with the first callback workload.

Items 1 – 4 can run in parallel branches; 2 and 3 touch the same checker region (REF-5's place
test and the `match` binder), so one branch should own both or they land in sequence.

## 6. The rule-impact scanner

`scripts/tools/rule_impact.sh --candidate <tree|ref> [--base <tree|ref>] [--corpus <tree|ref>]`
compiles every unit of one corpus with both compilers, using `--emit-llvm`, no linking, and the
corpus's own std for both. The units are:

- each standalone `.con` under `tests/` and `examples/`;
- each project with a `Concrete.toml`;
- `std` as one unit, whose full diagnostic list is reported;
- each Concrete program written inline in a gate script heredoc.

The scanner records each unit's outcome (accept, refuse, crash or timeout) and its set of
diagnostic codes. It writes `diff.tsv`, which lists every unit that changed, and `summary.txt`,
which counts newly refused units by code and separates refusals located inside std from refusals
in the unit itself. A compile that cannot run (status 125 – 127) makes the whole scan a setup
error, never a "no difference" report. A run over the whole corpus takes about 1 – 3 min with 16
jobs on the OVH server.

**Validation: train3 `949d79e9` against main `185d998c`.** The corpus was main's: 1 907 units,
made of 1 489 files, 243 inline programs, 174 projects and std. The scanner found 297 units whose
outcome or codes changed. A unit that times out (120 s) under server load is reported as `timeout`,
distinct from a refusal; one earlier run had a base timeout on `examples/hmac_sha256`. Run with the same compiler on both
sides, the scanner reports no difference (exit 0).

- **Newly refused by exit 2i's codes:**
  - std: 21 E0301 sites in 6 files (`cli`, `fs`, `io`, `net`, `numeric`, `process`). Through std,
    every project that imports it is refused too (111 projects);
  - 15 standalone files with E0301 and 7 with E0302, among them `examples/snippets/linked_list.con`,
    `examples/snippets/borrow.con`, the bug 124 reproducer and the 087, 093, 094 and 105 regression
    fixtures;
  - 7 inline gate programs with E0301, in `check_transform_register` (3), `check_match_guard_effects`
    (2), `check_contract_negatives` and `run_tests`. Two more that were refused already changed to
    E0305 (`check_bool_kernel`).

  These are the sites train 1b's agent migrated by hand: commits `c13a83b8` (inline gate probes),
  `b52c1671`, and the std and examples migrations of exit 2i.
- **Code shifts in units that were already refused:** 113 rows whose candidate codes include an
  exit 2i code. For example, E0231 / E0232 /
  E0234 / E0238 became E0302 or E0303, and E0214 became E0301. These are the expected-diagnostic
  updates the manifest rows record.
- **Train 2's own rules:** the remaining newly refused rows carry E0287, E0208, E0229 and E0278.
  They come from the safety-evaluation batch, not from exit 2i.

The scanner does not see Concrete programs generated by Python gate code or the fuzzers, inline
programs that expand shell variables, or anything after LLVM IR (`llvm-as`, linking and running).
Those still need the full gate set.
