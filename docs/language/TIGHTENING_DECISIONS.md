# Language Tightening Decisions

**Status:** proposal and decision record, 2026-10-23. Items marked *recommend now* or
*decide now* wait for the user's answers in [§4](#4-questions-for-the-user) where one is listed.
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
| T-G9 | `&mut` argument modes (G9, B1, B2) | SEMANTICS | a parameter `&mut` is reborrowed at each call; a borrow-block `&mut` is consumed by its first call (E0205 on the second); both may lapse unused | B1: every `&mut` argument is an implicit reborrow; B2: an unused exclusive reference may lapse | decide now (Q1), implement later (R-0496 reborrow form) |
| T-G10 | owner reads under a shared borrow block (G10, B7) | STYLE | refused (E0203) | none: keep the freeze | deferred: a workload that needs owner reads under a named shared borrow (R-0499) |
| T-REF5 | borrow of a temporary: literal, call result, aggregate literal (G12) | HOLE | accepted. `&alloc(..)` segfaults and `&int_to_string(..)` leaks (bug 182). `&42`, `&mk()` and `&P{..}` of Copy type give 42 in both consumers. 79 corpus sites borrow a string literal | refuse a borrow of a non-place whose type is not Copy (E0238 REF-5); Copy temporaries and string literals are anonymous locals of the enclosing statement (REF-5@2) | now (Q2 picks the variant) |
| T-G13 | trait method declaration returning a reference (G13) | SEMANTICS | accepted; every implementation is refused | E0236 at trait method declarations | now (exit 2i) |
| T-G15 | `extern fn` with `*const &T` in its signature (G15) | SEMANTICS | result and parameter forms accepted; reading the inner reference is E0238 | E0575 looks through raw-pointer pointees, in parameters and results; 0 corpus sites | now (exit 2i, with R-0285) |
| T-G16 | reference-returning function type via an alias or a field (G16) | SEMANTICS | accepted (no value can inhabit it) | E0236 after alias expansion and in struct/enum field declarations; 0 corpus sites | now (exit 2i) |
| T-G17 | arm binder through `match &o` after its root moves (G17) | HOLE (not reproduced) | three variants refused, E0204 (train3: E0204, E0208) | none new; record the variants as reject rows and close the §15.7 row | now (record) |
| T-BIND | Copy-keyed binding mode through a borrowed scrutinee | HOLE + SEMANTICS | non-Copy payload binds `&P`, Copy payload binds `P`. A generic payload is `&T` to Check and `T` to Elab: segfault at `T := Int` (bug 181) | one binding mode carried in Core; Q3 picks the rule | contain now (exit 0b), decide now (Q3), rule with R-0496 |
| T-G19 | `noalias` / exclusivity exploitation; Core reborrow form (G19, B4, B5) | SEMANTICS | no `noalias` emitted | policy: no pass exploits `&mut` exclusivity before an aliasing model covering raw pointers exists | decide now (adopt sentence), implement later (R-0505, R-0496) |
| T-ARITH | checked arithmetic | settled | `+ - *` trap (exit 134; interp error); `/ %` by zero trap; shift count checked; `i8 100 << 1` is -56 by rule | none | nothing to tighten |
| T-CAST | lossy numeric `as` | SEMANTICS | `300 as u8` is 44 silently in both consumers | decided 2026-10-21 (ARITHMETIC_POLICY §8): `as` lossless only; lossy is a compile error after the named operations exist | decide now (done), implement later (exit 2k after 2h) |
| T-RAWDUP | trusted `*p` duplicates a non-Copy pointee (the raw-pointer generic leak, bug 046 shape) | HOLE | accepted with and without generics; binary exits 0, train3 `--interp` reports a use after free | `*p` by value of a non-Copy pointee is refused in trusted code (new code); an explicit `ptr::read_owned` marks the transfer | decide now, implement later (R-0014 → R-0514 / V6; scanner measures std first) |
| T-165 | `println` binds a user `print_int` (bug 165) | HOLE | binary SIGABRT; IR calls `@_C.m.print_int` | no language rule: generated calls bind by identity | now (exit 0b repair) |
| T-166 | foreign function value called with the Concrete ABI (bug 166) | HOLE (trusted boundary) | IR: `declare i64 @take_pair(i64)` but `call i64 %f(ptr %p)` | refuse taking as a value an `extern fn` whose signature has a `#[repr(C)]` aggregate (new FFI code) | now (containment); adapter or typed convention deferred (Q4) |
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
- **Recommendation.** Decide now (Q1). Implement with R-0496. Until then, the documented
  borrow-block consumption stays enforced.

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
  `String` has capacity 0 and owns no heap. The corpus borrows string literals at 79 sites and an
  integer literal at 1 site; it has no call-result or aggregate-literal borrows (a textual scan of
  `std`, `examples` and `tests`).
- **Class.** HOLE: a leak of a linear value and a miscompile, both in safe code.
- **Rule options.**
  - (A) REF-5@1 as written. Every non-place borrow is E0238, and the 80 literal sites migrate to
    named locals.
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
- **Recommendation.** Now, with C (Q2). It is also bug 182's exit 0b containment.

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
- **Recommendation.** Now: record these three as reject rows in `tests/rules/exit_2i.tsv` and close
  the §15.7 row. The ROADMAP 0b clause about G17 then names a closed shape. Reference-parameter
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
  accepted examples) changes under (A). Corpus migration under (A) is unmeasured: the scanner
  measures it from a candidate build that implements (A).
- **Recommendation.** Contain now under exit 0b with bug 181's narrow repair: Elab uses Check's
  binder type. Decide (A) or (B) now, and implement the rule with R-0496's explicit binding mode. I
  recommend (A).

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
  rule is implemented. A generated adapter, or a calling convention carried in the function type,
  is deferred (Q4) until a workload passes such a callback to C or stores one.

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

## 4. Questions for the user

Each question names the recommended option first.

1. **Q1 (T-G9, exit 2i B1/B2):** is every `&mut` argument an implicit reborrow, and may an unused
   exclusive reference lapse? *Recommended: yes to both.* This is a relaxation that breaks nothing,
   and it removes one of two modes. The alternative is that every `&mut` argument consumes, which
   refuses `twice` above and breaks every parameter used twice.
2. **Q2 (T-REF5):** which version of REF-5?
   - *Recommended: (C).* Refuse borrowing a non-place of non-Copy type (E0238). Copy temporaries
     and string literals are anonymous locals of the enclosing statement. 0 corpus sites break.
   - (A) Refuse every non-place borrow. 80 sites migrate.
   - (B) Destroy linear temporaries after the call. This is implicit destruction, which the stance
     rejects.
3. **Q3 (T-BIND):** how does a payload bind through a borrowed scrutinee?
   - *Recommended: (A).* Always `&T`, and a Copy payload is read with `*value`. This gives one
     mode, and nothing depends on Copy-ness after substitution.
   - (B) Keep the Copy-keyed mode, decided before substitution and carried in Core. This needs no
     migration and keeps two modes.
4. **Q4 (T-166):** for a foreign function used as a value, is containment by refusal enough until a
   workload needs it, or should the calling convention become part of the function type now?
   *Recommended: refuse now, and defer the convention.*

## 5. Implementation plan (HOLE and SEMANTICS items)

The order puts silent defects first. Then come rules that only refuse code no corpus program uses,
then rules that need the user's answer or a migration. Every rule lands with a versioned migration
diagnostic and manifest rows, as MEMORY_SEMANTICS §15.5 – §15.6 require. Before it lands, its
branch runs `scripts/tools/rule_impact.sh` against main, and the report goes in the change
description.

1. **Exit 0b containments and repairs**, independent of each other:
   - bug 183: `Lower` evaluates the right-hand side first;
   - bugs 136 and 137: Elab binds the functional-update base once and keeps enum fields in source
     order;
   - bug 181: Elab gives the binder Check's type;
   - bug 182: refuse a borrowed non-Copy non-place. This is REF-5 variant (C)'s refusal half, so it
     waits for Q2. If (A) is chosen, it is the first slice of (A);
   - bug 166: refuse the foreign function value;
   - bug 165: generated calls carry the runtime callee.
2. **Exit 2i closure rules, no corpus migration:**
   - T-G13: E0236 on trait method declarations;
   - T-G16: E0236 after alias expansion and in field declarations;
   - T-G15: E0575 looks through pointees;
   - T-G2: one predicate post-mono, and the completeness gate scans `Verify.lean`;
   - T-G17: record the reject rows and close the row.

   Each is a reject row plus a run row in `tests/rules/exit_2i.tsv`.
3. **Exit 2i decision records:** Q1 (B1/B2) written into MEMORY_SEMANTICS, MUT_REF_SEMANTICS,
   OWNERSHIP_MODEL and VALUE_MODEL; B7 recorded as deferred (T-G10); the G19 policy sentence in
   VERIFIED_PIPELINE.
4. **REF-5@2** (T-REF5) per Q2, with the anonymous-local semantics written into MEMORY_SEMANTICS
   §15.3.
5. **Binding mode** (T-BIND) per Q3, with R-0496's explicit Core binding mode. Measure the
   migration with the scanner first.
6. **Contract typing** (T-H27) under R-0473, before V1a's contract claims.
7. **Trusted move-out** (T-RAWDUP): add `ptr::read_owned`, measure with the scanner, migrate std,
   refuse. This is in V6's trusted-code slice and may be pulled earlier if a trusted defect needs it.
8. **Casts** (T-CAST): the exit 2k staging after exit 2h. Named operations come first, then the
   rejection with migration diagnostics, then the migration.

Items 1 and 2 can run in parallel branches. Items 4 and 5 wait for Q2 and Q3.

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
in the unit itself. A run over the whole corpus takes about 65 – 95 s with 16 jobs on the OVH
server.

**Validation: train3 `949d79e9` against main `185d998c`.** The corpus was main's: 1 891 units,
made of 1 489 files, 227 inline programs, 174 projects and std. The scanner found 297 units whose
outcome or codes changed.

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
