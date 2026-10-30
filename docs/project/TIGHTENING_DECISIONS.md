# Language Tightening Decisions

Moved from docs/language: a decision record (paused, owner decisions OD1 to OD22); the rules on main are stated in docs/spec, and the open questions are OPEN-1 to OPEN-22 of docs/spec/decisions.md.

**Paused (2026-10-25).** After this record was written, the project owner paused its decisions for
negotiation. Each is now an [owner decision](../../ROADMAP.md#owner-decisions) (exit 2n): it stays
proposed until the owner agrees to it again, explicitly, and only the owner decides it. The words
"decided" and "implemented" below record what was proposed and landed on 2026-10-23; they are not
the owner's current answer. Rules already on main stay as they are until the owner answers. The
implementation plan of §5 is inactive: no agent implements, encodes or extends anything from it
until the owner has answered the decision it rests on. The record below is kept as written.

**Status:** decision record, 2026-10-23. The four questions this record raised were decided by the
user on 2026-10-23 ([§4](#4-decisions-of-2026-10-23)), with a second set of decisions (D5 – D12,
[§4.6](#46-decisions-of-2026-10-23-second-set); D5, D8 and D10 revised the same day) and the
ordered plan of §5. D1 – D4, D6 and T-RAWDUP were implemented on 2026-10-23 (landing train 6); their
rules are REF-10, REF-5 and REF-11, REF-9, FFI-1, OWN-D1 and TYPE-14, and TRUST-7 in
[docs/spec](../spec/README.md). The other
recommendations stand as recommendations until their owners implement them.
This document does not restate rules. Each rule is normative in its canonical owner: references and
binding in [MEMORY_SEMANTICS §15](../archive/design-records/MEMORY_SEMANTICS.md), evaluation order in
[STATEMENT_EXPRESSION_MODEL](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment),
arithmetic and casts in [ARITHMETIC_POLICY](../archive/design-records/ARITHMETIC_POLICY.md), the C boundary in
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
([DESIGN_POLICY.md](DESIGN_POLICY.md)). Each candidate gets one class:

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
| T-G2 | post-mono reference check weaker than the front end's (G2 remainder) | HOLE (review, 2026-10-23) | front end refuses `T := *const &Int` in a return (E0236 on train3, no code on main); a transitive generic reaches `id::<*const &Int>` and passes | structural signature well-formedness plus the reference-storage restrictions, checked as generic constraints before specialization; the post-mono check is defense in depth (E0236) | now (exit 2i) |
| T-G9 | `&mut` argument modes (G9, B1, B2) | SEMANTICS | a parameter `&mut` is reborrowed at each call; a borrow-block `&mut` is consumed by its first call (E0205 on the second); both may lapse unused | **decided (D1):** every `&mut` argument is an implicit reborrow; an unused `&mut` may lapse | implement with R-0496 (Core reborrow form); 0 corpus sites break |
| T-G10 | owner reads under a shared borrow block (G10, B7) | STYLE | refused (E0203) | none: keep the freeze | deferred: a workload that needs owner reads under a named shared borrow (R-0499) |
| T-REF5 | borrow of a temporary: literal, call result, aggregate literal (G12) | HOLE (bug 182) | accepted. `&alloc(..)` segfaults and `&int_to_string(..)` leaks (bug 182). `&42`, `&mk()` and `&P{..}` of Copy type give 42 in both consumers | **decided (D2, Austral model):** only a variable is borrowed (a string literal is a static root); references to parts are projections through a reference; `&s.f` is sugar; loans keyed by root | now (exit 2i, closes bug 182); migration: 2 std sites (`map.con:712`, `cli.con:77`) and 1 train3 fixture |
| T-G13 | trait method declaration returning a reference (G13) | HOLE (well-formedness; REF-1 is normative) | accepted; every implementation is refused | the uniform signature well-formedness check on trait method declarations (E0236) | now (exit 2i) |
| T-G15 | `extern fn` with `*const &T` in its signature (G15) | SEMANTICS | result and parameter forms accepted; reading the inner reference is E0238 | E0575 looks through raw-pointer pointees, in parameters and results; 0 corpus sites | now (exit 2i, with R-0285) |
| T-G16 | reference-returning function type via an alias or a field (G16) | HOLE (well-formedness; REF-2 is normative) | accepted | the same signature well-formedness after alias expansion and in field declarations (E0236); a callback type whose parameters borrow stays legal; 0 corpus sites | now (exit 2i) |
| T-G17 | arm binder through `match &o` after its root moves (G17) | HOLE | moving shapes refused, E0204 (train3: E0204, E0208); Copy payload: reassigning the root in the arm is accepted (42) | moving shapes: none new; Copy case: refused under D3 (the binder is a loan on the root) | moving shapes **closed 2026-10-23** (fixtures `error_g17_*.con`); Copy case **reopened**, fixed with D3 |
| T-BIND | Copy-keyed binding mode through a borrowed scrutinee | HOLE + SEMANTICS (bug 181) | non-Copy payload binds `&P`, Copy payload binds `P`. A generic payload is `&T` to Check and `T` to Elab: segfault at `T := Int` (bug 181) | **decided (D3):** every binder through a reference scrutinee is a reference (`&T`), Copy or not | now (exit 2i, closes bug 181); migration: 2 fixtures, 0 std or example sites |
| T-G19 | `noalias` / exclusivity exploitation; Core reborrow form (G19, B4, B5) | SEMANTICS | no `noalias` emitted | policy: no pass exploits `&mut` exclusivity before an aliasing model covering raw pointers exists | decide now (adopt sentence), implement later (R-0505, R-0496) |
| T-ARITH | checked arithmetic | settled | `+ - *` trap (exit 134; interp error); `/ %` by zero trap; shift count checked; `i8 100 << 1` is -56 by rule | none | nothing to tighten |
| T-CAST | lossy numeric `as` | SEMANTICS | `300 as u8` is 44 silently in both consumers | decided 2026-10-21 (ARITHMETIC_POLICY §8): `as` lossless only; lossy is a compile error after the named operations exist | decide now (done), implement later (exit 2k after 2h) |
| T-RAWDUP | trusted `*p` duplicates a non-Copy pointee (the raw-pointer generic leak, bug 046 shape) | HOLE | accepted with and without generics; binary exits 0, train3 `--interp` reports a use after free | `*p` by value of a non-Copy pointee is refused in trusted code (new code); an explicit `ptr::read_owned` marks the transfer | decide now, implement later (R-0014 → R-0514 / V6; scanner measures std first) |
| T-165 | `println` binds a user `print_int` (bug 165) | HOLE | binary SIGABRT; IR calls `@_C.m.print_int` | no language rule: generated calls bind by identity | now (exit 0b repair) |
| T-166 | foreign function value called with the Concrete ABI (bug 166) | HOLE (trusted boundary) | IR: `declare i64 @take_pair(i64)` but `call i64 %f(ptr %p)` | **decided (D4, revised):** refuse **every** foreign function value now; long term, function types carry calling convention, required capabilities and safety obligations | refusal now (exit 0b); migration: 2 fixtures; the typed form with the first callback workload |
| T-ORDER | evaluation-order lag: 136, 137, 183 | HOLE | functional update base runs twice; enum literal fields in declaration order; deref assignment destination first (binary 12, interp 21) | rule exists (exit 2j); implementation lags | now (exit 0b repairs) |
| T-MAIN | `main`'s `Int` result outside 0–255 | STYLE (specified) | wrapper masks with `and 255`: 256 → 0, -1 → 255 | none now | deferred: the open surface decision (MAIN_EXIT_MODEL) |
| T-H27 | ill-typed contracts (KNOWN_HOLES H27) | SEMANTICS | `#[requires(x < 9999999999)]` on `i32` and `(x > 0) + 1 > 0` accepted | a pure, non-consuming specification judgment: operand sorts must agree (`(x > 0) + 1` is E0220); arithmetic and literals are mathematical unless the explicit machine operations are used, so `x < 9999999999` is well-typed | decide now, implement later (R-0473) |

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
- **Class.** HOLE (reclassified by review, 2026-10-23): a normative rule (REF-1) whose enforcement
  depends on where the type is reached. Producing a `*const &Int` takes trusted code, and no
  unsound safe program was found.
- **Rule.** Not "one predicate everywhere": a raw `tyContainsRef` test is the wrong shape, because a
  stored `fn(&T) -> Int` stores no reference. The rule is **structural type well-formedness** (a
  signature may not return a reference, at any depth, function types included) **plus the
  reference-storage restrictions** (REF-3: no reference held in an aggregate, heap cell or
  constant). On generics both are **constraints checked before specialization** on the type
  parameters, so a specialization cannot introduce a violation; the post-mono check repeats them as
  defense in depth (E0236 with pass `post-mono`). The completeness gate also scans `Verify.lean`.
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
  [D2](#d2-the-austral-model-borrow-a-variable-then-project-q2-ref-5-g12-revised-2026-10-23), which supersedes an earlier, cruder textual
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
- **Decided (D2, 2026-10-23):** first "only places are borrowable" (variant A with string
  literals as static places), then replaced the same day by the Austral model: only a variable is
  borrowed, a reference to a part is a projection through a reference, `&s.f` is front-end sugar,
  and loans are keyed by root. The options above are kept as the record of what was weighed. It
  closes bug 182.

#### T-G13. Trait method declaration returning a reference

- **Witness:** `trait Get { fn get(&self) -> &Int; }` is accepted on all three revisions, and every
  `impl` of it is refused.
- **Class.** HOLE (well-formedness): REF-1 is already normative for every signature, and the
  declaration escapes its check.
- **Rule.** The uniform signature well-formedness check (T-G2) runs on trait method declarations,
  E0236 (REF-1).
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
- **Class.** HOLE (well-formedness): REF-2 is normative and these spellings escape its check. The
  justification is uniform signature well-formedness, not that the types are uninhabited.
- **Rule.** The well-formedness check of T-G2 runs after alias expansion and on struct and enum
  field types: a function type may not *return* a reference. A callback type whose parameters
  borrow (`fn(&T) -> Int`) stays legal wherever it is written, fields included. Code E0236 (REF-2).
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
- **Moving shapes closed (2026-10-23); the Copy case is reopened by D3** (see §4, G17). The three
  moving shapes are rejection fixtures (`tests/programs/error_g17_*.con`,
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
  HOLE at the trusted boundary. **Decided (D4, 2026-10-23, revised after review):** taking **any**
  `extern fn` as a value is refused now, with a new FFI code allocated when the rule is
  implemented (the first proposal refused only signatures with a `#[repr(C)]` aggregate and is
  superseded). The long-term design is a function type that carries its calling convention,
  capabilities and safety obligations (`extern "C" fn`), built with the first callback workload.

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
- **Rule** (as refined by the review, §4.5, which supersedes the first wording). A contract is
  checked by its own pure, non-consuming specification judgment. Operand sorts must agree, so
  `(x > 0) + 1` (a `bool` as an arithmetic operand) is refused (E0220). Arithmetic and literals are
  **mathematical** (unbounded) unless the contract uses the explicit machine operations, so
  `x < 9999999999` on an `i32` is well-typed and means the mathematical comparison: it is not an
  E0227 literal-range error. Exit 2i's E0305 already makes contracts binder- and statement-free.
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

**Homes.** MEMORY_SEMANTICS §15.7 G9 and REF-10's rationale (the former MUT_REF_SEMANTICS §2 – §3) record the decision; R-0496
(Core reborrow form) implements it with the checker change in exit 2i. **Migration:** none.

### D2. The Austral model: borrow a variable, then project (Q2, REF-5, G12; revised 2026-10-23)

**Decision.** The user first chose "only places are borrowable" and then, the same day, replaced it:
**"ok do the austral model with `&s.f` sugar"**. The earlier "places" rule (recursive places by
provenance) is superseded and kept below only as the record of what was weighed.

**Core rule.** The core language has exactly two operations that form a reference:

1. **borrowing a variable**: `&x`, `&mut x`. A string literal counts as a static, immutable
   variable-like root: `&"text"` is a borrow of that root, and `&mut "text"` is refused;
2. **projecting through a reference**: a field `f`, an array element `[i]` (bounds-checked, as
   every index is, ARITHMETIC_POLICY §1), a newtype `.0`, and any sequence of these.

Nothing else forms a reference: not a literal other than a string literal, a call or method-call
result, an aggregate or array literal, an operator expression or a constant. A temporary is never
a root, so bug 182 stays closed. A reference value is therefore always **a root slot plus a
sequence of projections**. That one shape serves the checker, the interpreter, the extracted model
and the verified pipeline. Concrete has no `->` (removed 2026-07-14); projecting through a
reference `r` is written `&r.f` / `&r[i]`, and reading a field through it `r.f`, which
auto-dereferences one layer.

**Surface sugar.** `&s.f`, `&mut s.f`, `&a[i]` and `&s.inner.x` stay legal as pure syntax. Before any
checking, the front end rewrites them to "borrow the root variable, then project" (`&s.f` is the
projection `f` of `&s`). They add no semantic case and no typing or loan rule of their own.

**Loans are keyed by root variable only.** A loan on `&s.f` is a loan on `s`. This matches the
whole-owner freezing a borrow block already does (G10, E0203). Disjoint field borrows of one root in
one call (`upd(&mut s.a, &s.b)`, `add(&mut s.a, s.b)`) are therefore refused, where today they are
accepted (both return 42 on main `185d998c` and train3 `949d79e9`). Field-precise loans are
**deferred**, and their pull condition is a real workload that needs two simultaneous borrows of
different fields of one owner (or a field borrow beside a sibling read) and cannot be restructured
with a `let` or a borrow block.

**Rationale.**

- "What can be borrowed" becomes trivial: a name. There are no recursive place or provenance rules,
  and no field-of-temporary loophole (`&mk().f`).
- One projection rule serves both borrowed variables and reference parameters: `&s.f` and `&r.f`
  are the same projection, applied to `&s` and to `r`.
- The loan checker needs no path-overlap analysis (today's `borrowPathOf` / `pathsOverlap` keys
  shrink to the root).
- The uniform reference shape (root slot plus projections) is the pointer shape a CompCert-style
  block-and-offset memory model uses.
- Whole-owner freezing already makes a field-precise borrow add no power inside borrow blocks and
  arm binders, so the change loses only the one-call disjoint form above.
- Austral (Fernando Borretti) uses the same core model; see the verification below.

**Checked against the Austral specification** (<https://austral-lang.org/spec/spec.html>, read
2026-10-23; the page has no numbered sections, so sections are cited by heading):

- *Statements → Borrow Statement*: "If `X` is a variable of a linear type `T` …". The borrowed
  operand is a variable, and "the variable `X` cannot be used in `B`, i.e. while it is borrowed":
  the whole variable is frozen. `borrow! X as X' in R` is the read-write form.
- *Expressions → Borrow Expressions*: "If `x` is a variable of a linear type `T`", `&x` (read-only)
  and `&!x` (read-write) are anonymous borrows. "Borrow expressions cannot escape the statement
  where they appear"; binding one with `let` is rejected.
- *Expressions → Path Expression*: a path is a head followed by elements: a slot accessor `.s`, a
  pointer slot accessor `->s` and an array index `[i]` (with `i` of type `Index`). "A path that
  starts in a reference is a reference": `x->y->z` with `x : &[T, R]` has type `&[U, R]`. Its stated
  purpose includes "transforming references to records and arrays into references to their
  contents". "All paths must end in a path in the `Free` universe."
- *Linearity Checking*, rules 2.5 and 7: "A borrow statement does not count as using the
  variable", and "a borrowed variable cannot appear in the body of the statement that borrows it".

Corrections to the model as first stated: Austral has **no `&s.f` sugar**. Its borrow forms take a
variable only, and a reference to a field is obtained only by a path that starts in a reference, so
the sugar is Concrete's own front-end rewrite. Austral spells the projection `->`, which Concrete
does not have. The specification does not say whether a path's array index is bounds-checked;
Concrete's bounds check comes from its own rule. Austral also restricts paths to end in the `Free`
universe (no linear value is read out through a path); Concrete's equivalent is the H11 rule that a
non-Copy value is not projected out of a place by value.

**Caveats kept from the review.**

- A string literal's whole representation, the `String` descriptor as well as its bytes, is static
  and immutable. Borrowing it never permits moving out of it, destroying it, or obtaining mutable
  access (no operation turns the `&String` into an owned or mutable `String`).
- A raw-pointer dereference is not one of the two operations. In trusted or `Unsafe` code, `&*p`
  forms a reference only under its own validity premise (the pointer is aligned, live and
  initialized for its type), which that code's author discharges.

**Migration (measured).**

| change | sites | where |
|---|---|---|
| a non-variable operand is refused (temporaries are never roots) | 1 corpus site, plus 2 reproducers | `std/src/map.con:712` (`m.remove(&2)`) needs a `let`; bug 182's two reproducers become rejection fixtures; `error_092_const_ref_array.con` is already refused |
| field of a temporary (`&mk().f`) | 0 | |
| loans keyed by root: a projection borrow plus another use of its root in one call | 1 std site, plus 1 train3 fixture | `std/src/cli.con:77` (`self.offs.push(self.names.len())`: the `&mut` receiver loan on `self` overlaps the read of `self.names`; write `let n = self.names.len();` first); train3's `regress_2i_ref8_call_disjoint.con` (`two(&mut w.a, &mut w.b)`) |
| string literals | 77 | unchanged |

The counts come from scans of every borrow operand, every call's argument list and every
method-call receiver in `std`, `examples` and `tests`, plus 256 inline gate programs. Arm binders
through a projection (`match &s.f`) followed by a write to another field of `s` were not scanned
precisely. The implementing branch confirms all of it with `scripts/tools/rule_impact.sh` before
it lands.

**Homes.** MEMORY_SEMANTICS §15.1 (the root and projection shape, the string-literal root), §15.3
REF-5 (the two forming operations and the sugar) and REF-8 (loans keyed by root), §15.7 G12 and G10;
enforcement in exit 2i; bug 182 closes with it.

**Superseded first version (places), kept as the record.**

> **Rule.** A borrow `&e` / `&mut e` is accepted only when `e` is a canonical place (MEMORY_SEMANTICS
> §15.1). A string literal is a place: it denotes static, read-only storage, so `&"text"` borrows that
> storage, and `&mut "text"` is refused. Everything else is E0238 (REF-5@1): an integer, float, `bool`
> or `char` literal, a call or method-call result, an aggregate or array literal, an operator
> expression and a constant. The rewrite is an explicit `let`. There are no hidden anonymous locals
> and no temporary materialization in the formal semantics.
>
> **Rationale.** The alternatives each add a case: variant B destroys temporaries implicitly, which
> the 2026-10-21 stance excludes, and variant C keeps call- or statement-scoped anonymous locals,
> which the semantics would have to define (lifetime, place, which operand positions). Under D2 every
> reference has a named or static root, which is INV-REF's premise. Bug 182 (a `Heap` temporary
> miscompiled, a linear temporary leaked) is unwritable under it.
>
> **Migration (measured).** A scan of every borrow operand in `std`, `examples` and `tests`, comments
> excluded (`&e` where `e` is not a binding followed by field, index or `.0` projections, a
> dereference of one, or a parenthesized place):
>
> | operand | sites | disposition |
> |---|---|---|
> | string literal | 77 | unchanged: a static place |
> | integer literal | 2 | `std/src/map.con:712` (`m.remove(&2)`, a std test) needs a `let`; `tests/programs/error_092_const_ref_array.con` is already refused (E0239) |
> | call result | 2 | the two bug 182 reproducers, which become D2's rejection fixtures |
> | call, aggregate or array literal elsewhere | 0 | |
>
> So **one** corpus site needs an explicit `let`. The 13 parenthesized operands the scan also
> reported (`&mut (*p).a`, `&mut (p as &mut P).v`) are places. The inline gate programs (256 heredoc
> programs) have no non-place borrow. The implementing branch confirms the count with
> `scripts/tools/rule_impact.sh`.
>
> **Homes.** MEMORY_SEMANTICS §15.3 REF-5 (the string-literal place, the closed list of refusals)
> and §15.7 G12; enforcement in exit 2i; bug 182 closes with it.

### D3. A binder through a reference is a reference (Q3)

**Rule.** Every pattern binder that binds through a scrutinee of reference type (`match r`,
`match &o`, `if let`, `while let`) has reference type: a payload field of type `T` binds as `&T`,
and a whole-value binder over `&T` binds as `&T`, whether or not `T` is Copy. A Copy value is read
with `*value`. Literal and range patterns still compare the referenced value. **Scope (decided
2026-10-23):** D3's `&T` binder rule applies to match-style binders: `match`, `if let` and
`while let` arms. A destructuring `let` of a struct through a reference (`let P { a, b } = r;` with
`r: &P`) is refused by REF-6 (E0302), like any `let` that would hold a reference. There is no
exception: reference-holding binders live only in `match` arms and borrow blocks, so the one rule
"a `let` never holds a reference" stays without a new case. A field is read through a reference as
`r.a`, or bound by a `match`. An earlier wording of this scope proposed binding the fields as `&T`;
it is superseded.

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

**Rule now (revised after review, 2026-10-23).** Taking **any** `extern fn` as a value is refused,
with a new FFI code; a direct call is unchanged. The first version of this decision refused only
signatures with a `#[repr(C)]` aggregate and kept scalar-only externs usable as values, because
both conventions happen to pass scalars alike. The review rejected that exemption: it is a second
rule that rests on an accidental ABI coincidence, and it would force a migration when function
types gain conventions. The rewrite is a Concrete wrapper function that calls the extern directly
and is passed instead.

**Migration (scan of every `extern fn` name used other than as a direct call):** two sites, both
fixtures. `tests/programs/regress_129_fn_value_extern.con` (`let f: fn(i32) -> i32 = abs;`) becomes
a wrapper or a rejection fixture, and bug 166's reproducer becomes D4's rejection fixture. No std or
example site takes an extern as a value. The implementing branch confirms with `rule_impact.sh`.

**Long-term design.** The calling convention is part of the function type. Its sketch, normative
in [FFI.md](../platform/FFI.md#calling-convention-in-function-types-decided-2026-10-23-not-implemented):

- `fn(A) -> R` is a Concrete function type; `extern "C" fn(A) -> R` is a C function type. They are
  distinct types and never convert implicitly. A function type carries three things: its **calling
  convention**, its **required capabilities** (`with(..)`, as Concrete function types already do)
  and its **safety obligations** (whether a call needs `Unsafe`, that is whether the target was
  declared `trusted extern`).
- An `extern fn` taken as a value has type `extern "C" fn(..)`; a Concrete function never does.
- A call through an `extern "C" fn` value lowers with the C ABI (the same flattening a direct
  extern call uses), and needs what a direct call of that extern needs (`with(Unsafe)`, unless the
  declaration is `trusted extern`).
- A Concrete function that C must call through a pointer is exported explicitly (an adapter with
  the C ABI, `#[export_name]`); it is not a value of `extern "C" fn` type by coercion.

It is implemented when the first workload passes a callback to C or stores a foreign function
value; at that point the refusal above is lifted for `extern "C" fn` values, and a mismatch becomes a
type error between the two function types. The containment is a step toward the design: nothing
written under it needs to change.

**Homes.** FFI.md (the section linked above); R-0283 / R-0287 (ABI checks); bug 166 closes with the
refusal.

### G17: the moving shapes are closed; the Copy case is reopened by D3

The three moving shapes of G17 are refused on main (E0204) and are rejection fixtures
(`tests/programs/error_g17_match_borrow_root_moved_in_arm.con`,
`error_g17_if_let_borrow_root_moved.con`, `error_g17_match_borrow_heap_freed_in_arm.con`, wired in
`run_tests.sh`). The review reopened the **Copy case**: today a Copy payload binds by value, so
reassigning the root inside the arm is safe and accepted, but under D3 the binder is a loan on the
root, so that reassignment must be refused while the binder lives. The shapes below are the
acceptance fixtures of D3 (the implementing branch adds them as rejection fixtures; probed
2026-10-23):

| shape (Copy `P`) | main `185d998c` | train3 `949d79e9` | under D3 |
|---|---|---|---|
| `match &o { Some { value } => { o = None; out = value.n; } }` | accepted, 42 | accepted, 42 | refused (write to a loaned root) |
| the same with a guard `if value.n > 0` | accepted, 42 | accepted, 42 | refused; a failed guard ends its loan before the next arm |
| `match r` with `r: &mut Option<P>`, then `*r = None` and `value.n` | refused, E0205 (`r` moved by the match) | refused, E0205 | refused as a write to the parent while its shared payload loan lives (not as a move) |
| `defer show(value.n)` then `o = None` | accepted (binary prints 42) | refused, E0279 (projection in a deferred argument) | `defer show(*value)`-style forms keep the loan until the deferred call runs, so the write is refused |
| `while let Some { value } = &o { o = None; out = value.n; break; }` | accepted, 42 | accepted, 42 | refused; each iteration makes a new loan, ended at the iteration's end or at the exit |

### 4.5 Refinements from review (2026-10-23)

A Codex review of D1 – D4 kept Q1 – Q3 and revised Q4 (above). Its refinements are part of the
decisions; each is written into the rule's home when the rule is implemented.

**D1, `&mut` as an exclusive scoped loan.** A `&mut` argument is a child loan of its parent
reference or place. While the child lives, the parent is **suspended**: it may not be used, so
`f(r, r)` with two overlapping exclusive arguments is refused (today already E0293, "borrowed more
than once in the same call with at least one `&mut`", on main and train3), while sequential calls
`f(r); f(r);` are fine. This holds for method receivers, indirect calls through function values and
generic calls; the reborrows are recorded in Core before specialization, so a specialization cannot
change them.

**Defer.** A deferred call's arguments are read at the exit, not at registration (OWN-12,
decided 2026-10-21: arguments are local names, borrows of local names or constants; a local passed
by value is reserved and one passed by borrow is pinned). A loan a deferred call retains, a borrow
argument or a reference binder passed to it, **stays live until the deferred call runs**, so its
root stays frozen to the end of the registering block.

**Exit order.** On `return`, `break` or `continue` the order is: (1) the exit's value is evaluated;
(2) for each scope being left, innermost first, its deferred calls run in LIFO order while that
scope's loans are still live; (3) then that scope's loans end (arm binders, borrow-block binders),
and a borrow block's write-back, where its representation has one, happens at its loan's end;
(4) control transfers. The fall-through end of a block follows the same steps. An abort (a trap,
`abort()`, OOM) runs none of them: no deferred call, no loan ending (see the arithmetic note below).

**D2, places.** Superseded by the Austral model (D2 above), which keeps the two caveats that
still apply: a string literal's whole representation is static and immutable and cannot be moved
out of, destroyed or made mutable, and a raw-pointer dereference needs its own trusted validity
premise. The recursive, provenance-based place definition is no longer needed: only variables are
borrowed, and everything else is a projection through a reference.

**D3, binding mode.** The binding mode of every binder is carried explicitly through Core and
through specialization, not recomputed from the instantiated type. Reading a Copy payload with
`*value` copies only under an established Copy constraint (a concrete Copy type or a `T: Copy`
bound); for an unconstrained `T` it is refused like any other move out of a borrow. Matching
through `&mut T` creates **shared** payload loans and suspends conflicting use of the parent
reference while they live. The rule covers nested patterns (each binder at any depth is a loan),
whole-value binders, guards (a guard's loans are the arm's; a failed guard ends them before the
next arm is tried) and `while let` (each iteration's loans end at the end of that iteration).

**T-RAWDUP, moving out of raw memory.** `ptr::read_owned(p)` consumes an initialized slot and
leaves it **logically uninitialized**: the slot must be written before it is read or dropped again.
A move out through `&mut T` must restore a valid `T` before the loan ends, or use an explicit
`replace` / `take` operation that does both at once. An arbitrary raw address carries a trusted
ownership obligation (who owns the pointee, and that it is initialized). The rule distinguishes
three operations on a slot: **initialization** (an uninitialized slot receives a value),
**replacement** (an initialized slot's value is taken out and another put in) and **disposal** (the
value is destroyed and the slot left uninitialized).

**T-H27, contracts.** A contract expression gets its own **pure, non-consuming specification
judgment**: it may read parameters and `result` but never moves, consumes or borrows mutably, and
it calls only spec functions. Its arithmetic is **mathematical** (unbounded integers) unless it uses
the explicit machine operations, so a machine-vs-mathematical distinction is part of the judgment
rather than an accident of extraction.

**T-ARITH, abort.** A trap is modelled explicitly as abort: the process ends, and no deferred call,
destructor or loan ending runs (the spec's "Abort and failure" (docs/spec/statements.md)'s abort-only model). Every "exactly once cleanup"
claim in these rules is therefore qualified: exactly once on every path that does not abort.

**T-CAST, lossless.** Losslessness is defined by **value domains**: a conversion is lossless when
every value of the source type is a value of the target type (so `i32 → i64` and `u8 → i32` are,
`i64 → f64` is not). The named conversions specify NaN, positive and negative infinity, and the
rounding mode for every float case (ARITHMETIC_POLICY §8 is the home).

**T-G19, exclusivity.** No pass, not only LLVM `noalias` emission, may exploit `&mut` exclusivity
(reordering, caching or eliminating loads and stores on the assumption that a `&mut` does not
alias) before an aliasing model that covers raw pointers exists. The operational loan rules of D1
and D3 are still defined now, so the checker and the formal model agree on which programs are legal.

**Validation.** Agreement between the interpreter and the native binary is necessary and not
sufficient: both can implement the same wrong rule. Each rule's run fixtures compare both against
the **specified transitions** (the expected outcome written from the rule, or the extracted model
where it covers the construct), as exit 2c's golden-oracle policy already requires.

### 4.6 Decisions of 2026-10-23, second set

The user extended the instruction to the rest of the discussion: **"the best long term solution
for everything we discussed"**. These are decided as recorded here; each is written into its rule
home when its slice is implemented (§5).

**D5. Generic kinds: bare `T` is Any (revised).** The first wording ("two kinds, Copy and Any,
always declared") was revised by the user the same day: **"yes, keep bare T as Any"**. A bare type
parameter `<T>` means **Any**: linear-capable, so the body must use each value of type `T` exactly
once. That is today's behaviour, now normative. `T: Copy` stays an explicit bound and is the only
kind annotation; there is no Linear-only kind. *Rationale:* the bare-`T` rule is already one clear
rule, and Copy is the only property that changes what a body may do, so mandatory annotations would
add syntax without removing a semantic case. Bug 181 is fixed by storing the binder mode in Core
(D3), not by kind annotations.

**D6. Copy is explicit.** A struct or enum, generic or not, is Copy only when declared `Copy`, and
the declaration is checked (every field Copy, under the declared kinds of its parameters). Copy is
never derived from the fields of an instantiation. *Rationale:* it removes the conditional-Copy
fixpoint Mono computed (`Concrete/IR/Mono.lean`, "iterate to a fixpoint … either kind's
Copy-ness can depend on the other's"), so Copy-ness is a fact of the declaration, not of a type
graph. *Refined 2026-10-23 (option (a)) and implemented the same day:* bare parameters keep kind
Any, so the declaration is checked once with its type parameters taken as Copy, and an
instantiation of a generic declared `Copy` is Copy exactly when every type argument is Copy, as a
derived `Copy` is ([OWN-D1](../spec/ownership.md#OWN-D1), [TYPE-14](../spec/types.md#TYPE-14)).

**D6 refined (a), 2026-10-23.** A `Copy` declaration on a generic struct or enum means **"Copy when
every type parameter is Copy"**: `struct Copy Pair<A, B> { .. }` makes `Pair<X, Y>` Copy exactly when
`X` and `Y` are Copy. This is decided from the declaration alone, with no type-graph fixpoint. The
declaration is checked once: every field must be Copy under the assumption that every parameter is
Copy. The builtin `Option` and `Result` are declared this way, so `Option<Int>` is Copy and
`Option<String>` is not. A generic that is not declared `Copy` is never Copy, whatever its
arguments. *Rationale:* it matches Rust's derived `Copy`, it is invariant under substitution (the
answer for an instantiation follows from the declaration and the arguments' own Copy-ness, both
known before specialization), and it keeps `Option<Int>` Copy without a fixpoint.

**D7. Trait coherence.** One impl per (trait, type), and it lives in the module of the trait or of
the type. A method reachable through two bounds must be called qualified (`Trait::method(x)`),
otherwise the call is refused. Method resolution is a lookup by typed identity (the trait's and the
type's resolved identities), never by spelling. *Being implemented on branch `trait-coherence`.*

**D8. Global state (revised).** The first wording ("no mutable globals in source at all") was
replaced by the user: **"unsafe i agree with the module and global state too with your
proposal"**.

- **Immutable globals** (`const`, and immutable statics) are allowed everywhere.
- **Mutable global state** exists only as **declared global cells**. A cell is defined in an
  unsafe-marked module (D10) and is reachable only through functions that require a capability:
  `with(Alloc)` for allocator state, or a named capability per cell. Safe code never touches a cell
  implicitly, and the dependency is visible in every signature on the path to it.
- **Semantics:** each declared cell is part of the world state its capability grants, modelled the
  way the process environment is modelled for `with(Env)`.
- There is no unrestricted `static mut`.
- **Trusted base now:** the globals the compiler emits (`__concrete_argc`, `__concrete_argv`,
  `Concrete/Backend/EmitSSA.lean`) and std.env's mutation of the process environment are inventoried
  as trusted base, with a gate that fails on any other mutable global or environment mutation.
- Global cells are built when a workload pulls them (allocators are the likely first); their shape
  is decided now, above.

**D9. Modules and interfaces.** One source file per module, with `pub` as its interface. Interface
artifacts are generated by the compiler, never written by hand (the user rejected separate
interface files). std's interface artifacts are committed and CI-checked, and dependents compile
against them. The first step, scheduled late (§5 step 6), is splitting interface data out of
`FileSummary`.

**D10. Unsafe-module marker in source (revised; replaces the allowlist).** Decided with D8 ("unsafe
i agree with the module …").

- **Syntax.** A module declaration takes the existing `trusted` prefix: `trusted mod ffi { ... }`.
  The grammar already admits it (`grammar/concrete.ebnf`: `top_decl` reads `'trusted'
  decl_after_trusted`, and `decl_after_trusted = decl_keyword`, which includes `'mod'`), so the
  marker adds no token and keeps the LL(1) property. The implementing branch confirms it with the
  LL(1) gate and gives the parser's acceptance its meaning. Under D9 (one file per module), the
  marker is on the module's declaration.
- **Rule.** Only a marked module may contain `trusted` functions, `extern` declarations,
  raw-pointer operations and D8's global cells. A marked module may also contain safe code.
- **The marker never confers trust.** Each function still declares `trusted` for the pointer
  operations it performs, and its `with(Cap)` obligations, `Unsafe` included, still apply.
- **Visibility.** A gate lists every marked module, so the trusted base can be read off the module
  tree.
- **Rationale.** It replaces the separate allowlist file the first wording proposed, so the fact
  lives in one place, the source, where a reviewer reads it.

**D11. Specification architecture.** The Core semantics in Lean is **normative**. `docs/spec` (its
pilot is on branch `wip/spec-single-source`) is its checked, readable view. Each rule in the view
links its Lean definition, its examples are checked against that definition, and each rule is
marked **tested**, **proved** or **trusted**. Authority moves per migrated chapter: once a rule's
chapter is in the checked view, its Lean definition is normative, and a disagreement with the prose
fails the check. Until its chapter migrates (§5 step 3), a rule's prose home named at the top of
this record stays normative.

**D12. Disciplines.** These apply to every slice:

- every pass states what it preserves (the property its output keeps from its input);
- identity resolution fails closed: an unresolved identity is an error, never a fallback by name;
- every safety gate has a mutation family that proves it can fail;
- interpreter and native agreement is checked against the specified semantics (D11), not only
  against each other (§4.5, validation).

## 5. Implementation plan (HOLE and SEMANTICS items)

The order, decided 2026-10-23 with D5 – D12, is below. Within a step, silent defects come first.
Every rule lands with a versioned migration diagnostic and manifest rows (MEMORY_SEMANTICS §15.5 –
§15.6). Before it lands, its branch runs `scripts/tools/rule_impact.sh` against main, and the report
goes in the change description. The soundness-holes agent implements these; this record is where
the two coordinate.

**Alongside, at any point:** the exit 0b repairs that need no decision: bug 183 (`Lower` evaluates
the right-hand side first), bugs 136 and 137 (functional-update base once, enum fields in source
order), bug 165 (generated calls carry the runtime callee).

1. **Traits (D7)**, on branch `trait-coherence`.
2. **The reference and kind core**, in this order:
   1. **The D3 binder in Core** (closes bug 181). The binder's mode is explicit in Core, `&T`
      through every reference scrutinee, and carried through specialization. The two
      `patterns/match_ref_*` fixtures migrate, and bug 181's reproducer becomes a run fixture.
      G17's Copy-case shapes (§4: root reassigned, guard, `&mut` parent, defer, `while let` with
      early exit) become rejection fixtures, with run controls.
   2. **Explicit Copy (D6)**, with D5's bare-`T`-is-Any rule written down as normative (no syntax
      change). Mono's conditional-Copy fixpoint is removed.
   3. **The D2 Austral references** (REF-5, E0238; closes bug 182 and G12):
      - `&s.f` / `&a[i]` is rewritten to borrow-then-project before checking;
      - only a variable or a string literal is borrowed;
      - loans are keyed by root;
      - migrate `std/src/map.con:712`, `std/src/cli.con:77` and train3's
        `regress_2i_ref8_call_disjoint.con`;
      - bug 182's reproducers become rejection fixtures.
   4. **The D1 loans**, with R-0496's Core reborrow form:
      - one `&mut` argument mode;
      - parent suspension (E0293 kept);
      - reborrows recorded before specialization;
      - the defer and exit-order rules of §4.5, written into OWN-12 and MEMORY_SEMANTICS;
      - the G19 policy sentence (every pass) into VERIFIED_PIPELINE.

      B7 stays deferred (T-G10).
   5. **The D4 refusal** of every foreign function value (closes bug 166; 2 fixtures migrate).

   The exit 2i well-formedness rules ride with the first of these slices that touches signatures:
   - T-G2: signature well-formedness and the storage restrictions as constraints checked before
     specialization, with post-mono as defense in depth; the completeness gate scans `Verify.lean`;
   - T-G13 and T-G16: the same check on trait declarations, after alias expansion and on field
     types;
   - T-G15: E0575 looks through pointees.
3. **The spec pilot (D11)**, then chapter-by-chapter migration of the language docs into the
   checked view.
4. **The trusted-base slice:**
   - the `trusted mod` marker and its gate listing every marked module (D10);
   - D8's global-state rule: immutable globals everywhere, no `static mut`, the inventory and gate
     for the compiler's globals and std.env (cells themselves wait for a workload);
   - fail-closed identity resolution (D12) across the compiler.
5. **Moving out of raw memory, contracts and casts**, each keeping its staging:
   - T-RAWDUP: add `ptr::read_owned` with the initialization / replacement / disposal distinction
     (§4.5), measure with the scanner, migrate std, then refuse a by-value `*p` of a non-Copy
     pointee, in one change;
   - T-H27: the contracts' specification judgment (R-0473), landed **before V1a states claims with
     contracts**;
   - T-CAST: exit 2k after exit 2h. First the named operations (with value-domain losslessness and
     specified NaN, infinities and rounding), then the rejection with versioned migration
     diagnostics, then the migration.
6. **Generated interfaces (D9)**, starting with splitting interface data out of `FileSummary`.

D4's typed calling convention (`extern "C" fn`) is built with the first callback workload, at
whatever point that arrives.

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
