# Bug 091: a trait impl's extra capabilities were hidden by generic dispatch

**Status:** FIXED
**Found:** 2026-10-18, checker bug hunt (probes `c2.con` and control `c2b.con`).
**Severity:** high. A program performed an effect with no capability declared anywhere on the
call chain: capability discipline, the property every caller relies on, did not hold.

## Reproducer

```
trait Act { fn act(&self) -> Int; }
struct Loud { v: Int }
impl Act for Loud {
    fn act(&self) with(Std) -> Int { println(777); return self.v; }
}
fn run<T: Act>(x: &T) -> Int { return x.act(); }
fn pure_caller() -> Int {
    let l: Loud = Loud { v: 3 };
    let r: Int = run::<Loud>(&l);
    let Loud { v } = l;
    return r;
}
fn main() -> Int { return pure_caller(); }
```

This compiled and printed `777`. `main`, `pure_caller` and `run` declare no capabilities. Calling
`l.act()` directly from `pure_caller` (the control) was refused with E0520.

## Cause

Three decisions were each missing, and together they opened the hole:

1. **Impl conformance compared only return types.** Trait impl validation
   (`Concrete/Check/CoreCheck.lean`, module validation step 8) checked that each trait method
   existed and that its return type matched. It never compared the impl method's capability set
   with the trait method's, so an impl could declare more authority than its trait.
2. **A call through a trait bound charged nothing.** Check's type-variable receiver path
   (`x.act()` with `x: &T`, `T: Act`) looked up the trait signature for argument and return types
   but never compared the trait method's `with(..)` with the caller's set. The call reaches Core as
   `T_act`, a name CoreCheck cannot resolve, so it was not checked there either.
3. **Nothing re-asked after monomorphization.** Mono rewrites `T_act` into a direct call to
   `Loud_act` inside `run_for_Loud`, and no later pass checked capabilities.

The rest of the signature was not compared either: the receiver form and parameter types could
differ from the trait's, and a trait method's own type parameters (`fn act<U>(..)`) were parsed
and dropped.

## Fix

- **Impl capabilities are bounded by the trait (E0586).** An impl method's capability set must be
  a subset of its trait method's declared set. This fails closed: when the trait method declares
  no `with(..)`, the impl may declare none. The builtin `Destroy` keeps its own bound, the H18 v1
  fence (E0584, at most `Alloc`).
- **The whole signature must conform (E0587).** The receiver form (`self`, `&self`,
  `&mut self`, or none), the non-receiver parameter count and types, with `Self` resolved to
  the impl type, aliases resolved and impl type parameters as type variables, must match. An impl
  method that declares its own type parameters or capability parameters is refused, because a
  trait method signature cannot declare either. `CTraitMethodSig` and `CTraitImpl` now carry
  these facts across the AST to Core boundary (`Concrete/Elab/Core.lean`, built in `Elab.lean`,
  canonicalized in `CoreCanonicalize.lean`).
- **A call through a trait bound costs the trait method's capabilities (E0240).** Check's
  type-variable receiver path now requires the caller to hold the trait method's declared set,
  `Unsafe` included, since nothing downstream sees it. A trusted body discharges `Unsafe`, as it
  does at a direct call. Under the H18 v1 design (RUNTIME_COLLECTIONS.md rule 4, "v1 glue is
  `with(Alloc)`") the builtin `Destroy::destroy` is bounded by `Alloc`, so `x.destroy()` through
  `T: Destroy` now requires `Alloc`.
- **Generic trait methods are refused at parse.** Their type parameters are no longer silently
  erased.
- **Defense in depth: a post-mono capability verifier.** `verifyCapsPostMono`
  (`Concrete/Check/Verify.lean`, part of `verifyPostMono`) re-checks the specialized call graph.
  Every direct call in a non-generic function must require only capabilities its caller declares.
  It is conservative and skips `Unsafe` (trust discharge belongs to CoreCheck), polymorphic or
  capability-parameter sets, and callee names with disagreeing homonyms, so it reports only real
  violations, as internal error E0601. To confirm it is load-bearing, E0586 was disabled and the
  reproducer was compiled again. The verifier refused it:
  `[post-mono] run_for_Loud: call to 'Loud_act' requires capabilities (...) that 'run_for_Loud'
  does not declare`.

### Standard library change

std declares no user traits. Its trait impls are all `Destroy`, and every one stays within
`Alloc`. The new trait-bound charge found two real instances of this bug's shape in std:
`HashMap::remove` (`std/src/map.con`) and `OrderedMap::remove` (`std/src/ordered_map.con`) destroy
the removed key through `K: Destroy` (`String`'s destructor deallocates) without declaring `Alloc`.
Both now declare `with(Alloc)`. So does `OrderedSet::remove`, which calls `OrderedMap::remove`.
`HashSet::remove` already declared it.

## Not changed (reported)

- A trait impl block's own `with(..)` (`impl Act for Loud with(Console) { .. }`) is parsed and
  then ignored: it reaches only the formatter and confers nothing on the methods. No program in
  the tree uses it.
- Generic traits (`trait Foo<T>`) keep their type parameters in the AST, but an impl's trait type
  arguments (`impl Foo<Int> for X`) are parsed and discarded. Conformance for parameters that
  mention a trait type parameter cannot be decided until that information is kept. No program in
  the tree declares a generic trait.
- Trait method declarations from imported modules: Check's type-variable path and CoreCheck's
  conformance check see only the current module's traits plus `Destroy`. Neither is new; the
  post-mono verifier covers the capability consequence program-wide.

## Gate

- `scripts/tests/run_tests.sh` (`--filter trait`) has these rows:
  - `error_trait_impl_caps_exceed_generic.con`: the reproducer, E0586.
  - `error_trait_impl_caps_exceed_direct.con`: the control, E0586.
  - `error_trait_bound_call_missing_cap.con`: the trait declares `Console` and the generic caller
    lacks it, E0240.
  - `error_trait_impl_sig_mismatch.con`: receiver and parameter type, E0587.
  - `error_trait_method_generic.con`: refused at parse.
  - `trait_impl_caps_declared.con`: positive control. The trait declares `Console`, one impl uses
    it, another declares less, and the generic caller holds it. It prints `3` and exits `7`.
- `scripts/tests/check_corecheck_boundary.sh` has class 4b: the smuggling program must not reach a
  binary.

## Follow-up: self-review of 423e2c64

* **Self-spelled impl signatures were refused.** Conformance (E0587) resolved `Self` on the
  trait side only, so `fn same(&self, other: &Self)` in an impl was "expected &P, got
  &Self"; a `-> Self` return was likewise E0582 (that half predates this bug). The impl
  side now resolves `Self` the same way. Fixture `tests/programs/trait_impl_self_spelling.con`
  (42).
* **verifyCapsPostMono** looked every callee up with a scan of all functions (quadratic in
  program size); definitions are indexed by name once.
* Confirmed sound as shipped: the impl-caps subset check fails closed (a trait method with
  no `with(..)` admits only impls with none), the bound charge at a `T: Trait` call runs
  before Core, the post-mono pass skips only what it cannot decide (cap variables, cap
  params, homonyms with different sets, `Unsafe`), and the three std `remove` signatures
  gained `with(Alloc)` because they destroy a `K: Destroy` key. Bug 052's synthesized
  array glue carries `Alloc` for the same reason, and its fixtures declare it.
* Trait-method calls on a type parameter are now spelled `T$m` in Core (bug 052
  follow-up); comments referring to `T_method` were updated.

## Codex round 2

* **Nested-module Destroy impls escaped the Alloc fence (P1).** The H18 fence (E0584) looked
  up `Type_destroy` among the module's functions, but `prefixModuleFnNames` had renamed a
  nested module's destructor `sub_Type_destroy`, so the lookup missed and the fence passed; a
  `with(Console)` destructor then ran under generic `x.destroy()` from a caller holding only
  `Alloc`. The fence now reads the impl's own method signature (`ti.methodSigs`). Mono's typed
  destructor dispatch names the RESOLVED definition (`CTraitImpl.fnPrefix` /
  `implMethodFnName`) instead of the `Type_destroy` alias, and `verifyCapsPostMono` resolves
  alias spellings itself (every alias any module declares, deciding only when all candidate
  definitions agree). Mono does not re-spell other non-generic calls: its program-wide alias
  pool is first-match, and re-spelling broke regress_039 (the backend prefers a module's own
  aliases). Same-named types with Destroy impls in several modules: the top-level impl wins
  (the old exact-name rule); otherwise an ambiguous choice is refused (E0814).
  Fixtures: `tests/programs/error_091_nested_destroy_caps.con` (E0584; previously compiled and
  printed 7 from the Alloc-only `quiet`), pipeline-test `mono/nested-destroy-verified` (the
  verifier names the widened nested destructor, exact and through its alias; failed before).
* **Generic trait calls ignored the receiver form (P1).** A call through a trait bound
  checked and consumed the explicit arguments but never applied `sig.selfKind`: with
  `consume(self)`, `x.consume(); return x;` returned an owner its impl had already destroyed
  (the fixture destroyed one value twice). Check now applies ONE receiver rule to concrete and
  bound calls (`consumeReceiver` / `receiverBorrowParts`: borrow conflicts, by-value move,
  projection restriction), and Elab shapes the bound receiver by the trait's form (an owned
  `y: T` calling `&mut self` is borrowed; it was passed by value, so the impl mutated a copy:
  4020410 instead of 4030412). Unifying the rule also closed the concrete sibling: a by-value
  `self` method called through `&T` on a non-Copy type moved out of a borrow (double destroy);
  now E0290. Fixtures: `error_091_generic_receiver_moved.con` (E0205),
  `error_091_receiver_through_ref.con` (E0290), `trait_generic_receiver_forms.con` (4030412).
  The interpreter does not run non-Destroy trait calls on type parameters before this round
  (see bug 052's round-2 note), so these legs are compiled-only.
* **Return-position `Self` dropped the impl's type arguments (P2).** The return check
  resolved `Self` with `tyFromName ti.typeName` (bare `Cell`), while parameters used
  `ti.selfTy` (`Cell<T>`), so a generic impl returning `Cell<T>` for `-> Self` was a false
  E0582; return types also skipped the alias normalization parameters get (`-> Num` vs
  `-> i32`). Both return signatures are now `sigTyNormalize`d in Elab and `Self` resolves to
  `ti.selfTy`. Fixtures `trait_impl_generic_self_return.con` (42; was E0582) and
  `error_091_generic_self_return_mismatch.con` (a genuinely wrong `Cell<i64>` is still E0582).
* **Gate regression: E0587 refused the canonical by-value destructor.** Conformance compared
  the receiver exactly against the builtin signature's `&self`, so `impl Destroy for File {
  fn destroy(self) { let File { fd } = self; } }` — the form OWNERSHIP_MODEL.md (H17) gives
  for a terminal consumer, used across the linear gates and `pressure_destroy_wrapper.con` —
  was rejected. Destroy admits `self` and `&self` (std's collections use `&self`); `&mut self`
  stays E0587. Fixtures `trait_destroy_by_value_receiver.con` (2, compiled and --interp,
  through generic `x.destroy()` and `destroy(t)`) and `error_091_destroy_mut_receiver.con`.
  **Superseded in round 3:** that fixture's `2` was one owner destroyed twice (see below).
* **Gate regression: admission lost its typeParamDispatch reason.** The assumption summary
  recognized a method call on a type parameter by the old `T_` prefix; with the reserved
  `T$m` spelling `main.show` was refused as an UNRESOLVED call. The summary now classifies
  `T$m` (`splitTraitParamCallee`, the type parameter checked against the function's own) and
  the builtin destructor dispatch in a generic body as `typeParamDispatch`
  (`check_proof_admission.sh`: was 15/17, now 17/17). The fixture's `show` takes `p: &T`: an
  owned `p: T` calling `describe(&self)` is now elaborated as a borrow (round 2 receiver fix),
  which extraction does not support, and that leg is about admission, not extraction.

## Round 3 (Codex review of batch 2)

* **`Destroy::destroy` consumed through a shared borrow (P1).** Admitting the by-value
  receiver left generic `x.destroy()` checked against the builtin trait's `&self`, so
  `kill<T: Destroy>(x: &T) { x.destroy(); }` ran an owner's destructor while the owner stayed
  live; `destroy(t)` then ran it again (`trait_destroy_by_value_receiver.con` printed 2 for
  ONE owner — a double free for a resource-releasing destructor). The same hole existed for a
  concrete `t.destroy()` through `&T`, and an owned `t.destroy()` on a `&self` impl did not
  consume `t` at all. `Destroy::destroy` is now the public destruction entry point with a
  CONSUMING ownership contract whichever receiver the impl declares (`consumeDestroyReceiver`
  in Check, both the bound and the concrete path): an owned receiver is moved; a reference
  receiver is E0290, except in `trusted` code — internal drop glue such as std's
  `(p as &T).destroy()`, whose owner the container already consumed. Methods of a `trusted
  impl` now count as trusted in Check (as Elab already recorded them). Dispatch is unchanged:
  Elab passes `&x` and Mono adapts to either impl receiver. Fixtures: the old fixture is now
  `error_091_destroy_through_ref.con` (E0290), plus `error_091_destroy_concrete_through_ref.con`
  (E0290), `error_091_destroy_method_then_destroy.con` (E0205) and
  `trait_destroy_owned_receiver.con` (11111: five owners, each destroyed exactly once, through
  both impl receivers, generic and concrete, compiled and --interp). The bug-052 fixtures whose
  `kill(x: &T)` is drop glue of an already-consumed owner mark it `trusted`;
  `interp_destroy_table.con` and `error_091_nested_destroy_caps.con` pass ownership instead.
* **`&mut self` through a shared reference built `&mut &T` (P1).** Round 2's receiver shaping
  fell through to `borrowMut cObj` for a `&T` receiver, so `fn f<T: Bump>(x: &T) { x.bump(); }`
  passed `P_bump` an alloca holding one pointer, and the impl's write to the second field hit
  offset 8 of that slot (out of bounds); --interp failed with `undefined function 'T$bump'`.
  The concrete path had the same shape (`x.bump()` on `x: &P` aborted), and neither path
  checked the implicit borrow at all: `let p = ..; p.bump();` mutated an immutable binding.
  Check now applies the explicit-borrow rules to every implicit receiver borrow
  (`checkImplicitReceiverBorrow`, concrete and trait-bound alike): `&mut self` through a
  shared reference — the receiver, `self` in a `&self` method, or the binding a projection is
  rooted at — is the new E0237; on an immutable binding E0233; while the binding is borrowed
  E0232; a `&self` receiver under a live `&mut` E0231. Elab refuses to build `&mut &T` as
  containment (same code). Fourteen std bindings that were mutated through `&mut self`
  methods without `mut` (fmt, hex, io, path, string) are now declared `let mut`. The
  interpreter forwards a reference-typed projection base's path (`w.p` with `w: &mut W`), so
  every legal receiver form agrees with the binary. Fixtures: `error_091_mut_receiver_*`
  (through_ref, through_ref_concrete, through_ref_projection, in_ref_self: E0237; immutable,
  immutable_concrete: E0233; borrowed: E0232) and `trait_mut_receiver_forms.con` (8, compiled
  and --interp; `trait_generic_receiver_forms.con` now also agrees under --interp).
* **Codex review of round 3: other spellings of the same two holes (P1).** `(u).destroy()`
  did not consume `u` and `(*t).destroy()` / `(*r).take()` moved out through `&T`
  (`consumeReceiver` now strips parentheses and refuses a dereference of a reference);
  `ByRef::destroy(&u)`, `ByRef_destroy(&u)` and a function value of `ByRef_destroy` ran a
  `&self` destructor through a borrow (`checkDirectDestroyImplUse`: E0290 outside trusted
  code); `(*(w)).p.bump()` escaped E0237 because the place walk recognized only `*ident`
  (`placeVia` now looks through parentheses, projections and dereferences alike). Fixtures
  `error_091_destroy_paren_then_destroy.con` (E0205), `error_091_destroy_static_borrowed.con`,
  `error_091_destroy_deref_ref.con`, `error_091_destroy_defname_borrowed.con`,
  `error_091_receiver_deref_moved.con` (E0290), `error_091_mut_receiver_paren_deref.con` (E0237).
  A `&`/`&mut` borrow or a cast to a reference inside the place (`(*(&p)).bump()`,
  `(*(&r)).take()`) is the reference the place is reached through, not a temporary:
  `error_091_mut_receiver_deref_borrow.con` (E0237), `error_091_receiver_deref_borrow_moved.con`
  (E0290).
  The place walk fails closed: a place reached through an expression it does not classify
  (an `if`/`match` that may evaluate to `&r`) counts as reached through a shared reference;
  only bindings, projections, dereferences, borrows, casts and owned temporaries (calls,
  literals) are classified. `error_091_destroy_deref_if_ref.con` (E0290),
  `error_091_mut_receiver_deref_if_ref.con` (E0237).
* **Codex review of round 3, second pass.** `w.0.destroy()` with `w: &W` took the owned-newtype
  whole-owner exemption; `&mut (*r)` manufactured exclusivity through `r: &P`; `&(p)` lost its
  borrow provenance; and the EXPLICIT `&mut` rule matched only a bare identifier, so `&mut (*r)`,
  `&mut (p)` (immutable) and `&mut w.p` (`w: &W`) were accepted. One place rule now serves
  explicit and implicit borrows (`checkMutBorrowPlace` / `checkSharedBorrowPlace` over
  `placeVia`, where a `&mut e` is only as exclusive as `e`); by-value receivers refuse any
  value reached through a reference (`receiverVia`); `&(x)` records provenance like `&x`.
  An `if`/`match` RECEIVER whose type is a value is an owned temporary (not a possible
  reference), and the interpreter gives a temporary receiver storage (`make().bump()`).
  Fixtures `error_091_destroy_newtype_through_ref.con` (E0290),
  `error_091_mut_receiver_reborrow_shared.con`, `error_091_mut_borrow_explicit_through_ref.con`,
  `error_091_mut_borrow_explicit_projection_through_ref.con` (E0237),
  `error_091_mut_borrow_explicit_paren_immutable.con` (E0233),
  `error_091_mut_receiver_paren_borrow_live.con` (E0232), `trait_mut_receiver_temporaries.con`
  (7, compiled and --interp).
* **Codex review of round 3, third pass.** `P_destroy(*r)` / `let q = *r` read a non-Copy pointee
  of `r: &P` by value (now E0290 outside place uses; `match *r` still inspects in place);
  `take((p))` did not consume `p` (every call-argument rule strips parentheses); `let r = (&p)`
  and `let r = &w.p` recorded no borrow of the root (provenance now strips parentheses and
  records a projection's owned root); and an `if`/`match` under a projection is classified by
  its branch values (`tailVia`), so `(if c { W {..} } else { W {..} }).p.bump()` is an owned
  temporary while a branch yielding `&r` (or an unseen binder) still fails closed. Fixtures
  `error_091_destroy_defname_deref_arg.con`, `error_091_deref_ref_by_value.con` (E0290),
  `error_091_destroy_defname_paren_arg.con`, `error_091_paren_call_arg_not_consumed.con` (E0205),
  `error_091_mut_receiver_paren_borrow_outer.con`,
  `error_091_mut_receiver_projection_borrow_live.con` (E0232), `trait_mut_receiver_if_projection.con` (7).
* **Codex review of round 3, fourth pass.** `(*(&mut p)).destroy()` classified the place by the
  owned root behind the borrow (`*e` is now classified by the reference `e` yields, `refVia`);
  `match *r` bound a non-Copy payload as owned (a by-value read of the pointee: E0290; `match r`
  binds borrowed views); `let r = &*(&p)` lost provenance (provenance now follows `*`, borrows
  and copies of tracked references, `placeProv`/`refProv`), and a reference binding whose
  source cannot be traced (`if c { &a } else { &b }`) is the new E0238; an `if` branch
  yielding a block-local owned binding is an owned temporary. Fixtures
  `error_091_destroy_deref_borrow_mut.con`, `error_091_match_deref_ref_payload.con` (E0290),
  `error_091_mut_receiver_deref_borrow_provenance.con` (E0232),
  `error_091_borrow_provenance_unknown.con` (E0238), `trait_mut_receiver_if_local_binding.con` (7).
* **Codex review of round 3, fifth pass.** A receiver already typed `&mut P` was passed as-is even
  when reached through a `&` (`(*s).bump()`, `s: & &mut P`: now E0237) or while a shared reborrow
  of it was live (`let s = &*r; r.bump()`: `checkExclusiveRefUse`, E0232 — a reborrow of a
  parameter reference records the reference itself as its source); `r = &p` did not update
  provenance (`recordRefProvenance` runs at `let` and at reassignment); a branch tail reached
  through a chain of block-local bindings is classified through the chain (`blockValueVia`).
  Fixtures `error_091_mut_receiver_ref_to_mut_ref.con` (E0237),
  `error_091_mut_receiver_reborrowed_mut_ref.con` (E0232), `error_091_mut_receiver_reassigned_ref.con`
  (E0238 since the sixth pass), `trait_mut_receiver_if_local_chain.con` (7).
* **Codex review of round 3, sixth pass.** Provenance is one source per binding and branch
  merging does not join it, so a reassignment may no longer RE-POINT a reference at different
  storage (`if c { r = &p; }`: E0238, use a new binding); call-argument borrows are read through
  parentheses and `*` (`p.bump((&p))`, `r.bump(&*r)`: E0293); a `match` over an owned scrutinee
  yielding an arm binder is an owned temporary under a projection. Fixtures
  `error_091_ref_repointed_in_branch.con` (E0238), `error_091_mut_receiver_paren_borrow_arg.con`,
  `error_091_mut_receiver_deref_reborrow_arg.con` (E0293), `trait_mut_receiver_match_owned_binder.con` (7).
* **Codex review of round 3, seventh pass.** `w.0` with `w: &W` took the owned-newtype exemption
  (now E0290 by value through a reference); `*r` no longer adds a path step, so `(*r).p` and
  `r.p` are one place for E0293; a Destroy call skips the implicit-borrow check it never makes
  (the owned `w.0.destroy()` was a false E0230). Fixtures
  `error_091_newtype_payload_through_ref.con` (E0290), `error_091_mut_receiver_deref_field_arg.con`
  (E0293), `trait_destroy_newtype_payload.con` (1, compiled; the interpreter does not model
  newtype casts).
* **Codex review of round 3, eighth pass.** A borrow nested in a dereference (`&*(&*r)`) or
  yielded by an `if`/`match` argument contributed no path to E0293 (`borrowPathOf` reads
  through inner borrows; `branchBorrowParts`); the heap-interior exemption covered
  `&Heap<W>` / `&mut Heap<W>` too, handing out a payload the heap still held (only an owned
  shell is exempt now: E0290); `(w).0.destroy()` was a false E0290. Fixtures
  `error_091_mut_receiver_nested_reborrow_arg.con`, `error_091_mut_receiver_if_reborrow_arg.con`
  (E0293), `error_091_heap_field_through_ref.con` (E0290), `trait_destroy_newtype_payload_paren.con` (1).
* **Codex review of round 3, ninth pass.** An `if`/`match` argument's branch borrows are ONE
  borrow made of alternatives (they never conflict with each other — the eighth pass had made
  `bump(if c { &mut p } else { &mut p })` a false E0293) and follow a block-local `let` the tail
  names (`let s = &*r; s`: E0293 against the receiver `r`); a `&self` receiver read through a
  reference while an exclusive reborrow of its storage is live is E0231. Fixtures
  `error_091_mut_receiver_if_local_reborrow_arg.con` (E0293),
  `error_091_ref_receiver_under_mut_reborrow.con` (E0231). Found while testing, NOT fixed here
  (pre-existing, separate): the compiled binary of `bump(if true { &mut p } else { &mut p })`
  segfaults while --interp prints 2 — a reference-valued `if` expression miscompiles.
* **Codex review of round 3, tenth pass (partial).** `checkExclusiveRefUse` counted an exclusive
  reference's own older parent as a conflicting borrow, so using a nested reborrow
  (`let r = &mut p; let s = &mut *r; s.bump()`) was a false E0232 introduced by the fifth pass;
  only references created after it count now. Fixture `trait_mut_receiver_nested_reborrow.con`
  (4). Still OPEN from this pass (not fixed): `(*r).p` through `r: &Heap<W>` (an explicit `*`
  restores the owned-heap exemption), a conditional `&mut` receiver
  (`(if c { &mut *r } else { &mut *r }).bump(&*r)`) contributes no receiver path to E0293, and
  `(*r).read()` skips the live-exclusive-reborrow check that `r.read()` gets.

## Round 3, structural fix: the normalized place rule

The per-spelling repairs above did not converge: every Codex pass found another spelling of
the same alias, because each check (explicit borrows, receivers, call arguments, provenance,
the E0293 overlap check, moves out of places, destruction, the heap-shell exemption) walked
expressions its own way. They now share ONE rule (`normPlace` / `normProjBase` / `normRef` in
`Concrete/Check/CheckHelpers.lean`): a canonical place is a binding followed by field / index /
newtype `.0` projections, where each `*` dereferences a reference or pointer whose source is
itself traced — a reference binding (its recorded provenance), a borrow of a canonical place,
or a cast of one. Parentheses and identity dereferences normalize away (`(*r).p` and `r.p` are
one place; `&*r` names `r`'s source), and every check consults the normalized place:
`checkMutAccess` / `checkSharedAccess` (explicit `&mut e` / `&e` and implicit receiver borrows),
`argBorrowParts` / `receiverBorrowParts` (E0293), `recordRefProvenance`, `checkRefArg`,
`consumeReceiver` / `consumeDestroyReceiver`, and the field-read heap-shell and newtype
exemptions. An expression that does not normalize is refused with ONE code, E0238.

**Deliberate restriction (fail closed), owned by ROADMAP row 4 / R-0499** — the authoritative
checker on Core, which can replace this syntactic rule with one over Core places. Valid
programs rejected today with E0238:

- a reference produced by an `if` / `match` used as a method receiver, a `*` operand, a borrow
  operand's base, or a reference-typed call argument (`(if c { &mut *r } else { &mut *r }).m()`,
  `*(if c { &r } else { &r })`, `f(if c { &a } else { &b })`);
- a reference binding initialized or reassigned from such an expression
  (`let r = if c { &a } else { &b };`), and any reassignment that RE-POINTS a reference binding
  at different storage (`r = &p` when `r` borrowed `q`; provenance is not joined across branches);
- an owned TEMPORARY as a `&mut self` receiver (`make().bump()`, `(P { .. }).bump()`,
  `(if c { a } else { b }).bump()`) — bind it to a `let mut` first;
- a projection whose base is an `if` / `match` (`(if c { w1 } else { w2 }).p.bump()`), even when
  every branch yields an owned value;
- `*` over a non-place or a double dereference (`(*s).m()` with `s: & &mut P`, `**r` — reads
  of Copy values included), and a reference stored in a place (`*w.r`);
- a field access or index whose base is not a traceable place
  (`(if c { a } else { b }).f`, `(**r).f`);
- a reference read back out of an aggregate — a struct field, an enum payload, or a generic
  field instantiated at a reference type — used as a place, binder or argument (storing a
  reference in an aggregate is accepted, but what it borrows is not traced afterwards);
- shadowing a name that a live reference records as its source or parent
  (`let r = &p; let p = q;`), since provenance and reborrow ancestry are recorded by name;
- any use of a reference binding — read, write, call — while a live exclusive alias of its
  storage that is not its own ancestor exists (no non-lexical lifetimes: a reborrow stays live
  to the end of its scope).

Codex's last three open shapes are rejected: `(*r).p` with `r: &Heap<W>` (E0290, the heap-shell
exemption now reads the normalized base), the conditional receiver hiding a conflicting
argument borrow (E0238), and `(*r).read()` under a live `s = &mut *r` (E0231, one place with
`r.read()`). Fixtures: `error_091_norm_heap_deref_payload.con`,
`error_091_norm_conditional_receiver.con`, `error_091_norm_deref_read_under_reborrow.con`; the
earlier positive fixtures for owned temporaries / `if` receivers are now the rejection fixtures
`error_091_norm_{temporaries,if_projection,if_local_binding,if_local_chain,match_owned_binder}.con`.
`run_tests.sh` carries a table of every bug-091 fixture with its expected outcome
(`r091_table`), so a change to the rule shows up as a changed row.

Codex review of the rule as a whole (first pass) found four entry points that bypassed it, now
closed: a cast-to-reference argument contributed no E0293 key (`r.bump(r as &P)`); a reference
argument was not checked as an access (passing the parent `r` while its reborrow is live);
aliasing was decided by creation order, so a copied exclusive reference (`t = r`) escaped it
(now by reborrow/copy ancestry, `refParent`, with the valid nested reborrow still accepted);
and an untraceable projection base (`(**r).p`) took the owned-heap exemption (now fail
closed). Fixtures `error_091_norm_{cast_arg_overlap,parent_arg_under_reborrow,copied_mut_ref,double_deref_heap}.con`.

Second pass of that review: reads and writes through a reference were not accesses — a field
write `r.n = 5` or any use of `r` while `s = &mut *r` is live (now: any use of a reference binding
under a live exclusive non-ancestor alias is E0231, and every assignment target is a normalized
WRITE, `checkWriteAccess`: never through `&`, not while an alias of its reference is live, not
into an owned root a live reference borrows); a scoped `borrow mut p as r` ignored live
references to `p` and gave `r` a key unrelated to `p`; shadowing a name a live reference
records as its source or parent detached it (now E0238); intrinsic calls skipped the
reference-argument check; and `*(h)` did not consume a heap. Raw-pointer targets
(`*self.ptr = v`, `*((base + i) as *mut u8) = v`) name raw storage, whatever reached the
pointer. Fixtures `error_091_norm_{field_write_under_reborrow,borrow_block_under_live_ref,paren_heap_load}.con`.

Third pass: a bare assignment `x = v` overwrote a binding under a live shared reference (now
E0232); a payload bound by matching THROUGH a reference got its own conflict key (it now records
the scrutinee's provenance and ancestry); and `*w.h` loaded and freed a heap its owner still held
(only a bare owned `Heap` binding is loaded; E0290). Fixtures
`error_091_norm_{assign_under_shared_ref,heap_field_load}.con`.

Fourth (static) pass: the remaining syntactic decisions were all `.ident` tests — consumption of
call arguments, self-assignment, borrow-block escape checks, array-write mutability — that a
parenthesized spelling evaded. Check now runs on each body with every `.paren` node removed
(`unparenDeep`), so a binding is spelled one way for every decision. And keys are exact: a
reference records the PATH it borrows (`VarInfo.borrowedPath`), so after `let r = &w.f`, `*r` keys
as `(w, [f])` exactly as `w.f` does; a reassignment that changes the path is refused like one
that changes the root. Fixture `error_091_norm_paren_array_write.con`.

Fifth (static) pass: every `*`, field access and index now normalizes for READS too (an
untraceable `**r` or `(**r).f` is E0238 even for a Copy value); a reference binding and a match
binder record provenance from the source normalized BEFORE the new name is bound (`let r = r`,
a binder shadowing its scrutinee); a reference cast whose operand is itself a reference
(`(r as &T) as &T`) points where the operand points, and only a computed address (pointer or
integer operand, trusted raw code) yields untracked storage. Fixtures
`error_091_norm_{double_deref_read,intrinsic_arg_under_reborrow}.con`.

Sixth (static) pass — binding introductions: every binder (let, match arm binders, catch-all
binders, struct destructuring) goes through one shadowing guard (`guardShadowedSource`); a
catch-all binder over a borrowed non-Copy enum is refused (E0290) instead of binding the whole
value out of the reference; a reference-valued field destructured out of a struct has no traced
source and is E0238; and `r[i] = v` with `r: &mut [T; N]` obeys the non-Copy overwrite rule of
`(*r)[i] = v`. Fixtures `error_091_norm_{ref_array_overwrite,catchall_binder_through_ref}.con`.

Seventh (static) pass — references stored in aggregates: a payload binder whose declared type is
a reference is E0238 (a reference stored in an enum has no traced source), and `peekExprType`
substitutes a generic struct's type arguments into field types (`G<&P>.v` is `&P`) and types
casts, so the normalized rule (and the intrinsic reference-argument check that uses it) sees
those references. Fixtures `error_091_norm_{enum_payload_ref,generic_field_ref_write}.con`.

Eighth (static) pass — the rule's type knowledge: `peekExprType` now auto-derefs heap shells and
types a newtype's `.0`, and a projection base whose type the peek cannot determine is treated as
untraceable unless it is a fresh temporary (fail closed); an intrinsic's place arguments must
normalize whatever their peeked type; a matched reference must normalize even when the arms
bind Copy values. Fixtures `error_091_norm_{heap_ref_field_write,match_untraced_ref}.con`.

