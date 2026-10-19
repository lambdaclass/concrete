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
