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
