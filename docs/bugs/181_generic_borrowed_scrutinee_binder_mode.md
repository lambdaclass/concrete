# Bug 181 — a payload binder through a borrowed scrutinee of generic type is `&T` to Check and `T` to Elab

**Status:** FIXED (branch `soundness-holes`, decision D3)
**Found:** 2026-10-23, by the language-tightening probes (witness programs for
[TIGHTENING_DECISIONS.md](../language/TIGHTENING_DECISIONS.md), the binding-mode row).
**Severity:** memory-unsafe wrong code in safe code. A generic function that matches through a
reference and passes the payload binder to a `fn(&T)` parameter compiles; specialized at a Copy
scalar such as `Int`, the binary dereferences the payload's value as an address (SIGSEGV), and
`--interp` fails ("deref of non-ref value"). No `trusted`, no `Unsafe`.

## Reproducer

`tests/regressions/bug181_generic_borrowed_scrutinee_binder_mode/repro.con`:

```con
fn get<T>(o: &Option<T>, k: fn(&T) -> Int) -> Int {
    match o {
        Option::Some { value } => { return k(value); },
        Option::None => { return 0; }
    }
}
fn read(p: &Int) -> Int { return *p; }
fn main() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    return get::<Int>(&o, read);
}
```

Expected: exit 42. Observed on main `185d998c`, `wip/train1` `f92b8f32` and `wip/train3`
`949d79e9`: the binary is killed by SIGSEGV (status 139); `--interp` exits 1 with "deref of non-ref
value". The same shape with `match &o` on a local of type `Option<T>` behaves the same. With a
struct payload the program happens to run: an aggregate held "by value" is a pointer to the payload,
so the call receives a valid address by accident.

## Cause

The binding mode is decided in one place and the binder's type in another:

- `Check.lean`, the `match` case (`bindTy`): through a reference scrutinee a payload binds as `&T`
  when its type is not Copy, by value when it is. In a generic body `T` is not known to be Copy, so
  `value : &T`, and `k(value)` type-checks against `fn(&T) -> Int`.
- Elab gives the Core binder the payload type itself (`--emit-core`:
  `Option::Some { value.b0: T } -> { return k(value.b0); }`), whatever the scrutinee's type.
- After monomorphization at `T := Int` the specialization loads the `i64` payload and passes it to
  `%k` (`call i64 %k(i64 %t6)`), where `read` expects a pointer.

So Check's `&T` never reaches Core, and the Copy test that chose it would choose differently after
substitution anyway (`Int` is Copy, so a non-generic `match` on `&Option<Int>` binds by value).

## Structural cause

One fact kept in two places ([structural class](STRUCTURAL_CLASSES.md) 12): the arm binder's type
is computed by Check and recomputed, differently, by Elab, and nothing compares them; Core carries
no binding mode, so no later pass (CoreCheck, Mono, the post-mono checks) can see the disagreement.
It is also a rule that depends on a property substitution changes (class 6's 105 pattern): a binding
mode keyed on Copy-ness is decided before monomorphization and means something else after it.

## Structural owner

R-0496 (S7 Core semantics): the binding mode becomes explicit in Core (a borrowed-payload binder of
type `&T`, or a by-value binder of a Copy payload), produced once by Elab from the checker's decision
and re-checked by CoreCheck after Mono. The language rule that removes the Copy dependence is decided
in [TIGHTENING_DECISIONS.md](../language/TIGHTENING_DECISIONS.md) (T-BIND). Narrow repair under
exit 0b: Elab gives the binder Check's type (`&T` through a reference scrutinee when the payload type
is not Copy at check time, kept through Mono), or Check refuses a match through a reference whose
payload type is a type parameter.

## Fix

Decision D3 (TIGHTENING_DECISIONS, Q3): every binder through a borrowed scrutinee is a reference
(`&T` for a payload field, the scrutinee's own reference for a whole-value binder), Copy payloads
included; a Copy value is read with `*value`. One function decides it,
`Concrete/Semantics/BindingMode.lean` (`payloadBinderTy`, `wholeBinderTy`), and both Check and Elab
call it, so the checker's binder and the Core binder are the same type by construction. The mode is
stored in Core as the binder's type; the module states what substitution preserves (Mono maps
`&T` to `&σ(T)` and never introduces or removes a reference head, and payload fields cannot be
references, REF-3), so `binderIsBorrow` reads the same mode before and after specialization. Lower
binds a borrowed payload binder to the payload's address in the scrutinee's storage (a zero-offset
GEP keeps the binder's type when SSA cleanup folds it), and a whole-value binder to the scrutinee
reference; the interpreter binds a reference path into the payload (`RefStep.field` now steps into
enum payloads). Copy-ness no longer chooses anything.

Because every such binder is now a reference, REF-8's freeze holds for all of them: reassigning or
mutably borrowing the root while the binder lives is E0232, Copy roots included.

Migration (`rule_impact.sh` over 2191 units against `wip/train3` 1df4cf48): the two fixtures D3
measured (`patterns/match_ref_enum.con` `return *x;`, `patterns/match_ref_scalar.con` `*n + 1000`),
and `error_091_norm_catchall_binder_through_ref.con`, which stays refused with E0302 instead of
E0290 (its catch-all binder is now the reference, and destructuring it with a `let` would bind
references). std and every example compile unchanged.

Regression: the reproducer exits 42 natively and under `--interp` (`run_ok`);
`regress_181_borrowed_binders.con` (generic at `Int` and at a struct, a guard, `while let`, a nested
pattern, an early exit, a deferred read, a whole-value binder, an owned scrutinee; exit 42, the
`--interp == binary` loop); `error_181_root_reassigned_{match,while_let,guard,nested,whole_binder,defer}.con`
(E0232, `run_err`).

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `signals`) requires the
binary to be killed by a signal; when the program exits 42, that leg fails until this document, the
corpus entry and the leg are updated together.
