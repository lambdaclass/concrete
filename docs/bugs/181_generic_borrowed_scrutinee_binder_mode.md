# Bug 181 — a payload binder through a borrowed scrutinee of generic type is `&T` to Check and `T` to Elab

<!-- bug-ledger
summary: a payload binder through a borrowed scrutinee was `&T` to Check and `T` to Elab, so a generic specialized at `Int` passed an integer as a pointer (SIGSEGV in safe code). Every binder through a reference is now `&T` (one function, `BindingMode.lean`, for Check and Elab; stored in Core and preserved by Mono), and the root stays frozen while it lives (E0232). Pinned by the reproducer, `regress_181_borrowed_binders.con` and six `error_181_root_reassigned_*.con` ((D3))
fixtures: regress_181_borrowed_binders.con error_181_root_reassigned_match.con error_181_root_reassigned_while_let.con error_181_root_reassigned_guard.con error_181_root_reassigned_nested.con error_181_root_reassigned_whole_binder.con error_181_root_reassigned_defer.con
class: 12 — an arm binder through a borrowed scrutinee: `&T` in Check, `T` in Elab
silent: yes
exit: 0b
-->

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

The rule decided on 2026-10-23
([TIGHTENING_DECISIONS D3](../language/TIGHTENING_DECISIONS.md#d3-a-binder-through-a-reference-is-a-reference-q3)):
every binder through a scrutinee of reference type is a reference, Copy or not, so the mode no
longer depends on a property substitution changes. Check, Elab and Mono carry that one type (exit
2i, R-0500), and R-0496 (S7 Core semantics) makes the binder's type explicit in Core, re-checked by
CoreCheck after Mono. Measured migration: two fixtures (`tests/programs/patterns/match_ref_enum.con`,
`match_ref_scalar.con`). The fix turns this reproducer into a run fixture (42, binary = `--interp`).

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

Codex review, round 1. The interpreter's reference into a payload is now its own path step,
`RefStep.payload variant field`, whose byte address `heapRefAddr` computes from the enum layout
(payload offset after the tag, then the field's offset in its variant), so a heap-rooted payload
reference compares by address as natively. The module's statement of what substitution preserves
was too strong for whole-value binders (an owned scrutinee of type `T` can specialize to `&Int`);
it now says that for those the mode is irrelevant (binding the reference and binding the value are
the same value).

Decided 2026-10-23: a destructuring `let` of a struct through a reference is refused like the enum
form (E0302, REF-6@1): it would bind references, and a `let` may not hold one. It had been accepted,
binding the fields by value; the exit 2i row is now a rejection
(`error_2i_ref6_struct_destructure_through_ref.con`) with the rewrite
`regress_2i_ref6_struct_fields_through_ref.con` (`s.x + s.y`). No other corpus site.

Regression: the reproducer exits 42 natively and under `--interp` (`run_ok`);
`regress_181_borrowed_binders.con` (generic at `Int` and at a struct, a guard, `while let`, a nested
pattern, an early exit, a deferred read, a whole-value binder, an owned scrutinee; exit 42, the
`--interp == binary` loop); `error_181_root_reassigned_{match,while_let,guard,nested,whole_binder,defer}.con`
(E0232, `run_err`).

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `signals`) requires the
binary to be killed by a signal; when the program exits 42, that leg fails until this document, the
corpus entry and the leg are updated together.
