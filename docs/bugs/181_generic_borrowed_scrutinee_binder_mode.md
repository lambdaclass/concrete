# Bug 181 — a payload binder through a borrowed scrutinee of generic type is `&T` to Check and `T` to Elab

**Status:** OPEN
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

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `signals`) requires the
binary to be killed by a signal; when the program exits 42, that leg fails until this document, the
corpus entry and the leg are updated together.
