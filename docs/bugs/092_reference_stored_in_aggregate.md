# Bug 092: a reference stored in an aggregate did not freeze its source

**Status:** FIXED
**Found:** 2026-10-18, checker ownership hunt (probe project `r3/`).
**Severity:** critical. A program the checker accepted read freed memory through a reference that
outlived the value it pointed to. Silent wrong code.

## Reproducer

```
mod main {
    struct W { t: &String }
    fn main() with(Std) -> i32 {
        let s: String = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
        let w: W = W { t: &s };
        s.drop();
        let u: String = "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
        let W { t } = w;
        println(t.len());
        u.drop();
        return 0;
    }
}
```

On `64cfea38` this built and printed `8482603264`, the length field read out of the freed `s`. The
same shape compiled through an enum payload (`let o: Option<&Heap<B>> = Option::Some { value: &h };
free(h);` then matching `o`) and an array (`let a: [&Heap<B>; 1] = [&h]; free(h); a[0].v`).

## Cause

`Concrete/Check/Check.lean` freezes a borrow's source through `borrowedFrom`, which `letDecl`
recorded only for a bare `let x = &y` / `let x = &mut y`. A reference reached a binding in any other
position (a struct field, an enum payload, an array element) without recording a source, so moving
or dropping `s` was not checked against it.

The language documents the rule that excludes the whole shape: references are second-class, "never
stored in an aggregate" (BYTE_VIEW.md, VALUE_MODEL.md: "non-storable"). Nothing enforced it; only
returning a reference (E0236) and storing a borrow-block reference (E0234) were checked. Tracking
sources through aggregates would have legalised a type the design excludes, so the fix enforces the
rule instead.

## Fix

New diagnostic **E0239** (`refStoredInAggregate`). A reference may not be held inside an aggregate:

- a struct or enum field type that holds a reference is rejected at the declaration;
- a struct literal field, enum payload or array literal element whose type holds one is rejected
  (this catches generic instantiations such as `Option::Some { value: &s }`);
- a binding or parameter whose type holds a reference inside an array, `Heap`, or generic argument
  (`Option<&T>`, `Vec<&T>`, `[&T; N]`, also behind an outer `&`) is rejected.

`tyStoresRef` does not look through function-pointer types (a `fn(&T) -> U` field stores no
reference) or raw pointers (the trusted escape). Bare reference bindings and reference arguments
are unchanged. The checker's error-recovery path no longer re-adds a binding whose declared type is
rejected, which used to replace the accumulated diagnostics with an unspanned one.

`tests/programs/error_escape_field.con` declared a `&Int` field to exercise the borrow-block escape
check (E0234); it is now rejected at the declaration with E0239, and its run_tests expectation says
so.

## Gate

`scripts/tests/run_tests.sh`: `error_092_ref_struct_field.con`, `error_092_ref_in_option.con` and
`error_092_ref_in_array.con` must fail with E0239 (all three compiled on the previous compiler);
`regress_092_refs_flow_down.con` (a bare borrow in a block, reference arguments, a struct holding a
fn pointer with a reference parameter) must still compile and return 42. Every other
`tests/programs` fixture keeps its previous check outcome, and `check_examples.sh` passes
(142/0/79).
