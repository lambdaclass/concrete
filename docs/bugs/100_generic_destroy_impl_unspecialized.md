# Bug 100 — a generic impl's `&self` destructor reached by `destroy(h)` is never specialized

**Status:** FIXED 2026-10-21 (`land5`)
**Found:** 2026-10-18, by the probes behind branch `bug099-generic-destroy`.
**Severity:** a valid program fails LLVM validation (`use of undefined value '@Holder_destroy'`);
the interpreter runs it. With a linker that accepted the undefined symbol the destructor would not
run.

## Reproducer

`tests/programs/regress_100_generic_ref_destroy.con` (abridged):

```con
struct Holder<T> { cell: *mut u64, v: T }
trusted impl<T: Copy> Destroy for Holder<T> {
    fn destroy(&self) { *self.cell = *self.cell + 1; }
}
...
let h: Holder<i64> = Holder::<i64> { cell: cell, v: 5 };
destroy(h);
let g: Holder<i32> = Holder::<i32> { cell: cell, v: 6 };
g.destroy();
print_int(count(cell) as i64);
```

On main (`9d17ccc5`): `--interp` prints 2; native compilation fails `llvm-as`: `use of undefined
value '@Holder_destroy'`. Expected: 2 from both. A by-value generic destructor
(`fn destroy(self) with(Alloc)`, the form batch 2's "Destroy consumes" rule made canonical) is
specialized correctly on main; only the `&self` form is affected.

## Cause

Elab lowers `destroy(h)` for a concrete `h` to a direct call of the impl's destructor with the
value itself, `Holder_destroy(h)`; Lower takes its address for the `&self` parameter. Mono's
ordinary call path infers a generic callee's type arguments by matching formal against actual
types (`inferTypeArgs`), and `&Holder<T>` does not match `Holder<i64>`. With no type arguments
inferred it left the call as written (`Concrete/IR/Mono.lean`, "couldn't infer, leave as-is"),
naming the generic definition, which is never emitted.

## Fix

In Mono's call path, a call to a destructor (a definition in `destroyImpls`) whose type arguments
the actual argument does not determine is matched through the receiver's implicit borrow (`&h` for
a `&self` destructor, `&mut h` for `&mut self`). A destructor whose type arguments still are not
determined, or are unresolved, is refused (E0814, "cannot be specialized ... (bug 100)") rather than
left for the backend. The reproducer runs in `run_tests.sh` natively and under `--interp`.

## Structural cause

Destruction dispatched by synthesized `<Type>_destroy` names ([structural class](STRUCTURAL_CLASSES.md)
3, with 052, 091, 099, 103 and 116): a destroy call carries a name, not the identity of the impl
instance the checker selected, so Mono re-derives the instance and an unresolved specialization
fell through to a call by the generic name (class 11's fail-open default).

## Structural owner

R-0510 (S8a checked specialization: "missing or substituted instances ... are rejected") with
R-0492: a destroy call resolves to the identity of the exact `Destroy` impl instance.
