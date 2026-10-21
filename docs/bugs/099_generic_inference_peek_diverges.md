# Bug 099 — Check and Elab infer a generic call's type arguments from two different argument-type peeks

**Status:** FIXED 2026-10-21 (`land5`)
**Found:** 2026-10-18, by the probes behind branch `bug099-generic-destroy` (bug 052's review:
`kill(&self.items)` instantiated `T = Placeholder` in Core).
**Severity:** a valid program fails to compile or fails LLVM validation. Before bug 052's
containment the Destroy-bounded shape silently skipped the elements' destructors; on main it is
refused (E0814) or fails `llvm-as`, never silently wrong.

## Reproducer

`tests/programs/regress_099_generic_infer_field_index.con` (abridged):

```con
fn pick<T: Copy>(x: &T) -> T { return *x; }
struct Copy Inner { a: i64, b: i64 }
struct Copy Outer { inner: Inner, n: i64 }
...
let i: Inner = pick(&o.inner);
let j: Inner = pick(&arr[1]);
print_int(i.a + i.b * 10 + j.a * 100 + j.b * 1000);
```

On main (`9d17ccc5`): `--interp` prints 43043; native compilation fails `llvm-as`
(`'%t10' defined with type 'i64' but expected 'ptr'`). Expected: 43043 from both.

`tests/programs/regress_099_generic_infer_call_results.con` passes a call result and a method-call
result to `fn kill<T: Destroy>(x: T) with(Alloc)`. On main, `kill(make_arr(cell))` alone is refused
by Mono (E0814, "destructor call on an unresolved type 'Placeholder'", bug 052's containment), and
`kill(m.both())` is refused by Check (E0220, "expected <unknown>, got [Tracked; 2]"). Expected: 4
from both engines.

## Cause

A generic call written without a turbofish infers its type arguments by unifying each parameter
type against the argument's type, read by a side-effect-free peek before the argument is checked.
Check (`CheckHelpers.peekExprType`) and Elab (`Elab.peekExprType`) each had their own peek. Elab's
answered `.placeholder` for a field access, an index and a call, so Core instantiated
`pick<Placeholder>` where Check had typed `pick<Inner>`; Check's answered `.placeholder` for a
method call, so it refused a valid argument. The first binding also won even
when it was `.placeholder`, so a later argument that determined the parameter was ignored.

## Fix

One peek, `Concrete/Resolve/PeekType.lean` (`peekExprTypeIn`), parameterized by each pass's
environment (`PeekCtx`: locals before constants, struct and newtype lookups, trait bounds,
resolution, substitution). It sees through fields (with the object's type and capability
arguments, auto-deref of one reference or heap layer, a newtype's `.0`), indexes, casts, unary and
comparison operators, and non-generic calls, static calls and method calls (a method at its
receiver's type arguments). A generic callee's return type stays unanswered, bug 063's rule, so an
uninferable capability variable still reports E0242 (`check_cap_inference.sh`). `recordInferred` lets a
`.placeholder` binding yield to a later determining argument. Check and Elab both call it, so they
infer the same type arguments by construction. The two reproducers run in `run_tests.sh` natively
and under `--interp`.

## Structural cause

Destruction dispatched by synthesized `<Type>_destroy` names with an unresolved specialization
filled in late ([structural class](STRUCTURAL_CLASSES.md) 3, with 052, 091, 100, 103 and 116), and a
duplicated authority: two passes computed the same judgment (an argument's type for inference)
with two implementations that drifted.

## Structural owner

R-0510 (S8a checked specialization): a specialization is checked against the instance the source
checker selected, so a `Placeholder` instantiation is rejected where it is produced. The shared
peek removes the duplicated authority now.
