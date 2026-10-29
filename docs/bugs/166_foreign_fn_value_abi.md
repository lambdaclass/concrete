# Bug 166 — a foreign function taken as a value is called with the Concrete ABI

<!-- bug-ledger
summary: a foreign function taken as a value was called with the Concrete ABI, so a small `#[repr(C)]` struct reached C as a pointer (silent wrong code at the C boundary). An `extern fn` is never a value (E0588, D4, FFI.md "Foreign Function Values"); pass a Concrete wrapper. Pinned by four `error_166_*.con` fixtures (E0588), the reproducer, and `regress_166_wrapper_value.con` (by containment)
fixtures: error_166_foreign_fn_value_repr_c.con error_166_foreign_fn_value_repr_c_let.con error_166_foreign_fn_value_scalar.con error_166_imported_extern_value.con error_166_extern_value_in_const.con error_166_extern_value_after_scope.con regress_166_wrapper_value.con
class: 14 — a foreign fn value called with the Concrete ABI
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED by containment (branch `soundness-holes`): an extern fn is never a value, E0588
**Found:** 2026-10-22, by the Codex review (round 5) of the symbol-name work (branch `symbols`).
**Severity:** silent wrong code at the C boundary. A direct call of an `extern fn` whose parameter
or result is a small `#[repr(C)]` struct is flattened to the C ABI (a struct of 8 bytes or less
travels in an integer register). The same extern passed as a function VALUE and called through it
uses the Concrete ABI (a pointer to the struct), so C receives the struct's address as its field.
Needs a `trusted extern fn` (or `with(Unsafe)`), so safe code cannot reach it without a trusted
boundary, but nothing at that boundary warns.

## Reproducer

`tests/regressions/bug166_foreign_fn_value_abi/repro.con`:

```con
#[repr(C)]
struct Copy Pair { v: Int }

trusted extern fn take_pair(p: Pair) with() -> Int;

fn apply(f: fn(Pair) -> Int, p: Pair) -> Int {
    return f(p);
}

fn main() -> Int {
    return apply(take_pair, Pair { v: 7 });
}
```

`--emit-llvm`: `declare i64 @take_pair(i64)`, but `apply` calls `%t0 = call i64 %f(ptr %p)` and
`main` passes `ptr @take_pair`. Expected: the call through `f` passes `Pair` the way C declares it,
or the program is refused. The defect predates the symbol work (it is the ABI, not the name).

## Cause

`EmitSSA` decides the extern ABI (`isExternCall`, `externABIFlattenParam` / `Ret`) only for a
DIRECT callee. A function type `fn(Pair) -> Int` carries no calling convention, so an indirect
call through a value cannot know that its target is a C function.

## Structural cause

Calling convention is a property of the callee's declaration that the function TYPE does not
carry ([structural class](STRUCTURAL_CLASSES.md) 14, trusted std and FFI code with no checked
contract: the C boundary's ABI is decided at one call form and not at the other).

## Structural owner

R-0283 / R-0287 (ABI checks for the C boundary). Decided 2026-10-23
([TIGHTENING_DECISIONS D4](../language/TIGHTENING_DECISIONS.md#d4-foreign-function-values-refuse-now-typed-calling-convention-later-q4)):
the containment under exit 0b refuses taking **any** `extern fn` as a value (revised after review:
a scalar-only exemption would rest on an accidental ABI match; 2 fixtures migrate); the long-term
design makes calling convention, capabilities and safety obligations part of the function type
(`extern "C" fn(..)`, [FFI.md](../platform/FFI.md#calling-convention-in-function-types-decided-2026-10-23-not-implemented)),
built with the first callback workload.

## Fix

Decision D4 (TIGHTENING_DECISIONS, 2026-10-23, widened after its Codex review; normative text in
[FFI.md "Foreign Function Values"](../platform/FFI.md#foreign-function-values-normative-decided-2026-10-23)):
an `extern fn` is never a value. Taking one as a value, own or imported, is refused with the new
code E0588 whatever its signature. The `#[repr(C)]` aggregates are where the conventions differ in
codegen, but the refusal is not keyed on them: the extern's authority (`with(Unsafe)` unless
`trusted extern`, its declared effects) is also a fact of the direct call that a function type
does not carry, so a value would let a caller reach the C function under the type's capabilities.
Elab decides it (`refuseForeignFnValue`, E0588, pass `elab`) at the one place a global function
becomes a value (an identifier or function reference that resolves to a function), on the
resolver's identity of the name (`resolveCallee`, namespace `extern`, own or imported), so scoping,
constants and import aliases are the resolver's. A compiler-intrinsic spelling is not foreign even
when declared `extern`. CoreCheck re-checks the Core forms (`checkForeignFnValue`, keyed on
`CFnDef.foreignCalls`) as defense in depth. The first version decided it only in CoreCheck by
name, and its Codex review found two holes there: a constant initializer (`const F: fn(Int) -> Int
= labs;`, never visited) and a binding of the extern's name that had gone out of scope (CoreCheck's
variable list outlives blocks); both are fixtures now. A direct call stays legal, and a Concrete
function whose body calls the extern is the supported value.

An adapter (a Concrete-ABI function whose body is the direct call) was implemented and reviewed
first and is parked on `wip/soundness-166-adapter-parked`; the long-term form is a calling
convention in the function type (`extern "C" fn`, D4).

Migration (`scripts/tools/rule_impact.sh`, 2172 units against `wip/train3` 1df4cf48): one corpus
program took an extern as a value, `regress_129_fn_value_extern.con` (bug 129's imported-extern
case). It now passes a Concrete wrapper whose body calls the imported `abs`, which keeps its point
(that call still binds C's `abs` beside a Concrete `abs`); the refused shape moved to
`error_166_imported_extern_value.con`. std has no foreign function value.

Regression fixtures (`run_tests.sh`): E0588 for `error_166_foreign_fn_value_repr_c.con` (the
reproducer's shape), `error_166_foreign_fn_value_repr_c_let.con` (let-bound, `#[repr(C)]`
result), `error_166_foreign_fn_value_scalar.con` (scalar signature) and
`error_166_imported_extern_value.con`, plus `error_166_extern_value_in_const.con` and
`error_166_extern_value_after_scope.con` (the two review cases); the reproducer is refused with E0588; the control
`regress_166_wrapper_value.con` passes a wrapper, exit 42. Before the fix all of them compiled, and
the first two passed the struct's address to C.

## Containment while open (historical)

`scripts/tests/run_tests.sh` (codegen section, "bug 166 pin") requires the reproducer's IR to
show the Concrete-ABI call through the value (`call i64 %f(ptr %p)`) next to the C declaration
`declare i64 @take_pair(i64)`; when the call changes, that leg fails until this document, the
corpus entry and the leg are updated together.
