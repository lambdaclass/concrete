# Bug 166 — a foreign function taken as a value is called with the Concrete ABI

**Status:** OPEN
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

R-0283 / R-0287 (ABI checks for the C boundary): either an adapter with the Concrete ABI is
generated when a foreign function is taken as a value, or the convention is part of the function
type and a mismatch is refused. The narrow containment, under exit 0b: refuse taking as a value an
`extern fn` whose signature contains a `#[repr(C)]` aggregate.

## Containment while open

`scripts/tests/run_tests.sh` (codegen section, "bug 166 pin") requires the reproducer's IR to
show the Concrete-ABI call through the value (`call i64 %f(ptr %p)`) next to the C declaration
`declare i64 @take_pair(i64)`; when the call changes, that leg fails until this document, the
corpus entry and the leg are updated together.
