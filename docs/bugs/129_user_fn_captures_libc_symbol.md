# Bug 129 — a safe user function named like a C library symbol replaces it for the whole program

**Status:** FIXED 2026-10-21 (branch `symbols`)
**Found:** 2026-10-21, by the semantics probes (finding N3, `symclash2.con`, `symclash.con`).
**Severity:** memory and authority safety in safe code. User functions are emitted under their bare
source names, so a safe `fn malloc` becomes the program's `malloc` and every allocation the compiler
emits (`alloc`, the string runtime) calls it. No `extern`, no `trusted`, no capability is needed for
safe code to change the behaviour of the whole program; the reproducer dies with a signal.

## Reproducer

`tests/regressions/bug129_user_fn_captures_libc_symbol/repro.con`:

```con
struct Copy B { v: Int }

fn malloc(n: Int) -> Int {
    return n;
}

fn main() with(Alloc) -> Int {
    let h: Heap<B> = alloc(B { v: 42 });
    let out: Int = h.v;
    free(h);
    return out + malloc(0);
}
```

On main (`5b63cd87`), arm64 macOS: the binary is killed by SIGABRT (status 134), with nothing on
stdout or stderr. `--interp` fails `interp: undefined function 'alloc'` (the interpreter has no
heap), so the differential gates do not see it. Expected: exit 42 (or a diagnostic refusing the
definition).

`--emit-llvm` shows the capture: `define i64 @malloc(i64 %n)` with no separate `declare` of the C
`malloc`, and `alloc(B { v: 42 })` lowers to `%t3 = call ptr @malloc(i64 8)`, followed by a store
through the returned value 8 and `call void @free(ptr %t3)` on it. The string runtime emitted into
the same module (`string_concat`, `int_to_string`, `string_to_int`, ...) calls the same `@malloc`.
Probe `symclash.con` shows the naming for other C names: `fn abs` and `fn strlen` are emitted as
`define i64 @abs` and `define i64 @strlen`. On ELF targets, symbol interposition would also send the C
library's own internal calls to the user definition; that was not run on Linux.

## Cause

`llvmFnName` in `Concrete/Backend/EmitSSA.lean` emits a function under its bare source name unless the
name collides between two Concrete modules (then it is module-qualified). The only guard is
`isReservedFnName` (`Concrete/Resolve/Intrinsic.lean`, checked by CoreCheck as E0578): the compiler's
own builtins (`alloc`, `free`, `abort`, ...), `user_main` and the `__` prefix (bug 028). C library
symbols that the emitted code itself calls (`malloc`, and every C name a user program might link)
share that one symbol namespace with user definitions.

## Structural cause

Declaration identity by spelled name ([structural class](STRUCTURAL_CLASSES.md) 1): a Concrete
function's link symbol is its spelling, so a user definition and a C symbol with the same spelling are
one symbol to the linker. It is also one fact kept in two places (class 12): which names belong to the
C runtime is known to the emitter that calls them but not to the checker that admits definitions, and
the reserved-name list (bug 028) and the `_destroy` synthesis (bug 103) are earlier spellings of the
same missing separation between Concrete-defined symbols and foreign ones.

## Fix

The decided symbol-name rule ([FFI.md, "Symbol Names"](../platform/FFI.md)) is implemented at
emission (`Concrete/Backend/Symbols.lean`, `EmitSSA`). Every Concrete-defined function is emitted
under `_C.<module path>.<name>` and every runtime helper under `_C$rt.<name>`; both contain a
character no C identifier has, so a Concrete definition can no longer occupy a C symbol. The
compiler's own C calls (`alloc` → `malloc`, `free`) became `SCallee.foreign`, which is never
resolved against Concrete definitions, so the user's `malloc` and the allocator are different
functions even though both are spelled `malloc` in source. The reverse capture is closed too: every
call the resolver binds to an extern, own or imported, carries that binding into emission
(`CFnDef.foreignCalls`, copied by Mono into specializations), so a module's `extern fn abs`, or an
imported one, binds C's `abs` even when another module defines a Concrete `abs` (before, the
extern's declaration was dropped and both calls reached the Concrete function). A foreign
binding's identity is its name, so two bindings of one name to different C symbols, or a binding
named like a runtime helper, are refused (E0118).

The reproducer now exits 42. Regressions in `run_tests.sh`: `regress_129_user_fn_named_malloc.con`
(exit 42, and an IR leg: `define i64 @_C.main.malloc`, C `malloc` stays a `declare`, no
`define @malloc`), `regress_129_extern_beside_same_named_fn.con` and
`regress_129_imported_extern_beside_fn.con` and `regress_129_fn_value_extern.con` (an imported
extern taken as a function value), `regress_129_generic_extern.con` (a generic body's binding
survives specialization; exit 105 each), and `regress_129_runtime_helper_beside_fn.con`
(a call bound to the runtime helper `print_int` is not captured by another module's
`fn print_int`; prints 7). The open-bug
reproducer row is dropped. Proof and report identities are unchanged (mangling happens after Core;
`check_identity_rows.sh` and `check_claim_inventory.sh` pass unchanged).

## Structural owner

R-0492 (S5 `DefId`): every Concrete-defined function gets a mangled link symbol derived from its
resolved identity, with foreign imports named only through `extern` (and an explicit link-name
attribute) and exports through an explicit export attribute, the A8 foreign-names direction; R-0510
(S8a) for synthesized definitions and R-0283/R-0286 for the C boundary. As a memory/authority defect
in safe code it joins exit 0b under the defect disposition rule; the smallest containment is refusing
a safe definition whose name is a symbol the emitted code or the C runtime uses, until mangling lands.

## Containment while open

While open, `scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `signals`)
required the binary to be killed by a signal; the row was removed with the fix.
