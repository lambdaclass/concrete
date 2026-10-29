# Bug 226 — a submodule can import a private function of another module

<!-- bug-ledger
summary: a submodule (a module written after `main`, any `mod x;` file, every std module) may import a private function or `extern fn` of another module, which MOD-2@1 refuses and Resolve refuses from a top-level module (E0111): Check's and Elab's `resolveImports` admits every function. Found by bug 217's fix, 2026-10-24; fixed after the owner decided OD20 (std.libc private to std, MOD-6)
coverage: reproducer tests/regressions/bug226_submodule_imports_private_function/repro.con, pinned by run_tests.sh (`run_err` legs for the reproducer, `error_226_submodule_imports_private_extern.con` and `error_226_submodule_imports_private_struct.con`; `run_ok` for `regress_226_submodule_imports_pub.con`) and check_spec.sh S4 (`MOD-2/submodule-imports-private`, `MOD-6/program-imports-libc`, `MOD-6/program-root-imports-libc`, `MOD-6/program-uses-std`)
class: 12 — which declarations an import may name: Resolve's rule checks `pub`, the submodule rule Check and Elab use does not
silent: no
ruling: Violates: MOD-2@1, MOD-6@1 — a submodule imports a private declaration; std.libc's bindings are private to std (OD20)
-->

**Status:** FIXED (branch `wip/bug-226`)
**Found:** 2026-10-24, by bug 217's fix: walking std's submodules with Resolve's own import rule
refused `std.string`'s `import std.libc.{memcmp, memcpy}` (E0111), while Check and Elab admit it.
**Severity:** a program MOD-2@1 refuses is accepted; both backends agree, so no wrong code. Whether
an import is refused depends on where the importing module sits: the same import is E0111 from a
top-level module and accepted from a submodule (a module written after `main`, any `mod x;` file of
a project, every std module).

Codex review 2026-10-25: NOT CONFIRMED — pending owner decision OD20/exit 2n: private imports are accepted contrary to MOD-2, but ROADMAP explicitly leaves this bug awaiting the std.libc visibility decision.

Codex confirmation 2026-10-29, after the owner decided OD20: CONFIRMED — Bug 226 remains a real visibility defect to fix, and OD20's decision removes its owner-decision blocker (`resolveImports` admits non-pub functions, externs, structs and enums; twelve std modules import non-pub `std.libc` bindings; no std module imports another module's private non-libc declaration; no other owner decision needed).

## Reproducer

`tests/regressions/bug226_submodule_imports_private_function/repro.con`:

```con
fn main() -> Int {
    return b::g();
}

mod a {
    fn secret() -> Int { return 42; }
}

mod b {
    import a.{secret};
    pub fn g() -> Int { return secret(); }
}
```

Observed on main `408880cb` and on `wip/bughunt-216-217`: compiles; the binary and `--interp`
return 42. With `main` written last (so `a` and `b` are top-level modules) the import is refused:
`(E0111) 'secret' is not public in module 'a'`. The same holds in a project for `import a.{secret}`
in `src/b.con` against a private `fn secret` in `src/a.con`, and for a private `extern fn`.

## Cause

Two import rules. Resolve's `summaryImports` (top-level modules) admits a symbol only when it is in
the exporting summary's `publicNames`. `FileSummary.resolveImports`, which Check (`checkSubmodules`)
and Elab apply to a submodule, looks a function up in `summary.functions ++ summary.externFnSigs`,
every function and foreign binding of the module, public or not; only a name that is no function,
struct, enum, public alias or public newtype reaches its "not public" branch.

## Why it waited

Making `resolveImports` require `pub` is a small change, but std depends on the hole: `std.libc`
declares its bindings without `pub`, and `std.string`, `std.io` and others import them. The fix has
to give those imports a visibility: mark the `std.libc` bindings `pub` (which lets every user program
import `std.libc.malloc` and the other trusted bindings, a std API and trust-boundary change), or a
package-internal visibility (the `pub(pkg)` MOD-2's rationale describes as designed for later). That
choice waited for the owner, who decided OD20 on 2026-10-29: `std.libc`'s bindings are private to
std ([MOD-6](../spec/modules.md#MOD-6)).

## Structural cause

Class 12 ("one fact kept in two places"): "which declarations an import may name" is implemented by
Resolve's `summaryImports` and by `FileSummary.resolveImports`, and the two disagree.

## Structural owner

R-0492/R-0514 (one resolver: one import rule, consumed by Check and Elab). This fix makes the rule
one predicate (`importRefusal`) that both import paths call, ahead of the single resolver.

## Fix

`FileSummary.importRefusal` is the import rule, and `resolveImports` (Check and Elab, every module)
and Resolve's `summaryImports` (top-level modules) both apply it before binding a name: a
declaration crosses a module boundary only when it is `pub` (MOD-2: E0111, E0285, E0419 by pass),
and a binding of `std.libc` is importable by a module of std only, `pub` or not (MOD-6, E0326 in
every pass). A module belongs to std by its definition path (`std` and the modules under it), which
`FileSummary.definitionPath` now records. `resolveImports` takes the importer's summary instead of
its type spellings, so every caller names the importer. MOD-2's pending example
`submodule-imports-private` is enforced (E0285 at check), and MOD-6 is enforced, with a reject
example for a program submodule (check) and for a program root (resolve).
