# Bug 226 — a submodule can import a private function of another module

<!-- bug-ledger
summary: a submodule (a module written after `main`, any `mod x;` file, every std module) may import a private function or `extern fn` of another module, which MOD-2@1 refuses and Resolve refuses from a top-level module (E0111): Check's and Elab's `resolveImports` admits every function. Found by bug 217's fix, 2026-10-24 ((the fix needs a visibility for std's internal `std.libc` imports))
coverage: a submodule may import a private function or extern of another module (FileSummary.resolveImports, used by Check and Elab for submodules, admits every function), while a top-level module importing it is E0111; fixing it needs a visibility for std's imports of std.libc's non-pub bindings; reproducer tests/regressions/bug226_submodule_imports_private_function/repro.con, found by bug 217's fix 2026-10-24; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, compiles)
class: 12 — which declarations an import may name: Resolve's rule checks `pub`, the submodule rule Check and Elab use does not
silent: no
-->

**Status:** OPEN (the fix needs std's internal imports of `std.libc` to be given a visibility, which
is a std API decision)
**Found:** 2026-10-24, by bug 217's fix: walking std's submodules with Resolve's own import rule
refused `std.string`'s `import std.libc.{memcmp, memcpy}` (E0111), while Check and Elab admit it.
**Severity:** a program MOD-2@1 refuses is accepted; both backends agree, so no wrong code. Whether
an import is refused depends on where the importing module sits: the same import is E0111 from a
top-level module and accepted from a submodule (a module written after `main`, any `mod x;` file of
a project, every std module).

Codex review 2026-10-25: NOT CONFIRMED — pending owner decision OD20/exit 2n: private imports are accepted contrary to MOD-2, but ROADMAP explicitly leaves this bug awaiting the std.libc visibility decision.

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

## Why it is not fixed here

Making `resolveImports` require `pub` is a small change, but std depends on the hole: `std.libc`
declares its bindings without `pub`, and `std.string`, `std.io` and others import them. The fix has
to give those imports a visibility: mark the `std.libc` bindings `pub` (which lets every user program
import `std.libc.malloc` and the other trusted bindings, a std API and trust-boundary change), or a
package-internal visibility (the `pub(pkg)` MOD-2's rationale describes as designed for later). That
choice is not made here.

## Structural cause

Class 12 ("one fact kept in two places"): "which declarations an import may name" is implemented by
Resolve's `summaryImports` and by `FileSummary.resolveImports`, and the two disagree.

## Structural owner

R-0492/R-0514 (one resolver: one import rule, consumed by Check and Elab), after the visibility of
std's internal bindings is decided.
