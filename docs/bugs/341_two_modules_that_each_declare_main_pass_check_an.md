# Bug 341 — two modules that each declare main pass Check and the native build fails in clang on a duplicate @__concrete_argc

**Status:** OPEN
**Found:** 2026-10-31, by a hand probe of the open question the spec quality audit raised on more than one `main` (OPEN-141 on the spec branch); confirmed by Codex and Grok
**Severity:** loud: a program Check accepts fails to build natively, inside clang, with no Concrete diagnostic

<!-- bug-ledger
summary: two modules that each declare main pass Check and the native build fails in clang on a duplicate @__concrete_argc
coverage: reproducer tests/regressions/bug341_two_modules_that_each_declare_main_pass_check_an/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, compile-fails on the duplicate global)
class: 1
silent: no
ruling: Violates: PASS-12@1
-->

## Reproducer

`tests/regressions/bug341_two_modules_that_each_declare_main_pass_check_an/repro.con`: two submodules, each with a `pub fn main() -> u8` (returning 7 and 9).

## Observed

Check accepts the program. The native build stops in clang: `error: redefinition of global '@__concrete_argc'` (`clang compilation failed`, exit 1), with no Concrete diagnostic or code. `--interp` prints 7: it runs the first `main` it meets. A submodule `main` beside a root `main` behaves the same way natively, and `--interp` then runs the submodule's.

## Expected

[PASS-12@1](../spec/pipeline.md#PASS-12): a program every rule accepts is compiled, and no stage after Check crashes on it or emits LLVM IR that fails validation. No rule refuses several `main`s today (MOD-3 makes a name unique within one module), so the program must compile, or, once the owner decides which `main` is the entry point, be refused at Check with a diagnostic. Which of those is the open owner question on more than one `main`; this bug is the invalid IR, which no answer allows.

## Cause

`Concrete/Elab/Elab.lean` marks every function spelled `main`, in any module, as an entry point (`isEntryPoint := f.name == mainFnName`), and `emitSModule` in `Concrete/Backend/EmitSSA.lean` emits `emitMainWrapper` for each module that has one; the wrapper defines the program-wide globals `@__concrete_argc` and `@__concrete_argv` and the process `main` under fixed names, so the second module's wrapper defines them again.

## Structural cause

Class 1 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): the entry point is a declaration identified by its spelling (`main`) in every module, not one resolved declaration of the program, so a program-level fact is emitted once per spelling. The test runner had the same per-module duplication of `@__concrete_argc` and was fixed by emitting the stubs once at program level (see `emitTestRunner`).

## Structural owner

R-0492 (declaration identities, `DefId` from Resolve on): the entry point becomes one resolved declaration of the program, chosen or refused once by the rule the owner decides, and the backend emits the process wrapper once from it.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the clang failure. The defect is loud, so no build produces wrong code; meanwhile a program declares `main` in one module only.
