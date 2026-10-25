# Bug 124 — duplicate parameter names pass the checker and fail in `llvm-as`

<!-- bug-ledger
summary: `fn f(a: Int, a: Int)` passed the checker and failed in `llvm-as` ("redefinition of argument"), while `--interp` ran it. Fixture `error_2i_scope1_duplicate_params.con`. (2026-10-21 (exit 2i SCOPE-1@1, E0301))
fixtures: error_2i_scope1_duplicate_params.con regress_2i_scope1_duplicate_params_renamed.con
class: 2
class: 13 — closed by SCOPE-1@1, exit 2i
silent: no
-->

**Status:** FIXED 2026-10-21 (exit 2i, SCOPE-1@1: E0301 at binder introduction)
**Found:** 2026-10-21, by the exit 2i specification (MEMORY_SEMANTICS §15.4 SCOPE-1, gap G3).
**Severity:** loud failure on an accepted program: the checker accepts it, LLVM validation rejects
the emitted IR, and the interpreter runs it.

## Reproducer

`tests/regressions/bug124_duplicate_parameter_names/repro.con`:

```con
fn f(a: Int, a: Int) -> Int {
    return a;
}

fn main() -> Int {
    return f(1, 2);
}
```

On main (`61ca25d1`):

```
LLVM IR validation failed for .../repro.con.ll:
llvm-as: .../repro.con.ll:836:23: error: redefinition of argument '%a'
define i64 @f(i64 %a, i64 %a) {
                      ^
```

`--interp` runs it and prints 2 (the later parameter wins). Same on `land2-fix` (`74f7284a`, built
at that revision): the same `llvm-as` error and the same interpreter result, so batch 2 does not
close it.

Expected: a checker diagnostic naming both parameters. Exit 2i's SCOPE-1 rejects `fn f(a: Int, a:
Int)` with E0301 (`shadowsVisibleBinding`), as the fix below records.

## Cause

`checkFn` in `Concrete/Check/Check.lean` adds each parameter to the environment with no check that
the name is already bound by an earlier parameter (E0292's `checkNoLiveShadow` covers `let` and
destructuring only). Lower and EmitSSA name LLVM arguments by source name, so two parameters become
two `%a`. The interpreter binds parameters into a name-keyed environment, where the second shadows
the first. This breaks LANGUAGE_INVARIANTS §19 (front-end acceptance implies back-end compilation,
and the interpreter must not run what the compiler cannot compile).

## Structural cause

Binder identity by name ([structural class](STRUCTURAL_CLASSES.md) 2, with 087, 093, 094 and 107):
the checker, the interpreter and the backend each key parameters by their spelling, so two binders
with one spelling are one binding to the interpreter and two clashing names to LLVM. It is also a
documented rule with no enforcing site (class 13): one lexical name, one binding (SCOPE-1, ratified
for R-0435 on 2026-07-25) is stated but not checked for parameters.

## Structural owner

R-0500 (exit 2i, SCOPE-1): the E0301 check at binder introduction covers parameters, with
`fn f(a: Int, a: Int)` as a rejection fixture; this bug is one input to that rule. R-0492 (S5
`LocalId`) removes the name-keyed backend argument naming. The repair is queued under exit 2c
(loud failures that keep valid programs out of the interpreter/native comparison); no safety or
authority defect is demonstrated.

## Fix

Exit 2i SCOPE-1@1 ([MEMORY_SEMANTICS.md](../language/MEMORY_SEMANTICS.md) §15.4). `checkFn` runs
`scope1Diagnostics` (`Concrete/Check/Check.lean`) before it checks the body: an explicit lexical
scope chain over every binder kind, starting with the parameters in order, so the second `a` of
`fn f(a: Int, a: Int)` is E0301 (`shadowsVisibleBinding`), with the versioned migration evidence
(`rule: SCOPE-1@1`, `spec: docs/language/MEMORY_SEMANTICS.md §15`) and the rewrite in `next:`. The
program no longer reaches Lower, EmitSSA or the interpreter.

Fixtures: `tests/programs/error_2i_scope1_duplicate_params.con` (E0301) and its renamed twin
`regress_2i_scope1_duplicate_params_renamed.con` (run 42, binary = `--interp`), both rows of
`tests/rules/exit_2i.tsv` (`scripts/tests/check_rules_2i.sh`) and of `run_tests.sh`. The reproducer
above now fails with E0301; its open-bug row in `check_example_outputs.sh` was dropped with the fix.
The name-keyed backend argument naming stays for R-0492 (`LocalId`); the source rule keeps it from
being reached.
