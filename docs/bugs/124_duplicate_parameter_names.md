# Bug 124 — duplicate parameter names pass the checker and fail in `llvm-as`

**Status:** OPEN
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
Int)` with the proposed E0301 (`shadowsVisibleBinding`).

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

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode
`compile-fails:redefinition of argument`) requires the recorded `llvm-as` failure. When the checker
rejects the program, that leg fails until this document, the corpus entry and the leg are updated
together.
