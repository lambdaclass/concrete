# Bug 194 — a function with a result type compiled with a reachable end and no `return`

**Status:** FIXED (branch `wip/bug194`)
**Found:** 2026-10-23, by the language specification's patterns chapter (`docs/spec/patterns.md`,
PAT-5's `let ... else` example whose `else` block falls through), on `wip/spec-single-source`
**Severity:** a program the language refuses reached the backend. Natively it failed in `llvm-as`
("value doesn't match function result type", a `ret void` in a non-void function), so it failed
closed but past every compiler check; `--interp` ran it and returned whatever the taken path
produced, so the two consumers disagreed.

## Reproducer

```con pseudocode
fn f() -> Int {
    let x: Int = 1;
}
fn main() -> Int { return f(); }
```

Expected: refused by the checker. Observed on `wip/train4` `a4a9c05d`: `llvm-as` error natively.
The same happens with `if c { return 1; }` as the last statement, with a method or a trait-impl
method, and with a `let ... else` whose `else` block falls through in a function with a result
type (the desugared `match`'s catch-all arm reaches the end).

## Cause

`Concrete/Check/Check.lean`, `checkFn`: nothing compared the end of the body with the result type.
Every `return e` is checked against it, but a path that reaches the end of the body was never
looked for. Function bodies have no trailing value expression, so a reachable end is always a
missing value. CoreCheck has no such check either, so Lower emitted `ret void`.

## Fix

`checkFn` refuses a function whose result type is not `Unit` or `!` when its body can reach its
end (`blockDiverges` and `blockNonTerminating` both false), with the new code E0307. Fixtures:
`error_194_missing_return_plain.con`, `error_194_missing_return_if.con`,
`error_194_missing_return_method.con` and `error_194_let_else_falls_through.con` (E0307, in
`run_tests.sh`). A rule-impact scan (`scripts/tools/rule_impact.sh`, 2,169 units of the train 4
corpus) found no program newly refused.

## Structural cause

Class 13, documented rules with no enforcing site: that a function returns a value of its result
type on every path is the most basic typing rule, and it had no site; the backend's verifier was
the only thing that noticed. It is also class 7 (semantics implemented several times): the
interpreter and the backend each did something different with the missing value.

## Structural owner

R-0499 (S8: checker rules authoritative on Core, including "every reachable exit returns a value
of the result type"), with R-0496 (S7) defining what falling off the end means. The language
specification states the rule (statements chapter) with a reject example that runs on both
consumers.
