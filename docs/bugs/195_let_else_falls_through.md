# Bug 195 — a `let ... else` whose `else` block falls through compiled

**Status:** FIXED (branch `wip/bug195`)
**Found:** 2026-10-23, by the language specification's patterns chapter (`docs/spec/patterns.md`,
PAT-5: the `else` block diverges on every path), on `wip/spec-single-source`
**Severity:** a documented rule had no enforcing site. In a function without a result type the
program compiled and ran with a meaning no document gives it: falling out of the `else` block left
the whole rest of the function unexecuted (both consumers agreed, so it was silent). In a function
with a result type it reached the backend (bug 194's `ret void`).

## Reproducer

```con pseudocode
fn check(o: Option<Int>, out: &mut Int) {
    let Option::Some { value } = o else {
        *out = 1;
    };
    *out = value;
}
fn main() -> Int {
    let mut out: Int = 0;
    check(Option::None, &mut out);
    return out;
}
```

Expected: refused (PATTERN_DESTRUCTURING §3 and PAT-5: "the compiler rejects an else block that
falls through"). Observed on `wip/spec-single-source` `dbb19f87` (train 4 base): native exit 1,
`--interp` prints 1.

## Cause

`Concrete/Frontend/AST.lean`, `desugarStmts`: a `let P = e else { E }; rest` becomes
`match e { P => { rest }, _ => { E } }` before the checker runs, so the checker never sees a
`let ... else`, and the divergence requirement written in the design had no site. A falling-through
`E` ends the match, which was the last statement of the function.

## Fix

`checkExprCore`'s `match` case recognizes the desugared form (a variant arm whose binders are
`letBound`, then a catch-all `_` arm) and refuses an `else` body that can fall through
(`blockDiverges` and `blockNonTerminating` both false) with the new code E0308. Fixtures:
`error_195_let_else_falls_through_unit.con` and `error_195_let_else_falls_through.con` (E0308, in
`run_tests.sh`). A rule-impact scan over the 2,169 units of the train 4 corpus found no program
newly refused. Limit: a pattern with no binders (`let E::A {} = e else { .. }`) is not recognized,
because nothing marks its desugared arm; R-0499 removes the limit by checking Core's own
`let ... else` form.

## Structural cause

Class 13, documented rules with no enforcing site: the rule was written in the design document and
then the construct was desugared away before any checker could see it. Desugaring before checking
erased the only place the rule could be stated.

## Structural owner

R-0499 (S8: checker rules on Core; elaboration keeps `let ... else` as a construct with its own
judgment instead of erasing it first) and R-0496 (S7: the elaboration relation defines it).
