# Bug 195 — a `let ... else` whose `else` block falls through compiled

<!-- bug-ledger
summary: a `let ... else` whose `else` block fell through compiled; the construct is desugared to a `match` before checking, so the design's divergence rule had no site, and falling out of the block skipped the rest of the function (both consumers, silent). Refused with E0308; pinned by `error_195_let_else_falls_through{,_unit}.con`. Found by the language specification's patterns chapter
fixtures: error_195_let_else_falls_through_unit.con error_195_let_else_falls_through.con
class: 13 — `let ... else` divergence: desugared away before any checker
silent: yes
exit: 0b
ruling: Violates: PAT-5@2
-->

**Status:** FIXED for every pattern that binds a field (branch `wip/bug195`); a `let ... else` over
a field-less variant, which binds nothing, is not recognized yet (see Fix)
**Found:** 2026-10-23, by the language specification's patterns chapter (`docs/spec/patterns.md`,
PAT-5: the `else` block diverges on every path), on `wip/spec-single-source`
**Severity:** a documented rule had no enforcing site. In a function without a result type the
program compiled and ran with a meaning no document gives it: falling out of the `else` block left
the whole rest of the function unexecuted (both consumers agreed, so it was silent). In a function
with a result type it reached the backend (bug 194's `ret void`).

Confirmed real by Codex review, 2026-10-25: PATTERN_DESTRUCTURING.md §3 and PAT-5 require a diverging else; desugaring instead silently skipped the continuation. Fieldless patterns remain incompletely fixed.

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
(its end is reachable by `blockEndUnreachable`, which looks at every statement of the block, the
same predicate as bug 194's) with the new code E0308. Fixtures:
`error_195_let_else_falls_through_unit.con` and `error_195_let_else_falls_through.con` (E0308, in
`run_tests.sh`). A rule-impact scan over the 2,169 units of the train 4 corpus found no program
newly refused. Limit: a pattern with no binders (a field-less variant, `let E::A {} = e else { .. }`)
is not recognized, because the only mark the desugaring leaves is on the binders. Such a statement
binds nothing, so the rest of the function stays meaningful, but a falling-through `else` still
skips it. The long-term fix keeps `let ... else` as a construct until the checker has seen it
(preserving its origin rather than inferring it), which R-0499's checker on Core does.

## Structural cause

Class 13, documented rules with no enforcing site: the rule was written in the design document and
then the construct was desugared away before any checker could see it. Desugaring before checking
erased the only place the rule could be stated.

## Structural owner

R-0499 (S8: checker rules on Core), on the condition that `let ... else` reaches that checker as
its own construct: moving the check later does not restore information a desugaring already
erased. R-0496 (S7) gives the construct its elaboration rule.
