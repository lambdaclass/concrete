# Bug 270 — Lower gives every loop a header phi for every binding in scope

<!-- bug-ledger
summary: Lower gives every loop a header phi for every binding in scope, so n loops in a function of n bindings emit n² phis that the cleanup then removes (quadratic to lower)
coverage: the `let`-then-`while` shape of the long-function measurement in this record; no gate bounds it yet (a time leg fails until it is fixed)
class: 17 — Lower's loop phis
silent: no
ruling: Outside the spec: performance — Lower gives every loop a header phi for every binding in scope (n² phis)
-->

**Status:** OPEN
**Found:** 2026-10-25, measuring the back end over the long-function shapes of bugs 255-261, after
bugs 265-267 were fixed.
**Severity:** loud (performance). A function of n bindings with n loops emits n² header phis; the
raw SSA, its verification and the cleanup are quadratic in the function's length (and memory
grows the same way).
Confirmed real by Codex review, 2026-10-25: although valid SSA construction, a header phi for every non-promoted binding without checking whether the loop changes it creates quadratically many avoidable phis that cleanup cannot make cheap after the fact.

## Reproducer

`let s<i>: i64 = s<i-1> + 1; while t < s<i> { t = t + 1; }` for i = 1..n. The `lower` stage took
3.7 s at 1000, 22.8 s at 2000 and 82.5 s at 4000 (after bugs 265-267; before them 4.6 s at 1000 and
36.4 s at 2000 on main).

## Cause

`IR/Lower.lean`, the `.while_` case of `lowerStmt`: every binding in scope that is not promoted gets
a header phi (`setVar name (.reg phiReg ty)`), and the exit table is rebuilt from all of them. A
binding the loop never writes gets `phi [pre, self]`, which `eliminateTrivialPhis` removes.

## Expected

A header phi only for the bindings the loop can write (Braun et al.'s on-demand SSA construction,
or a syntactic over-approximation of the loop's writes checked against the body's table after
lowering).

## Why not fixed here

Every repair changes the emitted IR. The header phi registers are numbered from the function's one
register counter, so dropping them renumbers every later register of the cleaned SSA; reserving the
numbers keeps the names but changes what the post-loop table holds (the phi register, which later
merges compare by name, versus the pre-loop value, which can alias another binding's), so the
merge decisions after a loop can differ. The fix needs an output-changing step that a whole-corpus
byte comparison cannot accept, so it is filed for a deliberate decision.

## Containment while open

Loud only: compile time grows quadratically in a function with many bindings and many loops, never
wrong code. A user splits such a function.

## Structural cause

Class 17: per-query recomputation in a constructive form: the loop answers "which bindings may
change here" by assuming all of them, and the cleanup pays to undo it.

## Structural owner

R-0422 (complexity envelopes), with the structured control-flow builder (ROADMAP #5) owning SSA
construction in Lower.
