# Bug 314 — a for loop whose body always breaks fails SSA verification natively (E0703) while --interp runs it

**Status:** OPEN
**Found:** 2026-10-28, by the class-13 spec audit's follow-up probes of `break` values in loops (`wip/class13-audit`, `research/process/class13-audit-2026-10/probes5/`), native against `--interp`.
**Severity:** loud: an accepted program fails to build natively with an internal SSA verifier error (E0703), while `--interp` runs it; the consumers disagree.

<!-- bug-ledger
compiler: legacy
summary: a for loop whose body always breaks fails SSA verification natively (E0703) while --interp runs it
coverage: archived reproducer(s) for bug 314; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 8
silent: no
ruling: Violates: PASS-9@1
-->

## Reproducer

`tests/regressions/bug314_a_for_loop_whose_body_always_breaks_fails_ssa_ve/repro.con`: `for (let mut i: Int = 0; i < 3; i = i + 1) { break true; }` in `main`, then `return 42;`.

## Observed

On main ca49fada the native build fails: `error[ssa-verify]: (E0703) main: block 'for.step3': use of %phi.0 defined in non-dominating block`. `--interp` prints 42. A bare `break;` at the end of a `for` body fails the same way (Codex confirmation); the same body in a `while` loop compiles.

## Expected

Historical expectation, recorded when the bug was filed; the decided language has since changed it.
Under [OD27](../spec/decisions.md) a valued `break` is refused, and under [OD47](../spec/decisions.md) C-style `for`
headers are removed, so today this program is refused. The SSA defect it exposed stays recorded
against `legacy-compiler-final`. At filing time the program was valid: [STMT-6](../spec/statements.md#STMT-6) admits `break` in a loop, and a `break` value is evaluated and discarded (bug 149). [PASS-9](../spec/pipeline.md#PASS-9) requires lowering to produce SSA that passes the verifier. It builds and exits 42 on both consumers.

## Cause

`Concrete/IR/Lower.lean`, `lowerStmt`'s `.while_` case for a `for` loop (the `hasStep` branch): after a body that ends terminated, it still started the step block and lowered the step into it, though neither the body's fall-through nor any `continue` reaches it. The step read the header's phi in a block the header does not dominate, and its back-edge gave the header a phantom incoming edge. The `while` path omits the back-edge when the body is terminated; the `for` path had no such guard.

## Structural cause

Class 8: storage, places and control flow decided ad hoc in `Lower`. Each loop form builds its
blocks and edges by hand and decides on its own which blocks are reachable; the step block of a
`for` is the one place a loop body's termination was not consulted, as 078 and 175 were the merge
blocks of `if` and `match` whose arms all diverge.

## Structural owner

ROADMAP #5 (the structured Lower/SSA builder, where a block is created only with its predecessor
edges) and R-0504 (S12 validators). Until then the step block is terminated as unreachable when no
edge reaches it.

## Historical branch containment (unlanded)

The filing branch contained an unlanded repair attempt; it is archived, not part of the frozen compiler.

## Archived fix attempt

In the `hasStep` branch, when the body ended terminated and no `continue` edge reaches the step,
the step block is terminated with `unreachable` before the step is lowered; `lowerStmts` then skips
the step, and the existing `stepTerm` guards drop the back-edge and its header-phi input. A step
that the body's fall-through or a `continue` reaches is lowered as before.

Codex confirmation 2026-10-28: CONFIRMED (loud) — after `break` terminates the body, lowering still creates and lowers `for.step`, which has no predecessor, reads the header's phi and emits a back-edge, so the header does not dominate it (E0703); the value is incidental; no owner decision is involved.

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-314`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/programs/regress_314_for_body_always_breaks.con`
- `tests/regressions/bug314_a_for_loop_whose_body_always_breaks_fails_ssa_ve/repro.con`
