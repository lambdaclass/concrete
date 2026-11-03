# Bug 339 — An ordering comparison of two bool values compiles natively and fails at run time under --interp

**Status:** OPEN
**Found:** 2026-10-30, by the outside-reader test of the specification (60 programs written from SPEC.md alone on wip/spec-phase2 d21a4cfd, run native and under `--interp` on main 71e3e804 and on d21a4cfd).
**Severity:** loud: `true < false` compiles and the binary exits 42, while `--interp` stops at run time with "interp: unsupported binop on given value types" (exit 1), so the two consumers give one accepted program different behaviours.

Codex confirmation 2026-10-30: REAL-BUG. "Whether Boolean ordering should be accepted is open, but agreement between consumers is independently required: PASS-13 promises 'the same result, the same output and the same traps' for an accepted program. Native execution returns 42 while the interpreter rejects the operation. OPEN-42 leaves the eventual operator policy undecided; it does not exempt currently accepted programs from PASS-13."

<!-- bug-ledger
compiler: legacy
summary: An ordering comparison of two bool values compiles natively and fails at run time under --interp
coverage: archived reproducer(s) for bug 339; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 7
silent: no
exit: 0b
ruling: Violates: PASS-13@1
-->

## Reproducer

`tests/regressions/bug339_an_ordering_comparison_of_two_bool_values_compil/repro.con`:

```con
fn main() -> Int {
    let b: bool = true < false;
    if (b) { return 1; }
    return 42;
}
```

## Observed

The program compiles; the binary exits 42 (`true < false` is false). `--interp` exits 1 with
"interp: unsupported binop on given value types". `==` and `!=` on `bool`, and `<` on `char`,
agree on both consumers.

## Expected

[PASS-13@1](../spec/pipeline.md#PASS-13) requires agreement between consumers on accepted programs.
The source record was filed while [OD42](../spec/decisions.md#od42) was undecided. The owner
subsequently decided on 2026-11-01 to refuse ordering on `bool` (keeping equality on bool/char
and ordering on char). The decided expected behaviour for this reproducer is refusal on both
consumers. Its native exit 42 and interpreter failure remain historical observations against
the frozen compiler, not required passing outcomes.

## Cause

`Concrete/Interp/Interp.lean`, the binary-operation evaluator: the `.bool` cases cover `.and_`,
`.or_`, `.eq` and `.neq` only, so an ordering operator on two `.bool` values falls through to
the final "unsupported binop on given value types" case. Check accepts the comparison (main
compares `bool`, OPEN-42's rationale), and the native lowering compares the two values as
integers.

## Structural cause

Class 7 (semantics implemented several times): the meaning of a comparison is given separately by
Check's operand rule, the lowering and the interpreter's evaluator, and nothing makes the
evaluator's case list follow the set of operand types Check admits. Bugs 140 and 134 are earlier
comparison instances of the class.

## Structural owner

R-0496 (S7 Core semantics: one definition of each Core operation that the interpreter evaluates
and the backends refine), with OD42 now deciding that bool ordering is refused.

## Historical branch containment (unlanded)

check_example_outputs.sh's open-bug reproducers leg pins the native acceptance (exit 42). Code
that needs an order on `bool` writes it explicitly (`!a && b` for `a < b`), which both consumers
agree on.

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-bool-order-interp`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug339_an_ordering_comparison_of_two_bool_values_compil/repro.con`
