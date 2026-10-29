# Bug 136 — functional update evaluates its base once per omitted field

<!-- bug-ledger
summary: functional update `S { f: e, ..base }` evaluated its base once per omitted field (599 instead of 59). Elab evaluates the explicit initializers, then the base exactly once, into temporaries (`letInExpr`) when the base is not a variable; pinned by `regress_136_137_literal_eval_order.con` (`--interp == binary`, 42) and the reproducer's `run_ok` (59)
fixtures: regress_136_137_literal_eval_order.con
class: 7 — until 127's fix the consumers disagreed
class: 13 — evaluation-order rule 6: Core cannot bind the base once
silent: yes
exit: 0b
ruling: Violates: STMT-4@1
-->

**Status:** FIXED (branch `soundness-holes`)
**Found:** 2026-10-21, while fixing [bug 127](127_struct_literal_field_order.md) (recorded as an
unfiled disagreement in the evaluation-order section of
[STATEMENT_EXPRESSION_MODEL](../language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment)
and in ROADMAP exit 0b).
**Severity:** silent wrong code. A base expression with effects runs once per field it supplies,
and each omitted field may come from a different evaluation of it.

## Reproducer

`tests/regressions/bug136_functional_update_base_reevaluated/repro.con`:

```con
struct Copy P { a: Int, b: Int, c: Int }

fn tick(t: &mut Int, v: Int) -> Int {
    *t = *t * 10 + v;
    return v;
}

fn mk(t: &mut Int) -> P {
    *t = *t * 10 + 9;
    return P { a: 1, b: 2, c: 3 };
}

fn main() -> Int {
    let mut t: Int = 0;
    let p: P = P { b: tick(&mut t, 5), ..mk(&mut t) };
    return t + p.a * 0 + p.b * 0 + p.c * 0;
}
```

With the bug 127 fix: `t` is 599 in both consumers (`tick(5)`, then `mk` once for `a` and once
for `c`), so the binary exits 87 (599 mod 256) and `--interp` prints 599. Expected: 59 (rule 6:
the explicit initializers in source order, then the base exactly once). On main before the bug 127
fix the two consumers also disagreed: the binary interleaved the base reads with the explicit
initializer in declaration order (`mk`, `tick`, `mk`: 959, exit 191).

## Cause

`Concrete/Elab/Elab.lean`, the `.structLit` case of `elabExprEv`: the base is elaborated once to a
Core expression, but every omitted field is then filled with `.fieldAccess cBase field`, so the
Core literal contains one copy of the base expression per omitted field, and every consumer
(`Lower`, the interpreter) evaluates each copy. The comment there says to use a variable as the
base, which is today's behaviour, not the rule.

## Structural cause

A documented rule with no enforcing site ([structural class](STRUCTURAL_CLASSES.md) 13): rule 6 is
normative but the desugaring that implements functional update duplicates a subexpression, and
Core has no expression-level `let` to bind it once. It is also class 7 in its history (the binary
and the interpreter disagreed until the bug 127 fix).

## Structural owner

R-0500 (exit 2j, evaluation order) for the rule's enforcing site, with R-0496 (S7 Core semantics,
where an expression-level binding makes "evaluate once" expressible). As silent wrong code it is in
exit 0b under the defect disposition rule. The repair binds the base to a fresh local after the
explicit initializers (which then also need temporaries, so they still run first) and reads the
omitted fields from that local.

## Fix

Elab builds both literals so that Core evaluates their initializers in the rule's order, through one
helper pair: `freshEvalTemp` / `letInExpr`. Core has no expression-level `let` yet (R-0496), and
`match value { t => body }` with one unguarded binder arm is exactly `let t = value in body` in
every consumer (Lower, the interpreter and the proof model bind a variable arm's scrutinee once).
The temporaries are named `ev.t<n>`, which no source identifier can spell.

- Bug 137, an enum-variant literal: initializers are elaborated in source order. The Core literal
  keeps declaration order, which is the payload's positional identity for Lower's layout, the
  interpreter and the proof model, so when the written order differs and at least two
  initializers are order-sensitive (anything but a literal), they are evaluated into temporaries
  in source order first (`withSourceOrder`). Literals already written in declaration order, and
  literals with at most one order-sensitive initializer, produce the same Core as before.
- Bug 136, functional update: the explicit initializers in source order, then the base exactly
  once, then the omitted fields read from the base's temporary. A base that is a variable is read,
  not evaluated, and every omitted field is read after the explicit initializers, so it keeps the
  old inline form; so does an update that omits nothing.

Check decides ownership for an enum literal's initializers in source order too, since that is now
the order they run in (a borrow written after a move is the borrow of a moved value). The enum
literal's evidence (its proof identity) records the fields in source order, so two literals that
differ only in written order, and therefore in execution order, have different identities; a
literal written in declaration order keeps its identity. All three were found by the Codex review
of the first version, with the array case below.

Regression fixture: `tests/programs/regress_136_137_literal_eval_order.con`: an enum literal written
in reverse declaration order, a three-field variant with a literal in the middle, a functional
update with one explicit initializer and with two written out of declaration order; each checks the
order its ticks recorded and the values; exit 42 natively and under `--interp` (`run_tests.sh`,
`run_ok` and the `--interp == binary` loop). The reproducers now exit 59 (136) and 56 (137)
(`run_ok`); before the fix they exit 87 and 65.

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `runs-exit:87`) requires
the binary to exit with the recorded wrong status. When it exits 59, that leg fails until this
document, the corpus entry and the leg are updated together. Until then, write the base as a
variable: `let b: P = mk(&mut t); P { b: ..., ..b }` evaluates `mk` once.
