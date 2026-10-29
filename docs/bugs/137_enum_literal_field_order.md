# Bug 137 — enum-variant literal fields are evaluated in declaration order

<!-- bug-ledger
summary: enum-variant literal fields ran in declaration order in both consumers (65 instead of 56). Elab elaborates them in source order and, when that differs from declaration order, evaluates them into temporaries before building the declaration-ordered payload; pinned by `regress_136_137_literal_eval_order.con` and the reproducer's `run_ok` (56)
fixtures: regress_136_137_literal_eval_order.con
class: 7 — the payload list is both evaluation order and positional identity
silent: yes
exit: 0b
ruling: Violates: STMT-4@1
-->

**Status:** FIXED (branch `soundness-holes`)
**Found:** 2026-10-21, while fixing [bug 127](127_struct_literal_field_order.md) (recorded as
violated, unfiled, in the evaluation-order table of
[STATEMENT_EXPRESSION_MODEL](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment);
the semantics probe `evalorder.con` measured it first).
**Severity:** silent wrong code against the normative rule. The binary and `--interp` agree with each
other, so the differential harness cannot see it.

## Reproducer

`tests/regressions/bug137_enum_literal_field_order/repro.con`:

```con
enum E { V { a: Int, b: Int } }

fn tick(t: &mut Int, v: Int) -> Int {
    *t = *t * 10 + v;
    return v;
}

fn main() -> Int {
    let mut t: Int = 0;
    let e: E = E::V { b: tick(&mut t, 5), a: tick(&mut t, 6) };
    match e {
        E::V { a, b } => { return t + a * 0 + b * 0; }
    }
}
```

Both the binary and `--interp` give 65 (`tick(6)` for `a` ran first). Expected: 56 (rule 4: struct
and enum literal fields in the order written).

## Cause

`Concrete/Elab/Elab.lean`, the `.enumLit` case of `elabExprEv`, walks the variant's declared fields
and looks each one up in the literal, so the Core literal's field list, which is the evaluation
order for every consumer, is in declaration order. The struct-literal case was changed to walk the
literal's own list on 2026-08-08; the enum case was not.

## Structural cause

Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7): the field list's
order is both the evaluation order and, for enums, the payload's positional identity. `Lower`
stores the payload by position (`Layout.variantFieldOffset` with the list index), and the
interpreter (`bindEnumFields`) and the proof model (`matchPat`) bind a match arm's payload by
position. Changing Elab alone would therefore change layout and binding, not just order, so the
repair has to make the payload name-keyed (or evaluate into temporaries) in all of them at once.

## Structural owner

R-0500 (exit 2j, evaluation order) for the rule, with R-0501 (S10 Place MIR: evaluation order
independent of layout) and R-0496 (S7 Core semantics). Silent wrong code: exit 0b under the defect
disposition rule. The extracted model and the v2 identity bytes record the payload list, so a
repair that reorders it states which identities move.

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

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `runs-exit:65`) requires
the binary to exit with the recorded status 65. When it exits 56, that leg fails until this
document, the corpus entry and the leg are updated together. Until then, write enum-variant fields
in declaration order when their initializers have effects.
