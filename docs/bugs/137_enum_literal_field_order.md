# Bug 137 — enum-variant literal fields are evaluated in declaration order

**Status:** OPEN
**Found:** 2026-10-21, while fixing [bug 127](127_struct_literal_field_order.md) (recorded as
violated, unfiled, in the evaluation-order table of
[STATEMENT_EXPRESSION_MODEL](../language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment);
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

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `runs-exit:65`) requires
the binary to exit with the recorded status 65. When it exits 56, that leg fails until this
document, the corpus entry and the leg are updated together. Until then, write enum-variant fields
in declaration order when their initializers have effects.
