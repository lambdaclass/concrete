# Bug 136 — functional update evaluates its base once per omitted field

**Status:** OPEN
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

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `runs-exit:87`) requires
the binary to exit with the recorded wrong status. When it exits 59, that leg fails until this
document, the corpus entry and the leg are updated together. Until then, write the base as a
variable: `let b: P = mk(&mut t); P { b: ..., ..b }` evaluates `mk` once.
