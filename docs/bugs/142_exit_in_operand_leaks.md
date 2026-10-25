# Bug 142 — a `return` inside a call argument or literal field leaked the values already moved into it

<!-- bug-ledger
summary: a `return` inside a call argument or a struct/enum/array literal field, after an owned value was moved into the pending construct, was accepted and the value never destroyed (EVALUATION2 D3; `--interp` refused the shape at run time, D4's native half). Rule (d): no `return`/`break`/`continue` inside a call argument, literal element or operator operand (E0278). `error_142_*.con`, `regress_142_exits_allowed.con`. Joins exit 0b. ((`safety-eval2`))
fixtures: error_142_struct_field_return.con error_142_call_arg_return.con error_142_array_elem_return.con error_142_enum_field_return.con error_142_operand_return.con error_142_method_arg_return.con error_142_break_in_arg.con error_142_deref_operand_return.con error_142_for_cond_break.con error_142_assign_index_return.con regress_142_exits_allowed.con
class: 15 — a value moved into a construct an exit left incomplete
silent: no
exit: 0b
-->

**Status:** FIXED 2026-10-21 (branch `safety-eval2`)
**Found:** 2026-10-21, by the second semantics evaluation (EVALUATION2 D3; probes `c01` – `c06`,
`c13`, `c15`, `c16`, `v02`), not filed then. D15 (a `{ break; }` branch in value position is typed
`()`, which is all that kept `break` out of the same hole) is closed by the same rule.
**Severity:** skipped destruction in safe code: a `String` leaked (`c16`), and for a linear resource
with an external release, a leaked file, socket or lock. Also a loud interpreter/native
disagreement (`v02`: the binary returned, `--interp` failed "unexpected return in value position").

## Reproducers

`tests/regressions/bug142_exit_in_operand/repro.con` (`R` logs its id when destroyed):

```con
fn f(p: *mut Int, n: Int) -> Int {
    let a: R = mk(1, p);
    let w: W2 = W2 { a: a, b: if flag(n) { return 7; } else { mk(2, p) } };
    destroy(w);
    return 0;
}
```

On main (`5a064e77`): accepted; the binary exits 7, so `a` was never destroyed (a destroyed `a`
would make it 107). The checker marked `a` consumed when it checked the field, so the `return` saw
nothing live; at run time the literal never completed. The same held for enum and array literal
elements, call and method arguments, a nested literal (a completed inner aggregate was lost) and a
`String` field (`c16`, a heap leak).

`tests/regressions/bug142_exit_in_operand/repro_arg.con` (`return id(if n > 0 { return 3; } else {
4 }) + 1;`): the binary exits 35 (`f(1)` returned 3 without calling `id`); `--interp` fails at run time.

Fixed: both are refused at the checker, natively and under `--interp`:

```
error[check]: (E0278) `return` may not occur inside a field of the 'W2' literal: an exit there leaves a call or literal whose earlier operands were already evaluated and moved into it
```

## Cause

`Concrete/Check/Check.lean` checks each operand of a call or literal in value position, which moves
a linear operand into the pending construct, and accepted `return` anywhere an expression may
appear. Nothing modelled "moved into a construct that has not completed", and the lowering has no
cleanup for half-built constructs (Concrete has no implicit destruction). Bug 086's "no `return` in
a non-tail expression" was a rule of the proof-model extractor (`cExprNoEscape`) only.

## Fix

Rule (d) (user decision, 2026-10-21; normative text in [STATEMENT_EXPRESSION_MODEL, "Exits in
operand position"](../language/STATEMENT_EXPRESSION_MODEL.md#exits-in-operand-position)): `return`,
`break` and `continue` may not occur inside a call argument (receivers included), a struct, enum or
array literal element (a functional-update base included), or an operator operand (binary, unary,
cast, index), unless a `break`/`continue` is bound by a loop entered inside that operand. They stay
legal in statement position, in a tail, and in a let or assignment initializer's branch chain (the
let-else idiom). Enforced at `checkExpr`, the single expression entry (`protectedOperands`,
`firstOperandEscape?` in `CheckHelpers.lean`), before any operand is checked: E0278
(`exitInOperand`).

Fixtures (`run_tests.sh`): `error_142_struct_field_return.con`, `error_142_call_arg_return.con`,
`error_142_array_elem_return.con`, `error_142_enum_field_return.con`,
`error_142_operand_return.con`, `error_142_method_arg_return.con`, `error_142_break_in_arg.con`
(D15), `error_142_deref_operand_return.con` (dereference, borrow and field-access bases are operands
too) `error_142_for_cond_break.con` (a `for` condition is outside its loop) and
`error_142_assign_index_return.con` (an assignment's destination is an operand; all three from Codex
review), and the positive control `regress_142_exits_allowed.con` (let-initializer return, tail
`return if`, statement match, a `break` bound inside the operand; native and `--interp` agree).

Migration: 1 site, `tests/regressions/bug086_discarded_tail/repro.con`'s `argret` (which existed
to show the extractor declines that shape); it is removed there, since the language now
refuses the shape (`repro_arg.con` above is the same program), and `check_model_diff.sh`
leg T no longer lists it. `std/` and `examples/`: 0.

## Structural cause

Ownership state not modeled along control-flow paths ([structural class](STRUCTURAL_CLASSES.md) 15):
the checker's "consumed" meant "handed to a construct", not "the construct completed", so an exit
between the two lost the value. Also a documented-rule gap (class 13): the only statement of the
rule was the extractor's subset, with no enforcing site in the language.

## Structural owner

Containment (this rule): ROADMAP exit 0b. The normative statement-versus-value-position rule:
exit 2j (R-0500). The Core form is rule B (an operand already evaluated for a pending construct is
a live owned temporary until the construct completes), which R-0499's Core checker enforces once
operands are A-normal, and which accepts everything rule (d) accepts; Place MIR (R-0501) then needs
no partial-construction cleanup edges.
