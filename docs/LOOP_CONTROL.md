# Loop Control

Status: IMPLEMENTED — ROADMAP Phase 6 #4. Gated by
`scripts/tests/check_loop_control.sh` (fixtures in `tests/programs/loop_control/`).
Date: 2026-06-21

This documents the loop-control surface Concrete already has: `break`,
`continue`, labeled loops, and loop results. It is a behavior reference,
not a proposal — every rule below is locked by the gate.

## Loop forms

- **`while cond { … }`** — statement loop.
- **`for (init; cond; step) { … }`** — C-style statement loop (`init`/`step` are
  ordinary statements; `cond` is the continue test).
- **`while cond { … } else { … }`** — while-as-**expression** (see below). The
  `else` block supplies the value when the loop ends without `break`.

There is no bare `loop { … }` form; use `while true { … }`.

## `break` and `continue`

- `break;` exits a loop. `continue;` jumps to the next iteration (for `for`, the
  `step` still runs via the loop back-edge).
- **Scope: innermost by default.** An unlabeled `break`/`continue` affects only
  the innermost enclosing loop.
- **Labeled loops.** A loop may carry a label (`'outer: while …`), and
  `break 'outer;` / `continue 'outer;` target that loop, exiting/continuing all
  inner loops in between. Labels are the only way to act on an outer loop.

## while-as-expression — REMOVED (Phase 6D item 2)

Value `while … else` was removed: `while` is statement-only. The loop-result
migration is a pre-declared mutable variable assigned before `break;`
(`tests/programs/loop_break_result_*.con`). The section below documents the
PRE-6D behavior for historical reference.

## while-as-expression (historical, pre-6D)

A `while` used in value position produces a value:

```
let v: i32 = while i < 10 {
    if found(i) { break i; }   // break value -> the loop's value
    i = i + 1;
} else {
    -1                         // else value -> used when the loop ends normally
};
```

- A `break <expr>;` inside the loop makes `<expr>` the loop's value.
- If the loop's condition goes false without a `break`, the **`else` block's
  trailing value** is the loop's value. The `else` block follows the
  statement-vs-trailing-expression rule (see `docs/STATEMENT_EXPRESSION_MODEL.md`):
  a trailing expression with no `;` is the value; `else { 42; }` is `Unit` and
  will not typecheck where a value is expected.
- **Type agreement:** the `break` value type and the `else` value type must
  match. A mismatch is **E0222** (`while-expression break type 'T' does not match
  else type 'U'`).
- **Width correctness:** the value is produced at its declared width. (A prior
  bug stored the break/else value as the default int width `i64` into a narrower
  result slot — e.g. an `i32` while-expression read back `0`. Fixed: the value is
  coerced to the result type before the store, mirroring `if`-expressions. The
  regression is `tests/programs/loop_control/while_expr_{break_value,else_value}_i32.con`.)

## Interaction with linear values (cleanup)

References are second-class and most non-`Copy` values are **linear** (must be
consumed exactly once). `break`/`continue` must not silently skip a still-live
linear value:

- A `break` that would leave an unconsumed linear value live is rejected with
  **E0210** (`break would skip unconsumed linear variable 'x'`).
- A `continue` in the same situation is rejected with **E0211**.
- Consuming the linear value *before* the `break`/`continue` is fine — the rule
  is about values still live at the jump, not about the loop body as a whole.

This is the same "no silently-dropped linear value" discipline the checker
applies to early `return`; `break`/`continue` are just additional exit edges it
accounts for.

## Interaction with bounded-loop analysis, obligations, and contracts

- `break`/`continue` do not disable runtime-safety obligations: an array index
  inside a loop with a `break` still generates and discharges its bounds
  obligation normally.
- Loop contracts (`#[invariant(...)]`, `#[variant(...)]`) attach to the loop and
  are unaffected by the existence of `break`/`continue` edges; they are checked
  as usual.

## Not provided (deliberate, V1)

- No bare `loop { }` keyword (use `while true`).
- No `break`/`continue` out of a non-loop block.
- `break <expr>` only carries a value for a `while`-expression in value position;
  in statement position the value is unused (the loop produces no value).
