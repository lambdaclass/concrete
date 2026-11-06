# fixed_point — runtime safety on a real-ish program

A small Q8 fixed-point sample pipeline (scale + clamp, then mix), chosen to
exercise the **runtime-safety** obligations on realistic scalar arithmetic with
`#[requires]` bounds and `#[overflow_checked]`. Its real value is that the audit
is **honest** about exactly what it proves and what it doesn't.

```sh
concrete examples/fixed_point/src/main.con -o /tmp/fp && /tmp/fp   # prints 75
concrete examples/fixed_point/src/main.con --report contracts      # the evidence
```

Evidence (`--report contracts`, Runtime-safety sections):

| obligation | disposition | why |
|---|---|---|
| `scaled / 256`, `(a+b) / 2` | **checked** | nonzero constant divisor |
| `a + b` (in `mix`) | **proved_by_kernel_decision (omega)** | bounded operands, **linear** → omega bounds it |
| `sample * gain` | **unproven** | product of two variables is **nonlinear**; omega is linear and can't bound `255 * 256` — the audit says so rather than pretending |
| functional result ranges (`#[ensures]`) | not stated yet | would be a Lean step (`PROOFKIT_GUIDE.md`) |

This is the point of the thesis in miniature: **no semantically dark
constructs.** Division safety and linear overflow are machine-checked; the
nonlinear-multiply overflow and the functional spec are explicitly pending, with
`concrete prove fixed_point.scale_clamp` pointing at the next obligation —
nothing is silently assumed safe.

Closing the nonlinear case needs interval/bitvector reasoning over the bounded
operands (a future runtime-safety tactic); the functional postconditions need a
Lean refinement proof.
