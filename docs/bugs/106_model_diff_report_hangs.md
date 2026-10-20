# Bug 106 — `--report model-diff` never terminated on thesis_demo, and its gate stayed green

**Status:** FIXED 2026-10-20 (report budgeted, gate fails closed); a structural residual is open
(see below).
**Found:** 2026-10-20, while running `check_model_diff.sh` during bug 091 round 3. Present at
b8d4f34c (confirmed on a fresh build), so it predates that work.
**Severity:** a dark gate. A report that hung or crashed was invisible: the gate printed 87/87.

```sh
concrete examples/thesis_demo/src/main.con --report model-diff   # ran for hours
```

`validate(data, len)` loops `len` times. Model-diff samples every scalar parameter at its type's
boundaries, so the runtime side evaluated `validate(1, 9223372036854775807)` — up to the
interpreter's 10,000,000-iteration loop limit per call, for many boundary inputs, in an
interpreter that is not built for that volume. `check_model_diff.sh` ran each report as
`"$BIN" "$f" --report model-diff > ... || true`, so a hang (when killed) or a nonzero exit
contributed nothing and the gate's population thresholds were still met.

## Fixed

- The interpreter's loop budget is a harness parameter (`Interp.loopFuelVar`, default
  10,000,000); `--report model-diff` runs the runtime side with 10,000 iterations per loop, and
  running out is the explicit outcome `exhausted` (its own verdict, counted in the summary line,
  re-derived by the gate's V check) — never a trap and never agreement. thesis_demo now reports
  in ~2.5 s; `validate(1, i64::MAX)` reads `exhausted`, small inputs still `agree`.
- `check_model_diff.sh` runs every report under `timeout 300` (required; missing `timeout` is
  an error) and FAILS on a timeout or on any nonzero exit, except six named deliberately
  ill-formed examples, which must keep being refused (and fail if they start succeeding).
  New legs pin the thesis_demo `exhausted` row and its agreeing rows (92 checks).

## Structural cause

The pure interpreter has no global evaluation budget: only `while` loops count iterations, with a
fixed constant, and harnesses that evaluate sampled inputs inherited the run-a-real-program limit.
The gate composed reports with `|| true`, which turns every failure mode into silence.

## Structural owner

Model-diff and its gate: R-0460 (model/runtime agreement). The residual — recursion and calls are
not budgeted, so a sampled input driving deep recursion can still run long (the gate's 300 s
watchdog now turns that into a FAIL instead of a hang) — belongs to the same row: a step budget
threaded through `evalExpr` rather than per-loop fuel.
