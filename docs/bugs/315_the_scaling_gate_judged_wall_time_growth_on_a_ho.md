# Bug 315 — the scaling gate judged wall-time growth on a host that cannot count instructions, so a loaded dev host false-failed linear stages

**Status:** FIXED (branch `wip/bug-315`)
**Found:** 2026-10-28, by the pre-push hook on the OVH dev host (agent@148.113.198.57, Ubuntu 24.04,
`kernel.perf_event_paranoid=4`, load average 15 – 26 from agent builds): `check_compile_scaling.sh`
failed a stage whose instructions grow linearly.
**Severity:** loud (false failure of a gate). No compiler output is affected; a correct change could
not be pushed from the dev host while it was busy. The inverse was latent: nothing on CI required the
counters, so a CI runner that lost them would have judged by the method this bug shows is unsound.

<!-- bug-ledger
summary: on a host whose perf cannot count instructions:u (the dev host, perf_event_paranoid 4), the R-0550 scaling gate fell back to judging each stage's wall-time growth between sizes against the x63 bound and its wall budget unnormalized; its own record (bug 294) shows a wall ratio measures the caches and the host's load, so under load a linear stage failed (enums/lower x80, 2 ms at 10^4, while CI counted its run growing linearly), and no CI step required counters, so a CI host without them would have judged the same way. Class 11.
coverage: check_compile_scaling.sh
class: 11 — a host without instruction counters read as licence to judge by wall time
silent: no
ruling: Outside the spec: tooling — the scaling gate judged wall-time growth on a host without instruction counters
-->

## Reproducer

On the OVH dev host, inside `nix develop`, under its usual agent load (load average 15 – 26):

```
bash scripts/tests/check_compile_scaling.sh --shape enums
```

## Observed

```
  note: perf cannot count instructions:u on this host; wall budgets are judged unnormalized and instruction budgets are not judged
  FAIL enums/lower time grew x80.0 from 10000 to 100000 > x63.1 (n^1.8) [10000:2ms 100000:160ms]
```

after three attempts (the retry budget). On quiet CI with counters (concrete-ci-2, run
40593154242) the same stage reads `enums/lower [10000:3ms 100000:69ms] growth by instructions host
x1.07`: its growth was judged by the run's retired instructions, which grew within the x11.5 bound.
(`host x1.07` is that run's host factor, not its instruction growth; Codex confirmation.) The 10^4
time, 2 ms, is the `floor_ms` resolution, so the ratio is noise.

## Expected

A gate's verdict depends on the compiler, not on the host that runs it (ROADMAP, result
vocabulary). A verdict a host cannot make soundly is not made there: it is reported as not judged,
which is not a pass, and made where it can be (CI, which counts instructions).

## Cause

`scripts/tests/lib/compile_scaling.py`, `judge()`: a stage whose run was not counted (`by_instr`
empty because `perf_ok()` is false) took the wall branch, `t(n2) / max(t(n1), floor_ms) > (n2 /
n1) ** exp`, and the budget branch compared raw times (`host_factor` is 1 without counts).
Bug 294's docstring (GROWTH) records that this ratio false-fails linear stages near the bound
(x45 – x60 on an idle host), and kept it for "a run nobody counted". `main()` printed a note and
continued, on CI too: no workflow step and no leg of the gate required counters.

## Fix

- **Not judged where it cannot be judged.** Where perf cannot count instructions:u and the run is
  not CI's, `judge()` (`wall_judged=False`) judges no wall growth and no wall budget. A stage nothing
  else fails reports `NOT-JUDGED` (never `ok`), the summary counts `NOT-JUDGED=N`, and the gate exits
  77 when nothing failed (`exit_status`; 0 is only for a run that judged everything).
- **What does not depend on the host is still judged there:** allocation heartbeats (exact: budget
  and growth), every compile's result (a refusal, a wrong answer, a missing baseline), the run
  ceiling (`ceiling_factor` x the stage budgets, re-run alone at twice that by bounded.sh), and a
  catastrophic overrun: a stage over `uncounted_fail_factor` (10) x its budget. A budget is at least
  4 x the CI host's quiet median and at least 200 ms over it, so the threshold is at least 40 x the
  median and at least 2 s; load slowed a stage x2 – x3 in bugs 294 and 315, so no load reaches it,
  while a hang or an exponential does.
- **CI must count (fail closed).** On CI (`GITHUB_ACTIONS=true`, or `CI` true/1) a host that cannot
  count fails the gate with `FAIL CI host must count instructions:u` before judging a shape: the
  verdicts a dev host defers are decided there and cannot be deferred again. No workflow step
  asserted counter availability before; this leg is that assertion, and `check_compile_scaling.sh`
  runs it end to end on every run (the real entry point with `GITHUB_ACTIONS=true` and no perf on
  `PATH` must exit 1 with that message).
- **Deferred, declared, never a pass.** `scripts/tests/lib/host_tools.sh` declares the gates that may
  defer (`HOST_DEFERRED_TABLE`, row `check_compile_scaling.sh|ci-counted`, i.e. judged_on:
  ci-counted) and the status they use (`GATE_DEFERRED_RC=77`). `run_ci_gates_local.sh` records exit
  77 as DEFERRED, apart from passed and failed, only when `gate_deferral` finds the gate declared AND
  run by CI as an unconditional step (`delegated_gate_step`); from any other gate exit 77 is a
  failure. A deferral leaves the runner's exit 0, so the pre-push hook accepts the push, but the run
  is PARTIAL: `deferred_to_ci(N not judged here)` blocks `completed=1` like a host skip, and
  `.ci-gates-summary` records `deferred=N` and `deferred_gates=`. The hook names the deferred gates,
  and its validation record says `status=pass-deferred` with a `deferred_gate=` line per gate, never
  `status=pass`; a reuse of that record names them again, and a runner that did not report its
  deferrals gets no record.
- **Hardened in review (Codex, Kimi).** A deferring command must be exactly the declared gate
  (`bash`/`python3`/`sh` or `./`, its path, plain arguments): no `VAR=` prefix (`BASH_ENV` or `ENV`
  could exit 77 before the gate runs), and no shell operator, substitution, comment or line break
  anywhere in it. A row's `judged_on` is checked where it is used. The pool's child authorizes a
  deferral before recording it, and a pool failure whose sequential re-run only exits 77 stays a
  failure (the re-run made no verdict). A catastrophic overrun is not retried. `--measure` is refused
  on an uncounted CI host too. A record has exactly one `status=` line, and `deferred_gate=` lines
  exactly when it says `pass-deferred`.
- **Counting hosts unchanged.** Where perf counts, every verdict is made as before (budgets with the
  host factor, growth by instructions, the wall bound for a run whose count failed).
- **Controls.** The gate's eleven judge controls stand; ten more run with every judgment: off CI
  without counts, a x63 wall growth and a stage within its bounds are both `NOT-JUDGED` (not FAIL,
  not ok), a stage over its budget but under 10 x it is `NOT-JUDGED`, a stage over 10 x fails, a
  heartbeat overrun fails; uncounted on CI is refused (`GITHUB_ACTIONS=true`, `CI=1`); uncounted off
  CI is deferred; a counting host is judged as before on CI or off it and never reports
  `NOT-JUDGED`; `NOT-JUDGED` exits 77, any failure 1. The wrapper adds the end-to-end CI refusal.
  `check_gate_registration.sh` checks the deferral table (known judged_on, an unconditional CI step,
  the gate's own exit status, an undeclared gate and a conditional step refused) and the runner's
  wiring; `check_gate_lock.sh` drives the real runner (sequential and pool) on a declared and an
  undeclared exit 77; `check_prepush_record.sh` drives the hook through a deferral, its reuse and a
  runner that reported none.

## Structural cause

Class 11 (absence read as a fact): the absence of an instruction counter was read as licence to
judge by wall time, a method the gate's own record had shown measures the host, so the host's load
decided the verdict; and on CI the same absence was read as nothing worth failing. Bug 294 is the
same class in the same gate (the host's speed at judgment, never measured, read as its speed at
`--measure`). The structural rule: a verdict that depends on a host capability is either made with
it, or reported as not judged and made where the capability is required, never approximated.

## Structural owner

R-0550 (exit 2o, the scaling gate) for the method; R-0513 (exits 2d – 2f, trustworthy gate
execution) for the result vocabulary in the runner and the hook: the deferred status and its
declaration (`host_tools.sh`) are the general mechanism, for any gate whose verdict needs a CI-only
capability.

Codex confirmation 2026-10-28: CONFIRMED — At HEAD `ca49fada`, unavailable perf counters leave the gate vulnerable to wall-time false failures and weaken CI coverage without causing a counter-availability failure.
