# Bug 294 — the scaling gate judged absolute wall budgets, so a loaded CI host failed unchanged work

<!-- bug-ledger
summary: the R-0550 scaling gate compared each stage's fastest-of-three wall time with an absolute budget (4 x the median on a quieter CI host); under sustained load the shared CI host runs the same instructions several times slower, so a stage whose work was unchanged failed its budget (assignments/monomorphize at 10^5: 418 ms against 412 in train 37's CI, at an unchanged instruction count; main itself over budget in 4 of 8 runs at load 16-28). The verdict depended on host load, not on the compiler. Class 11.
coverage: check_compile_scaling.sh
class: 11 — the host's speed at judgment, never measured, read as its speed at --measure
silent: no
-->

**Status:** FIXED (branch `wip/bug-294`)
**Found:** 2026-10-26: train 37's CI (run 40289036334) failed `assignments/monomorphize n=100000
418ms > budget 412ms`; bug 280's record shows the stage's callgrind instruction count unchanged
(1,291,486,649 on main, 1,292,832,790 on the train); main over the same budget in 4 of 8 runs
at load average 16 – 28 (landing coordinator).
**Severity:** loud (false failure of a CI gate). No compiler output is affected; a correct change
could not land while the CI host was busy, and a retry that passed proved nothing.

## Reproducer

On concrete-ci-2 at load average 24, one binary (wip/perf-286, 43a65e3e), three
`--emit-trace-json` runs of the `assignments` shape at 10^5: whole-process wall 4637, 3585 and
2190 ms, retired user instructions (`perf stat -e instructions:u`) 13,615,260,511,
13,615,074,685 and 13,615,210,545 (equal to 0.002 %), `monomorphize` 575, 355 and 261 ms against
its 412 ms budget. Five more runs at load 18: 267, 110, 317, 278, 269 ms.

The stage itself is linear: callgrind on the dev host counts 129.0 M instructions for
`Pipeline.monomorphize` at 10^4 assignments and 1292.9 M at 10^5 (x10.02): `monoProgram`, the
seven whole-program walks of `verifyPostMono` and `elabVerifyModules`, each linear. Its wall time
grows x27 because the 10^5 program leaves the caches (whole-program instructions per cycle fall
from about 3.2 to 1.35), which is also what makes it sensitive to other jobs' memory traffic.

## Cause

`scripts/tests/lib/compile_scaling.py` judged `t(n, stage) > budget(n, stage)` with `t` the fastest
of up to three attempts and the budget recorded once (`--measure`) at whatever speed the CI host
had then. Nothing in the gate measured the host's speed when judging, so sustained contention that
outlasts the retries failed a stage, and a budget measured under load let a regression through.

## Fix

The gate now separates the compiler's work from the host's speed (compile_scaling.py, "THREE
KINDS OF MEASUREMENT"):
- Every timed run is wrapped in `perf stat -e instructions:u` where the host can count (as
  `check_ssa_lowering_scaling.sh` already does). A run's retired instructions are judged exactly,
  without retries: against a budget (`instr`, 1.25 x the count at `--measure`) and for growth
  between sizes (`instr_exp` 1.2, as for heartbeats).
- Each stage's allocation heartbeats are judged against a budget too (`hbbudget`, 1.25 x).
- A stage's wall time is judged against its budget after dividing by its run's host factor: how
  much slower per instruction the run went than the rate recorded at `--measure` (`rate`), at
  least 1 and at most `norm_cap` (2). The run's own instruction count is the denominator, so work
  a change adds does not raise its allowance (it fails `instr`); a change that makes the same
  instructions slower is excused only up to `norm_cap` (Codex review: the normalization trades
  that sensitivity for load immunity; the cap bounds it). Growth is judged on raw times, as before.
- `--measure` scales each run to the session's quietest rate and keeps the fastest, which removes
  the variation between its runs but not load lasting the whole session (Codex review); so it
  refuses a host whose load average is above `measure_max_load` (8 of 32 threads) and stamps the
  load it ran at.
- Growth between sizes is judged on the instruction counts where perf counted the run, above the
  mode's baseline (a one-line program), against `instr_exp` 1.06 (x11.5 for x10; a linear compile
  grows x10.0 – x10.3 there, and a quadratic part 1.6 % of the run at 10^4 exceeds it). Those runs'
  stages are no longer judged for wall-time growth: on an idle dev host linear stages grew x45 – x60
  from 10^4 to 10^5 (functions coreCheck x60, enums lower x54) as their data left the caches, close
  to the x63 bound a quadratic had to exceed, so a loaded host would have false-failed them, while
  the instructions of those runs grew x10.1 – x10.3. A quadratic multiplies the instructions, so it
  still fails, and exactly. The wall growth bound stays for runs nobody counted.
- A host that cannot count instructions judges as before and says so.
- Negative controls run with every judgment: a stage over budget on an unloaded host fails; the
  same time at half host speed passes; a stage over `norm_cap` x its budget fails however slow the
  host; an instruction or heartbeat overrun fails whatever the host factor; instructions growing
  x60 for x10 (a planted quadratic) fail; a counted run whose wall time grew x63 at x9 instructions
  passes; the same wall growth fails a run nobody counted.

All budgets are re-measured with the new method on concrete-ci-2 (see the `measured:` stamp in
`compile_scaling_budgets.tsv`).

## Structural cause

Class 11 (absence read as a fact): the gate never measured the host's speed when it judged, and
read that absence as "the host runs as fast as when the budgets were measured", so a fact outside
the system under test (the CI host's load) decided the verdict.
The structural rule is that a performance gate judges work by an exact counter and judges time
only relative to the host speed measured in the same run.

## Structural owner

R-0550 (exit 2o, the scaling gate) for the method; R-0543 (machine-wide load control) for the
runner's load.

Codex review 2026-10-26: CONFIRMED — fixed absolute wall budgets and three finite retries can reject unchanged work under sustained contention, although the specific train-37 failure is not proven to be entirely environmental.
