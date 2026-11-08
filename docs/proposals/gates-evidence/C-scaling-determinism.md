# Brief C: scaling and determinism gates — branch gate-scaling-determinism

You are working in the full clone /home/unbalancedparen/projects/gates-scaling, branch
`gate-scaling-determinism`, based on branch `test-manifest` (brief B: `tests/manifest.tsv`, its
reader `scripts/checks/lib/test_manifest.py`, the runner `scripts/checks/compiler/tests.sh`),
itself on origin/wip/slice0 (not yet on main). Read AGENTS.md (repository root), CLAUDE.md,
Concrete/README.md (gates 8 and 11, "Reproducible builds"), scripts/checks/compiler/slice0_repro.sh,
scripts/checks/lib/repro_compare.py, scripts/checks/compiler/slice0_artifacts.sh (its
compile_times.tsv) and scripts/checks/compiler/slice0_bir.lean first.

Do NOT push, do NOT touch git remotes, do NOT run `git rebase/reset/clean/stash`. Commit on the
branch in small commits. Every command bounded with `timeout`. No heavy runs: at most
`timeout 1800 lake build Concrete`; never the native gates (I run them on the CI server). Scratch
only under mktemp.

## Goal (owner-approved)

"Scaling and determinism: (1) generate programs at 1×, 10×, 100× size; fail if compile time grows
clearly faster than linear (use a robust ratio test with a noise margin, measured on the same
machine in the same job; report the measured exponent); (2) compile every test program twice (also
from different working directories/paths) and require byte-identical outputs at every emitted
artifact."

## (1) Scaling gate: `scripts/checks/compiler/scaling.sh` + a Lean driver

- Today's compiler is BIR → {LLVM printer, LCB adapter + in-process LCB assembly} + runtime; the
  native step is clang + LCB's command line. "Compile" = every stage the pipeline runs: measure the
  in-process stages separately (inside Lean, `IO.monoNanosNow` around each stage, forcing the
  result, e.g. by taking the output's size/hash so laziness cannot hide work: LLVM.module text,
  LCB.emit, LCB.assemble for the target) and the native step as a whole (wall clock around
  `slice0_native.sh`).
- Program family: a deterministic generator `scaled k` producing a WF BIR program that LCB.emit
  accepts, whose size is linear in k: k repetitions of the example shapes (a checked op with its
  trap branch, a wrapped op, a trunc), chained through registers so nothing is dead, ending in an
  exit. Sizes 1×, 10×, 100× of a base k0 large enough that fixed costs do not dominate the 1×
  point (measure; state k0 and why). Verify each scaled program's BIR `run` exits with the expected
  value (correctness of the family) before timing it.
- Measurement: same machine, same job; for each size, R repetitions (e.g. 5) interleaved across
  sizes (1×,10×,100×,1×,10×,100×,…) to cancel drift; take the minimum (or a low quantile) per
  size. Exponent per step: e = log(t(10x)/t(1x)) / log(10), and likewise 10×→100×. The gate fails
  when the 10×→100× exponent exceeds 1 + margin (start with margin 0.35, i.e. e > 1.35, and make
  it an env var `SCALING_MAX_EXPONENT`), for any stage or the total. Report every stage's times and
  exponents in a table (stdout and `scaling.tsv`). Guard against vacuity: fail if the 100× time is
  below a minimum resolution (e.g. 20 ms for in-process stages) — then the base size is too small to
  measure anything; increase k0 instead of passing.
- Planted-failure control (must fail FOR THE PLANTED REASON, grep the FAIL line naming the stage
  and its exponent): the driver accepts `--plant quadratic` (refused without an explicit control
  flag) which adds a stage that does O(n²) work in the program size (e.g. for every instruction,
  scan all instructions, forcing a result) — the gate must report that stage with exponent ≈ 2 and
  fail. A second control, `--plant linear` (an extra O(n) stage), must PASS, proving the threshold
  does not reject linear work.
- Bounded run time (the whole gate within ~10 min on the CI server); bounded_run for every timed
  command, INCOMPLETE is never a pass.

## (2) Determinism gate: `scripts/checks/compiler/determinism.sh`

Gate 8 (`slice0_repro.sh`) already builds the compiler twice from two paths and compares every
artifact; it is heavy and runs in its own CI jobs. This gate is the per-test complement with ONE
compiler build: for every `bir:` row of the manifest (through the reader), emit and build natively
twice — the two runs from different absolute paths, different working directories, different TMPDIR
and a different environment order/HOME — and compare every artifact byte for byte (all files the
emitter writes and every object/executable `slice0_native.sh` writes). Reuse
`scripts/checks/lib/repro_compare.py` (extend it if needed rather than writing a second
comparator; keep its existing behaviour and its existing controls in slice0_repro.sh passing).
Also run the emitter twice in the SAME process (two `emit` calls) for in-process determinism if
cheap.
- Planted-failure controls (each must fail FOR THE PLANTED REASON, grep the comparator's message
  naming the artifact): (a) the emitter's `--plant nondeterministic` (refused without the control
  flag) writes the current working directory into `llvm.ll` as a comment — must fail naming
  `llvm.ll`; (b) a plant that writes the monotonic time into `lcb_body.s` — must fail naming it;
  (c) a missing artifact on one side — must fail. And a clean double run must pass.
- CI: add "Determinism (x86-64)" and "Determinism (aarch64-macos)" and "Scaling (x86-64)" jobs
  to .github/workflows/lean_action_ci.yml mirroring the Tests jobs (same setup, job-level
  timeout), and add them to required-checks.txt. Scaling runs on x86-64 Linux only (one machine,
  one job; the macOS hosted runner is too noisy) — say so in the gate header.

## Docs

Concrete/README.md gates list: add the two gates (what they check, the controls, the exponent
threshold and why, the relation to gate 8 and gate 11's report-only compile_times.tsv — say whether
compile_times.tsv should stay; do not remove it). A changelog.d fragment.

## Constraints

- No language/spec decisions; ConcreteSpec/ and Concrete/ untouched (plants live in the drivers).
- Run `bash -n`, shellcheck if present, `timeout 1800 lake build Concrete` only if needed, and
  smoke runs of the Lean drivers (`timeout 600 lake env lean --run ...`) for the in-process parts
  at small sizes. Do not run the native gates.
- Final message: commits, what you ran with results, measured exponents from your smoke runs,
  open questions.
