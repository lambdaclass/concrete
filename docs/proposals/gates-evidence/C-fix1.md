Review round 1 of branch gate-scaling-determinism. It is now REBASED: HEAD is 4ed5b4e1, on top of gates-manifest c2245c99 (main ea212942 + the test manifest). Run `git log --oneline -6` first. Same rules as before (no push, no remotes, no rebase/reset/clean/stash, bounded commands). NEW: you may run the two gates at small sizes to verify fixes, one at a time, under nice:
  `timeout 1500 nix develop --command nice -n 10 bash scripts/checks/compiler/determinism.sh`
  `timeout 1500 nix develop --command env SCALING_REPETITIONS=2 nice -n 10 bash scripts/checks/compiler/scaling.sh`
Commit fixes as commits whose messages say "review round 1".

Context: the scaling gate works. It found a real defect on main, filed as ISS-0030: LCB.emit is cubic and LLVM.module quadratic. The fix is on another branch (wip/lcb-numbering). So the clean scaling run is EXPECTED to fail on lcb_emit, llvm_module and the totals until that lands. Do not weaken the threshold or exempt stages.

CI-server results at 4ed5b4e1 (concrete-ci-2):
- determinism.sh: every row byte-identical, then `FAIL slice0_determinism: answer: lcb_rt.s: missing (.../first/output-with-one-absolute-path/out/answer/lcb_rt.s)`. lcb_rt.s is written by slice0_native.sh (LCB's command line), so the first side's native step did not produce it, or the comparator expects it in the wrong place. Find out which and fix it.
- scaling.sh (k0=1, 5 reps): exponents 10x->100x: lcb_emit 2.860, llvm_module 1.509, lcb_assemble 1.073, native 0.000 (254 ms flat: process start-up dominates), totals 2.797/1.968.

Findings to fix (Codex = C, agy = G):
1. (known) The emit result is forced with `reprStr f`, which is itself superlinear. Use a linear structural digest: blocks, instruction counts and terminators, no repr, no string building. Check the same for every stage's force function.
2. (C2, G1) The planted controls must run automatically in every gate run, without rebuilding:
   - plant linear must PASS;
   - plant quadratic must FAIL naming plant_quadratic with an exponent in [1.7, 2.3] and the threshold diagnostic;
   - any other failure (validation, timeout, resolution floor, missing samples) makes the control fail as "control setup: ...".
   They must pass even while the clean run fails (ISS-0030): the controls analyse only their planted stage.
3. (G2) In plant mode measure ONLY the planted stage (branch on plant before running the real stages).
4. (C3, G4) The report must require the exact stage inventory per mode: normal = llvm_module, lcb_emit, lcb_assemble, in_process_total, native, total; in-process-only and plant modes = theirs. It must also require every (stage, size, repetition) sample. A missing stage or sample fails. Give native its own positive floor.
5. (G3) Guard timedPure against hoisting/CSE: make the timed thunk depend on a per-repetition value the compiler cannot see through (e.g. pass `rep` into the program constructor or `@[noinline]` + an opaque identity function defined in the driver). State in the header that `lean --run` measures the interpreted compiler, and why that is acceptable for exponents today (the exe `concrete` does not exist yet).
6. (G5) Native timing: write each size's inputs once, outside the timed region, and copy them to a fresh directory per repetition before timing. Remove the dead `generate` command.
7. (C1, G6) Determinism must compare EVERY file each side produces: a recursive inventory of each program's output directory, equal on both sides, and every file byte-compared. Keep the required-file assertions so an artifact missing from both sides is caught. Keep gate 8's (slice0_repro.sh) behaviour and its controls unchanged; it can keep its explicit list if it wants, but determinism.sh must not use an allow-list. Add a control: an extra unexpected file on one side must fail, naming it.
8. (C4, G7) Environment order: build both environments explicitly with `env -i`, inserting the same keys in opposite orders, and vary HOME, TMPDIR, cwd and the absolute path. Note in the header that slice0_native.sh normalises its own child environment (env -i), so the native step varies only paths, cwd and TMPDIR. Invoke repro_compare.py with `python3 -I` everywhere.
9. Keep the macOS exclusion of scaling, but state the reason as measured or as a decision ("hosted macOS runners are shared VMs; exponents there are not comparable"), not as an unmeasured claim.

Final message: commits, the summary of your two small runs (each control's ok line; the scaling table), and anything not fixed with a reason.
