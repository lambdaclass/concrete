Review round 1 of branch gate-spec-diff. It is now REBASED: HEAD is 64db2932, on top of gates-manifest c2245c99 (main ea212942 + the test manifest). Run `git log --oneline -6` first. Same rules as before (no push, no remotes, no rebase/reset/clean/stash, bounded commands). NEW: you may run the gate itself at a small size to verify your fixes, at most one run at a time:
  `timeout 1500 nix develop --command env SPEC_DIFF_N=24 SPEC_DIFF_SECS=1200 nice -n 10 bash scripts/checks/compiler/spec_diff.sh`
Commit fixes as commits whose messages say "review round 1".

First, the CI server run at 64db2932 failed: `scripts/checks/compiler/spec_diff.sh: line 123: label: unbound variable` right after the determinism control; no native control ran. Fix it and make sure every control actually runs end to end in your small run.

Findings to fix (Codex = C, agy = G):
1. (owner-coordinator decision; C6, G5) Move the generators out of the compiler library: remove Concrete/SpecDiffRandom.lean, SpecDiffSpecGen.lean and SpecDiffBirGen.lean from Concrete/ and from Concrete.lean. The best placement: one Lean file per generator under scripts/checks/compiler/ (e.g. spec_diff/Random.lean, spec_diff/SpecGen.lean, spec_diff/BirGen.lean, or flat names), imported by the driver through a test-only import path (LEAN_SRC_PATH / `lean --run` with `-R`/search path, or a `lake env` with an added path), with NO new lake lib, no new root entry, and nothing in Concrete/ importing them. If Lean can't import them that way, merge the generators into the one driver file, and say which you did and why. Update the README's layout text accordingly.
2. (C1, G3) Coverage and vacuity guards must be computed over the programs actually JUDGED on all legs, not over the generated corpus. Require nonzero exits AND traps among the judged BIR programs, and every required construct among the judged ones. When spec_compiled > 0, the spec leg needs its own minimum judged count and coverage. If the deadline stops the run before the guards are met, report INCOMPLETE (exit 3), never pass, and never FAIL with bogus mismatch output.
3. (C2, G1) Controls must assert the PRECISE planted effect:
   - First require the unmutated candidate to match.
   - drop-overflow-llvm must produce exactly the computed non-trap result (status = the wrapped value's exit code, empty stderr) on the LLVM leg, while the LCB leg still traps.
   - lcb-off-by-one must produce exactly the incremented encoded result on BOTH the native LCB leg and the in-process evaluator. Pick an exit program whose status is below 255, or mask, so the +1 stays an exit status.
   - Apply the mutation without breaking LCB's canonical numbering: mutate the BIR return before LCB.emit, while computing the expected observation from the unmutated program, or renumber canonically. Assert that the mutated artifact differs from the original before building.
   - A timeout, crash, `incomplete`, a driver failure or a setup failure is a control FAILURE ("control setup: ..."), never the expected failure.
   - Do not disable `set -e` by accident: inside run_control the `( fn ) || rc=$?` form turns errexit off in fn, so guard every step of each control explicitly.
4. (C3, G3) One absolute deadline. SPEC_DIFF_SECS must bound generation, controls and comparisons, including bounded_run's retry (pass budgets so that a retry can't exceed the deadline). Start the clock after the shared compiler build (the build keeps its own SLICE0_BUILD_SECS bound, as the other compiler gates have). Every control call gets the remaining time, checked > 0. A deadline hit is INCOMPLETE (3).
5. (C4) Failing seeds must be SAVED as regression tests, not only printed:
   - On a mismatch, write a regression bundle to a directory outside the scratch cleanup, `SPEC_DIFF_REGRESSIONS` (default `.lake/concrete/spec_diff/regressions/<seed>-<index>/`). It holds the seed, index, leg, generated program term, expected and observed observations, the exact manifest row and the Examples definition.
   - Add an `actions/upload-artifact` step on failure to both CI jobs, uploading that directory.
   - Keep the replay command.
   - Distinguish the two replay legs: SPEC_DIFF_REPLAY=bir:SEED:INDEX and spec:SEED:INDEX, keeping bare SEED:INDEX as bir (G4). A spec-leg mismatch must print the spec replay and regression instructions for the spec leg.
6. (C5, G2) The spec generator's production table must describe what is generated:
   - `.letLiteral` must emit `let x = <lit>` (an atom). Track unread bindings so programs stay accepted (OWN-5: every binding used).
   - Return selection must use the return weights.
   - Coverage must be derived from emitted syntax.
   - Widen literals to 0..255, with the boundary 255 included, and occasionally generate candidates the spec's `check` REFUSES (out-of-range literal >= 256, a known overflow, an unused binding). Count them as `rejected` with their reason, so the filter is load-bearing and the yield floor means something. Keep the yield floor over accepted programs.
7. (G4) writeSpecOutcome picks the first site of the trap kind. Until the spec's outcome carries the site, refuse (setup error) a trapping spec outcome when the lowered program has more than one site of that kind, with a comment saying why.
8. (G6) Update the workflow header comment (the job count and list) to match.

Final message: commits, the exact output summary of your small gate run (each control's ok line, counts per outcome and construct for the judged set, spec leg line), and anything not fixed with a reason.
