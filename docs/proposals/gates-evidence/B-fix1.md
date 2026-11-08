Review round 1 of branch test-manifest (HEAD 16ef6eb6): fix the following, same rules as before (no push, no remotes, no rebase/reset/clean/stash, bounded commands, no native gates). Commit as one or more commits whose messages say "review round 1".

The native gates passed on the CI server at 16ef6eb6 (tests.sh 32 examples plus all controls,
slice0_artifacts.sh, slice0_repro.sh). Two root checks fail there and in GitHub CI:

1. scripts/checks/landing_script.sh ~lines 991-997: its required-checks controls still index
   `w["jobs"]["backends"]` and expect "Slice 0 backends (riscv64)". Point them at the renamed job
   (`tests`, "Tests (${{ matrix.os }})", "landing-tier job not required: Tests (riscv64)").
2. RETENTION.tsv: "no row governs tests/manifest.tsv". Add a retained row for the new root test
   manifest (follow the header's rules and scripts/land/retention.py; criterion "root: never;
   legacy/ copy: none (new at the root, nothing to delete)" as other new-root rows do; owner = the
   roadmap item for manifest-driven tests if ROADMAP.md names one, else R-0558). Make sure a future
   tests/slice0/*.con is governed too (a row for tests/manifest.tsv and tests/slice0, or say why not).
   Run `bash scripts/checks/run.sh` at the end (it is light) and report its summary line.

Reviewer findings to fix:
3. (Codex) tests.sh manifest controls copy only the manifest into $work/ctl, but the reader resolves
   con: paths relative to the manifest's parent; a manifest with con: rows would make the controls
   fail for the wrong reason. Copy the tests directory (as test_manifest.sh's control does) and point
   CONCRETE_TEST_MANIFEST at the copy's manifest.
4. (agy) The nonexistent-program control replaces the `answer` row, so it also removes `answer`
   and passes even if the check were one-directional. Append a new row instead, and keep missing-row
   separate, so each direction of the inventory check has its own control with its own message
   (make the two directions print distinct messages: "program without a manifest row: X" vs
   "manifest row names no program: X", and grep those).
5. (agy) Under CONCRETE_TEST_EMIT_ONLY=1 the loop exits 0 after the first row; the wrong-expectation
   control only works because `answer` is row 1. Emit every row in emit-only mode (continue), exit
   after the loop. Add a second wrong-expectation control on a trap row (e.g. `overflow` with a
   wrong kind or site) so the trap-kind/site check is load-bearing too.
6. (agy) test_manifest.py: reject two rows naming the same bir: program (one owner per test);
   reject `refuse` on bir: rows; run the reader's `self-test` from scripts/checks/test_manifest.sh;
   add root-check controls for the duplicate bir program and refuse-on-bir.
7. (agy) slice0_artifacts.sh enumerates the manifest without checking it against the emitter's list.
   Move the both-directions inventory check into slice0_common.sh's manifest_rows for kind bir, so
   every consumer enforces it (tests.sh then uses it instead of its own copy).
8. (agy) test_manifest.sh: use `set -uo pipefail` like the other accumulator checks; in each control,
   assert the mutation changed the copy (cmp) and that a row lookup is non-empty (duplicate_id);
   in validate_source compute the source from the resolved tests dir and reject con: paths under
   tests/golden/.

Not changed, with reason (no action): agy's finding that the `example : run ... := by decide` lines in
Concrete/BIR.lean duplicate the manifest. Those are kernel-checked facts about BIR's semantics that
document the examples; the manifest is the test expectation the runner checks. Leave them.

Final message: commits, what you ran (test_manifest.sh output summary, run.sh summary), anything you
could not fix.
