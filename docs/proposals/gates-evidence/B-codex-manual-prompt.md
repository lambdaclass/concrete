You are a code reviewer working in a throwaway export of the Concrete repository (current directory) at commit c2245c99 of branch gates-manifest, on top of main ea212942. Do not modify any file, do not build, no network. REVIEW.diff is the branch's whole diff against main.

The branch implements an owner-approved item: one test format. Each test is one program with its expected outcome in a single manifest tests/manifest.tsv (id, program bir:NAME or con:PATH, expect exit N / trap KIND site N / refuse RULE-ID, slice); one shared reader scripts/checks/lib/test_manifest.py; one runner scripts/checks/compiler/tests.sh (renamed from slice0_backends.sh); one owner per test; the slice-0 BIR examples' expected outcomes moved out of Concrete/BIR.lean Examples.all into the manifest; slice0_artifacts.sh and slice0_repro.sh enumerate through the reader; a root check scripts/checks/test_manifest.sh; CI job renamed to "Tests (...)" with required-checks-transition rows.

Review for correctness bugs, reading the actual files (not only the diff). In particular:
1. Does each planted-failure control (test_manifest.sh controls; tests.sh manifest_control wrong-expectation, wrong-trap-expectation, missing-row, nonexistent-program; the judge controls) fail FOR THE PLANTED REASON? Could any pass vacuously, or fail for an unrelated reason while reporting ok?
2. Does any test expectation have two homes? (The `example : run ... := by decide` lines in Concrete/BIR.lean are kept on purpose as semantic facts.)
3. Do slice0_artifacts.sh and slice0_repro.sh still cover every test, and does the inventory check in slice0_common.sh's manifest_rows behave correctly under set -e inside command substitution?
4. Are .github/workflows/lean_action_ci.yml, required-checks.txt, required-checks-transition.tsv and scripts/land/required_checks.py consistent; RETENTION.tsv and scripts/checks/root_inventory.txt consistent with the new files?
5. Does the change pick a side on any owner decision in docs (OD rows)? It should not.
For each finding: file:line, severity, concrete fix. Last line exactly: VERDICT: CLEAN or VERDICT: FINDINGS.
