### CI Triage, A Flake Ledger And Nightly Qualification (2026-10-22)

`scripts/ci/triage.sh <run>` classifies each failed CI job as infra, infra-defect (our tooling
cannot build a pinned tool), known-flaky or real. It reruns the failed jobs once only when every
failure is infra or known-flaky, never on a second attempt and never for a run cancelled as a
whole; otherwise it writes a report with the gate, its FAIL lines, a local reproduction and the
suspected commits. A rerun is diagnostic: qualification reads only a first attempt.

`scripts/ci/flake_history.py` derives per-gate outcomes from GitHub's run records, and
[flaky_gates.tsv](docs/project/flaky_gates.tsv) is the human-curated ledger: a known-flaky or
quarantined gate needs an owner, a quarantine needs an expiry, and a quarantined gate still runs
and still fails its job.

`nightly.yml` dispatches CI on main with `campaign=true` every night, so each night yields a stage
(c) verdict for main's tip. It runs on lambdaclass/concrete, and on this repository when
`CONCRETE_NIGHTLY_QUALIFICATION` is `true`. The report job lists the verdict, the gates that newly
failed, the triage and the mutation-coverage difference from the previous night
(`scripts/ci/coverage_diff.py`), and is red unless the run qualified with no new failure and no
lost coverage. CI lost its own crons, so nothing runs twice. `check_ci_triage.sh` (CI) pins the
behaviour against the real failure logs of 2026-10-22.
