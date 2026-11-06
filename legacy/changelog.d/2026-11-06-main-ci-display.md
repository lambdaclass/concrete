### Completed Task R-0570

- **Main's CI run records the run that qualified its exact SHA.** `train.sh land` no longer cancels
  main's push run. That run's new first job, `Landing record`, runs `train.sh record <head> --base
  <before>`: the newest completed dispatch of CI at exactly the landed SHA and at that base must have
  every landing-tier job and every required job (`required-checks.txt`, transition rows, base
  included) succeeded, with the head strictly ahead of the base. `train.sh ci` now dispatches with
  `-f base=<train base>`: the new `base` input feeds `LL1_BASE`, `TOTALITY_BASE`,
  `CONFORMANCE_BASE` and `SPEC_BASE` (as `github.event.before` does on a push; `SPEC_BASE` is now
  also set on a push to main, where it used to fall back to `HEAD~1`), and the run's title records
  it (`<ref> at base <sha>`), so a record is bound to the base its checks used. Then the other jobs are skipped and
  main's run concludes success, its summary linking the qualifying run; otherwise every job runs on
  main. Nothing is carried across SHAs. The mirror's CI reads the record from the train's
  repository. `land --keep-main-ci`, `LAND_KEEP_MAIN_CI` and `LAND_DUP_POLL*` are removed.
- `Landing record` is a required job. `check_landing_script.sh` covers the record's refusals and
  checks that every landing-tier job is gated on it (with controls); `run_ci_gates_local.sh` does
  not treat the record command as a gate; `check_gate_hygiene.sh` lists `train.sh` among the
  workflow-run scripts that are not gates.
- The landing-record steps are optional for a qualification dispatch (`qualification_record.py`:
  they run only on a push to main), and the skip-owner rule (`example_skip_owner.py`) accepts the
  record gate, exactly as spelled with exactly `needs: landing-record`, as a job CI still runs on
  every dispatch and pull request (controls for a near miss and for other needs).
