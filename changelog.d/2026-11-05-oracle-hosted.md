### Oracle check on a GitHub-hosted runner (2026-11-05)

- **New required job `Oracle (GitHub-hosted fetch + smoke)`** (ISS-0001, ISS-0005, P13, R-0564). It runs
  on `ubuntu-latest`: `scripts/oracle/ci_check.sh` fetches the published oracle with `fetch.sh`
  (download, artifact sha256, install manifest), then runs `scripts/oracle/smoke.con` through
  `run.sh` and requires exit 42. A missing, corrupt or unverifiable artifact, or an install `run.sh`
  refuses, makes the check INCOMPLETE (exit 3); a wrong smoke result fails it. Neither passes.
- **New gate `check_oracle_fetch.sh`** holds those mappings with negative controls on a test artifact
  built by the real packer and a fake `curl`.
- **Runner pools recorded** in `docs/project/SELF_HOSTED_RUNNERS.md`: concrete-ci-2 self-hosted
  (`concrete` 6 runners, `concrete-light` 2) for the landing pool, at most two heavy runs; GitHub-hosted
  runners as a regular second pool; self-hosted never for `pull_request`. The missing per-runner CPU
  and memory caps are filed as ISS-0007.
- **ISS-0004 closure reworded** to the approved takedown: both site URLs return 404. The remaining
  action is an organisation admin unpublishing lambdaclass/concrete Pages.
