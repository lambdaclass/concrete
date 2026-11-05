### Restart infrastructure, follow-up (2026-11-05)

- **The oracle is published.** With the owner's approval (P13), the frozen compiler is the release
  asset `oracle-legacy-compiler-final-x86_64-linux.tar.gz` of release `oracle-legacy-compiler-final`
  on `unbalancedparentheses/concrete2`, its tag at the locked commit `066c9d9b8836`.
  `scripts/oracle/ORACLE.lock` now names it in `artifact_url`, and `scripts/oracle/fetch.sh` downloads,
  verifies and installs it (no longer INCOMPLETE). ISS-0001 stays open until a GitHub-hosted runner
  has done the same.
- **`publish.sh` tags the locked commit.** `gh release create` ran without `--target`, which tags the
  default branch's head: a first publish would have put the oracle's name on today's tree. It now
  passes `--target` at the lock's commit, refuses an existing release whose tag resolves elsewhere,
  re-verifies instead of re-uploading an asset already there, and refuses an abbreviated lock commit.
  New gate `scripts/tests/check_oracle_publish.sh` runs it against a fake `gh` and `curl` (9 cases,
  6 of them refusals). The first publish was made by hand with `--target`, so the bug never reached
  the release.
- **The legacy/ move needs an accepted rehearsal of that exact candidate.** `train.sh land` (and
  `--dry-run land`) refuses a candidate that adds `legacy/` or `scripts/checks/root_inventory.txt`
  unless `<state>/rehearsals.tsv` holds an `accepted` row for the same train-head tree and base.
  `scripts/land/acceptance.sh` writes that row for every verdict. The gate runs after every other land
  check; a dry run refused only by it exits 5, which acceptance's own dry-run leg reads as passing for a
  move candidate. `check_landing_script.sh` gains 12 controls (no record, another tree, another base,
  a failed or incomplete verdict, the accepted case, and the root inventory alone as a move).
