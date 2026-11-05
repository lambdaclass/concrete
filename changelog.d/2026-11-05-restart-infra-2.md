### Restart infrastructure, follow-up (2026-11-05)

- **The oracle is published.** With the owner's approval (P13), the frozen compiler is the release
  asset `oracle-legacy-compiler-final-x86_64-linux.tar.gz` of release `oracle-legacy-compiler-final`
  on `unbalancedparentheses/concrete2`, its tag at the locked commit `066c9d9b8836`.
  `scripts/oracle/ORACLE.lock` now names it in `artifact_url`, and `scripts/oracle/fetch.sh` downloads,
  verifies and installs it (no longer INCOMPLETE). ISS-0001 stays open until a GitHub-hosted runner
  has done the same.
- **`publish.sh` holds the release to the locked commit.** It already created the release with
  `--target` at the lock's commit; it now also refuses an existing release whose tag (resolved through
  `commits/tags/<tag>`) names another commit, checks the new tag after creating it, re-verifies instead
  of re-uploading an asset already there, and refuses an abbreviated lock commit. New gate
  `scripts/tests/check_oracle_publish.sh` runs it against a fake `gh` and `curl` (9 cases, 6 of them
  refusals).
- **The legacy/ move needs an accepted rehearsal of that exact candidate.** `train.sh land` (and
  `--dry-run land`) refuses a candidate that touches `legacy/` or `scripts/checks/root_inventory.txt`
  (so a move staged over several candidates needs a rehearsal at each step) unless
  `<state>/rehearsals.tsv` holds an `accepted` row for the same train-head tree and base; a malformed
  row refuses outright. `scripts/land/acceptance.sh` writes that row for every verdict, after refusing a
  `--name` or `--ref` that could forge one. The gate runs after every other land check; a dry run
  refused only by it exits 5 with a marker line (one definition in `scripts/land/lib.sh`), which
  acceptance's dry-run leg reads as passing only as exit 5 plus that whole line. The gate trusts the
  reviewed train.sh that runs the land; review must keep it there. `check_landing_script.sh` gains
  controls for no record, another tree, another base, failed and incomplete verdicts, a malformed row,
  a second train of the same tree authorizing the first, a real rehearsed land, a staged move, the
  root inventory alone, the dry-run classifier, and forged `--name`/`--ref`.
