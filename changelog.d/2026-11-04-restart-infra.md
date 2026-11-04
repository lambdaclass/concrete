### Restart infrastructure, prepared before the legacy/ move

- `required-checks.txt` at the root lists every landing-tier CI job. `scripts/land/train.sh` `watch`
  and `land` now refuse a run in which a listed job is missing, skipped, cancelled or not
  successful. The required set is the base's file united with the candidate's, so a candidate
  cannot drop a requirement main imposes, and a tree with no file at all is refused rather than
  read as "nothing required". GitHub branch protection is not configured, so this file is the record.
  `check_landing_script.sh` covers each refusal and checks the file against the workflow's
  landing tier; disabling the check makes seven of its legs fail.
- `scripts/oracle/` pins the legacy oracle: `ORACLE.lock` (tag `legacy-compiler-final`, commit,
  Lean and clang versions, `flake.lock` hash, the corpus trees and the binary's sha256),
  `build.sh` (fresh clone of a public remote, every locked input checked, build in the tag's own
  dev shell, binary hash compared) and `run.sh` (refuses a binary whose hash differs from the lock).
- `scripts/land/acceptance.sh` is the landing workflow acceptance test: from a fresh clone it runs
  build, a train, the hook's check, exact-SHA CI, `watch` and a dry-run `land`, then repeats
  `watch` and the dry-run `land` against the same run with a tampered verdict (a required job
  removed, skipped or cancelled; another head SHA). Each must be refused. It never lands.
- `RETENTION.tsv` is a first draft of the root retention manifest: every asset group of the tree
  has a disposition (retained, migrate, archived, retire), a destination, an owning task and a
  deletion criterion. `check_retention_manifest.sh` fails on a path with no row, with negative
  controls.
