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
  dev shell, binary hash compared) and `run.sh`. Before each use `run.sh` checks the binary's hash
  and that the installed checkout (its `std/`, `flake.nix`, `flake.lock`) is the locked commit with
  nothing changed or added, and it fixes the three environment variables the compiler reads.
  Three fresh builds on two hosts gave the same binary hash.
- `scripts/land/acceptance.sh` is the landing workflow acceptance test: from a fresh clone it runs
  build, a train, the hook's check, exact-SHA CI, `watch` and a dry-run `land`, then repeats
  `watch` and the dry-run `land` against the same run with a tampered verdict (a required job
  removed, skipped or cancelled; another head SHA). Each must be refused with its own diagnostic,
  and the tampering must be recorded. It never lands, independently of `--dry-run`: its hooks path
  refuses any push to main, it refuses landing scripts that use `--no-verify`, and it compares
  both remotes' main before and after.
- `RETENTION.tsv` is a first draft of the root retention manifest: every asset group of the tree
  has a disposition (retained, migrate, archived, retire), a destination, an owning task and a
  deletion criterion. `check_retention_manifest.sh` fails on a path with no row, with negative
  controls. Because it reads docs/ entries through the manifest, the docs-only selection control
  in `check_gate_selection.sh` now allows 7 commands instead of 6.
