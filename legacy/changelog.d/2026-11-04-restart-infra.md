### Restart infrastructure, prepared before the legacy/ move

- **Required checks and their transitions.** `required-checks.txt` at the root lists every
  landing-tier CI job. `scripts/land/train.sh` `watch` and `land` refuse a run in which a required
  job is missing, skipped, cancelled or failed. `scripts/land/required_checks.py` owns the rule. The
  required set is the candidate's list plus every job the base requires. A candidate can drop a
  base requirement only through a row of `required-checks-transition.tsv`. The row either replaces
  the job with jobs the candidate itself requires, which must pass at that SHA, or retires it with
  a reason. A retired job is reported as retired, not run and never passed. A tree with no list is
  refused. GitHub branch protection is not configured, so these files are the record.
  `check_landing_script.sh` covers each case, and checks the list against the workflow's landing
  tier.
- **New-root CI.** A `Root checks` job (the name the moved root keeps) runs, beside the old jobs, only what the day-one
  root keeps: the retention manifest, the issue record and the landing tooling.
- **Legacy oracle** (`scripts/oracle/`).
  - `ORACLE.lock` pins the following: the tag `legacy-compiler-final` and its commit; the Lean
    and clang versions; the `flake.lock` hash; the corpus trees; and the sha256 of the binary and
    of a byte-reproducible artifact (the binary, `std/` and the dev shell's flake files, with a
    manifest).
  - `build.sh` rebuilds both from a fresh clone of the tag and verifies them. Three builds on two
    hosts gave identical digests.
  - `fetch.sh` downloads the published artifact, verifies it and installs it. With no published
    artifact, or on any failure, it reports INCOMPLETE (exit 3).
  - `publish.sh` uploads the artifact as a release asset. It waits for the owner, so `artifact_url`
    is empty.
  - `run.sh` re-verifies the install against the manifest before each use, with nothing added. It
    also fixes the three environment variables the compiler reads.
  - `.github/workflows/oracle.yml` reruns `build.sh` weekly.
- **Acceptance test** (`scripts/land/acceptance.sh`). From a fresh clone, it runs the following:
  a train of the candidate; a build at the train head; the hook's check; exact-SHA CI; `watch`;
  and a dry-run `land`. Then come the controls, each of which must fail for its own reason:
  - a commit that breaks the build;
  - a commit that a hook gate refuses;
  - a tampered verdict for the same run. The variants are a required job removed, skipped,
    cancelled or failed, and another head SHA.

  The test never lands. A `git` shim refuses any push that could move main, a pre-push guard does
  the same, and both remotes' main are compared before and after.
- **Retention manifest** (`RETENTION.tsv`, first draft). Every tracked file has exactly one
  governing row, the longest listed prefix, with `legacy/P` governed as `P`. Each row records a
  disposition (retained, migrate, archived, retire), a destination, an owning task, a deletion
  criterion and a status (open or resolved).
  - Archived services are never deleted automatically.
  - Retained evidence is split from old test support.
  - The ci-3 corpus and perf history are recorded as lost (P14).

  `check_retention_manifest.sh` enforces this. Because the manifest names docs/ entries, the
  docs-only selection control in `check_gate_selection.sh` now allows 7 commands instead of 6.
- **`ISSUES.tsv`** is the minimal issue record (P12), with `check_issues.sh`. It holds four open
  issues: the unpublished oracle artifact, the required-check transition at the move, the lost
  ci-3 data and the frozen-site record.
- **`docs/archive/site-frozen.md`** is the frozen site deployment's record. Its values
  stay placeholders until a tag-built bundle has been checked and deployed.

Review round 3. The oracle lock now pins `manifest_sha256`, so verifying an install authenticates
`std/` and `shell/` as well as the binary, and an install holding a symlink or any other non-regular
entry is refused. In RETENTION.tsv, `status` now applies to the legacy/ copy only. A retained row's
criterion names both halves (`root: ...; legacy/ copy: ...`), so B3 can release the frozen
duplicates, including the old workflows, without the manifest ever authorizing deletion of a live
root file. A transition row retires a job only through a literal `-`; a malformed replacement list
is refused. ISS-0005 tracks the unrehearsed GitHub-hosted fallback for the oracle check.
acceptance.sh reports a dry-run land refused because main moved as `incomplete`.

The CI job is named `Root checks`, the name the moved root keeps, so one job has one name. ISS-0001
records that P13 is approved (concrete-ci-2 plus GitHub-hosted runners as one pool, oracle as a
release asset); `publish.sh` creates the release tag at the locked commit. ISS-0006 records that bug
282's wall-clock budget fails under host load (owner R-0543); the budget is unchanged.
