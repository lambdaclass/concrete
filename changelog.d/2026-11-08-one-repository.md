### One repository: lambdaclass/concrete (2026-11-08)

The owner approved migrating everything to lambdaclass/concrete and archiving
unbalancedparentheses/concrete2, and accepted that every lambdaclass org member with push (44
accounts) reaches the self-hosted runners through branch pushes and `workflow_dispatch` ("A"). These
are the owner's decisions of 2026-11-06, pending their record in `docs/decisions.md`. The
`pull_request` guard on every self-hosted job stays.

- **No mirror; enforced canonical destination.** `origin` is lambdaclass/concrete in every clone,
  with one canonical identity (`LAND_CANONICAL_REPO="lambdaclass/concrete"`,
  `LAND_CANONICAL_REMOTE="origin"`) owned by `scripts/land/lib.sh`. `scripts/land/train.sh`
  (`LAND_MIRROR` and `land --no-mirror` are gone), `scripts/push-both.sh`, `scripts/land/ready.sh`,
  `scripts/land/notes.sh`, `scripts/land/impact_gates.sh`, `scripts/land/acceptance.sh` and
  `.githooks/pre-push` (when pushing `main`) verify the resolved fetch and push URLs of `origin`
  (`git remote get-url --all` and `--push --all`, normalizing GitHub HTTPS and SSH URLs to one
  `owner/name` identity and accounting for `pushurl`, `insteadOf` and `pushInsteadOf`) and refuse
  before any ref moves unless there is a single destination and it is `lambdaclass/concrete`.
  Multiple push URLs, fetch/push URL disagreements, destination overrides (`LAND_REMOTE`,
  `PUSH_REMOTE`, `BRANCH`, CLI destination arguments) and CI-evidence repository overrides
  (`LAND_REPO`, `ACCEPT_REPO`, `GH_REPO`) are refused fail-closed.
- **CI.** The Landing record job reads runs from `github.repository`. The concrete-ci-2 runners are
  registered on lambdaclass/concrete, with `CONCRETE_SELF_HOSTED=true` there.
- **Oracle.** The release `oracle-legacy-compiler-final` is republished on lambdaclass/concrete with
  the same asset (sha256 `74d8f1e3…`), and `scripts/oracle/ORACLE.lock` points at it; `mirror_url`
  is gone.
- **Review notes.** `refs/notes/concrete-review*` were copied to lambdaclass/concrete after the
  stray-notes cleanup.
