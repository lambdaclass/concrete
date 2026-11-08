### One repository: lambdaclass/concrete (2026-11-08)

The owner approved migrating everything to lambdaclass/concrete and archiving
unbalancedparentheses/concrete2, and accepted that every lambdaclass org member with push (44
accounts) reaches the self-hosted runners through branch pushes and `workflow_dispatch` ("A"; both in
`docs/decisions.md`). The `pull_request` guard on every self-hosted job stays.

- **No mirror.** `origin` is lambdaclass/concrete in every clone. `scripts/land/train.sh` lands on
  `origin` only (`LAND_MIRROR` and `land --no-mirror` are gone), `LAND_REPO` defaults to
  lambdaclass/concrete, and `acceptance.sh` follows. `push-both.sh` keeps its name, shared lock and
  optional CI wait, and pushes one remote. Main will be protected server-side; the smaller local
  remote guard is a separate later branch and is not part of this switch.
- **CI.** The Landing record job reads runs from `github.repository`. The concrete-ci-2 runners are
  registered on lambdaclass/concrete, with `CONCRETE_SELF_HOSTED=true` there.
- **Oracle.** The release `oracle-legacy-compiler-final` is republished on lambdaclass/concrete with
  the same asset (sha256 `74d8f1e3…`), and `scripts/oracle/ORACLE.lock` points at it; `mirror_url`
  is gone.
- **Review notes.** `refs/notes/concrete-review*` were copied to lambdaclass/concrete after the
  stray-notes cleanup.
