### One repository: lambdaclass/concrete (2026-11-07)

The owner approved migrating everything to lambdaclass/concrete and archiving
unbalancedparentheses/concrete2, and accepted that every lambdaclass org member with push (44
accounts) reaches the self-hosted runners through branch pushes and `workflow_dispatch` ("A"; both in
`docs/decisions.md`). The `pull_request` guard on every self-hosted job stays.

- **No mirror.** `origin` is lambdaclass/concrete in every clone. `scripts/land/train.sh` lands on
  `origin` only (`LAND_MIRROR` and `land --no-mirror` are gone), `LAND_REPO` defaults to
  lambdaclass/concrete, and the bot, `acceptance.sh` and `install-bot.sh` follow. `push-both.sh`
  keeps its name and lock and pushes one remote.
- **Stale clones fail closed.** `LAND_REPOSITORIES` in `scripts/land/lib.sh` (the guard of
  `2026-11-08-remote-guard.md`) is now lambdaclass/concrete alone (`LAND_CANONICAL_REPO`, the
  default `LAND_REPO`), so every guarded command, the bot and its launcher, the gate transport and
  the pre-push hook refuse unbalancedparentheses/concrete2 under any URL form or ssh alias, before any
  fetch or push, and `LAND_REPO` must name lambdaclass/concrete on a GitHub remote. `install-bot.sh`
  repoints the bot clone, removes its `lambdaclass` remote and migrates an old `LAND_REPO` line
  (`scripts/checks/landing_script.sh` and `landing_bot.sh` cover each).
- **CI.** The Landing record job reads runs from `github.repository`. The concrete-ci-2 runners are
  registered on lambdaclass/concrete, with `CONCRETE_SELF_HOSTED=true` there.
- **Oracle.** The release `oracle-legacy-compiler-final` is republished on lambdaclass/concrete with
  the same asset (sha256 `74d8f1e3…`), and `scripts/oracle/ORACLE.lock` points at it; `mirror_url`
  is gone.
- **Review notes.** `refs/notes/concrete-review*` were copied to lambdaclass/concrete after the
  stray-notes cleanup.
