### The landing tooling talks only to the project's repositories (2026-11-08)

Before the move to one repository (lambdaclass/concrete), every script that fetches from or pushes
to GitHub, and the pre-push hook, refuse a remote outside the project's repositories.

- **One guard.** `land_remote_bad` in `scripts/land/lib.sh` accepts a remote only when every fetch
  and push URL that is a network URL is `github.com/<owner>/<name>` for one of `LAND_REPOSITORIES`
  (a constant: lambdaclass/concrete and, until the one-repository change, unbalancedparentheses/
  concrete2), all of them the same repository and, where gh reads it, `LAND_REPO`. ssh host aliases
  are resolved with `ssh -G` (with the URL's user and port; it connects nowhere). Another host,
  another repository, a fetch URL at one repository and a push URL at the other, and unknown
  schemes are refused. Local paths (test fixtures) pass.
- **Where.** `train.sh` build, check, ci, watch and land (and the land mirror), the bot at startup
  and its launcher before its first fetch, `push-both.sh` (both remotes), `ready.sh`, `notes.sh
  fetch|put`, `review.sh`, `verify.sh --fetch`, and the gate transport, which checks the gate
  host's clone on the host before it fetches (a refusal is INCOMPLETE). `.githooks/pre-push`
  refuses a push to any URL outside the set, before every tier and whatever CONCRETE_SKIP_GATES
  says, so a push by hand is covered too.
- **Credentials.** Refusals print URLs with any userinfo replaced by `***`.
- **install-bot.sh** drops push URLs of origin and lambdaclass in the bot clone, and logs URLs
  redacted.
- `scripts/checks/landing_script.sh` and `landing_bot.sh` cover each refusal, prove with git shims
  that no fetch or push happens first, and check that both project repositories still pass.
