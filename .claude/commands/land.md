---
description: Hand a validated branch to the landing train (scripts/land/train.sh); never push main by hand
argument-hint: "<branch or ref> [train name]"
---

Land: $ARGUMENTS

Rules: [docs/project/AGENTS.md](../../docs/project/AGENTS.md). The manual is
[docs/project/LANDING.md](../../docs/project/LANDING.md); `scripts/land/train.sh` (header) is the
contract. Landings reach `origin` and `lambdaclass` only through it or the landing bot.

**Dependency.** `scripts/land/train.sh` and `LANDING.md` are on branch `wip/landing-script` until it
lands. If they are not in your checkout, check that branch out detached elsewhere
(`git worktree add --detach <dir> origin/wip/landing-script`) and run `bash <dir>/scripts/land/train.sh ...`
from your checkout (it works on the repository of its working directory), or do not land: report the evidence
below to the orchestrator. Never fall back to pushing `main` by hand.

**Preconditions** (stop and report the missing one):
- the branch is pushed as `wip/<branch>` and that SHA is the one you validated;
- `/review` finished (clean, or 5 rounds with follow-ups recorded);
- the fast and area gates passed at that SHA; a full set ran on the server or in CI if the change
  is broad;
- bug records, `changelog.d/` fragment and regenerated views are in the branch;
- a branch that changes a language rule goes in its own train, ahead of work that depends on it.

**If you are the landing orchestrator** (the user or the orchestrator asked you to land):

```sh
scripts/land/train.sh build <name> origin/wip/<branch> [more refs]   # train/<name> = origin/main + refs
scripts/land/train.sh check <name>     # hook selection; full set on the server via concrete-remote
scripts/land/train.sh ci    <name>     # push ci/<name>, dispatch CI
scripts/land/train.sh watch <name>     # verdict, failing job log tails
scripts/land/train.sh land --dry-run <name> && scripts/land/train.sh land <name>
```

Each step stops on any failure; a conflict outside generated views exits 3 and names the files,
which you resolve on the member branch (`/rebase`), not on the train. A moved `origin/main`
means `build --force` and a new run: a result belongs to the exact SHA that ran. A red CI run: read
the logs, fix on the member branch, rebuild. Never land on a timeout or a skipped job.

**Otherwise** do not land: report the branch, head SHA and the evidence above, so the orchestrator
puts it on a train.

After a landing: `gh run list --workflow CI --branch main` and confirm the run for the landed SHA
is green; a red main stops the line.
