---
description: Hand a validated branch to the landing train (scripts/land/train.sh); never push main by hand
argument-hint: "<branch or ref> [train name]"
---

Land: $ARGUMENTS

Rules: [AGENTS.md](../../AGENTS.md). The manual is
[docs/LANDING.md](../../docs/LANDING.md); `scripts/land/train.sh` (header) is the
contract. Landings reach `origin` (lambdaclass/concrete, the one repository) only through it.

**Where.** Run `train.sh` from a full clone with the repository hook enabled (`git clone`, then
`git config core.hooksPath .githooks`), never from a linked worktree: it refuses both a worktree and
a clone with the hook off (ISS-0015, LANDING.md "Where a train runs"). Use one clone per train, from
`build` through `land` (the train's state lives in that clone's git directory), and no other push
from it while the train runs. If you cannot, do not land: report the evidence below to the orchestrator. Never fall back to
pushing `main` by hand.

**Preconditions** (stop and report the missing one):
- the branch is pushed as `wip/<branch>` and that SHA is the one you validated;
- `/creview` finished (clean, or 5 rounds with follow-ups recorded);
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
