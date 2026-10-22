### Shared Agent Playbooks And A Race-Safe Bug Scaffold (2026-10-22)

Every agent session now runs the same procedures by name: `.claude/commands/` holds `/fix-bug`,
`/roadmap-item`, `/file-bug`, `/review`, `/rebase`, `/land` and `/status`, and
[docs/project/AGENTS.md](docs/project/AGENTS.md) owns the rules they share (structural cause and
owner per bug, one bug or rule per branch, `wip/*` pushes, landings only through the train, at most
two heavy runs per machine, no piped rebases).

`scripts/gen/new_bug.sh "<title>" [--class N] [--silent yes|no]` files a bug. It takes the number
above every record on origin's branches, local branches, every worktree and every reservation, and
owns it by creating `refs/bug-reservations/NNN` on origin with an absent-lease, so two concurrent
agents never get one number. It writes the record with its ledger block, a reproducer skeleton and a
fragment, regenerates the ledger views and lists its fill-in placeholders, which
`check_bug_filing_complete.sh` refuses at landing. The pre-push hook accepts a reservation (a
parentless empty-tree commit) without gates and refuses anything else on that namespace.
`check_new_bug.sh` tests allocation against each source, six concurrent allocations, the scaffold's
ledger consistency, the refusals and the hook tier in a scratch repository (256 gate commands).
