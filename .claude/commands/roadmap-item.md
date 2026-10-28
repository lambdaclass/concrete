---
description: Carry one roadmap task (R-NNNN) to a reviewed, landable candidate
argument-hint: <R-NNNN>
---

Roadmap task: $ARGUMENTS

Rules: [docs/project/AGENTS.md](../../docs/project/AGENTS.md).

1. **Read the task**: its `### Task $ARGUMENTS` section in `docs/project/ROADMAP_TASKS.md` (or
   `docs/project/ROADMAP_BACKLOG.md`), its row in `ROADMAP.md`'s "The current execution queue", the exits it owns ("Phase 1 exits") and their acceptance tests, and the docs
   it cites. If its prerequisites have not landed, stop and report which.
2. **Check nobody has it**: `git branch -r | grep -i <id or topic>`, and the "Implemented, not yet
   accepted" table. Continue an existing `wip/*` branch rather than starting a second one.
3. **Branch**: `scripts/worktree-new.sh <r-nnnn-slug> origin/main`. One rule per branch; a
   language-rule change lands first and stays small, so split the task into slices that each land.
4. **Done-when first.** Turn the task's acceptance test into a failing gate leg or fixture before
   implementing. Assert properties, not counts.
5. **Implement the slice.** Derive from the single source of each rule; do not retype it.
   Language invariants: link the normative docs listed in AGENTS.md; a change to them updates the
   doc with accepted and rejected examples in the same branch.
6. **Bugs found on the way**: `/file-bug` each one; fix it on its own branch unless it blocks this
   slice.
7. **Record**: a `changelog.d/` fragment (`### Completed Task $ARGUMENTS` when the task is finished,
   otherwise a slice heading), and the roadmap prose updated without status tags.
8. **Validate** in `nix develop`: the new legs, `run_fast_surface_gates.sh`, the area gates. Full
   set on the server (`concrete-remote`) or in CI. Commit, push `wip/<branch>` every 5 minutes.
9. `/creview`, then `/land`.

Report: the slice delivered, what remains of the task, the head SHA, the gates and their results,
the Codex verdict.
