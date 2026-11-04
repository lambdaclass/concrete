---
description: Carry one roadmap task (R-NNNN) to a reviewed, landable candidate
argument-hint: <R-NNNN>
---

Roadmap task: $ARGUMENTS

Rules: [docs/project/AGENTS.md](../../docs/project/AGENTS.md).

1. **Read the task**: its row under `ROADMAP.md`'s "Tasks" (objective, done when, depends on), its
   queue row, the rules in force at the top of `ROADMAP.md` (and the records they link), and the owner decisions in
   `docs/spec/decisions.md`. Read the obligations it carries: the rows of
   `docs/archive/roadmap-2026-11/task_disposition.tsv` whose `destination` is `$ARGUMENTS`, and
   its own row there. The full old body, if any, is in that archive's `ROADMAP_TASKS.md` or
   `ROADMAP_BACKLOG.md`; the roadmap row and the rules in force override it. If its prerequisites
   have not landed, stop and report which.
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
