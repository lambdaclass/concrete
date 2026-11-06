---
description: Carry one roadmap task (R-NNNN) to a reviewed, landable candidate
argument-hint: <R-NNNN>
---

Roadmap task: $ARGUMENTS

Rules: [AGENTS.md](../../AGENTS.md).

1. **Read the task**: its row in [ROADMAP.md](../../ROADMAP.md) (what it delivers, its done-when, what
   it depends on), the owner decisions it touches in [docs/decisions.md](../../docs/decisions.md), and
   the documents it cites. An archived task ID resolves through the roadmap archive's task disposition
   table. If its prerequisites have not landed, stop and report which. A language question that is
   not decided goes to the owner; never decide it by implementing it.
2. **Check nobody has it**: `git branch -r | grep -i <id or topic>`. Continue an existing `wip/*`
   branch rather than starting a second one.
3. **Branch**: `scripts/worktree-new.sh <r-nnnn-slug> origin/main`. One rule per branch; a
   language-rule change lands first and stays small, so split the task into slices that each land.
4. **Done-when first.** Turn the task's acceptance test into a failing check, example or theorem
   statement before implementing. Assert properties, not counts.
5. **Implement the slice.** Derive from the single source of each rule; do not retype it. Anything
   brought back from `legacy/` is copied in this change (never edited in place), and its line is
   added to `scripts/checks/root_inventory.txt` when it is a new root entry.
6. **Defects found on the way**: a row in [ISSUES.tsv](../../ISSUES.tsv) each; fix it on its own
   branch unless it blocks this slice.
7. **Record**: a `changelog.d/` fragment, and the roadmap prose updated without status tags.
8. **Validate** in `nix develop`: `lake build` and `bash scripts/checks/run.sh`; CI on the candidate.
   Commit, push `wip/<branch>` every 5 minutes.
9. `/creview`, then `/land`.

Report: the slice delivered, what remains of the task, the head SHA, the checks and their results,
the Codex verdict.
