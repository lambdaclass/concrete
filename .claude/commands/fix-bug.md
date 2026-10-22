---
description: Reproduce, classify and repair (or contain) one Concrete bug on its own branch
argument-hint: <bug number or description>
---

Fix bug: $ARGUMENTS

Rules: [docs/project/AGENTS.md](../../docs/project/AGENTS.md). Read it first if you have not this session.

1. **Find or file it.** A number: read `docs/bugs/NNN_*.md` on `origin/main` and on any `wip/*` or
   `ci/*` branch that touches it (`git log --all --oneline -- 'docs/bugs/NNN_*'`). A description
   with no record: run `/file-bug` first.
2. **Branch.** `scripts/worktree-new.sh bugNNN-<slug> origin/main`. One bug per branch. If the fix
   changes a language rule, keep that rule change alone on the branch so it can land first.
3. **Reproduce** on the current build, natively and with `--interp`; record the exact command,
   observed and expected result in the record. If it no longer reproduces, find the commit that
   fixed it and say so in the record instead of fixing anything.
4. **Classify before patching.** Write `## Structural cause` and `## Structural owner`: which class
   in `docs/bugs/STRUCTURAL_CLASSES.md` this is an instance of (add `class: N` to the ledger block),
   which other bugs share it, which roadmap task or pipeline stage removes the class. Search for the
   other spellings of the same cause and list them. If the repair belongs to that owner and the bug
   is loud, contain and pin it instead of patching (ROADMAP defect disposition rule).
5. **Repair at the owner**, not at the symptom: the single source of the rule (for example
   `Concrete/Semantics/IntArith.lean` for traps), not a second copy of it.
6. **Regression leg.** A fixture under `tests/programs/` registered in `scripts/tests/run_tests.sh`,
   or a leg in the gate that owns the area. Show it fails on the pre-fix compiler and passes now.
   Point the record's `fixtures:` or `coverage:` at it; set `**Status:** FIXED (branch, commit)`;
   remove the reproducer from the open-bug table of `check_example_outputs.sh` if it was pinned there.
7. **Regenerate and log.** `python3 scripts/gen/bug_ledger.py --write`; add a `changelog.d/` fragment.
8. **Validate** inside `nix develop`: the targeted fixture, then `bash scripts/tests/run_fast_surface_gates.sh`
   and the area gates. Run `bash scripts/tests/check_examples.sh` after Lower/EmitSSA changes. Full
   sets go to `concrete-remote wip/<branch> '...'` or CI, never the laptop.
9. Commit, push `wip/<branch>`, run `/creview`. When review and gates are clean, run `/land`.

Report: the branch and head SHA, the structural cause and owner, the regression leg (and proof it
failed before), the gates run with their results, the Codex verdict.
