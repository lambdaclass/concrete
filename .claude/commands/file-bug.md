---
description: File a new bug with scripts/gen/new_bug.sh (race-safe number, record, reproducer, fragment)
argument-hint: <one-line title of the defect>
---

File a bug: $ARGUMENTS

Rules: [docs/project/AGENTS.md](../../docs/project/AGENTS.md).

1. **Is it new?** Search `docs/bugs/` on `origin/main` and the `wip/*` and `ci/*` branches
   (`git grep -i '<key words>' $(git for-each-ref --format='%(refname)' refs/remotes/origin) -- docs/bugs`)
   and `docs/verification/KNOWN_HOLES.md`. A known bug gets a note in its record, not a new number.
2. **Reduce it** to the smallest program; run it natively and with `--interp`.
3. **Classify**: pick its class in `docs/bugs/STRUCTURAL_CLASSES.md` (or decide it adds one), and
   whether it is silent (wrong code or an unsound claim) or loud (crash, refusal, invalid IR).
4. **Scaffold** in your bug branch's worktree:
   ```sh
   scripts/gen/new_bug.sh "<title>" --class N --silent yes|no --batch "<text from the probe cell>"
   scripts/gen/new_bug.sh "<title>" --class N --silent yes|no \
       --new-batch <kind> --probe "<what was probed>" --effort "<N> hours|programs|probes|examples|cases"
   ```
   The title says what the defect is, never its status. `--batch` names the existing
   `docs/bugs/discovery.tsv` row of the probe batch that found the bug; `--new-batch` adds that row
   (kinds and effort rules are in the file's header). The script checks all of this before it
   reserves the number on origin (`refs/bug-reservations/NNN`), so no other agent gets it, then
   writes `docs/bugs/NNN_<slug>.md`, `tests/regressions/bugNNN_<slug>/repro.con`, a `changelog.d/`
   fragment and the discovery row, regenerates the views and the dashboard, and lists every
   `FILL-IN(new_bug.sh)` placeholder. If it fails after reserving, it prints the
   `new_bug.sh --number NNN ...` command that files the same bug under the held number; rerun that,
   never write the record by hand. With `--number-only` it only reserves.
5. **Fill in** every placeholder: Reproducer, Observed, Expected (cite the normative rule), Cause,
   Structural cause, Structural owner, Containment. Pin the reproducer in a gate leg (an open bug
   usually goes in the open-bug table of `scripts/tests/check_example_outputs.sh`) and name that leg
   in the ledger block's `coverage:` line.
6. `python3 scripts/gen/bug_ledger.py --write`, then `bash scripts/tests/check_bug_filing_complete.sh`
   (it refuses a leftover placeholder). Commit and push `wip/<branch>`.
7. A silent bug is stopped now (repair or fail-closed containment); continue with `/fix-bug NNN`.

Report: the number, the record path, class, silent or loud, the pinning leg, and the owner.
