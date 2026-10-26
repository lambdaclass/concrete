# Bug 300 — new_bug.sh reserves a number, then refuses the filing for want of a discovery row naming it

**Status:** FIXED (2026-10-26, branch `wip/bug-new-bug-discovery-row`)
**Found:** 2026-10-26, by two agents filing bugs 284 and 299 the same day: each run of
`scripts/gen/new_bug.sh` reserved its number and then aborted, so both records were written by hand
under the reserved numbers. Reproduced on main `ed95ba30` and confirmed real by Codex (read-only
review of the scripts and docs; verdict below).
**Severity:** loud, in tooling only. The documented way to file a bug could not succeed on a tree
with `docs/bugs/discovery.tsv` and `scripts/gen/bug_dashboard.py` (main since the dashboard landed):
every filing reserved a number on origin, then removed its own files and left the number reserved
and unfiled, and the author had to write the record, fragment and discovery row by hand. No
compiler output or claim is affected.

<!-- bug-ledger
summary: `new_bug.sh` reserved a bug number and then failed in `bug_dashboard.py`, which refuses a record from 079 on with no `docs/bugs/discovery.tsv` row; the row has to name the number, which does not exist before the reservation, and the scaffold neither wrote the row nor offered a way to name it, so every filing left a reserved, unfiled number. The scaffold now writes the attribution itself (`--batch` joins an existing row, `--new-batch` adds one), checks it and the dashboard before reserving, undoes its `discovery.tsv` edit on a later failure, and prints a `--number NNN` command that files the same bug under the held number.
coverage: `check_new_bug.sh` leg 3 (the scaffold runs through `bug_dashboard.py`, which its scratch copy now includes, and writes the new batch's row) and leg 6 (the discovery attribution: refusals before any reservation, `--batch` on an existing row, rollback of `discovery.tsv` after a failure past the reservation, and the printed `--number` rerun with no second reservation)
class: 12
silent: no
-->

## Reproducer

In a scratch clone of main `ed95ba30` whose `origin` is a local bare repository:

```
$ bash scripts/gen/new_bug.sh "Repro control: dummy defect without a discovery row" --class 1 --silent no
new_bug: reserved bug 299 on origin (refs/bug-reservations/299)
bug dashboard: bugs 299 have records but no discovery batch in .../docs/bugs/discovery.tsv; add each to the batch that found it
new_bug: filing failed; removed the files it had written (bug 299 stays reserved: reuse it or leave the gap)
$ git ls-remote origin 'refs/bug-reservations/*'
fd5246e0...	refs/bug-reservations/299
$ git status --short
$
```

## Observed

The reservation survives on the remote; the record, reproducer skeleton and changelog fragment are
removed by the rollback. Nothing in the script let the author supply the row: the row names the
number, `--number-only` followed by a normal run reserves a second number, and there was no way to
file under a number already held. The only workaround was to guess the next number, add a row
naming it before running the script, and pass `--silent` (which this bug's own filing used).

## Expected

[docs/bugs/README.md](README.md), "Filing a bug", and [AGENTS.md](../project/AGENTS.md) make
`new_bug.sh` the way to file a bug, and [discovery.tsv](discovery.tsv)'s header requires the
discovery row in the same commit. The scaffold should produce a filing the dashboard accepts, and
anything that can refuse should refuse before a number is reserved
(the script's own rule: "a refusal never burns one").

## Cause

`scripts/gen/new_bug.sh` checked the ledger and the changelog before reserving, wrote the record,
reproducer and fragment after, and then ran `bug_dashboard.py --write` under `set -e`.
`bug_dashboard.py` `load_discovery` refuses (exit 2) any record numbered 079 or higher that no
discovery row lists. The EXIT trap removed the files and kept the reservation. `bug_ledger.py`'s
`check_discovery` also compares a listed bug's `silent:` line with the row, so even a pre-added row
needed `--silent`.

`scripts/tests/check_new_bug.sh` copied the ledger generators into its scratch repository but not
`bug_dashboard.py`, so its scaffold leg skipped the dashboard (`[ -f scripts/gen/bug_dashboard.py ]`)
and passed while every real filing failed.

## Fix

- `new_bug.sh` writes the discovery attribution: `--batch <text>` adds the bug to the one existing
  row whose probe cell contains the text, `--new-batch <kind> --probe <text> --effort <size>` inserts
  a row dated today in date order; the number also goes in the row's silent cell with
  `--silent yes`. When the tree has the discovery record and the dashboard, a batch and `--silent`
  are required and checked before the reservation, against the dashboard's own rules (kinds, the
  effort format of counting batches, class tags), together with `bug_dashboard.py --check`.
- A failure after the reservation removes the other files and takes only its own edit back out of
  `discovery.tsv`, hands the number
  back and prints the command that files the same bug under it. A reservation's first line names
  its state: `bug NNN reserved:` is held and free to file (`--number-only`, or handed back by a
  failed filing), `bug NNN filing:` is taken by a run. `new_bug.sh --number NNN ...` files under a
  `reserved` number this user holds that no branch or worktree has a record for, claiming it by
  moving the ref to a `filing` commit with a lease on the one it read, so of two concurrent claims
  exactly one files it (Codex review).
- A full filing holds a per-worktree lock (`git rev-parse --git-path new_bug.lock`) from its first
  check to its last write or rollback, so two filings in one worktree cannot interleave their
  rewrites of `discovery.tsv` and the generated views; `discovery.tsv` is rewritten through a
  temporary file renamed into place (Codex review).
- `check_new_bug.sh` copies `bug_dashboard.py`, so leg 3 runs the whole filing through the
  dashboard, and leg 6 covers the attribution, the refusals before any reservation, the rollback
  and the `--number` rerun. The automatic filing paths print commands that pass as printed: the
  fuzz farm's `promote` names the finding's own generator, seed and compiler as a `fuzz` batch, and
  CI triage names the run and gate as a `ci` batch and leaves `--silent SILENT` for the filer
  (a CI failure does not say whether the defect is silent; the bare placeholder is refused before
  any reservation); leg 6 runs both (the farm's from its golden).
  The work-queue dispatcher files nothing itself: its fuzz tasks go through `promote`, and agents
  file through `/file-bug`. The pre-push hook selects `check_new_bug.sh` when `bug_dashboard.py`
  changes.

## Structural cause

Class 12 of [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), one fact kept in two places: a bug's
discovery attribution (and its silent flag, a second time) lives in `discovery.tsv`, beside the
record that owns every other fact about the bug, and the one tool that creates records wrote only
the record. A precondition in the second place was enforced after an irreversible step in the
first, and the scaffold's test copied only the first place's generator, so the disagreement was
invisible to it. Other bugs of the class: 042, 057, 064, 070, 121, 122.

## Structural owner

R-0542 (generated ledgers: the bug record as the single source of the bug's facts). Moving the
attribution into the record's ledger block, with `discovery.tsv` keeping only the batches, would
remove the second place; until then `new_bug.sh` is the single writer of both at filing, and
`check_new_bug.sh` leg 6 pins it.

## Codex verdict

VERDICT: REAL. "The documented scaffold enforces discovery attribution before the author can supply
the allocated number, rolls back the filing, and leaves the remote reservation behind." Codex also
found that no documented workflow avoids it (pre-entering the number works only by predicting the
allocation, which is unsafe under concurrent reservations) and that `check_new_bug.sh` masked it by
not copying `bug_dashboard.py`.
