# Bug 232 — the discovery-signal controls assume the quiet streak is zero

<!-- bug-ledger
summary: `check_docs_drift.sh`'s discovery-signal controls grepped the regenerated dashboard for a literal streak ("1 consecutive" after a quiet batch, "0" after a token one), true only while the last counting batch was not quiet, so recording a quiet batch failed the gate; the controls now read the record's own streak and expect one more, or none. Covered by `check_docs_drift.sh` itself. Class 12.
coverage: the docs-drift gate pinned the discovery signal's streak as a literal (1 after a quiet batch, 0 after a token batch), so recording a quiet batch failed the gate. Regression vehicle is the docs-drift gate itself (its controls now read the record's streak and expect +1 / +0) (2026-10-24)
class: 12 — the discovery signal's streak: computed by the dashboard generator, restated as a literal by the docs-drift controls
silent: no
-->

**Status:** FIXED (branch `wip/diffhunt-1025`)
**Found:** 2026-10-24, while recording the differential hunt's batch in `docs/bugs/discovery.tsv`:
with that batch at one loud bug (quiet, 45 programs), `check_docs_drift.sh` failed four controls
on a correct record.
**Severity:** loud (gate false failure). Recording a quiet counting batch, the event exit 2m's
signal exists to count, failed the docs-drift gate and so the fast surface gates and the pre-push
hook.

Confirmed real by Codex review, 2026-10-25: Literal streak expectations rejected legitimate quiet batches, contradicting ROADMAP.md exit 2m’s counting rule and exit 2d’s prohibition on incidental count pins.

## Reproducer

On main `9622f7e0`, append a quiet counting batch to `docs/bugs/discovery.tsv` (`2026-10-24 hunt
… 45 programs 231 -`) with its record, regenerate the dashboard (`bug_dashboard.py --write`) and
run `scripts/tests/check_docs_drift.sh`: `positive control: a well-formed probe batch failed to
regenerate, check or count` and the three `token batch … moved the discovery signal` controls fail.
Expected: every control passes; the dashboard is right ("1 consecutive").

## Cause

The controls in section 6 of `check_docs_drift.sh` append a batch to a copy of the record and grep
the regenerated dashboard for a literal streak: `signal (exit 2m): 1 consecutive` after a quiet
batch, `0 consecutive` after a token batch. Those literals hold only while the record's own streak
is 0, i.e. while the last eligible counting batch was not quiet.

## Fix

The gate first generates the dashboard from the unmodified record and reads its streak (`STREAK0`;
a failed generation or an unreadable streak is a failure, and the controls then cannot match), then
expects `STREAK0 + 1` after a quiet batch and `STREAK0` after each token
batch. The gate's own run is the regression leg: with this branch's record (a quiet batch last) the
controls pass, and they still fail if a token batch moves the signal or a quiet one does not.

## Structural cause

Class 12 ("one fact kept in two places"): the current streak is computed by the dashboard
generator from the record, and the gate restated it as a constant, a count pin rather than the
property (a quiet batch adds one, a token batch adds none).

## Structural owner

Exit 2d (R-0513: checked inventories and properties replace incidental count pins).
