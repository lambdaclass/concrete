### Only the new things at the root: the first legacy/ sweep (2026-11-08)

The owner asked to keep "only the new things under the root" and approved how a file enters the
frozen `legacy/` ("ok to 1"): `scripts/checks/legacy.lock` names each sweep's source commit, and
`legacy_frozen.sh` admits an addition only as a byte-for-byte move of a root file that is gone from
the root. Edits of `legacy/` files are still refused; new controls cover a valid move, a missing
sweep line, a copy left at the root, a modified copy, a mode change, a file with no root original
and an edit of a frozen file.

This sweep moved the old bug ledger's disposition, the landing audit, the finished move tooling
(`legacy_move.sh`, `move_prereqs.py`) and the answered proposals `IDEAS.md` and `MERGED.md`; the site
takedown record's root copy was a link-rewrite duplicate of its frozen copy and was removed. "ok to
2" moves all of `docs/archive/` once `docs/decisions.md` holds every decision and quote the archive
files carry; the decision log, task register, reconciliation and restructure records, the
2026-11-05 rows and R4 rationale, and the 2026-11-06 proposals stay until that consolidation. The
landing bot stays (its root copy carries the ISS-0015 fix its frozen copy lacks), as do
`worktree-new.sh` (the live playbooks use it), `SPARK.md` and `EXAMPLES.md` (open items). README no
longer calls the proposed spec normative.
