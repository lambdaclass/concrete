### Only the new things at the root: the first legacy/ sweep (2026-11-08)

The owner asked to keep "only the new things under the root" and approved how a file enters the
frozen `legacy/` ("ok to 1"): `scripts/checks/legacy.lock` names each sweep's source commit and the
`legacy/` tree it produced, and `legacy_frozen.sh` admits an addition only as a byte-for-byte move of
a root file that is gone from the root. It judges every step from the frozen tree through each sweep
tree to the current `legacy/`, so a swept file is then as frozen as an original one. New controls
cover a valid move, a missing sweep line, a copy left at the root, a modified copy, a mode change, a
file with no root original, an edit of a frozen file, a later edit or open-row deletion of a swept
file, a sweep commit outside HEAD's history, a sweep line without its tree, a path spelled as pathspec
magic and a path git would C-quote.

The check also judges trusted history, main's first-parent chain: a lock row that landed is never
rewritten or removed, and a `legacy/` file never changes from a version it had there, so neither a
replaced sweep row nor a deletion and re-addition can rewrite a file. The lock has one reader,
`scripts/checks/lib/legacy_lock.py`, which refuses a NUL, a CR, a repeated row, a last line without
its newline and any line that is not exactly a `base`, `tree` or `sweep` row; `legacy_frozen.sh` and
`doc_links.py` take its rows from there, and a control refuses any other reader.

This sweep moved the old bug ledger's disposition, the landing audit, the finished move tooling
(`legacy_move.sh`, `move_prereqs.py`) and the answered proposals `IDEAS.md` and `MERGED.md`; the site
takedown record's root copy was a link-rewrite duplicate of its frozen copy and was removed. "ok to
2" moves all of `docs/archive/` once `docs/decisions.md` holds every decision and quote the archive
files carry; the decision log, task register, reconciliation and restructure records, the
2026-11-05 rows and R4 rationale, and the 2026-11-06 proposals stay until that consolidation. The
landing bot stays (its root copy carries the ISS-0015 fix its frozen copy lacks), as do
`worktree-new.sh` (the live playbooks use it), `SPARK.md` and `EXAMPLES.md` (open items). README no
longer calls the proposed spec normative.
