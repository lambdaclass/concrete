### Every bug record states its spec ruling (2026-10-29)

The spec decides what is a bug. Each record in `docs/bugs/` now carries a `ruling:` line in its ledger
block: the versioned rule of `docs/spec/` the implementation violates (`Violates: OWN-1@2`), a spec
gap (no rule decides the behaviour, so the fix starts by writing one), an open owner question
(`Open: OPEN-17`), the rule that permits it (not a bug), or why it is outside the spec (tooling,
performance or library). `scripts/gen/bug_ledger.py` refuses a missing or malformed ruling and a
cited rule that is not a key of `docs/spec/ids.tsv`; `scripts/gen/new_bug.sh` requires one of
`--violates`, `--spec-gap`, `--open` or `--outside` and checks it before reserving a number. The new
generated view `docs/bugs/RULINGS.md` counts the bugs by ruling, by spec chapter and by violated rule
and lists the spec gaps; the roadmap's bug dashboard shows the open bugs by ruling, and `bug_ledger.py
report` (with `--json` and `--sqlite`) carries the rulings too. Every open bug and every bug from 300
on is ruled. Older fixed bugs may say `Violates: unrecorded` while they are listed in
`bug_rulings.UNRULED`, a list `check_bug_filing_complete.sh` keeps equal to them, so it only shrinks.
