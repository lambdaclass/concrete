### One Owner Per Fact: Ledger Generators And Changelog Fragments (R-0542, 2026-10-22)

The bug ledgers were kept by hand in several shared files, and every rebase conflicted on them.
The generators and gates that give each fact one owner have landed:

- `scripts/gen/bug_ledger.py` reads a ledger block in each `docs/bugs/NNN_*.md` record (summary,
  coverage, structural classes, silent flag, exits) and generates the bug lists of
  [docs/bugs/README.md](docs/bugs/README.md), the bugs column of the structural classes, the
  roadmap's exit spans and the audit coverage maps;
- changelog entries are fragments in `changelog.d/`, which `scripts/gen/changelog.py --fold` folds
  into this file on main ([changelog.d/README.md](changelog.d/README.md)); once stamped, a hand
  edit of this file fails the docs-drift gate;
- `scripts/gen/refresh_generated.sh` regenerates every derived view after a conflicted rebase;
- `check_ledgers.sh`, a docs-drift leg, runs the one-time migration on a scratch copy, with
  negative controls.

Changelog fragments have been in use since. The one-time migration itself
(`scripts/gen/migrate_ledgers.sh`) has not yet run on main, so the bug lists are still kept by hand
and this file is not yet stamped.
