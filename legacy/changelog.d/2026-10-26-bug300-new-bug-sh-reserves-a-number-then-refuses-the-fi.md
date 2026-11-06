### Bug 300: new_bug.sh reserves a number, then refuses the filing for want of a discovery row naming it (2026-10-26)

`scripts/gen/new_bug.sh` now writes the bug's `docs/bugs/discovery.tsv` attribution itself
(`--batch <probe text>` joins an existing row, `--new-batch <kind> --probe <text> --effort <size>`
adds one) and requires it, with `--silent`, before it reserves a number, so a filing the dashboard
would refuse no longer leaves a reserved, unfiled number. A failure after the reservation takes the
bug back out of `discovery.tsv`, hands the number back as `reserved`, and prints a `new_bug.sh --number NNN ...`
command that files the same bug under it; `--number` claims the number with a lease, so two runs
cannot both file it. `check_new_bug.sh` now runs the scaffold through `bug_dashboard.py` (leg 3) and pins
the attribution, rollback and rerun (leg 6). Structural owner: R-0542
([bug 300](docs/bugs/300_new_bug_sh_reserves_a_number_then_refuses_the_fi.md)).
