### The old tree moved to `legacy/` (R-0558, 2026-11-04)

Everything of the old compiler, its tests, examples, std, proofs, docs, research, site and gates moved
under `legacy/`, which is a byte-for-byte copy of main's tree at the move's base commit: never edited,
never run. `scripts/checks/legacy_frozen.sh` refuses any change to it, and any deletion
`RETENTION.tsv` does not allow. The root keeps only the day-one set (`scripts/checks/root_inventory.txt`):
the README, roadmap, AGENTS.md, a new changelog, the philosophy and the decisions, the specification's
build files, the landing, review and oracle scripts, the hook, CI, `required-checks.txt`,
`RETENTION.tsv` and the minimal issue record `ISSUES.tsv`. The landing tools now run the root checks
(`scripts/checks/run.sh`) instead of the old gate runner, and CI runs two jobs, "Root checks" and
"Spec build". The old workflows, the site build (`book.yml`), the nightly campaign and branch health
moved with the tree and no longer run. The website comes down (decision row P9): its build stops
with this change, and the published site is unpublished by the documented manual step in
`docs/archive/site-down.md`, at a moment the owner confirms; a new site is built later with the new
compiler (R-0569). `scripts/land/legacy_move.sh`
reproduces this commit from its base.

The required set changes through `required-checks-transition.tsv`: each old landing job is retired
with its reason (legacy/ is never run; the pinned oracle carries the differential), so the train
reports those jobs NOT RUN, never passed, and "Root checks" stays required on both sides. Two of the
restart's checks join the root checks: `retention_manifest.sh` (every tracked file has one governing
`RETENTION.tsv` row) and `issues.sh` (the issue record is well formed). `RETENTION.tsv` and
`ISSUES.tsv` were drafted before P9, so their site rows now follow it: the site and its browser
tooling are archived, not migrated, and ISS-0004 tracks the takedown, not a frozen bundle. The
2026-11-05 decision rows (`docs/archive/decisions-2026-11-05/`) stay at the root beside
`docs/decisions.md`. `docs/archive/bugs-2026-11/bug_disposition.tsv` gives every record of the frozen
bug ledger its P10/P11 disposition: retired with the old implementation, carried with std, or
carried as a ruling. It includes the corrections of the 2026-11-04 review and of the 2026-11-05
decisions.

It may land only when these hold, and none is claimed done here. The generator refuses a landable
move (it builds only a preview, which `legacy_frozen.sh` fails) until the base has: the oracle pin and
its scheduled rebuild (P1, P2), the retention manifest's status column (P3), the takedown record
(P4), the oracle published at an https `artifact_url` (P5), the website takedown done after the
owner's confirmation (P6: `status: done DATE`), and no open `ISSUES.tsv` row of the preparation tasks
R-0563, R-0564 and R-0565 (P7). The acceptance rehearsal of the exact candidate (R-0563) is a
landing-time fact: the train must refuse the move without an `accepted` record for it.
