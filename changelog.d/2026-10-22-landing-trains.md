### Ready Branches Land In Trains, With A Landing Bot (R-0539, 2026-10-22)

Ready branches now reach main together as a train: one integrated candidate, one CI run, and
bisection by member only when that run is red. `scripts/land/train.sh` runs the cycle that had been
done by hand: `build` (main plus each member, `--no-ff`, in order), `check` (fast surface gates,
impact-selected gates and the remote full set, in parallel), `ci` (push `ci/<name>` and dispatch),
`watch`, `land` (verify, then push the exact SHA to origin and lambdaclass) and `bisect`. Conflicts
are resolved mechanically only in append-only ledgers, whole generated files and generated blocks
whose generator the train will run; anything else stops the build. After the last merge the
generated views are regenerated once. Each train keeps a local per-stage log, and
`scripts/land/report.sh` reports the median time to land. The procedure is
[LANDING.md](docs/project/LANDING.md). Main has landed through numbered trains since, through
train 22 on 2026-10-25.

Pushing to both remotes is required (the owner's decision of 2026-10-22): `push-both.sh` mirrors
to lambdaclass by default and checks the mirror before the primary moves, and `train.sh land`
refuses a missing mirror. A branch is ready when it is a `wip/*` branch whose head carries a clean
review note and a passing gates note (`scripts/land/ready.sh` writes the latter).

`scripts/land/bot.sh` automates the loop: it collects ready branches, orders rule-changing branches
first, builds and dispatches the train, bisects and ejects a culprit, rebases idle ready branches
onto a moved main, and resumes after a restart, under a lock and a kill switch. Its installer
writes systemd user units but never enables them; running the bot is the maintainers' decision.
`check_landing_script.sh` and `check_landing_bot.sh` (CI) test both scripts against scratch
repositories.
R-0539 stays open in the roadmap until its measured exit criterion holds.
