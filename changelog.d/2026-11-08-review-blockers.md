### Two landing-tool gaps a train review found (2026-11-08)

The review of train/slice0 (9ebdda15b2) reported two defects in code already on main.
`scripts/checks/sandbox_isolation.sh` scanned only `scripts/*.sh` and `scripts/*/*.sh`, so a
repository-making script two directories down, or one without a `.sh` suffix, was never checked.
The scan now covers every shell script under `scripts/` at any depth (a `*.sh` file or an sh/bash
shebang), and a control plants an unsandboxed maker three directories down and a suffixless one,
both refused, and a sandboxed one at depth, accepted.
`scripts/land/install-bot.sh`'s launcher snapshotted only `scripts/land` and `git_sandbox.sh`, so
`train.sh`'s review provenance found no `scripts/review/verify.sh` and refused every train run by
the bot. The snapshot is now one `SNAPSHOT` list covering every file the landing scripts read
outside `scripts/land` (`verify.sh`, `prepush_record.sh` and the `treestate.sh` it sources); an
incomplete cached snapshot is rebuilt; `scripts/checks/landing_bot.sh` fails when a landing script
reads a path the list lacks, with the pre-fix list as its control.
