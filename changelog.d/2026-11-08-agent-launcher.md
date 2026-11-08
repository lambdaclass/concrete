### Background agent jobs get a bounded watcher (2026-11-08)

`scripts/agent/launch.sh` starts a writer, reviewer or gate once as a bounded transient systemd
user unit and records its output and exit status. The agent runs the printed `WAIT` command as its
own background task, so completion re-invokes the agent instead of leaving a finished job unnoticed.
A lost unit and a deadline overrun are distinct non-passing results, and writers are never retried
automatically. Job directories are exclusive, command arguments survive systemd expansion byte for
byte, and the quoted `WAIT` command is safe for paths containing shell metacharacters. Lost units
retain their timeout/signal/OOM result where systemd provides one. Failed `systemctl is-active` or
`show` queries are operational errors (2), not completed jobs. A failed `show` writes no result cache
and does not collect the unit, so a later wait can read the real result. Stub-backed controls cover
these behaviours and all watcher outcomes.
