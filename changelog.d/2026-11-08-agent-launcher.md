### Background agent jobs get a bounded watcher (2026-11-08)

`scripts/agent/launch.sh` starts a writer, reviewer or gate once as a bounded transient systemd
user unit and records its output and exit status. The agent runs the printed `WAIT` command as its
own background task, so completion re-invokes the agent instead of leaving a finished job unnoticed.
A lost unit and a deadline overrun are distinct non-passing results, and writers are never retried
automatically. Stub-backed controls cover the launcher and all watcher outcomes.
