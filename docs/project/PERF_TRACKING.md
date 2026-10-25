# Compiler performance tracking

The engine lives in `scripts/perf/`: `suite.py` measures, `store.py` keeps records on the
`perf-data` branch (never merged into main), `detect.py` decides, `perf_bisect.py` names the
culprit, `report.py` shows trends, and `runner.sh` runs one cycle. `runner.sh`'s header documents
the layout, the states and how a cycle shares the machine. `scripts/tests/check_perf_tracking.sh`
gates the decision logic.

## What is measured

Retired user-space instructions (`perf stat -e instructions:u -x,`, whole process tree) are the
primary metric, because no machine is ever quiet and instruction counts do not move with load,
frequency or scheduling. Peak RSS (`/usr/bin/time`) is the memory metric. Wall time is recorded as
information and never alarmed on. Growth exponents of the synthetic stress programs are computed
from instruction counts. Where `perf` cannot count `instructions:u` (macOS, or a Linux host whose
`kernel.perf_event_paranoid` is above 2), a run falls back to wall time and its record is marked
`comparable: false`: kept for reading, never a baseline, never an alarm.

## Noise and threshold

Measured on concrete-ci-1 (AMD EPYC 8324P, the CI server until it was deleted on 2026-10-23) on
main `185d998c`, three suite runs at load 35 with
CI runners and gate runs active: compiler instruction counts varied by at most 0.002% between runs
(median 0.000%) and 0.008% between repeats within a run. Peak RSS varied by up to 0.6% between
runs and 1.4% within one. An alarm needs a metric above
`baseline * (1 + band) * (1 + threshold)` and above an absolute floor (5M instructions, 4 MB):
`band` is 3x the measured robust spread but never below 0.5%, and `threshold` is 1%, so an
instruction-count alarm fires at about +1.5%. Multi-threaded builds (`lake build`) and the test
suite may spin while waiting on other threads, which can add instructions under load; their band
comes from their own history.

## Server provisioning (lives outside this repository)

Server pieces belong to the owner's private server provisioning, not to concrete. The tracker's
host is the CI server, concrete-ci-2 (a Hetzner AX102 on Arch Linux), where `concrete-perf.service`
and `concrete-perf.timer` are installed and the timer is not enabled (2026-10-25). The steps below
were written for its predecessor, concrete-ci-1, an Ubuntu host; on Arch the packages are `perf`
and `time`. What that provisioning must contain:

1. **sysctl**: `kernel.perf_event_paranoid = 2` (Ubuntu ships 4, which refuses unprivileged
   counting). For example, `/etc/sysctl.d/60-perf-tracking.conf` containing that line, applied with
   `sysctl --system`. Value 2 lets a user count its own processes in user space only; it does not
   allow kernel profiling. Packages: `linux-tools-$(uname -r)` (for `perf`) and `time` (GNU time).
2. **Harness checkout**: a clone of this repository at the perf tooling ref (for now
   `wip/perf-tracking`, later `main`), owned by root and read-only to the perf user, for example
   `/opt/concrete-perf/tool`. Update it only while holding the runner's lock:
   `flock <PERF_HOME>/runner.lock git -C /opt/concrete-perf/tool checkout --detach origin/<ref>`.
3. **Working directories**, owned by the perf user (`dev` on concrete-ci-2), under
   `PERF_HOME=/home/dev/perf`: `repo/` (a clone of origin), `data/` (a worktree of `repo/` on branch
   `perf-data`: `git -C repo worktree add --orphan -b perf-data ../data` when origin has no
   `perf-data` yet, else `worktree add -B perf-data ../data origin/perf-data`), then
   `python3 <tool>/scripts/perf/store.py init --data $PERF_HOME/data`.
4. **Service** `concrete-perf.service`:

   ```ini
   [Unit]
   Description=Concrete compiler perf tracking (one cycle for the newest origin/main commit)
   After=network-online.target nix-daemon.service
   Wants=network-online.target

   [Service]
   Type=oneshot
   User=dev
   Environment=PERF_HOME=/home/dev/perf
   Environment=PERF_MACHINE=concrete-ci-2
   Environment=PATH=/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin
   ExecStart=/opt/concrete-perf/tool/scripts/perf/runner.sh
   TimeoutStartSec=8h
   KillMode=control-group
   Nice=10
   ```

5. **Timer** `concrete-perf.timer`:

   ```ini
   [Unit]
   Description=Run concrete-perf.service 10 minutes after the previous cycle

   [Timer]
   OnBootSec=15min
   OnUnitInactiveSec=10min

   [Install]
   WantedBy=timers.target
   ```

6. **Optional**: `PERF_DATA_PUSH=<remote>` in the service environment, with a credential for that
   remote kept outside `PERF_HOME`, to publish the `perf-data` branch. Without it the branch stays
   on the server. `scripts/land/notify.sh` (when present in the harness) receives regression alerts.

Operate: `systemctl enable --now concrete-perf.timer` (start), `systemctl disable --now
concrete-perf.timer` (stop; a running cycle finishes), `systemctl stop concrete-perf.service`
(abort a cycle), `journalctl -u concrete-perf.service` and `$PERF_HOME/out/logs/<sha12>.log`
(logs), `$PERF_HOME/out/summary.json` and `report.html` (status page inputs).
