#!/usr/bin/env bash
# Install the compiler perf tracker on the CI server (run as an admin with sudo, e.g. ubuntu@).
#
#   install-perf.sh [--user dev] [--machine concrete-ci-1] [--tool-ref wip/perf-tracking]
#                   [--repo-url URL] [--no-timer] [--update-tool]
#
# Creates, as the perf user, $HOME/perf/{tool,repo,data,out,work} (see runner.sh for the layout),
# the `perf-data` branch worktree (from origin's perf-data when it exists, otherwise a new orphan
# branch), and two system units:
#   concrete-perf.service  oneshot, User=<user>, runs tool/scripts/perf/runner.sh
#   concrete-perf.timer    every 10 minutes after the previous cycle ends (OnUnitInactiveSec)
# The timer is enabled unless --no-timer. --update-tool only moves tool/ to the newest --tool-ref.
#
# Operate:
#   stop      sudo systemctl disable --now concrete-perf.timer      (a running cycle finishes)
#   abort     sudo systemctl stop concrete-perf.service
#   start     sudo systemctl enable --now concrete-perf.timer
#   one cycle sudo systemctl start --no-block concrete-perf.service
#   logs      journalctl -u concrete-perf.service ; ~<user>/perf/out/logs/<sha12>.log
#   report    sudo -u <user> python3 ~<user>/perf/tool/scripts/perf/report.py \
#               --records ~<user>/perf/data/data/<machine>/records.jsonl
set -euo pipefail

USER_NAME=dev
MACHINE=concrete-ci-1
TOOL_REF=wip/perf-tracking
REPO_URL=https://github.com/unbalancedparentheses/concrete2
TIMER=1
UPDATE_ONLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --user) USER_NAME="$2"; shift 2 ;;
    --machine) MACHINE="$2"; shift 2 ;;
    --tool-ref) TOOL_REF="$2"; shift 2 ;;
    --repo-url) REPO_URL="$2"; shift 2 ;;
    --no-timer) TIMER=0; shift ;;
    --update-tool) UPDATE_ONLY=1; shift ;;
    *) echo "install-perf: unknown argument $1" >&2; exit 2 ;;
  esac
done
case "$USER_NAME$MACHINE$TOOL_REF" in *[!A-Za-z0-9._/-]*) echo "install-perf: unsafe user/machine/ref" >&2; exit 2 ;; esac
id "$USER_NAME" >/dev/null 2>&1 || { echo "install-perf: no user $USER_NAME" >&2; exit 2; }
HOME_DIR="$(getent passwd "$USER_NAME" | cut -d: -f6)"
PH="$HOME_DIR/perf"
as_user() { sudo -u "$USER_NAME" -H env PATH="/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin" "$@"; }

for bin in git python3 flock /usr/bin/time; do
  command -v "$bin" >/dev/null || { echo "install-perf: $bin missing (apt install git python3 util-linux time)" >&2; exit 2; }
done

as_user mkdir -p "$PH/out/logs" "$PH/work"
if [ ! -d "$PH/tool/.git" ]; then
  as_user git clone -q "$REPO_URL" "$PH/tool"
fi
as_user git -C "$PH/tool" fetch -q origin "+refs/heads/$TOOL_REF:refs/remotes/origin/$TOOL_REF"
as_user git -C "$PH/tool" checkout -q --force --detach "origin/$TOOL_REF"
echo "tool at $(as_user git -C "$PH/tool" rev-parse --short HEAD) ($TOOL_REF)"
[ "$UPDATE_ONLY" = 1 ] && exit 0

if [ ! -d "$PH/repo/.git" ]; then
  as_user git clone -q "$REPO_URL" "$PH/repo"
fi
if [ ! -e "$PH/data/.git" ]; then
  if as_user git -C "$PH/repo" fetch -q origin "+refs/heads/perf-data:refs/remotes/origin/perf-data" 2>/dev/null; then
    as_user git -C "$PH/repo" worktree add -q -B perf-data "$PH/data" origin/perf-data
  else
    as_user git -C "$PH/repo" worktree add -q --orphan -b perf-data "$PH/data"
  fi
fi
as_user python3 "$PH/tool/scripts/perf/store.py" init --data "$PH/data"

cat > /etc/systemd/system/concrete-perf.service <<EOF
[Unit]
Description=Concrete compiler perf tracking (one cycle: measure newest origin/main if unrecorded)
After=network-online.target nix-daemon.service
Wants=network-online.target

[Service]
Type=oneshot
User=$USER_NAME
Environment=PERF_HOME=$PH
Environment=PERF_MACHINE=$MACHINE
Environment=PATH=/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin
ExecStart=$PH/tool/scripts/perf/runner.sh
TimeoutStartSec=8h
Nice=0
EOF
cat > /etc/systemd/system/concrete-perf.timer <<EOF
[Unit]
Description=Run concrete-perf.service 10 minutes after the previous cycle

[Timer]
OnBootSec=15min
OnUnitInactiveSec=10min

[Install]
WantedBy=timers.target
EOF
systemctl daemon-reload
if [ "$TIMER" = 1 ]; then
  systemctl enable --now concrete-perf.timer
  echo "timer enabled: $(systemctl is-active concrete-perf.timer)"
else
  systemctl disable --now concrete-perf.timer 2>/dev/null || true
  echo "timer installed but not enabled (start: sudo systemctl enable --now concrete-perf.timer)"
fi
