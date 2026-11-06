#!/usr/bin/env bash
# Install the compiler perf tracker on the CI server (run as an admin with sudo, e.g. ubuntu@).
#
#   install-perf.sh [--user dev] [--machine concrete-ci-1] [--tool-ref wip/perf-tracking]
#                   [--repo-url URL] [--tool-dir /opt/concrete-perf/tool] [--no-timer] [--update-tool]
#
# Creates:
#   --tool-dir   root-owned clone at --tool-ref: the harness (runner.sh, suite, detection). The perf
#                user can read and run it but not modify it.
#   ~<user>/perf/{repo,data,out,work}   (runner.sh documents the layout), with the `perf-data`
#                branch worktree in data/ (from origin's perf-data when it exists, else a new orphan)
#   concrete-perf.service  oneshot, User=<user>, runs <tool-dir>/scripts/perf/runner.sh
#   concrete-perf.timer    the next cycle 10 minutes after the previous one ends (OnUnitInactiveSec)
# The timer is enabled unless --no-timer. --update-tool only moves the harness to the newest
# --tool-ref, holding the runner's lock so it never changes under a running cycle.
#
# Operate:
#   stop      sudo systemctl disable --now concrete-perf.timer      (a running cycle finishes)
#   abort     sudo systemctl stop concrete-perf.service
#   start     sudo systemctl enable --now concrete-perf.timer
#   one cycle sudo systemctl start --no-block concrete-perf.service
#   logs      journalctl -u concrete-perf.service ; ~<user>/perf/out/logs/<sha12>.log
#   report    sudo -u <user> python3 <tool-dir>/scripts/perf/report.py \
#               --records ~<user>/perf/data/data/<machine>/records.jsonl
set -euo pipefail

USER_NAME=dev
MACHINE=concrete-ci-1
TOOL_REF=wip/perf-tracking
REPO_URL=https://github.com/unbalancedparentheses/concrete2
TOOL_DIR=/opt/concrete-perf/tool
TIMER=1
UPDATE_ONLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --user) USER_NAME="$2"; shift 2 ;;
    --machine) MACHINE="$2"; shift 2 ;;
    --tool-ref) TOOL_REF="$2"; shift 2 ;;
    --repo-url) REPO_URL="$2"; shift 2 ;;
    --tool-dir) TOOL_DIR="$2"; shift 2 ;;
    --no-timer) TIMER=0; shift ;;
    --update-tool) UPDATE_ONLY=1; shift ;;
    *) echo "install-perf: unknown argument $1" >&2; exit 2 ;;
  esac
done
case "$USER_NAME$MACHINE$TOOL_REF$TOOL_DIR" in *[!A-Za-z0-9._/-]*) echo "install-perf: unsafe user/machine/ref/dir" >&2; exit 2 ;; esac
[ "$(id -u)" = 0 ] || { echo "install-perf: run with sudo" >&2; exit 2; }
id "$USER_NAME" >/dev/null 2>&1 || { echo "install-perf: no user $USER_NAME" >&2; exit 2; }
HOME_DIR="$(getent passwd "$USER_NAME" | cut -d: -f6)"
PH="$HOME_DIR/perf"
as_user() { sudo -u "$USER_NAME" -H env PATH="/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin" "$@"; }

for bin in git python3 flock /usr/bin/time; do
  command -v "$bin" >/dev/null || { echo "install-perf: $bin missing (apt install git python3 util-linux time)" >&2; exit 2; }
done

as_user mkdir -p "$PH/out/logs" "$PH/work"
as_user touch "$PH/runner.lock"

# --- the harness: root-owned, updated only while holding the runner's lock ----------------------
mkdir -p "$(dirname "$TOOL_DIR")"
if [ ! -d "$TOOL_DIR/.git" ]; then
  git clone -q "$REPO_URL" "$TOOL_DIR"
fi
update_tool() {
  git -C "$TOOL_DIR" fetch -q origin "+refs/heads/$TOOL_REF:refs/remotes/origin/$TOOL_REF"
  git -C "$TOOL_DIR" checkout -q --force --detach "origin/$TOOL_REF"
  git -C "$TOOL_DIR" clean -qfdx
  chown -R root:root "$TOOL_DIR"
  chmod -R go-w "$TOOL_DIR"
  chmod 755 "$(dirname "$TOOL_DIR")" "$TOOL_DIR"
}
# shellcheck disable=SC2094
( flock -w 28800 9 || { echo "install-perf: a perf cycle held the lock for 8 h" >&2; exit 1; }; update_tool ) 9<"$PH/runner.lock"
echo "harness at $(git -C "$TOOL_DIR" rev-parse --short HEAD) ($TOOL_REF) in $TOOL_DIR"
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
as_user python3 "$TOOL_DIR/scripts/perf/store.py" init --data "$PH/data"

cat > /etc/systemd/system/concrete-perf.service <<EOF
[Unit]
Description=Concrete compiler perf tracking (one cycle for the newest origin/main commit)
After=network-online.target nix-daemon.service
Wants=network-online.target

[Service]
Type=oneshot
User=$USER_NAME
Environment=PERF_HOME=$PH
Environment=PERF_MACHINE=$MACHINE
Environment=PATH=/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin
ExecStart=$TOOL_DIR/scripts/perf/runner.sh
TimeoutStartSec=8h
KillMode=control-group
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
