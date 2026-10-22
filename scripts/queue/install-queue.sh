#!/usr/bin/env bash
# Install the work-queue dispatcher for the current user (run as `agent` on concrete-ci-1).
# docs/project/WORK_QUEUE.md is the manual.
#
#   install-queue.sh [<clone-url>]
#
# It installs, and enables NOTHING:
#   ~/.local/share/concrete-queue/bin     a copy of scripts/queue (the timer never runs a checkout
#                                         an agent can edit)
#   ~/.local/share/concrete-queue/hooks   pre-push = the push guard (wip/* and readiness notes only)
#   $QUEUE_REPO (~/queue/concrete2)       the queue's own clone: origin only, core.hooksPath = the
#                                         guard, automatic gc/maintenance off
#   ~/.config/concrete-queue/env          the configuration template (kept if present)
#   ~/.config/systemd/user/concrete-queue.{service,timer}
#
# The clone URL defaults to the origin of ~/concrete2. Re-running it updates the copy and the units.
# The timer is enabled by hand after Claude is logged in on every cswap slot; the commands are
# printed at the end. QUEUE_NO_SYSTEMCTL=1 skips `systemctl --user daemon-reload` (tests).
set -euo pipefail
SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SHARE="${XDG_DATA_HOME:-$HOME/.local/share}/concrete-queue"
CONFDIR="${XDG_CONFIG_HOME:-$HOME/.config}"
REPO="${QUEUE_REPO:-$HOME/queue/concrete2}"
UNITS="$CONFDIR/systemd/user"
die() { echo "install-queue: $*" >&2; exit 1; }

url="${1:-}"
if [ -z "$url" ] && [ -d "$HOME/concrete2/.git" ]; then
  url="$(git -C "$HOME/concrete2" remote get-url origin)"
fi

mkdir -p "$SHARE/bin" "$SHARE/hooks" "$CONFDIR/concrete-queue" "$UNITS"
for f in dispatch.sh dispatch.py build_queue.py run_agent.sh push_guard.sh; do
  [ -f "$SRC/$f" ] || die "missing $SRC/$f"
  install -m 0755 "$SRC/$f" "$SHARE/bin/$f"
done
install -m 0755 "$SRC/push_guard.sh" "$SHARE/hooks/pre-push"

if [ ! -d "$REPO/.git" ]; then
  [ -n "$url" ] || die "no clone at $REPO and no clone URL (pass one, or have ~/concrete2)"
  mkdir -p "$(dirname "$REPO")"
  git clone -q --origin origin "$url" "$REPO"
fi
for r in $(git -C "$REPO" remote); do
  [ "$r" = origin ] || die "$REPO has remote '$r'; the queue clone keeps origin only (remove it by hand)"
done
git -C "$REPO" config core.hooksPath "$SHARE/hooks"
git -C "$REPO" config gc.auto 0
git -C "$REPO" config maintenance.auto false

if [ ! -f "$CONFDIR/concrete-queue/env" ]; then
  cat > "$CONFDIR/concrete-queue/env" <<EOF
# concrete-queue configuration (sourced by dispatch.sh; the defaults are in dispatch.py).
QUEUE_REPO=$REPO
QUEUE_MAX_AGENTS=3
QUEUE_MAX_AGENT_HOURS=36
# QUEUE_MAX_DAILY_USD=0          # 0 = no cap; measured from each run's total_cost_usd
# QUEUE_MAX_DAILY_TOKENS=0       # 0 = no cap
# QUEUE_TASK_MAX_USD=            # per-run cap, passed as claude --max-budget-usd
QUEUE_SLOTS="1 2 3 4"
QUEUE_STUCK_MINUTES=60
QUEUE_MAX_ATTEMPTS=2
# QUEUE_FUZZ_DIR=\$HOME/fuzz-farm/findings
# QUEUE_STATUS_JSON=\$HOME/status/queue.json
# QUEUE_NOTIFY=                  # default: scripts/status/notify.sh in the source checkout
EOF
  chmod 600 "$CONFDIR/concrete-queue/env"
fi

cat > "$UNITS/concrete-queue.service" <<EOF
[Unit]
Description=Concrete work-queue dispatcher tick
After=network-online.target

[Service]
Type=oneshot
ExecStart=$SHARE/bin/dispatch.sh tick
# Agents are started detached and must outlive the tick that started them.
KillMode=process
TimeoutStartSec=15min
Nice=5
EOF
cat > "$UNITS/concrete-queue.timer" <<EOF
[Unit]
Description=Run the Concrete work-queue dispatcher every 5 minutes

[Timer]
OnBootSec=5min
OnUnitActiveSec=5min
Persistent=true

[Install]
WantedBy=timers.target
EOF

if [ "${QUEUE_NO_SYSTEMCTL:-0}" != 1 ] && command -v systemctl >/dev/null 2>&1; then
  systemctl --user daemon-reload 2>/dev/null || echo "install-queue: systemctl --user daemon-reload failed (no user session?); run it after logging in" >&2
fi

cat <<EOF
install-queue: installed, NOT enabled.
  code   $SHARE/bin   clone $REPO   config $CONFDIR/concrete-queue/env
  units  $UNITS/concrete-queue.{service,timer}
To enable, after logging Claude in on every slot (cswap list shows credentials for 1-4):
  sudo loginctl enable-linger $(id -un)        # once, as ubuntu: the timer runs without a login
  systemctl --user enable --now concrete-queue.timer
Pause: touch ~/.concrete-queue-paused      Stop agents too: echo kill > ~/.concrete-queue-paused
EOF
