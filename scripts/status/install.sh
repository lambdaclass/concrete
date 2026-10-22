#!/usr/bin/env bash
# Install the status page as systemd USER units for the account that runs it (on the CI server: dev).
#
#   scripts/status/install.sh [--enable] [--port N] [--public] [--log-dir DIR]
#
# Writes, under ~/.config/systemd/user/:
#   concrete-status.service       oneshot: refresh a dedicated clone to origin/main, then run status.py
#   concrete-status.timer         every 2 minutes
#   concrete-status-http.service  python3 -m http.server bound to 127.0.0.1:<port> (default 8787),
#                                 serving ONLY ~/concrete-status/www (status.html + status.json)
#
# Without --enable it only writes the files and prints the commands that would enable them: nothing
# starts. The data clone (~/concrete-status/repo) is separate from any working clone, so the timer's
# fetches never contend with concrete-remote or a human's checkout. status.py is COPIED to
# ~/concrete-status/bin, so switching branches in any checkout cannot break the timer; re-run to update.
#
# Prerequisites (one time):
#   - gh authenticated for this user (`gh auth login`, or GH_TOKEN in ~/.config/concrete/status.env)
#     with a read-only token: Actions read + Metadata read; listing self-hosted runners additionally
#     needs Administration read. Without it, the runner row says "unavailable" and the rest renders.
#   - `sudo loginctl enable-linger <user>` so user units run without an open login session.
#   - The landing log lives in the git common dir of the clone that runs trains. If that is another
#     account, pass --log-dir with a directory this account can read (report.sh --export writes one).
#
# View it from a laptop:  ssh -N -L 8787:127.0.0.1:8787 dev@<server>   then http://localhost:8787/status.html
# --public additionally writes a sanitized ~/concrete-status/public/ (see docs/project/STATUS_PAGE.md).
set -euo pipefail

ENABLE=0 PORT=8787 PUBLIC=0 LOGDIR=""
while [ "$#" -gt 0 ]; do
  case "$1" in
    --enable) ENABLE=1 ;;
    --port) PORT="${2:?--port needs a value}"; shift ;;
    --public) PUBLIC=1 ;;
    --log-dir) LOGDIR="${2:?--log-dir needs a value}"; shift ;;
    *) echo "usage: install.sh [--enable] [--port N] [--public] [--log-dir DIR]" >&2; exit 2 ;;
  esac
  shift
done
case "$PORT" in ''|*[!0-9]*) echo "install: --port must be a number" >&2; exit 2 ;; esac

SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/status.py"
[ -f "$SRC" ] || { echo "install: $SRC not found" >&2; exit 1; }
BASE="$HOME/concrete-status"
UNIT_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"
REPO_URL="${STATUS_REPO_URL:-https://github.com/unbalancedparentheses/concrete2.git}"
mkdir -p "$BASE/www" "$BASE/bin" "$UNIT_DIR" "$HOME/.config/concrete"
chmod 750 "$BASE"
install -m 755 "$SRC" "$BASE/bin/status.py"

if [ ! -d "$BASE/repo/.git" ]; then
  git clone --quiet --filter=blob:none "$REPO_URL" "$BASE/repo"
  git -C "$BASE/repo" remote add lambdaclass https://github.com/lambdaclass/concrete.git
fi

PUBLIC_ARG=""
[ "$PUBLIC" = 1 ] && { mkdir -p "$BASE/public"; PUBLIC_ARG=" --public-out $BASE/public"; }
ENV_LINE=""
[ -n "$LOGDIR" ] && ENV_LINE="Environment=LAND_LOG_DIR=$LOGDIR"

cat > "$UNIT_DIR/concrete-status.service" <<EOF
[Unit]
Description=Concrete status page (regenerate)

[Service]
Type=oneshot
Nice=10
WorkingDirectory=$BASE/repo
Environment=STATUS_ROOT=$BASE/repo
$ENV_LINE
EnvironmentFile=-%h/.config/concrete/status.env
ExecStartPre=/usr/bin/git -C $BASE/repo fetch --quiet --prune origin
ExecStartPre=/usr/bin/git -C $BASE/repo checkout --quiet --detach origin/main
ExecStart=/usr/bin/python3 -I $BASE/bin/status.py --fetch --out $BASE/www$PUBLIC_ARG
TimeoutStartSec=300
EOF

cat > "$UNIT_DIR/concrete-status.timer" <<'EOF'
[Unit]
Description=Regenerate the Concrete status page every 2 minutes

[Timer]
OnBootSec=1min
OnUnitActiveSec=2min
AccuracySec=15s

[Install]
WantedBy=timers.target
EOF

cat > "$UNIT_DIR/concrete-status-http.service" <<EOF
[Unit]
Description=Concrete status page (localhost-only static server)

[Service]
ExecStart=/usr/bin/python3 -I -m http.server $PORT --bind 127.0.0.1 --directory $BASE/www
Restart=on-failure
NoNewPrivileges=true

[Install]
WantedBy=default.target
EOF

echo "install: wrote $UNIT_DIR/concrete-status.{service,timer} and concrete-status-http.service"
ENABLE_CMD="systemctl --user daemon-reload && systemctl --user enable --now concrete-status.timer concrete-status-http.service"
if [ "$ENABLE" = 1 ]; then
  systemctl --user daemon-reload
  systemctl --user enable --now concrete-status.timer concrete-status-http.service
  systemctl --user start concrete-status.service || true
  echo "install: enabled; first page in $BASE/www/status.html"
else
  echo "install: NOT enabled. To enable:"
  echo "  sudo loginctl enable-linger $(id -un)   # once, as an admin"
  echo "  $ENABLE_CMD"
fi
echo "view: ssh -N -L $PORT:127.0.0.1:$PORT $(id -un)@<server>, then http://localhost:$PORT/status.html"
