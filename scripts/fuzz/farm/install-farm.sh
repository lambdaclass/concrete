#!/usr/bin/env bash
# install-farm.sh — install the fuzz farm on concrete-ci-1 (run as root, from a checkout).
#
#   sudo bash scripts/fuzz/farm/install-farm.sh [--smoke MINUTES | --production] \
#        [--seed-lake-from /home/dev/concrete2] [--no-start]
#
# Creates:
#   user `fuzz`        no sudo, own group only, home /home/fuzz mode 750 (gha*/dev cannot read)
#   /home/fuzz/farm    installed copy of scripts/fuzz/farm (the engine)
#   /home/fuzz/src     dedicated clone of origin/main (built by the track job)
#   /srv/fuzz-farm     world-readable status JSON for the status page (fuzz-status.json)
#   /etc/fuzz-farm.conf
#   systemd: fuzz.slice, fuzz-farm.service (campaign loop),
#            fuzz-farm-track.service + .timer (main tracking, rebuild, re-check, summary; 15 min)
#
# Resource policy (see README.md): the slice has CPUWeight=1, IOWeight=1, CPUQuota=4800%
# (75% of 64 threads), MemoryHigh=56G, MemoryMax=64G, no swap; services run SCHED_IDLE,
# Nice=19, IOSchedulingClass=idle; the loop pauses while non-farm CPU use > 8 cores or
# load1 > 48. Smoke mode overrides at runtime: WORKERS=4, CPUQuota=800%, a fixed window.
#
# Stop / start:
#   sudo systemctl stop fuzz-farm.service fuzz-farm-track.timer      # stop everything
#   sudo systemctl disable --now fuzz-farm.service fuzz-farm-track.timer
#   sudo systemctl enable --now fuzz-farm-track.timer fuzz-farm.service
set -euo pipefail

MODE=production SMOKE_MIN=10 SEED_LAKE="" START=1
while [ $# -gt 0 ]; do
  case "$1" in
    --smoke) MODE=smoke; SMOKE_MIN="${2:?minutes}"; shift 2 ;;
    --production) MODE=production; shift ;;
    --seed-lake-from) SEED_LAKE="${2:?dir}"; shift 2 ;;
    --no-start) START=0; shift ;;
    *) sed -n '2,25p' "$0"; exit 2 ;;
  esac
done
[ "$(id -u)" = 0 ] || { echo "run as root" >&2; exit 1; }
SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
H=/home/fuzz

# --- user
if ! id fuzz >/dev/null 2>&1; then
  useradd --create-home --home-dir "$H" --shell /bin/bash --user-group fuzz
fi
chmod 750 "$H"
for g in $(id -nG fuzz); do [ "$g" = fuzz ] || gpasswd -d fuzz "$g" >/dev/null; done
# nobody else may be in group fuzz (gha*/dev/agent must not read the farm)
for u in $(getent group fuzz | cut -d: -f4 | tr , ' '); do gpasswd -d "$u" fuzz >/dev/null; done

# --- engine + dirs
install -d -o fuzz -g fuzz -m 750 "$H/farm" "$H/state" "$H/findings" "$H/reports" "$H/snap" "$H/tmp"
install -o fuzz -g fuzz -m 755 "$SRC_DIR"/farm.sh "$SRC_DIR"/farm.py "$SRC_DIR"/predicate.sh \
  "$SRC_DIR"/promote.sh "$H/farm/"
[ -f "$SRC_DIR/README.md" ] && install -o fuzz -g fuzz -m 644 "$SRC_DIR/README.md" "$H/farm/"
install -d -o fuzz -g fuzz -m 755 /srv/fuzz-farm

# --- dedicated clone (optionally seeded with a built .lake to skip the cold build)
if [ ! -d "$H/src/.git" ]; then
  sudo -u fuzz git clone --quiet https://github.com/unbalancedparentheses/concrete2 "$H/src"
  if [ -n "$SEED_LAKE" ] && [ -d "$SEED_LAKE/.lake" ]; then
    cp -a "$SEED_LAKE/.lake" "$H/src/.lake" && chown -R fuzz:fuzz "$H/src/.lake"
  fi
fi

# --- config
if [ "$MODE" = smoke ]; then
  WORKERS=4; QUOTA=800%; SMOKE_SECONDS=$((SMOKE_MIN * 60))
else
  WORKERS=40; QUOTA=4800%; SMOKE_SECONDS=0
fi
cat > /etc/fuzz-farm.conf <<EOF
# fuzz farm config (written by install-farm.sh, mode=$MODE). Sourced by farm.sh.
WORKERS=$WORKERS
CAMPAIGN_SECONDS=900
MIN_SECONDS=900
GENERATORS=diff,lin,mut
BATCH=40
SMOKE_SECONDS=$SMOKE_SECONDS
PAUSE_OTHER_CORES=8
RESUME_OTHER_CORES=4
PAUSE_LOAD=48
FARM_PUBLIC=/srv/fuzz-farm
EOF
chmod 644 /etc/fuzz-farm.conf

# --- systemd units
cat > /etc/systemd/system/fuzz.slice <<'EOF'
[Unit]
Description=Fuzz farm slice (idle priority; must never slow CI or the gate host)

[Slice]
CPUWeight=1
StartupCPUWeight=1
IOWeight=1
CPUQuota=4800%
MemoryHigh=56G
MemoryMax=64G
MemorySwapMax=0
TasksMax=8192
EOF

common_service() {
  cat <<'EOF'
User=fuzz
Group=fuzz
Slice=fuzz.slice
WorkingDirectory=/home/fuzz
Environment=FARM_HOME=/home/fuzz
Environment=FARM_PUBLIC=/srv/fuzz-farm
Environment=PATH=/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin
Nice=19
CPUSchedulingPolicy=idle
IOSchedulingClass=idle
NoNewPrivileges=yes
PrivateTmp=yes
ProtectSystem=full
ProtectHome=read-only
ReadWritePaths=/home/fuzz /srv/fuzz-farm
EOF
}

{ cat <<'EOF'
[Unit]
Description=Concrete fuzz farm (campaign loop)
After=network-online.target
Wants=fuzz-farm-track.timer

[Service]
Type=simple
ExecStart=/home/fuzz/farm/farm.sh run
Restart=on-failure
RestartSec=120
KillMode=control-group
TimeoutStopSec=30
EOF
common_service
cat <<'EOF'

[Install]
WantedBy=multi-user.target
EOF
} > /etc/systemd/system/fuzz-farm.service

{ cat <<'EOF'
[Unit]
Description=Concrete fuzz farm: track origin/main, rebuild, re-check findings, summary

[Service]
Type=oneshot
ExecStart=/home/fuzz/farm/farm.sh track
TimeoutStartSec=3h
EOF
common_service
} > /etc/systemd/system/fuzz-farm-track.service

cat > /etc/systemd/system/fuzz-farm-track.timer <<'EOF'
[Unit]
Description=Fuzz farm main tracking every 15 minutes

[Timer]
OnBootSec=3min
OnUnitActiveSec=15min
Persistent=true

[Install]
WantedBy=timers.target
EOF

systemctl daemon-reload
if [ "$MODE" = smoke ]; then
  systemctl set-property --runtime fuzz.slice CPUQuota="$QUOTA"
else
  systemctl set-property --runtime fuzz.slice CPUQuota=4800% 2>/dev/null || true
fi
[ "$START" = 1 ] || { echo "installed (not started, mode=$MODE)"; exit 0; }
echo "building the first snapshot (fuzz-farm-track.service)..."
systemctl start fuzz-farm-track.service
systemctl enable --now fuzz-farm-track.timer
if [ "$MODE" = smoke ]; then
  systemctl start fuzz-farm.service      # exits by itself after SMOKE_SECONDS
  echo "smoke run started for ${SMOKE_MIN} min (WORKERS=$WORKERS, CPUQuota=$QUOTA)"
else
  systemctl enable --now fuzz-farm.service
  echo "production farm enabled and started (WORKERS=$WORKERS, CPUQuota=$QUOTA)"
fi
