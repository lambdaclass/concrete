#!/usr/bin/env bash
# Continuous fuzz farm driver (runs as the unprivileged `fuzz` user on concrete-ci-1).
#
#   farm.sh run      the campaign loop (systemd: fuzz-farm.service). Runs WORKERS parallel
#                    `farm.py campaign` workers for CAMPAIGN_SECONDS, writes the status JSON,
#                    repeats. A load monitor pauses the workers while gates/runners are busy.
#   farm.sh track    fetch origin/main; on a new SHA rebuild in the dedicated clone
#                    (`nix develop -c lake build`), publish a snapshot, re-check open findings
#                    against it, refresh the daily summary and status JSON
#                    (systemd: fuzz-farm-track.timer, every 15 min).
#   farm.sh monitor  the load monitor alone (run starts it itself).
#   farm.sh status   print the status JSON.
#
# Layout under FARM_HOME (default /home/fuzz):
#   src/                 dedicated clone of origin/main (build happens here, never in a gate tree)
#   snap/<sha>/          immutable snapshot: bin/concrete, gen/fuzz_*.py, scripts/land/notify.sh
#   current -> snap/<sha>
#   farm/                this engine (installed copy; tracks this script, not main)
#   findings/<sig>/      repro.con minimized.con meta.json log;  findings/findings.json index
#   state/               farm.json, drift.json, seeds.log, stats/<day>/, paused (flag)
#   reports/summary-<day>.md
#   FARM_PUBLIC/fuzz-status.json   (default /srv/fuzz-farm) the status-page contract
#
# Config: /etc/fuzz-farm.conf (sourced if present), else the defaults below.
set -uo pipefail

FARM_HOME="${FARM_HOME:-/home/fuzz}"
[ -r /etc/fuzz-farm.conf ] && . /etc/fuzz-farm.conf
WORKERS="${WORKERS:-40}"
CAMPAIGN_SECONDS="${CAMPAIGN_SECONDS:-900}"
MIN_SECONDS="${MIN_SECONDS:-900}"
GENERATORS="${GENERATORS:-diff,lin,mut}"
BATCH="${BATCH:-40}"
SMOKE_SECONDS="${SMOKE_SECONDS:-0}"          # >0: run exits after this many seconds
PAUSE_OTHER_CORES="${PAUSE_OTHER_CORES:-8}"  # pause when non-farm CPU use exceeds this
RESUME_OTHER_CORES="${RESUME_OTHER_CORES:-4}" # resume when below this for RESUME_SAMPLES
RESUME_SAMPLES="${RESUME_SAMPLES:-4}"
PAUSE_LOAD="${PAUSE_LOAD:-48}"               # or when the 1-min load average exceeds this
MONITOR_INTERVAL="${MONITOR_INTERVAL:-15}"
FARM_CGROUP="${FARM_CGROUP:-/sys/fs/cgroup/fuzz.slice}"
REPO_URL="${REPO_URL:-https://github.com/unbalancedparentheses/concrete2}"
BRANCH="${BRANCH:-main}"
KEEP_SNAPSHOTS="${KEEP_SNAPSHOTS:-3}"
export FARM_HOME FARM_PUBLIC="${FARM_PUBLIC:-/srv/fuzz-farm}"

FARM_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PY="python3 $FARM_DIR/farm.py"
STATE="$FARM_HOME/state"
mkdir -p "$STATE" "$FARM_HOME/snap" "$FARM_HOME/reports" "$FARM_PUBLIC" 2>/dev/null
log() { echo "[farm $(date -u +%H:%M:%S)] $*"; }

set_state() {  # set_state <state> [sha]
  python3 - "$STATE/farm.json" "$1" "${2:-}" <<'EOF'
import json, sys, datetime, os
p, st, sha = sys.argv[1:4]
try: d = json.load(open(p))
except Exception: d = {}
d["state"] = st; d["at"] = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
if sha: d["sha"] = sha
json.dump(d, open(p + ".tmp", "w"), indent=1); os.replace(p + ".tmp", p)
EOF
}

# ---------------------------------------------------------------- tracking / build

snapshot() {  # snapshot <sha>: copy the built compiler + generators out of the clone
  local sha="$1" s="$FARM_HOME/snap/$1" src="$FARM_HOME/src"
  rm -rf "$s.tmp"; mkdir -p "$s.tmp/bin" "$s.tmp/gen" "$s.tmp/scripts/land"
  cp "$src/.lake/build/bin/concrete" "$s.tmp/bin/" || return 1
  cp "$src/scripts/tests/fuzz_differential.py" "$src/scripts/tests/fuzz_linearity.py" "$s.tmp/gen/" || return 1
  [ -f "$src/scripts/land/notify.sh" ] && cp "$src/scripts/land/notify.sh" "$s.tmp/scripts/land/"
  echo "$sha" > "$s.tmp/SHA"
  rm -rf "$s"; mv "$s.tmp" "$s"
  ln -sfn "$s" "$FARM_HOME/current.tmp" && mv -T "$FARM_HOME/current.tmp" "$FARM_HOME/current"
  # prune old snapshots (never the current one)
  ls -1dt "$FARM_HOME"/snap/*/ 2>/dev/null | tail -n +"$((KEEP_SNAPSHOTS + 1))" | while read -r d; do
    [ "$(readlink -f "$d")" = "$(readlink -f "$FARM_HOME/current")" ] || rm -rf "$d"
  done
}

track() {
  exec 8>"$STATE/track.lock"
  flock -n 8 || { log "track already running"; return 0; }
  local src="$FARM_HOME/src"
  if [ ! -d "$src/.git" ]; then
    log "cloning $REPO_URL"
    git clone --quiet --branch "$BRANCH" "$REPO_URL" "$src" || { set_state clone-failed; return 1; }
  fi
  git -C "$src" fetch --quiet origin "$BRANCH" || { log "fetch failed"; return 1; }
  local new cur
  new="$(git -C "$src" rev-parse "origin/$BRANCH")"
  cur="$(cat "$FARM_HOME/current/SHA" 2>/dev/null || true)"
  if [ "$new" != "$cur" ]; then
    if [ "$(cat "$STATE/build_failed" 2>/dev/null)" = "$new" ]; then
      log "build of $new already failed; waiting for main to move"
    else
      log "main moved ${cur:0:12} -> ${new:0:12}; building"
      set_state building "$cur"
      git -C "$src" checkout --quiet --detach "$new"
      if (cd "$src" && nix develop -c lake build) > "$STATE/build.log" 2>&1 && snapshot "$new"; then
        rm -f "$STATE/build_failed"
        set_state running "$new"
        log "snapshot $new ready; re-checking open findings"
        $PY recheck --compiler "$FARM_HOME/current/bin/concrete" --sha "$new"
      else
        echo "$new" > "$STATE/build_failed"
        set_state build-failed "$cur"
        log "build FAILED at $new (see $STATE/build.log); fuzzing continues on ${cur:0:12}"
      fi
    fi
  fi
  $PY summary >/dev/null
  $PY status
}

# ---------------------------------------------------------------- load monitor

cpu_busy_usec() {  # total busy CPU time on the machine, in usec
  awk '/^cpu /{ b=$2+$3+$4+$7+$8+$9; printf "%d\n", b * 1000000 / 100 }' /proc/stat
}
farm_usec() { awk '/^usage_usec/{print $2}' "$FARM_CGROUP/cpu.stat" 2>/dev/null || echo 0; }

monitor() {
  local pb pf cb cf other load calm=0 t0 t1
  pb=$(cpu_busy_usec); pf=$(farm_usec); t0=$(date +%s%N)
  while :; do
    sleep "$MONITOR_INTERVAL"
    cb=$(cpu_busy_usec); cf=$(farm_usec); t1=$(date +%s%N)
    # cores used by everything that is NOT the farm over the last interval
    other=$(( ((cb - pb) - (cf - pf)) * 1000 / ((t1 - t0) / 1000) ))  # milli-cores
    pb=$cb; pf=$cf; t0=$t1
    load=$(awk '{print int($1)}' /proc/loadavg)
    echo "$(date -u +%FT%TZ) other_mcores=$other load1=$load paused=$([ -e "$STATE/paused" ] && echo 1 || echo 0)" >> "$STATE/monitor.log"
    if [ "$other" -gt $((PAUSE_OTHER_CORES * 1000)) ] || [ "$load" -gt "$PAUSE_LOAD" ]; then
      [ -e "$STATE/paused" ] || log "PAUSE: other=${other}m-cores load1=$load"
      echo "other_mcores=$other load1=$load at=$(date -u +%FT%TZ)" > "$STATE/paused"; calm=0
    elif [ -e "$STATE/paused" ] && [ "$other" -lt $((RESUME_OTHER_CORES * 1000)) ]; then
      calm=$((calm + 1))
      if [ "$calm" -ge "$RESUME_SAMPLES" ]; then rm -f "$STATE/paused"; log "RESUME: other=${other}m-cores"; calm=0; fi
    fi
    tail -n 20000 "$STATE/monitor.log" > "$STATE/monitor.log.tmp" 2>/dev/null && mv "$STATE/monitor.log.tmp" "$STATE/monitor.log"
  done
}

# ---------------------------------------------------------------- campaign loop

run() {
  local end=0 mon=""
  [ "$SMOKE_SECONDS" -gt 0 ] && end=$(( $(date +%s) + SMOKE_SECONDS ))
  [ -e "$FARM_HOME/current/bin/concrete" ] || track
  [ -e "$FARM_HOME/current/bin/concrete" ] || { log "no snapshot to fuzz with"; return 1; }
  rm -f "$STATE/paused"
  if [ -d "$FARM_CGROUP" ]; then monitor & mon=$!; else log "no $FARM_CGROUP: load monitor disabled"; fi
  trap '[ -n "$mon" ] && kill "$mon" 2>/dev/null; kill $(jobs -p) 2>/dev/null; set_state stopped; exit 0' INT TERM
  while :; do
    local snap sha secs="$CAMPAIGN_SECONDS" pids=()
    snap="$(readlink -f "$FARM_HOME/current")"; sha="$(cat "$snap/SHA")"
    if [ "$end" -gt 0 ]; then
      secs=$(( end - $(date +%s) )); [ "$secs" -le 0 ] && break
      [ "$secs" -gt "$CAMPAIGN_SECONDS" ] && secs="$CAMPAIGN_SECONDS"
    fi
    set_state running "$sha"
    log "campaign: $WORKERS workers x ${secs}s at ${sha:0:12} ($GENERATORS)"
    for i in $(seq 0 $((WORKERS - 1))); do
      $PY campaign --compiler "$snap/bin/concrete" --gen-dir "$snap/gen" --sha "$sha" \
        --snapshot "$snap" --seconds "$secs" --worker-id "$i" --generators "$GENERATORS" \
        --batch "$BATCH" --min-seconds "$MIN_SECONDS" >> "$STATE/worker-$i.log" 2>&1 &
      pids+=($!)
    done
    wait "${pids[@]}"
    $PY status >/dev/null
    for i in $(seq 0 $((WORKERS - 1))); do
      tail -n 2000 "$STATE/worker-$i.log" > "$STATE/worker-$i.log.tmp" && mv "$STATE/worker-$i.log.tmp" "$STATE/worker-$i.log"
    done
  done
  [ -n "$mon" ] && kill "$mon" 2>/dev/null
  rm -f "$STATE/paused"
  $PY summary >/dev/null; $PY status >/dev/null
  set_state smoke-done
  log "smoke window over"
}

case "${1:-}" in
  run) run ;;
  track) track ;;
  monitor) monitor ;;
  status) $PY status --print ;;
  *) sed -n '2,25p' "$0"; exit 2 ;;
esac
