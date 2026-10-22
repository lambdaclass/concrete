#!/usr/bin/env bash
# One perf-tracking cycle for the newest origin/main commit: measure it if it has no clean record
# yet, then detect, bisect a regression, notify, publish. Started by concrete-perf.timer
# (install-perf.sh); safe to run by hand as the perf user.
#
# LAYOUT
#   $PERF_TOOL (default: the checkout this script is in; /opt/concrete-perf/tool on the server)
#          the harness. On the server it is root-owned and read-only to the perf user, so neither
#          a measured commit nor another job running as that user can rewrite it.
#   $PERF_HOME (default ~/perf):
#     repo/  dedicated clone of origin; checked out detached at the commit being measured
#     data/  worktree of repo/ on the `perf-data` branch (records, processed list, summaries, bisects)
#     out/   summary.json, report.html, report.txt for the status page; last-run.json; logs/
#     work/  bisect scratch worktree
#
# TRUST. The measured code is origin/main, which CI and remote gate runs already execute as the
# same user; it can still write $PERF_HOME (and so could forge a record). The harness is protected
# (root-owned); the records are only as trustworthy as main. Pushing perf-data needs a credential
# this script never holds: PERF_DATA_PUSH names a remote whose credential lives outside $PERF_HOME.
#
# COORDINATION with the other work on the machine (CI runners gha1..6, remote gate runs as dev,
# the fuzz farm):
#   1. flock on $PERF_HOME/runner.lock: one perf cycle at a time. install-perf.sh --update-tool
#      takes the same lock, so the harness never changes under a running cycle.
#   2. The machine-wide heavy-run slot (scripts/tests/lib/heavy_slots.sh, R-0543) when the measured
#      tree has it: acquired with --no-wait, so a busy machine just means "try at the next tick".
#      The semaphore is per OS user, which is why the runner runs as `dev`, the user remote gate
#      runs (concrete-remote) use.
#   3. `suite.py quiet`: wait (up to PERF_QUIET_WAIT seconds) until no GitHub Actions job
#      (Runner.Worker), no gate runner/test suite/lake build outside this process tree, and at most
#      nproc/8 foreign runnable non-idle threads are seen for three consecutive samples.
#      Idle-priority threads (nice 19 or SCHED_IDLE: the fuzz farm) do not block a run; they are
#      recorded in host.foreign.idle_running_mean.
#   4. During the run a sampler re-checks every 2 s; foreign heavy work, a burst of foreign runnable
#      threads, or a sampling failure marks the record contaminated. A contaminated record is never
#      a baseline and never an alarm; the commit is re-measured at the next tick (3 attempts).
#   The suite runs at normal priority. Its cold `lake build` and run_tests.sh use every core; a CI
#   job that starts in the middle shares the machine for those minutes (and contaminates the record).
#
# STATES. A commit is MEASURED once it has a clean record (or 3 attempts), and PROCESSED once
# detection, any bisect and notification finished (data/<machine>/processed). A cycle interrupted
# between the two resumes at detection for that commit (`store.py pending`), even when main has moved
# on. A crashed bisect is retried at the next tick, at most 3 times. The perf-data push (when
# PERF_DATA_PUSH is set) is retried at every tick.
#
# EXIT: 0 done / nothing to do / machine busy (retry next tick); nonzero only on harness errors.
set -euo pipefail

PERF="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PERF_HOME="${PERF_HOME:-$HOME/perf}"
MACHINE="${PERF_MACHINE:-$(hostname -s)}"
QUIET_WAIT="${PERF_QUIET_WAIT:-1800}"
BRANCH="${PERF_MAIN_BRANCH:-main}"
REPO="$PERF_HOME/repo"
DATA="$PERF_HOME/data"
OUT="$PERF_HOME/out"
mkdir -p "$OUT/logs" "$PERF_HOME/work"
ts() { date -u +%Y-%m-%dT%H:%M:%SZ; }
log() { echo "[perf-runner $(ts)] $*"; }
state() { # status detail
  python3 -c 'import json,sys; json.dump({"at":sys.argv[1],"status":sys.argv[2],"detail":sys.argv[3]}, open(sys.argv[4],"w"))' \
    "$(ts)" "$1" "$2" "$OUT/last-run.json"
}
store() { python3 "$PERF/store.py" "$@"; }

exec 9>"$PERF_HOME/runner.lock"
if ! flock -n 9; then log "another perf cycle holds the lock"; exit 0; fi

# Publishing is retried at every tick, independent of processing, so one failed push is not lost.
push_data() {
  if [ -n "${PERF_DATA_PUSH:-}" ]; then
    git -C "$DATA" push -q "$PERF_DATA_PUSH" perf-data:perf-data || log "push of perf-data to $PERF_DATA_PUSH failed (retried next tick)"
  fi
}
push_data

# Coordination before any heavy step (measurement or bisect): the machine-wide heavy slot when the
# tree has one, then a quiet machine. Returns nonzero (and the cycle exits 0) when busy.
SLOT_HELD=0
coordinate() {
  local label="$1" lib="$REPO/scripts/tests/lib/heavy_slots.sh"
  if [ "$SLOT_HELD" = 0 ] && [ -f "$lib" ]; then
    # shellcheck source=/dev/null
    source "$lib"
    if ! heavy_slot_acquire "$label" --no-wait; then
      log "no machine-wide heavy slot free; retry at the next tick"; state busy "heavy slots taken"; return 1
    fi
    SLOT_HELD=1
    trap 'heavy_slot_release || true' EXIT
  fi
  if ! python3 "$PERF/suite.py" quiet --wait "$QUIET_WAIT"; then
    log "machine stayed busy for ${QUIET_WAIT}s; retry at the next tick"; state busy "machine busy"; return 1
  fi
}

git -C "$REPO" fetch -q origin "+refs/heads/$BRANCH:refs/remotes/origin/$BRANCH"
# A commit measured by an interrupted cycle is finished first, even if main has moved on since.
HEAD_SHA="$(store pending --data "$DATA" --machine "$MACHINE")"
[ -n "$HEAD_SHA" ] || HEAD_SHA="$(git -C "$REPO" rev-parse "origin/$BRANCH")"
S12="${HEAD_SHA:0:12}"
if store processed --data "$DATA" --machine "$MACHINE" --sha "$HEAD_SHA"; then
  log "$S12 already processed"; state idle "$S12 already processed"; exit 0
fi
RECS="$DATA/data/$MACHINE/records.jsonl"
RUNLOG="$OUT/logs/$S12.log"

# --- measure (skipped when a clean record, or the attempt limit, already exists) -----------------
if ! store has --data "$DATA" --machine "$MACHINE" --sha "$HEAD_SHA"; then
  log "measuring $S12 on $MACHINE"
  git -C "$REPO" checkout -q --force --detach "$HEAD_SHA"
  git -C "$REPO" clean -qfdx -e .lake

  coordinate "perf-suite $S12" || exit 0

  REC="$OUT/record-$S12.json"
  rm -f "$REC"
  set +e
  nix develop "$REPO" -c python3 "$PERF/suite.py" run --repo "$REPO" --machine "$MACHINE" --out "$REC" >>"$RUNLOG" 2>&1
  rc=$?
  set -e
  if [ ! -s "$REC" ]; then
    log "suite produced no record (exit $rc); see $RUNLOG"; state error "suite exit $rc, no record"; exit 1
  fi
  store append --data "$DATA" --record "$REC"
  rm -f "$REC"
fi

# --- detect -------------------------------------------------------------------------------------
SUMMARY="$OUT/summary.json"
NEW="$OUT/summary.new.json"
rm -f "$NEW"
set +e
python3 "$PERF/detect.py" --records "$RECS" --sha "$HEAD_SHA" --out "$NEW"
drc=$?
set -e
if [ "$drc" -ne 0 ] && [ "$drc" -ne 1 ]; then
  log "detect.py failed (exit $drc); nothing published"; state error "detect exit $drc"; exit 1
fi
python3 - "$NEW" "$HEAD_SHA" <<'PY' || { log "summary does not describe $S12; nothing published"; state error "summary sha mismatch"; exit 1; }
import json, sys
s = json.load(open(sys.argv[1]))
sys.exit(0 if s.get("sha") == sys.argv[2] and s.get("schema") == "concrete.perf.summary.v1" else 1)
PY
VERDICT="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["verdict"])' "$NEW")"
if [ "$VERDICT" = contaminated ] && ! store has --data "$DATA" --machine "$MACHINE" --sha "$HEAD_SHA"; then
  log "$S12 measurement contaminated; re-measuring at the next tick"; state retry "$S12 contaminated"; exit 0
fi

if [ "$drc" -eq 1 ]; then
  # Bisect the worst regression (largest ratio) that has a good end.
  read -r METRIC GOOD BAND < <(python3 - "$NEW" <<'PY'
import json, sys
s = json.load(open(sys.argv[1]))
regs = sorted(s["regressions"], key=lambda n: -s["metrics"][n].get("ratio", 0))
for n in regs:
    m = s["metrics"][n]
    if m.get("good"):
        print(n, m["good"], m["band"]); break
else:
    print("- - -")
PY
)
  if [ "$METRIC" != "-" ]; then
    BISECT_JSON="$OUT/bisect-$S12.json"
    ATTEMPTS_FILE="$OUT/bisect-$S12.attempts"
    ATTEMPTS="$(cat "$ATTEMPTS_FILE" 2>/dev/null || echo 0)"
    if [ ! -s "$BISECT_JSON" ]; then
      coordinate "perf-bisect $S12" || exit 0
      echo $((ATTEMPTS + 1)) > "$ATTEMPTS_FILE"
      log "regression in $METRIC; bisecting ${GOOD:0:12}..$S12 (attempt $((ATTEMPTS + 1)))"
      set +e
      nix develop "$REPO" -c python3 "$PERF/perf_bisect.py" --repo "$REPO" --metric "$METRIC" --good "$GOOD" \
        --bad "$HEAD_SHA" --band "$BAND" --machine "$MACHINE" --work "$PERF_HOME/work" --out "$BISECT_JSON.tmp" \
        >>"$RUNLOG" 2>&1
      brc=$?
      set -e
      # A bisect that crashed is retried at the next tick (3 attempts), not silently dropped.
      if [ "$brc" -eq 0 ] || [ "$brc" -eq 2 ]; then
        [ -s "$BISECT_JSON.tmp" ] && mv -f "$BISECT_JSON.tmp" "$BISECT_JSON"
      elif [ "$((ATTEMPTS + 1))" -lt 3 ]; then
        log "bisect failed (exit $brc); retrying at the next tick"; state retry "$S12 bisect exit $brc"; exit 0
      else
        log "bisect failed 3 times; reporting the regression without a culprit"
      fi
    fi
    if [ -s "$BISECT_JSON" ]; then
      store put --data "$DATA" --machine "$MACHINE" --name "bisect/$S12.json" --file "$BISECT_JSON"
      python3 - "$NEW" "$BISECT_JSON" <<'PY'
import json, sys
s = json.load(open(sys.argv[1])); b = json.load(open(sys.argv[2]))
s["bisect"] = {k: b.get(k) for k in ("metric", "verdict", "culprit", "culprit_subject", "candidates",
                                      "good", "bad", "good_value", "bad_value", "limit")}
json.dump(s, open(sys.argv[1], "w"), indent=1, sort_keys=True)
PY
    fi
  fi
  MSG="$(python3 - "$NEW" <<'PY'
import json, sys
s = json.load(open(sys.argv[1]))
lines = ["%s on %s: %d metric(s) slower" % (s["sha"][:12], s["machine"], len(s["regressions"]))]
for n in s["regressions"][:8]:
    e = s["metrics"][n]
    lines.append("%s %.4g -> %.4g %s (x%s, band %.0f%%)" % (n, e["baseline"], e["current"], e["unit"],
                 e.get("ratio", "?"), 100 * e["band"]))
b = s.get("bisect")
if b:
    lines.append("bisect %s: %s %s" % (b.get("metric"), b.get("verdict"),
                 ((b.get("culprit") or "")[:12] + " " + (b.get("culprit_subject") or "")).strip()))
print("\n".join(lines))
PY
)"
  NOTIFY=""
  for n in "$PERF/../land/notify.sh" "$REPO/scripts/land/notify.sh"; do
    [ -x "$n" ] && { NOTIFY="$n"; break; }
  done
  if [ -n "$NOTIFY" ]; then
    "$NOTIFY" warn "perf regression $S12" "$MSG" || true
  else
    log "no scripts/land/notify.sh; regression message:"; echo "$MSG"
  fi
fi

mv -f "$NEW" "$SUMMARY"
store put --data "$DATA" --machine "$MACHINE" --name summary.json --file "$SUMMARY"
python3 "$PERF/report.py" --records "$RECS" --summary "$SUMMARY" --html "$OUT/report.html" >"$OUT/report.txt" || true
store processed --data "$DATA" --machine "$MACHINE" --sha "$HEAD_SHA" --mark
push_data
state processed "$S12 verdict $VERDICT"
log "done: $S12 ($VERDICT)"
