#!/usr/bin/env bash
# One perf-tracking cycle: measure the newest origin/main commit if it is not recorded yet.
# Started by concrete-perf.timer (install-perf.sh); safe to run by hand as the perf user.
#
# LAYOUT ($PERF_HOME, default ~/perf):
#   tool/  clone at the perf tooling ref; THIS script and the suite run from here, so a measured
#          commit can never change the harness that measures it
#   repo/  dedicated clone of origin; checked out detached at the commit being measured
#   data/  worktree of repo/ on the `perf-data` branch (records, summaries, bisect results)
#   out/   summary.json and report.html for the status page, last-run.json, logs/
#
# COORDINATION with the other work on the machine (CI runners gha1..6, remote gate runs as dev,
# the fuzz farm):
#   1. flock on $PERF_HOME/runner.lock: one perf cycle at a time.
#   2. The machine-wide heavy-run slot (scripts/tests/lib/heavy_slots.sh, R-0543) when the measured
#      tree has it: acquired with --no-wait, so a busy machine just means "try at the next tick".
#      The semaphore is per OS user, which is why the runner runs as `dev`, the user that remote gate
#      runs (concrete-remote) use.
#   3. `suite.py quiet`: wait (up to PERF_QUIET_WAIT seconds) until no GitHub Actions job
#      (Runner.Worker), no gate runner/test suite/lake build outside this process tree, and at most
#      nproc/8 foreign runnable non-idle tasks are seen for three consecutive samples. Idle-priority
#      tasks (nice 19 or SCHED_IDLE: the fuzz farm) do not block a run, but are recorded.
#   4. During the run a sampler re-checks the same conditions; a record measured while foreign heavy
#      work appeared is stored with contaminated=true, never used as a baseline, never alarmed on.
#   The suite itself runs at normal priority. Its cold `lake build` and run_tests.sh use every core;
#   a CI job that starts in the middle shares the machine with it for those minutes.
#
# EXIT: 0 measured / nothing to do / machine busy (retry next tick); nonzero only on harness errors.
set -euo pipefail

PERF_HOME="${PERF_HOME:-$HOME/perf}"
MACHINE="${PERF_MACHINE:-$(hostname -s)}"
QUIET_WAIT="${PERF_QUIET_WAIT:-1800}"
BRANCH="${PERF_MAIN_BRANCH:-main}"
TOOL="$PERF_HOME/tool"
REPO="$PERF_HOME/repo"
DATA="$PERF_HOME/data"
OUT="$PERF_HOME/out"
PERF="$TOOL/scripts/perf"
mkdir -p "$OUT/logs"
ts() { date -u +%Y-%m-%dT%H:%M:%SZ; }
log() { echo "[perf-runner $(ts)] $*"; }
state() { # status detail
  python3 -c 'import json,sys; json.dump({"at":sys.argv[1],"status":sys.argv[2],"detail":sys.argv[3]}, open(sys.argv[4],"w"))' \
    "$(ts)" "$1" "$2" "$OUT/last-run.json"
}

exec 9>"$PERF_HOME/runner.lock"
if ! flock -n 9; then log "another perf cycle holds the lock"; exit 0; fi

git -C "$REPO" fetch -q origin "+refs/heads/$BRANCH:refs/remotes/origin/$BRANCH"
HEAD_SHA="$(git -C "$REPO" rev-parse "origin/$BRANCH")"
if python3 "$PERF/store.py" has --data "$DATA" --machine "$MACHINE" --sha "$HEAD_SHA"; then
  log "$HEAD_SHA already measured"; state idle "$HEAD_SHA already measured"; exit 0
fi
log "measuring $HEAD_SHA on $MACHINE"
git -C "$REPO" checkout -q --force --detach "$HEAD_SHA"
git -C "$REPO" clean -qfdx -e .lake

# --- coordination -------------------------------------------------------------------------------
HEAVY_LIB="$REPO/scripts/tests/lib/heavy_slots.sh"
if [ -f "$HEAVY_LIB" ]; then
  # shellcheck source=/dev/null
  source "$HEAVY_LIB"
  if ! heavy_slot_acquire "perf-suite $HEAD_SHA" --no-wait; then
    log "no machine-wide heavy slot free; retry at the next tick"; state busy "heavy slots taken"; exit 0
  fi
  trap 'heavy_slot_release || true' EXIT
fi
if ! python3 "$PERF/suite.py" quiet --wait "$QUIET_WAIT"; then
  log "machine stayed busy for ${QUIET_WAIT}s; retry at the next tick"; state busy "machine busy"; exit 0
fi

# --- measure ------------------------------------------------------------------------------------
REC="$OUT/record-${HEAD_SHA:0:12}.json"
RUNLOG="$OUT/logs/${HEAD_SHA:0:12}.log"
set +e
nix develop "$REPO" -c python3 "$PERF/suite.py" run --repo "$REPO" --machine "$MACHINE" --out "$REC" >"$RUNLOG" 2>&1
rc=$?
set -e
if [ ! -s "$REC" ]; then
  log "suite produced no record (exit $rc); see $RUNLOG"; state error "suite exit $rc, no record"; exit 1
fi
python3 "$PERF/store.py" append --data "$DATA" --record "$REC"
RECS="$DATA/data/$MACHINE/records.jsonl"

# --- detect -------------------------------------------------------------------------------------
SUMMARY="$OUT/summary.json"
set +e
python3 "$PERF/detect.py" --records "$RECS" --sha "$HEAD_SHA" --out "$SUMMARY"
drc=$?
set -e
if [ "$drc" -eq 1 ]; then
  # Bisect the worst regression (largest ratio) between its last good record and HEAD.
  read -r METRIC GOOD < <(python3 - "$SUMMARY" <<'PY'
import json, sys
s = json.load(open(sys.argv[1]))
regs = sorted(s["regressions"], key=lambda n: -s["metrics"][n].get("ratio", 0))
for n in regs:
    if s["metrics"][n].get("good"):
        print(n, s["metrics"][n]["good"]); break
else:
    print("- -")
PY
)
  BISECT_JSON=""
  if [ "$METRIC" != "-" ]; then
    BISECT_JSON="$OUT/bisect-${HEAD_SHA:0:12}.json"
    log "regression in $METRIC; bisecting ${GOOD:0:12}..${HEAD_SHA:0:12}"
    set +e
    nix develop "$REPO" -c python3 "$PERF/perf_bisect.py" --repo "$REPO" --metric "$METRIC" --good "$GOOD" \
      --bad "$HEAD_SHA" --machine "$MACHINE" --work "$PERF_HOME/work" --out "$BISECT_JSON" \
      >>"$RUNLOG" 2>&1
    set -e
    if [ -s "$BISECT_JSON" ]; then
      python3 "$PERF/store.py" put --data "$DATA" --machine "$MACHINE" --name "bisect/${HEAD_SHA:0:12}.json" --file "$BISECT_JSON"
      python3 - "$SUMMARY" "$BISECT_JSON" <<'PY'
import json, sys
s = json.load(open(sys.argv[1])); b = json.load(open(sys.argv[2]))
s["bisect"] = {k: b.get(k) for k in ("metric", "verdict", "culprit", "culprit_subject", "candidates",
                                      "good", "bad", "good_value", "bad_value")}
json.dump(s, open(sys.argv[1], "w"), indent=1, sort_keys=True)
PY
    fi
  fi
  MSG="$(python3 - "$SUMMARY" <<'PY'
import json, sys
s = json.load(open(sys.argv[1]))
lines = ["%s on %s: %d metric(s) slower" % (s["sha"][:12], s["machine"], len(s["regressions"]))]
for n in s["regressions"][:8]:
    e = s["metrics"][n]
    lines.append("%s %.4g -> %.4g %s (x%.2f, band %.0f%%)" % (n, e["baseline"], e["current"], e["unit"], e["ratio"], 100 * e["band"]))
b = s.get("bisect")
if b:
    lines.append("bisect %s: %s %s" % (b.get("metric"), b.get("verdict"),
                 ((b.get("culprit") or "")[:12] + " " + (b.get("culprit_subject") or "")).strip()))
print("\n".join(lines))
PY
)"
  NOTIFY=""
  for n in "$REPO/scripts/land/notify.sh" "$TOOL/scripts/land/notify.sh"; do
    [ -x "$n" ] && { NOTIFY="$n"; break; }
  done
  if [ -n "$NOTIFY" ]; then
    "$NOTIFY" warn "perf regression ${HEAD_SHA:0:12}" "$MSG" || true
  else
    log "no scripts/land/notify.sh; regression message:"; echo "$MSG"
  fi
fi
python3 "$PERF/store.py" put --data "$DATA" --machine "$MACHINE" --name summary.json --file "$SUMMARY"
python3 "$PERF/report.py" --records "$RECS" --summary "$SUMMARY" --html "$OUT/report.html" >"$OUT/report.txt" || true

if [ -n "${PERF_DATA_PUSH:-}" ]; then
  git -C "$DATA" push -q "$PERF_DATA_PUSH" perf-data:perf-data || log "push of perf-data to $PERF_DATA_PUSH failed"
fi
state measured "$HEAD_SHA verdict $(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["verdict"])' "$SUMMARY")"
rm -f "$REC"
log "done: $HEAD_SHA"
