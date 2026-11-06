#!/usr/bin/env bash
# Landing report (ROADMAP R-0547) over the per-train logs that scripts/land/train.sh writes.
#
#   report.sh                  median time to land, and median duration per stage
#   report.sh --trains         also one line per train (built, CI dispatched, CI concluded, landed)
#   report.sh --export <dir>   copy every per-train log into <dir> (for a docs-only publication commit)
#
# Time to land = the landed row's time minus the train's FIRST successful build. CI wall clock =
# the watch verdict's time minus the dispatch time, queueing included. Only `passed` and
# `landing-green` rows count as a stage completing; failed, refused, cancelled and incomplete rows
# are counted and shown, never folded into a median. LAND_LOG_DIR overrides the log directory.
set -euo pipefail
ROOT="$(git rev-parse --show-toplevel)"
COMMON="$(cd "$ROOT" && cd "$(git rev-parse --git-common-dir)" && pwd)"
LOG_DIR="${LAND_LOG_DIR:-${LAND_STATE_DIR:-$COMMON/concrete-landing}/log}"

if [ "${1:-}" = "--export" ]; then
  out="${2:?usage: report.sh --export <dir>}"
  mkdir -p "$out"
  n=0
  for f in "$LOG_DIR"/*.tsv; do [ -f "$f" ] || continue; cp "$f" "$out/"; n=$((n+1)); done
  echo "report: exported $n train log(s) to $out"
  exit 0
fi
TRAINS=0; [ "${1:-}" = "--trains" ] && TRAINS=1

python3 -I - "$LOG_DIR" "$TRAINS" <<'PY'
import collections, datetime, glob, os, statistics, sys

log_dir, show_trains = sys.argv[1], sys.argv[2] == "1"
files = sorted(glob.glob(os.path.join(log_dir, "*.tsv")))
if not files:
    print(f"report: no landing logs in {log_dir}")
    sys.exit(0)

def ts(s):
    return datetime.datetime.strptime(s, "%Y-%m-%dT%H:%M:%SZ")

def fmt(sec):
    sec = int(sec)
    return f"{sec // 3600}h{(sec % 3600) // 60:02d}m" if sec >= 3600 else f"{sec // 60}m{sec % 60:02d}s"

OK = {"passed", "landing-green", "dispatched"}
stage_dur = collections.defaultdict(list)
stage_res = collections.defaultdict(collections.Counter)
to_land, ci_wall, rows_by_train = [], [], {}
for f in files:
    lines = open(f, encoding="utf-8").read().splitlines()
    if not lines or not lines[0].startswith("utc\ttrain\tstage"):
        print(f"report: {f}: not a landing log (header), skipped", file=sys.stderr)
        continue
    rows = [dict(zip(lines[0].split("\t"), ln.split("\t"))) for ln in lines[1:] if ln]
    if not rows:
        continue
    rows_by_train[rows[0]["train"]] = rows
    for r in rows:
        stage_res[r["stage"]][r["result"]] += 1
        if r["result"] in OK:
            stage_dur[r["stage"]].append(int(r["duration_s"]))
    built = [ts(r["utc"]) for r in rows if r["stage"] == "build" and r["result"] == "passed"]
    landed = [ts(r["utc"]) for r in rows if r["stage"] == "land" and r["result"] == "passed"]
    if built and landed:
        to_land.append((landed[-1] - built[0]).total_seconds())
    disp = [ts(r["utc"]) for r in rows if r["stage"] == "ci" and r["result"] == "dispatched"]
    verdict = [ts(r["utc"]) for r in rows if r["stage"] == "watch" and r["result"] in ("passed", "landing-green", "failed")]
    if disp and verdict and verdict[-1] >= disp[-1]:
        ci_wall.append((verdict[-1] - disp[-1]).total_seconds())

print(f"trains logged: {len(rows_by_train)}   landed: {len(to_land)}")
if to_land:
    print(f"median time to land (first build -> landed): {fmt(statistics.median(to_land))} over {len(to_land)} train(s)")
if ci_wall:
    print(f"median CI wall clock (dispatch -> verdict):  {fmt(statistics.median(ci_wall))} over {len(ci_wall)} run(s)")
print()
print(f"{'stage':<14}{'median':>9}  results")
for st in ["build", "check", "check-remote", "ci", "watch", "land", "bisect-step", "bisect"]:
    if st not in stage_res:
        continue
    med = fmt(statistics.median(stage_dur[st])) if stage_dur[st] else "-"
    res = ", ".join(f"{k}={v}" for k, v in sorted(stage_res[st].items()))
    print(f"{st:<14}{med:>9}  {res}")
if show_trains:
    print()
    for name, rows in rows_by_train.items():
        last = {r["stage"]: r for r in rows}
        land = last.get("land", {})
        print(f"{name}: head {last.get('build', {}).get('head', '-')[:10]} run {land.get('run_id') or last.get('ci', {}).get('run_id', '-')} "
              f"land={land.get('result', '-')} members={rows[-1].get('members', '-')}")
PY
