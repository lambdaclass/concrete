#!/usr/bin/env bash
# One continuous-profiling cycle (R-0551): profile the oldest landed main commit that has no
# profile yet, compare it with the previous profiled commit, record a finding, publish. Meant to be
# started periodically by the host's scheduler (the unit and timer live in the owner's server
# provisioning, outside this repository). Safe to run by hand.
#
# LAYOUT
#   the harness: the checkout this script is in (keep it read-only to the user that runs it, so a
#                profiled commit cannot rewrite the code that judges it)
#   $LP_HOME (default ~/leanprof-profile):
#     repo/                 dedicated clone of origin, detached at the commit being profiled
#     history/<sha>/        one commit: profile.json, summary.md, raw/, and against its baseline
#                           comparison.json, comparison.md; or failed.json after 3 attempts
#     attempts/<sha>        attempts so far for a commit that has not completed
#     findings.jsonl        one line per finding: sha, base, workload, kind, name, shares
#     out/index.md          the last 30 profiled commits, newest first; out/latest -> history/<sha>
#     logs/<sha12>.log      each attempt's output
#
# WHICH COMMITS. Landed commits are origin/main's first-parent chain. A cycle takes the oldest one
# after the newest profiled commit, at most $LP_BACKLOG (10) back, so every landing gets a profile
# when they arrive slower than a cycle and the job catches up after a pause; the first cycle on an
# empty history profiles origin/main only. The baseline is the newest profiled ancestor on that
# chain whose profile has the same mode.
#
# ENVIRONMENT: LEANPROF (required: the leanprof binary), LP_HOME, LP_MODE (auto, see
# leanprof_profile.sh), LP_BRANCH (main), LP_BACKLOG (10), LP_NICE (10), LP_REMOTE_URL (origin of
# repo/ on first use, default the harness's origin), and LP_PROFILE_ARGS (extra arguments for
# leanprof_profile.sh, e.g. "--cores 0-7 --scaling-corpus DIR").
# Notifications go through the harness's scripts/land/notify.sh when present.
# EXIT: 0 done or nothing to do; 1 harness error.
set -euo pipefail
HARNESS="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
LP_HOME="${LP_HOME:-$HOME/leanprof-profile}"
MODE="${LP_MODE:-auto}" BRANCH="${LP_BRANCH:-main}" BACKLOG="${LP_BACKLOG:-10}"
REPO="$LP_HOME/repo" HIST="$LP_HOME/history" OUT="$LP_HOME/out"
ts() { date -u +%Y-%m-%dT%H:%M:%SZ; }
log() { echo "[leanprof-runner $(ts)] $*"; }
[ -n "${LEANPROF:-}" ] && [ -x "$LEANPROF" ] || { log "LEANPROF must name the leanprof binary"; exit 1; }
mkdir -p "$HIST" "$OUT" "$LP_HOME/logs" "$LP_HOME/attempts"
exec 9>"$LP_HOME/runner.lock"
flock -n 9 || { log "another cycle holds the lock"; exit 0; }
NIX=(nix --extra-experimental-features 'nix-command flakes')

if ! [ -d "$REPO/.git" ]; then
  url="${LP_REMOTE_URL:-$(git -C "$HARNESS" remote get-url origin)}"
  git clone -q "$url" "$REPO"
fi
git -C "$REPO" fetch -q origin "+refs/heads/$BRANCH:refs/remotes/origin/$BRANCH"
done_for() { [ -f "$HIST/$1/profile.json" ] || [ -f "$HIST/$1/failed.json" ]; }
profiled() { [ -f "$HIST/$1/profile.json" ]; }

# The queue: first-parent commits newer than the newest done one, oldest first.
mapfile -t CHAIN < <(git -C "$REPO" rev-list --first-parent -n "$BACKLOG" "origin/$BRANCH")
TODO=""
if [ -z "$(ls -A "$HIST")" ]; then
  TODO="${CHAIN[0]}"
else
  for c in "${CHAIN[@]}"; do done_for "$c" && break; TODO="$c"; done
fi

publish() {
  python3 - "$HIST" "$OUT/index.md" "$LP_HOME/findings.jsonl" "$REPO" <<'PY'
import json, os, subprocess, sys
hist, out, fpath, repo = sys.argv[1:]
rows = []
for sha in os.listdir(hist):
    d = os.path.join(hist, sha)
    p, f = os.path.join(d, "profile.json"), os.path.join(d, "failed.json")
    if os.path.exists(p):
        prof = json.load(open(p))
        when, ws = prof.get("date", ""), prof["workloads"]
        status = ", ".join("%s %s" % (w["name"], w["status"]) for w in ws if w["status"] != "ok") or "ok"
        cmp_ = os.path.join(d, "comparison.json")
        if os.path.exists(cmp_):
            c = json.load(open(cmp_))
            vs = "%d finding(s) vs %s" % (len(c["findings"]), (c.get("base_sha") or "?")[:12])
        else:
            vs = "no baseline"
    elif os.path.exists(f):
        fl = json.load(open(f)); when, status, vs = fl.get("at", ""), "FAILED: " + fl.get("detail", ""), "-"
    else:
        continue
    rows.append((when, sha, status, vs))
rows.sort(reverse=True)
L = ["# Compiler profiles (leanprof, R-0551)", "",
     "Newest first. Each commit's `summary.md` and `comparison.md` are in `history/<sha>/`.", "",
     "| commit | profiled | workloads | comparison |", "|---|---|---|---|"]
for when, sha, status, vs in rows[:30]:
    L.append("| %s | %s | %s | %s |" % (sha[:12], when, status, vs))
tmp = out + ".tmp"
open(tmp, "w").write("\n".join(L) + "\n")
os.replace(tmp, out)
PY
  local c
  for c in $(git -C "$REPO" rev-list --first-parent -n 200 "origin/$BRANCH"); do
    profiled "$c" && { ln -sfn "$HIST/$c" "$OUT/latest"; break; }
  done
}

if [ -z "$TODO" ]; then publish; log "nothing to profile (origin/$BRANCH ${CHAIN[0]:0:12} done)"; exit 0; fi
S12="${TODO:0:12}"
LOG="$LP_HOME/logs/$S12.log"
n=$(( $(cat "$LP_HOME/attempts/$TODO" 2>/dev/null || echo 0) + 1 ))
echo "$n" > "$LP_HOME/attempts/$TODO"

BASE=""
for c in $(git -C "$REPO" rev-list --first-parent -n 200 "$TODO^" 2>/dev/null); do
  if profiled "$c" && [ "$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["mode"])' "$HIST/$c/profile.json")" = "$MODE" ]; then
    BASE="$HIST/$c/profile.json"; break
  fi
done

log "profiling $S12 (attempt $n, mode $MODE, baseline ${BASE:+$(basename "$(dirname "$BASE")" | cut -c1-12)}${BASE:-none})"
git -C "$REPO" checkout -q --force --detach "$TODO"
git -C "$REPO" clean -q -fdx -e .lake
rm -rf "$HIST/$TODO.tmp"
# shellcheck disable=SC2206  # LP_PROFILE_ARGS is a list of words by design
EXTRA=(${LP_PROFILE_ARGS:-})
set +e
nice -n "${LP_NICE:-10}" "${NIX[@]}" develop "$REPO" --command bash -c '
  set -e; cd "$1"; shift; lake build >/dev/null; exec "$@"' _ "$REPO" \
  "$HARNESS/scripts/perf/leanprof_profile.sh" profile --source "$REPO" --mode "$MODE" --leanprof "$LEANPROF" \
  --sha "$TODO" --out "$HIST/$TODO.tmp" ${BASE:+--baseline "$BASE"} "${EXTRA[@]}" > "$LOG" 2>&1
rc=$?
set -e
NOTIFY=""
[ -x "$HARNESS/scripts/land/notify.sh" ] && NOTIFY="$HARNESS/scripts/land/notify.sh"
notify() { if [ -n "$NOTIFY" ]; then "$NOTIFY" "$1" "$2" "$3" || true; else log "$2: $3"; fi; }

if [ "$rc" -le 1 ] || [ "$rc" = 3 ]; then
  [ -f "$HIST/$TODO.tmp/profile.json" ] || { log "exit $rc without profile.json"; rc=2; }
fi
if [ "$rc" -le 1 ] || [ "$rc" = 3 ]; then
  rm -rf "${HIST:?}/$TODO"; mv "$HIST/$TODO.tmp" "$HIST/$TODO"; rm -f "$LP_HOME/attempts/$TODO"
  if [ -f "$HIST/$TODO/comparison.json" ]; then
    python3 - "$HIST/$TODO/comparison.json" "$TODO" "$LP_HOME/findings.jsonl" <<'PY'
import json, sys
c = json.load(open(sys.argv[1]))
with open(sys.argv[3], "a") as f:
    for x in c["findings"]:
        f.write(json.dumps(dict(x, sha=sys.argv[2], base=c.get("base_sha")), sort_keys=True) + "\n")
PY
  fi
  if [ "$rc" = 1 ]; then
    notify warn "profile finding $S12" "$(grep '^FINDING' "$LOG" | head -10)
$HIST/$TODO/comparison.md"
  fi
  [ "$rc" != 3 ] || notify warn "profile incomplete $S12" "a workload failed or timed out; $HIST/$TODO/summary.md"
  log "$S12 profiled (exit $rc)"
else
  rm -rf "$HIST/$TODO.tmp"
  log "$S12: leanprof_profile.sh exit $rc (attempt $n); log $LOG"
  if [ "$n" -ge 3 ]; then
    python3 -c 'import json,sys; json.dump({"at":sys.argv[1],"detail":sys.argv[2]}, open(sys.argv[3],"w"))' \
      "$(ts)" "exit $rc after $n attempts; $(tail -1 "$LOG" | cut -c1-200)" "$HIST/$TODO.failed.json"
    mkdir -p "$HIST/$TODO"; mv "$HIST/$TODO.failed.json" "$HIST/$TODO/failed.json"; rm -f "$LP_HOME/attempts/$TODO"
    notify error "profile failed $S12" "3 attempts; $LOG"
  fi
fi
publish
