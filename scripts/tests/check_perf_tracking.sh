#!/usr/bin/env bash
# Perf-tracking harness gate (scripts/perf/).
#
# The tracker's job is to raise an alarm on a real slowdown and stay silent on noise; a tracker that
# cries wolf gets ignored and one that never fires is the status quo it replaces. This gate checks
# the decision logic with fixtures, never with timings, so it is fast and machine-independent:
#   1. fixture results: scripts/perf/fixtures/records.jsonl parses, every record validates, and
#      malformed records (bad sha, unknown unit, lo > median, cross-field errors) are refused;
#   2. noise-band detection: no alarm on commits that differ only by noise, an alarm on a planted
#      2x slowdown, none on a contaminated record, none without enough history, and a contaminated
#      outlier in the history does not move the baseline;
#   3. bisect selection: for every culprit position (with and without unbuildable commits) the
#      search names the culprit within ceil(log2(n))+2 probes, and the midpoint rule classifies;
#   4. the data-branch format: store.py init/append/has/put/check on a scratch git repo writes
#      data/<machine>/records.jsonl one line per commit, commits each write, refuses a duplicate
#      and a malformed record, and detect.py / report.py read what it wrote (HTML has no external
#      assets).
# Shell syntax of runner.sh and install-perf.sh is also checked (bash -n).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PERF="$ROOT_DIR/scripts/perf"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

for f in perflib.py suite.py store.py detect.py perf_bisect.py report.py runner.sh install-perf.sh suite.json fixtures/records.jsonl; do
  [ -f "$PERF/$f" ] || no "scripts/perf/$f missing"
done

echo "=== shell syntax ==="
for f in runner.sh install-perf.sh; do
  if bash -n "$PERF/$f" 2>"$TMP/syn"; then ok "$f parses"; else no "$f: $(cat "$TMP/syn")"; fi
done

echo "=== decision logic (fixtures, detection, bisect) ==="
python3 -I - "$PERF" <<'PY' >"$TMP/py.out" 2>&1
import json, math, random, sys, copy
sys.path.insert(0, sys.argv[1])
import perflib

fails = 0
def check(cond, msg):
    global fails
    print(("  ok   " if cond else "  FAIL ") + msg)
    if not cond:
        fails += 1

# 1. fixture results
recs = perflib.parse_records(open(sys.argv[1] + "/fixtures/records.jsonl").read())
check(len(recs) == 15, "fixture: 15 records parse and validate")
check(all(perflib.validate_record(r) == [] for r in recs), "fixture: every record validates")
check(len({r["sha"] for r in recs}) == 15, "fixture: one record per commit")
suite = json.load(open(sys.argv[1] + "/suite.json"))
check(isinstance(suite.get("suite_version"), int) and suite["repeats"] >= 3,
      "suite.json: integer suite_version and >= 3 repeats")
good = recs[0]
for mut, why in [
        (lambda r: r.update(sha="abc"), "short sha"),
        (lambda r: r.update(schema="v0"), "wrong schema"),
        (lambda r: r.update(machine=""), "empty machine"),
        (lambda r: r.update(machine="../x"), "path-unsafe machine"),
        (lambda r: r.update(status="great"), "unknown status"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].update(unit="parsecs"), "unknown unit"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].update(lo=10 ** 9), "lo > median"),
        (lambda r: r["metrics"].update({"nodots": {"unit": "ms", "median": 1, "lo": 1, "hi": 1, "n": 1}}), "bad metric name"),
        (lambda r: r.pop("suite_version"), "missing suite_version"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].update(n=-4, samples=["bad"]), "negative n, non-numeric samples"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].update(n=2.5), "fractional n"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].update(hi=float("inf")), "infinite hi"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].pop("samples"), "missing samples"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].update(median=r["metrics"]["compile.lox.wall_ms"]["lo"]),
         "median disagrees with samples"),
        (lambda r: r["metrics"]["compile.lox.wall_ms"].update(n=4), "n disagrees with the sample count")]:
    r = copy.deepcopy(good)
    mut(r)
    try:
        perflib.dump_record(r)
        check(False, "malformed record refused: " + why)
    except ValueError:
        check(True, "malformed record refused: " + why)
try:
    perflib.parse_records(json.dumps(good) + "\n{not json\n")
    check(False, "a non-JSON line is refused")
except ValueError as ex:
    check("line 2" in str(ex), "a non-JSON line is refused and named (line 2)")

# 2. noise-band detection on the fixture: commits 1..11 are noise, 12..14 plant a 2x lox slowdown,
#    commit 8 is a contaminated 2.8x outlier.
for i in range(3, 12):
    s = perflib.detect(recs[:i], recs[i])
    if recs[i].get("contaminated"):
        check(s["verdict"] == "contaminated" and not s["regressions"],
              "fixture commit %d: contaminated record is never an alarm" % i)
    else:
        check(s["regressions"] == [], "fixture commit %d: noise only, no alarm (%s)" % (i, s["regressions"]))
s = perflib.detect(recs[:12], recs[12])
check(s["regressions"] == ["compile.lox.wall_ms"], "fixture commit 12: planted 2x lox slowdown flagged, nothing else")
e = s["metrics"]["compile.lox.wall_ms"]
check(1.8 < e["ratio"] < 2.2, "fixture commit 12: ratio ~2 (%.3f)" % e["ratio"])
clean = [r for r in recs[:12] if not r["contaminated"]][-perflib.DEFAULTS["window"]:]
hist = [r["metrics"]["compile.lox.wall_ms"]["median"] for r in clean]
check(len(clean) == 10 and recs[8] not in clean, "baseline window: last 10 clean records, outlier excluded")
check(abs(e["baseline"] - perflib.median(hist)) < 1e-3, "baseline skips the contaminated outlier")
perflib.annotate_bisect_targets(recs[:12], recs[12], s)
check(s["metrics"]["compile.lox.wall_ms"]["good"] == recs[11]["sha"], "bisect good end = newest unregressed record")
s = perflib.detect(recs[:2], recs[2])
check(all(m["verdict"] == "insufficient_history" for m in s["metrics"].values()) and not s["regressions"],
      "two prior records: insufficient history, no verdict")

# Synthetic: many noise levels, many seeds. No false alarm at any noise level the band was
# measured from; a planted 2x always fires while the noise is under 10%.
def mk(sha_i, val, noise_sd, rnd, n=5, unit="ms", machine="m1", sv=1, contaminated=False):
    samples = [val * (1 + rnd.gauss(0, noise_sd)) for _ in range(n)]
    return {"schema": perflib.RECORD_SCHEMA, "suite_version": sv, "machine": machine,
            "sha": "%040x" % sha_i, "measured_at": "x", "status": "ok", "contaminated": contaminated,
            "metrics": {"compile.p.wall_ms": perflib.summarize(samples, unit)}}
false_alarms = misses = trials = 0
for noise in (0.005, 0.01, 0.02, 0.04, 0.08):
    for seed in range(40):
        rnd = random.Random(seed * 1000 + int(noise * 1000))
        hist = [mk(i, 1000.0, noise, rnd) for i in range(12)]
        same = mk(99, 1000.0, noise, rnd)
        slow = mk(100, 2000.0, noise, rnd)
        trials += 1
        if perflib.detect(hist, same)["regressions"]:
            false_alarms += 1
        if not perflib.detect(hist, slow)["regressions"]:
            misses += 1
check(false_alarms == 0, "noise sweep: 0 false alarms in %d noise-only trials (got %d)" % (trials, false_alarms))
check(misses == 0, "noise sweep: planted 2x detected in all %d trials (missed %d)" % (trials, misses))
rnd = random.Random(7)
hist = [mk(i, 1000.0, 0.01, rnd) for i in range(12)]
check(not perflib.detect(hist, mk(98, 1000.0 * 1.04, 0.01, rnd))["regressions"],
      "a 4% change inside a 1% noise floor + 5% threshold is not an alarm")
check(perflib.detect(hist, mk(97, 1000.0 * 1.25, 0.01, rnd))["regressions"] == ["compile.p.wall_ms"],
      "a 25% slowdown on a quiet metric is an alarm")
other = [mk(i, 1000.0, 0.01, rnd, machine="m2") for i in range(12)]
check(perflib.detect(other, mk(96, 2000.0, 0.01, rnd))["metrics"]["compile.p.wall_ms"]["verdict"] == "insufficient_history",
      "records from another machine are never a baseline")
oldv = [mk(i, 1000.0, 0.01, rnd, sv=0) for i in range(12)]
check(perflib.detect(oldv, mk(95, 2000.0, 0.01, rnd))["metrics"]["compile.p.wall_ms"]["verdict"] == "insufficient_history",
      "records from another suite_version are never a baseline")
tiny = [mk(i, 4.0, 0.01, rnd) for i in range(12)]
check(not perflib.detect(tiny, mk(94, 8.0, 0.01, rnd))["regressions"],
      "a 2x change of a 4 ms metric stays under the 25 ms absolute floor")
noisy = [mk(i, 1000.0, 0.0, rnd, n=1) for i in range(12)]
for k, r in enumerate(noisy):
    r["metrics"]["compile.p.wall_ms"] = perflib.summarize([1000.0 * (0.6 if k % 2 else 1.4)], "ms")
check(perflib.detect(noisy, mk(93, 2000.0, 0.0, rnd, n=1))["metrics"]["compile.p.wall_ms"]["verdict"] == "too_noisy",
      "a metric whose history spread exceeds max_band is reported too_noisy, never alarmed")

# Verdicts beyond regression.
base = [mk(i, 1000.0, 0.01, rnd) for i in range(6)]
bf = mk(90, 1000.0, 0.01, rnd); bf["status"] = "build_failed"; bf["metrics"] = {}
check(perflib.detect(base, bf)["verdict"] == "build_failed", "a build-failed record's verdict is build_failed, not ok")
gone = mk(91, 1000.0, 0.01, rnd); gone["metrics"] = {}
sg = perflib.detect(base, gone)
check(sg["verdict"] == "incomplete" and sg["missing"] == ["compile.p.wall_ms"],
      "a metric that disappeared makes the verdict incomplete and is named")
part = mk(92, 1000.0, 0.01, rnd); part["status"] = "partial"
check(perflib.detect(base, part)["verdict"] == "incomplete", "a partial record is incomplete, not ok")
zero = [mk(i, 0.0, 0.0, rnd) for i in range(6)]
check(perflib.detect(zero, mk(89, 100.0, 0.0, rnd))["regressions"] == ["compile.p.wall_ms"],
      "a 0 ms baseline that becomes 100 ms is a regression (absolute floor, no ratio)")
check(not perflib.detect(zero, mk(88, 10.0, 0.0, rnd))["regressions"], "0 -> 10 ms stays under the 25 ms floor")
check(perflib.detect(base, mk(87, 1000.0, 0.01, rnd))["verdict"] == "ok", "a quiet commit with history is ok")

# Contamination verdict (pure).
import importlib.util
spec = importlib.util.spec_from_file_location("suite", sys.argv[1] + "/suite.py")
suite = importlib.util.module_from_spec(spec); spec.loader.exec_module(suite)
q = {"heavy": [], "running": 0, "idle_running": 30, "load1": 1.0}
check(suite.contamination([q] * 10, 0, 8)[0] is False, "contamination: idle-priority load alone is clean")
check(suite.contamination([dict(q, running=64)] + [q] * 99, 0, 8)[0] is True,
      "contamination: one burst of 64 runnable threads among 99 quiet samples is caught")
check(suite.contamination([dict(q, heavy=["Runner.Worker"])] + [q] * 9, 0, 8)[0] is True,
      "contamination: one sample with a CI job is caught")
check(suite.contamination([], 0, 8)[0] is True and suite.contamination([q, q], 1, 8)[0] is True,
      "contamination: no samples or a sampling error fails closed")
check(suite.contamination([dict(q, running=9)] * 10, 0, 8)[0] is True,
      "contamination: sustained foreign load above the limit is caught")

# 3. bisect selection
lim = perflib.regression_limit(1000.0, 0.03, 0.05, "ms")
check(abs(lim - 1000.0 * 1.03 * 1.05) < 1e-6 and perflib.regression_limit(0.0, 0.03, 0.05, "ms") == 25.0,
      "regression_limit: relative band over a positive value, absolute floor over zero")
check(perflib.classify(1100, lim) == "bad" and perflib.classify(1050, lim) == "good",
      "classify: above the detector's alarm line is bad")
worst = 0
allok = True
for n in (2, 3, 5, 17, 64):
    commits = ["c%03d" % i for i in range(n)]
    for culprit in range(1, n):
        seen = []
        def measure(sha):
            seen.append(sha)
            return 2000.0 if commits.index(sha) >= culprit else 1000.0
        got, cands, probes = perflib.bisect(commits, measure, lim)
        bound = math.ceil(math.log2(max(n - 1, 1))) + 2
        worst = max(worst, len(probes))
        if got != commits[culprit] or len(probes) > bound or commits[0] in seen or commits[-1] in seen:
            allok = False
            print("  bisect n=%d culprit=%d got=%s probes=%d" % (n, culprit, got, len(probes)))
check(allok, "bisect: every culprit position found within ceil(log2 n)+2 probes, endpoints never re-probed (worst %d)" % worst)
commits = ["c%02d" % i for i in range(10)]
unbuildable = {"c05"}
def measure(sha):
    if sha in unbuildable:
        return None
    return 2000.0 if commits.index(sha) >= 5 else 1000.0
got, cands, _ = perflib.bisect(commits, measure, lim)
check(got is None and cands == ["c05", "c06"], "bisect: an unbuildable culprit widens to the candidate range [c05, c06] (%s %s)" % (got, cands))
def measure2(sha):
    if sha == "c03":
        return None
    return 2000.0 if commits.index(sha) >= 6 else 1000.0
got, cands, _ = perflib.bisect(commits, measure2, lim)
check(got == "c06", "bisect: an unbuildable commit away from the culprit is skipped (%s)" % got)
steps = {"c00": 1000, "c01": 1000, "c02": 1300, "c03": 1300, "c04": 1600, "c05": 1600, "c06": 2000}
order = sorted(steps)
got, _, _ = perflib.bisect(order, lambda sha: steps[sha], perflib.regression_limit(1000.0, 0.03, 0.05, "ms"))
check(got == "c02", "bisect: a staged slowdown 1000 -> 1300 -> 1600 -> 2000 names the first step past the alarm line (%s)" % got)
step = perflib.next_probe(["g", "b"], {})
check(step[0] == "done" and step[1] == "b", "bisect: adjacent good/bad names bad directly")
sys.exit(1 if fails else 0)
PY
pyrc=$?
cat "$TMP/py.out"
PASS=$((PASS + $(grep -c '^  ok ' "$TMP/py.out")))
FAIL=$((FAIL + $(grep -c '^  FAIL' "$TMP/py.out")))
[ "$pyrc" -eq 0 ] || [ "$(grep -c '^  FAIL' "$TMP/py.out")" -gt 0 ] || no "decision-logic block exited $pyrc without a FAIL line"

echo "=== data-branch format (store.py on a scratch repo) ==="
D="$TMP/data"
git init -q "$D"
git -C "$D" checkout -q --orphan perf-data
G() { git -C "$D" -c user.name=t -c user.email=t@t "$@"; }
python3 -I "$PERF/store.py" init --data "$D" && [ -f "$D/README.md" ] && ok "init writes README.md" || no "init"
FIX="$PERF/fixtures/records.jsonl"
n=0
while IFS= read -r line; do
  printf '%s\n' "$line" > "$TMP/rec.json"
  python3 -I "$PERF/store.py" append --data "$D" --record "$TMP/rec.json" || { no "append record $n"; break; }
  n=$((n+1))
done < "$FIX"
REC="$D/data/fixture-1/records.jsonl"
[ "$(wc -l < "$REC" | tr -d ' ')" = 15 ] && ok "append: data/<machine>/records.jsonl has one line per commit (15)" || no "records.jsonl line count"
[ "$(G log --oneline | wc -l | tr -d ' ')" = 17 ] && ok "every append is its own commit on perf-data (17 with README and .gitignore)" || no "commit count $(G log --oneline | wc -l)"
[ "$(G rev-parse --abbrev-ref HEAD)" = perf-data ] && ok "writes land on the perf-data branch" || no "branch"
printf '%s\n' "$(sed -n 3p "$FIX")" > "$TMP/dup.json"
if python3 -I "$PERF/store.py" append --data "$D" --record "$TMP/dup.json" 2>/dev/null; then no "duplicate commit appended"; else ok "a commit already recorded is refused"; fi
python3 -I -c 'import json,sys; r=json.loads(open(sys.argv[1]).read()); r["sha"]="zz"; open(sys.argv[2],"w").write(json.dumps(r))' "$TMP/dup.json" "$TMP/bad.json"
if python3 -I "$PERF/store.py" append --data "$D" --record "$TMP/bad.json" 2>/dev/null; then no "malformed record appended"; else ok "a malformed record is refused before writing"; fi
[ "$(wc -l < "$REC" | tr -d ' ')" = 15 ] && ok "refusals left records.jsonl untouched" || no "records.jsonl changed by a refusal"
SHA12="$(python3 -I -c 'import json,sys; print(json.loads(open(sys.argv[1]).readline())["sha"])' "$FIX")"
python3 -I "$PERF/store.py" has --data "$D" --machine fixture-1 --sha "$SHA12" && ok "has: a recorded sha" || no "has: recorded sha"
python3 -I "$PERF/store.py" has --data "$D" --machine fixture-1 --sha "$(printf '%040d' 0)" && no "has: unknown sha" || ok "has: an unrecorded sha is absent"
python3 -I "$PERF/store.py" check --data "$D" >/dev/null && ok "check validates every records.jsonl" || no "check"
echo '{"x":1}' > "$TMP/s.json"
python3 -I "$PERF/store.py" put --data "$D" --machine fixture-1 --name bisect/abc.json --file "$TMP/s.json" && [ -f "$D/data/fixture-1/bisect/abc.json" ] && ok "put: bisect/<sha12>.json under data/<machine>" || no "put"
python3 -I "$PERF/store.py" put --data "$D" --machine fixture-1 --name ../../escape.json --file "$TMP/s.json" 2>/dev/null && no "put escaped data/<machine>" || ok "put refuses a path outside data/<machine>"
python3 -I "$PERF/store.py" put --data "$D" --machine ../.. --name x.json --file "$TMP/s.json" 2>/dev/null && no "put accepted machine ../.." || ok "put refuses a path-unsafe machine id"
ln -s "$TMP" "$D/data/fixture-1/link"
python3 -I "$PERF/store.py" put --data "$D" --machine fixture-1 --name link/escaped.json --file "$TMP/s.json" 2>/dev/null && no "put followed a symlink out of data/<machine>" || ok "put refuses a symlink that resolves outside data/<machine>"
rm -f "$D/data/fixture-1/link"
python3 -I "$PERF/store.py" processed --data "$D" --machine fixture-1 --sha "$SHA12" && no "processed before marking" || ok "processed: a measured sha is not processed until marked"
python3 -I "$PERF/store.py" processed --data "$D" --machine fixture-1 --sha "$SHA12" --mark && python3 -I "$PERF/store.py" processed --data "$D" --machine fixture-1 --sha "$SHA12" && ok "processed: --mark records completion" || no "processed --mark"
python3 -I -c 'import json,sys; r=json.loads(open(sys.argv[1]).read()); r["sha"]="e"*40; r["contaminated"]=True; r["machine"]="fixture-2"; open(sys.argv[2],"w").write(json.dumps(r))' "$TMP/dup.json" "$TMP/c.json"
for i in 1 2; do python3 -I "$PERF/store.py" append --data "$D" --record "$TMP/c.json" >/dev/null 2>&1; done
python3 -I "$PERF/store.py" has --data "$D" --machine fixture-2 --sha "$(printf "e%.0s" $(seq 40))" && no "two contaminated attempts counted as measured" || ok "has: contaminated attempts below the limit ask for a re-measurement"
python3 -I "$PERF/store.py" append --data "$D" --record "$TMP/c.json" >/dev/null 2>&1
python3 -I "$PERF/store.py" has --data "$D" --machine fixture-2 --sha "$(printf "e%.0s" $(seq 40))" && ok "has: the third contaminated attempt stops re-measuring" || no "has: attempt limit"
mkdir -p "$TMP/outside"
ln -s "$TMP/outside" "$D/data/evil"
python3 -I "$PERF/store.py" put --data "$D" --machine evil --name x.json --file "$TMP/s.json" 2>/dev/null && no "put wrote through a symlinked machine directory" || ok "put refuses a symlinked machine directory"
[ -z "$(ls "$TMP/outside")" ] && ok "nothing was written outside the data tree" || no "a write escaped through data/evil"
rm -f "$D/data/evil"
[ "$(python3 -I "$PERF/store.py" pending --data "$D" --machine fixture-1)" = "$(tail -1 "$REC" | python3 -I -c 'import json,sys; print(json.loads(sys.stdin.read())["sha"])')" ] \
  && ok "pending: the newest measured-but-unprocessed commit is resumed first" || no "pending"
NEWEST="$(tail -1 "$REC" | python3 -I -c 'import json,sys; print(json.loads(sys.stdin.read())["sha"])')"
PREV="$(tail -2 "$REC" | head -1 | python3 -I -c 'import json,sys; print(json.loads(sys.stdin.read())["sha"])')"
python3 -I "$PERF/store.py" processed --data "$D" --machine fixture-1 --sha "$NEWEST" --mark
[ "$(python3 -I "$PERF/store.py" pending --data "$D" --machine fixture-1)" = "$PREV" ] \
  && ok "pending: an older unprocessed commit behind a processed one is not stranded" || no "pending skipped an older unprocessed commit"
ln -s "$TMP/outside/victim" "$D/data/fixture-1/.planted"
python3 -I "$PERF/store.py" put --data "$D" --machine fixture-1 --name planted-dest.json --file "$TMP/s.json" >/dev/null 2>&1
[ ! -e "$TMP/outside/victim" ] && ok "atomic writes never follow a planted symlink" || no "a write followed a planted symlink"
rm -f "$D/data/fixture-1/.planted"
G rm -q --cached "data/fixture-1/planted-dest.json" >/dev/null 2>&1; rm -f "$D/data/fixture-1/planted-dest.json"; G commit -qm "test cleanup" >/dev/null 2>&1
[ -z "$(G status --porcelain)" ] && ok "the data worktree is clean after every write" || no "uncommitted data: $(G status --porcelain | head -3)"

echo "=== detect.py / report.py on the stored data ==="
python3 -I "$PERF/detect.py" --records "$REC" --sha "$(sed -n 12p "$FIX" | python3 -I -c 'import json,sys; print(json.loads(sys.stdin.read())["sha"])')" --out "$TMP/sum11.json" --quiet
[ $? -eq 0 ] && ok "detect.py exit 0 on a noise-only commit" || no "detect.py false alarm on commit 11"
python3 -I "$PERF/detect.py" --records "$REC" --out "$TMP/sum.json" --quiet
[ $? -eq 1 ] && ok "detect.py exit 1 on the newest (regressed) commit" || no "detect.py missed the planted regression"
python3 -I -c '
import json,sys; s=json.load(open(sys.argv[1]))
assert s["schema"]=="concrete.perf.summary.v1" and s["verdict"]=="regression"
assert s["regressions"]==["compile.lox.wall_ms"], s["regressions"]
' "$TMP/sum.json" && ok "summary JSON: schema, verdict and the one regressed metric" || no "summary JSON content"
python3 -I - "$TMP/zero.jsonl" <<'PY'
import json, sys
base = {"schema": "concrete.perf.record.v1", "suite_version": 1, "machine": "z", "measured_at": "x", "status": "ok"}
with open(sys.argv[1], "w") as f:
    for i, v in enumerate([0.0, 0.0, 0.0, 0.0, 100.0]):
        r = dict(base, sha="%040x" % (i + 1), metrics={"pass.p.lower_ms": {"unit": "ms", "median": v, "lo": v, "hi": v, "n": 1, "samples": [v]}})
        f.write(json.dumps(r) + "\n")
PY
python3 -I "$PERF/detect.py" --records "$TMP/zero.jsonl" --out "$TMP/zero.json" >"$TMP/zero.txt" 2>&1
zrc=$?
[ "$zrc" -eq 1 ] && ! grep -q Traceback "$TMP/zero.txt" && ok "detect.py CLI: a 0 -> 100 ms regression exits 1 without crashing" || no "detect.py CLI zero baseline (exit $zrc): $(tail -2 "$TMP/zero.txt")"
python3 -I "$PERF/report.py" --records "$REC" --summary "$TMP/sum.json" --html "$TMP/r.html" > "$TMP/r.txt" \
  && grep -q "compile.lox.wall_ms" "$TMP/r.txt" && grep -q "<regression" "$TMP/r.txt" \
  && ok "report.py prints trends and marks the regression" || no "report.py text"
if [ ! -s "$TMP/r.html" ] || ! grep -q '</html>' "$TMP/r.html" || ! grep -q '<svg' "$TMP/r.html"; then
  no "report HTML missing, truncated or without charts"
elif grep -qiE '(src|href)="?(https?:)?//' "$TMP/r.html"; then no "report HTML references external assets"
else ok "report HTML is a complete page with inline charts and no external assets"; fi

echo "=== suite helpers ==="
python3 -I - "$PERF" <<'PY' && ok "synthetic generators emit every family; timed() measures wall and rss" || no "suite helpers"
import sys, json
sys.path.insert(0, sys.argv[1])
import suite
spec = json.load(open(sys.argv[1] + "/suite.json"))
for s in spec["synthetic"]:
    src = suite.gen(s["family"], 8)
    assert src.startswith("mod m") and "fn main" in src, s
w, rss, rc = suite.timed(["true"], "/")
assert rc == 0 and w >= 0
if suite.TIME_BIN:
    w, rss, rc = suite.timed([sys.executable, "-c", "b = bytearray(64 * 1024 * 1024); b[::4096] = b'x' * len(b[::4096])"], "/")
    assert rc == 0 and rss is not None and rss >= 48 * 1024, ("peak RSS not measured", rss)
PY
python3 -I - "$PERF" "$TMP" <<'PY' && ok "timed(): a timeout kills the benchmark's whole process tree" || no "timed() timeout left a descendant running"
import os, sys, time
sys.path.insert(0, sys.argv[1])
import suite
mark = os.path.join(sys.argv[2], "survivor")
w, rss, rc = suite.timed(["bash", "-c", "(sleep 3; touch %s) & wait" % mark], "/", timeout=1)
time.sleep(3.5)
assert rc == 124 and not os.path.exists(mark), (rc, os.path.exists(mark))
mark2 = os.path.join(sys.argv[2], "stubborn")
w, rss, rc = suite.timed(["bash", "-c", "(trap '' TERM; sleep 13; touch %s) & sleep 30" % mark2], "/", timeout=1)
time.sleep(13)
assert rc == 124 and not os.path.exists(mark2), ("a TERM-ignoring descendant survived", rc)
PY

echo
echo "check_perf_tracking: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
