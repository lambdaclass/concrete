#!/usr/bin/env bash
# Perf-tracking harness gate (scripts/perf/).
#
# The tracker's job is to raise an alarm on a real slowdown and stay silent on noise; a tracker that
# cries wolf gets ignored and one that never fires is the status quo it replaces. This gate checks
# the decision logic with fixtures, never with timings, so it is fast and machine-independent:
#   1. fixture results: scripts/perf/fixtures/records.jsonl (instruction counts, schema v2) parses,
#      every record validates, and malformed records (bad sha, unknown unit or counter, a wall-time
#      record claiming to be comparable, cross-field errors) are refused;
#   2. noise-band detection on instruction counts: no alarm on instruction noise, an alarm on a
#      planted 2x and a planted 3% slowdown, a 2x wall-time spike is information only, and the
#      fallback path (perf unavailable: counter "wall") is never comparable and never alarms;
#   3. bisect selection: for every culprit position (with and without unbuildable commits) the
#      search names the culprit within ceil(log2(n))+2 probes, using the detector's alarm line;
#   4. the data-branch format: store.py init/append/has/put/check on a scratch git repo writes
#      data/<machine>/records.jsonl one line per commit, commits each write, refuses a duplicate
#      and a malformed record, and detect.py / report.py read what it wrote (HTML has no external
#      assets).
# Shell syntax of runner.sh is also checked (bash -n). The server provisioning (systemd unit and
# timer, perf_event_paranoid) lives outside this repository; docs/project/PERF_TRACKING.md lists it.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PERF="$ROOT_DIR/scripts/perf"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

for f in perflib.py suite.py store.py detect.py perf_bisect.py report.py runner.sh suite.json fixtures/records.jsonl; do
  [ -f "$PERF/$f" ] || no "scripts/perf/$f missing"
done

echo "=== shell syntax ==="
for f in runner.sh; do
  if bash -n "$PERF/$f" 2>"$TMP/syn"; then ok "$f parses"; else no "$f: $(cat "$TMP/syn")"; fi
done

echo "=== decision logic (fixtures, detection, bisect) ==="
python3 -I - "$PERF" <<'PY' >"$TMP/py.out" 2>&1
import json, math, random, sys, copy, os
sys.path.insert(0, sys.argv[1])
import perflib

fails = 0
def check(cond, msg):
    global fails
    print(("  ok   " if cond else "  FAIL ") + msg)
    if not cond:
        fails += 1

# 1. fixture results (instruction counts; commit 5 has a 2x WALL spike, commit 8 is a wall-time
#    fallback record, commits 12..14 plant a 2x INSTRUCTION slowdown in lox).
recs = perflib.parse_records(open(sys.argv[1] + "/fixtures/records.jsonl").read())
check(len(recs) == 15, "fixture: 15 records parse and validate")
check(all(perflib.validate_record(r) == [] for r in recs), "fixture: every record validates")
check(len({r["sha"] for r in recs}) == 15, "fixture: one record per commit")
check(sum(1 for r in recs if r["counter"] == "instructions:u" and r["comparable"]) == 14
      and recs[8]["counter"] == "wall" and recs[8]["comparable"] is False,
      "fixture: 14 instruction-count records and one wall-time fallback record")
suite = json.load(open(sys.argv[1] + "/suite.json"))
check(isinstance(suite.get("suite_version"), int) and suite["repeats"] >= 3,
      "suite.json: integer suite_version and >= 3 repeats")
good = recs[0]
M = "compile.lox.instr"
for mut, why in [
        (lambda r: r.update(sha="abc"), "short sha"),
        (lambda r: r.update(schema="v0"), "wrong schema"),
        (lambda r: r.update(machine=""), "empty machine"),
        (lambda r: r.update(machine="../x"), "path-unsafe machine"),
        (lambda r: r.update(status="great"), "unknown status"),
        (lambda r: r.update(counter="cycles"), "unknown counter"),
        (lambda r: r.pop("comparable"), "missing comparable flag"),
        (lambda r: r.update(counter="wall"), "a wall-time record claiming to be comparable"),
        (lambda r: r["metrics"][M].update(unit="parsecs"), "unknown unit"),
        (lambda r: r["metrics"][M].update(lo=10 ** 12), "lo > median"),
        (lambda r: r["metrics"].update({"nodots": {"unit": "ms", "median": 1, "lo": 1, "hi": 1, "n": 1}}), "bad metric name"),
        (lambda r: r.pop("suite_version"), "missing suite_version"),
        (lambda r: r["metrics"][M].update(n=-4, samples=["bad"]), "negative n, non-numeric samples"),
        (lambda r: r["metrics"][M].update(n=2.5), "fractional n"),
        (lambda r: r["metrics"][M].update(hi=float("inf")), "infinite hi"),
        (lambda r: r["metrics"][M].pop("samples"), "missing samples"),
        (lambda r: r["metrics"][M].update(median=r["metrics"][M]["lo"]), "median disagrees with samples"),
        (lambda r: r["metrics"][M].update(n=4), "n disagrees with the sample count")]:
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

# 2. detection on the instruction-count fixture.
for i in range(3, 12):
    s = perflib.detect(recs[:i], recs[i])
    if not recs[i]["comparable"]:
        check(s["verdict"] == "not_comparable" and not s["regressions"],
              "fixture commit %d: a wall-time fallback record is not comparable, never an alarm" % i)
    else:
        check(s["regressions"] == [], "fixture commit %d: instruction noise only, no alarm (%s)" % (i, s["regressions"]))
s5 = perflib.detect(recs[:5], recs[5])
check(s5["metrics"]["compile.lox.wall_ms"]["verdict"] == "info_slower" and not s5["regressions"],
      "fixture commit 5: a 2x wall-time spike is reported as information, never an alarm")
s = perflib.detect(recs[:12], recs[12])
check(s["regressions"] == [M], "fixture commit 12: planted 2x instruction slowdown flagged, nothing else (%s)" % s["regressions"])
e = s["metrics"][M]
check(1.9 < e["ratio"] < 2.1, "fixture commit 12: ratio ~2 (%.4f)" % e["ratio"])
check(e["band"] <= 0.01, "fixture: the instruction-count band stays under 1%% (%.4f)" % e["band"])
clean = [r for r in recs[:12] if r["comparable"]][-perflib.DEFAULTS["window"]:]
hist = [r["metrics"][M]["median"] for r in clean]
check(len(clean) == 10 and recs[8] not in clean, "baseline window: last 10 comparable records, the fallback excluded")
check(abs(e["baseline"] - perflib.median(hist)) < 1e-3 * max(hist), "baseline is the median of the comparable window")
perflib.annotate_bisect_targets(recs[:12], recs[12], s)
check(s["metrics"][M]["good"] == recs[11]["sha"], "bisect good end = newest unregressed record")
s = perflib.detect(recs[:2], recs[2])
check(all(m["verdict"] in ("insufficient_history",) for m in s["metrics"].values()) and not s["regressions"],
      "two prior records: insufficient history, no verdict")

# Synthetic instruction-count sweeps.
def mk(sha_i, val, noise_sd, rnd, n=5, unit="instr", machine="m1", sv=2, counter="instructions:u",
       name="compile.p.instr"):
    samples = [val * (1 + rnd.gauss(0, noise_sd)) for _ in range(n)]
    return {"schema": perflib.RECORD_SCHEMA, "suite_version": sv, "machine": machine,
            "sha": "%040x" % sha_i, "measured_at": "x", "status": "ok", "contaminated": False,
            "counter": counter, "comparable": counter == "instructions:u",
            "metrics": {name: perflib.summarize(samples, unit)}}
P = "compile.p.instr"
false_alarms = misses2 = misses3 = trials = 0
for noise in (0.0001, 0.0005, 0.001, 0.002):
    for seed in range(40):
        rnd = random.Random(seed * 1000 + int(noise * 1e6))
        hist = [mk(i, 2e10, noise, rnd) for i in range(12)]
        trials += 1
        if perflib.detect(hist, mk(99, 2e10, noise, rnd))["regressions"]:
            false_alarms += 1
        if not perflib.detect(hist, mk(100, 4e10, noise, rnd))["regressions"]:
            misses2 += 1
        if not perflib.detect(hist, mk(101, 2e10 * 1.03, noise, rnd))["regressions"]:
            misses3 += 1
check(false_alarms == 0, "instruction noise sweep (0.01%%..0.2%%): 0 false alarms in %d trials (got %d)" % (trials, false_alarms))
check(misses2 == 0, "instruction noise sweep: planted 2x detected in all %d trials (missed %d)" % (trials, misses2))
check(misses3 == 0, "instruction noise sweep: planted 3%% slowdown detected in all %d trials (missed %d)" % (trials, misses3))
rnd = random.Random(7)
hist = [mk(i, 2e10, 0.0005, rnd) for i in range(12)]
check(not perflib.detect(hist, mk(98, 2e10 * 1.01, 0.0005, rnd))["regressions"],
      "a 1% instruction change sits inside band 0.5% + threshold 1% and is not an alarm")
wall = [mk(i, 1000.0, 0.01, rnd, unit="ms", name="compile.p.wall_ms") for i in range(12)]
check(not perflib.detect(wall, mk(97, 3000.0, 0.01, rnd, unit="ms", name="compile.p.wall_ms"))["regressions"],
      "a 3x wall-time change on an instruction-count record is information, never an alarm")
other = [mk(i, 2e10, 0.0005, rnd, machine="m2") for i in range(12)]
check(perflib.detect(other, mk(96, 4e10, 0.0005, rnd))["metrics"][P]["verdict"] == "insufficient_history",
      "records from another machine are never a baseline")
oldv = [mk(i, 2e10, 0.0005, rnd, sv=1) for i in range(12)]
check(perflib.detect(oldv, mk(95, 4e10, 0.0005, rnd))["metrics"][P]["verdict"] == "insufficient_history",
      "records from another suite_version are never a baseline")
tiny = [mk(i, 2e6, 0.0005, rnd) for i in range(12)]
check(not perflib.detect(tiny, mk(94, 4e6, 0.0005, rnd))["regressions"],
      "a 2x change of a 2M-instruction metric stays under the 5M absolute floor")
noisy = [mk(i, 2e10, 0.0, rnd, n=1) for i in range(12)]
for k, r in enumerate(noisy):
    r["metrics"][P] = perflib.summarize([2e10 * (0.6 if k % 2 else 1.4)], "instr")
check(perflib.detect(noisy, mk(93, 4e10, 0.0, rnd, n=1))["metrics"][P]["verdict"] == "too_noisy",
      "a metric whose history spread exceeds max_band is reported too_noisy, never alarmed")

# The fallback path: wall-time records are never comparable.
fb_hist = [mk(i, 1000.0, 0.01, rnd, unit="ms", name="compile.p.wall_ms", counter="wall") for i in range(12)]
sfb = perflib.detect(fb_hist, mk(92, 3000.0, 0.01, rnd, unit="ms", name="compile.p.wall_ms", counter="wall"))
check(sfb["verdict"] == "not_comparable" and not sfb["regressions"],
      "fallback: a wall-time record is not_comparable even after a 3x change")
mixed = fb_hist + [mk(200 + i, 2e10, 0.0005, rnd) for i in range(2)]
check(perflib.detect(mixed, mk(91, 4e10, 0.0005, rnd))["metrics"][P]["verdict"] == "insufficient_history",
      "fallback records never count as instruction-count history")

# Verdicts beyond regression.
base = [mk(i, 2e10, 0.0005, rnd) for i in range(6)]
bf = mk(90, 2e10, 0.0005, rnd); bf["status"] = "build_failed"; bf["metrics"] = {}
check(perflib.detect(base, bf)["verdict"] == "build_failed", "a build-failed record's verdict is build_failed, not ok")
gone = mk(89, 2e10, 0.0005, rnd); gone["metrics"] = {}
sg = perflib.detect(base, gone)
check(sg["verdict"] == "incomplete" and sg["missing"] == [P],
      "a metric that disappeared makes the verdict incomplete and is named")
part = mk(88, 2e10, 0.0005, rnd); part["status"] = "partial"
check(perflib.detect(base, part)["verdict"] == "incomplete", "a partial record is incomplete, not ok")
zero = [mk(i, 0.0, 0.0, rnd) for i in range(6)]
check(perflib.detect(zero, mk(87, 1e8, 0.0, rnd))["regressions"] == [P],
      "a 0-instruction baseline that becomes 100M is a regression (absolute floor, no ratio)")
check(not perflib.detect(zero, mk(86, 1e6, 0.0, rnd))["regressions"], "0 -> 1M instructions stays under the floor")
check(perflib.detect(base, mk(85, 2e10, 0.0005, rnd))["verdict"] == "ok", "a quiet commit with history is ok")
check(perflib.alarmable("growth.syn_match.instr_exp", "exp") and not perflib.alarmable("growth.syn_match.worst_pass_ms_exp", "exp")
      and perflib.alarmable("compile.x.rss_kb", "kb") and not perflib.alarmable("compile.x.wall_ms", "ms"),
      "alarmable: instructions, RSS and instruction exponents; never times")

# Legacy (pre-instruction) records still parse, but are never comparable.
legacy = {"schema": "concrete.perf.record.v1", "suite_version": 1, "machine": "m1", "sha": "%040x" % 300,
          "measured_at": "x", "status": "ok", "contaminated": False,
          "metrics": {P: perflib.summarize([2e10] * 3, "instr")}}
check(perflib.parse_records(json.dumps(legacy) + "\n")[0]["sha"] == legacy["sha"],
      "a legacy v1 record still parses (old history does not block the runner)")
try:
    perflib.dump_record(legacy); check(False, "a v1 record can no longer be written")
except ValueError:
    check(True, "a v1 record can no longer be written")
check(perflib.detect([dict(legacy, sha="%040x" % (400 + k), suite_version=2) for k in range(6)],
                     mk(84, 4e10, 0.0005, rnd))["metrics"][P]["verdict"] == "insufficient_history",
      "legacy records are never a baseline")

# perf stat output parsing and the counter fallback.
import importlib.util
spec = importlib.util.spec_from_file_location("suite", sys.argv[1] + "/suite.py")
suite = importlib.util.module_from_spec(spec); spec.loader.exec_module(suite)
check(suite.parse_perf_csv("132236,,instructions:u,424587,100.00,,\n") == 132236,
      "parse_perf_csv reads the instructions:u count")
check(suite.parse_perf_csv("<not supported>,,instructions:u,0,100.00,,\n") is None
      and suite.parse_perf_csv("<not counted>,,instructions:u,0,0.00,,\n") is None
      and suite.parse_perf_csv("Error:\nAccess to performance monitoring and observability operations is limited.\n") is None
      and suite.parse_perf_csv("") is None,
      "parse_perf_csv: unsupported, uncounted, refused and empty output all mean no count")
# A host that counts, but one run's counter failed: the metric is dropped and the record partial.
suite._COUNTER = "instructions:u"
run = suite.Run("/", {"projects": [], "single_files": []}, 3)
run.put_run("compile.x", [(1.0, 1000, 5e9), (1.0, 1000, None), (1.0, 1000, 5e9)])
check("compile.x.instr" not in run.metrics and "instr:compile.x.instr" in run.failed and "compile.x.rss_kb" in run.metrics,
      "a run whose instruction counter failed drops that metric and marks the record partial")
run.put_run("compile.y", [(1.0, 1000, 5e9)] * 3)
check(run.metrics["compile.y.instr"]["median"] == 5e9, "complete instruction samples are kept")
import shutil as _sh; _sh.rmtree(run.tmp, ignore_errors=True)
os.environ["PERF_COUNTER"] = "wall"
suite._COUNTER = None
check(suite.counter() == "wall", "PERF_COUNTER=wall forces the wall-time fallback")
w, rss, rc, ins = suite.timed(["true"], "/")
check(rc == 0 and ins is None and w >= 0, "fallback timed(): no instruction count, wall time still measured")

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
ilim = perflib.regression_limit(2e10, 0.005, 0.01, "instr")
got, _, _ = perflib.bisect(order, lambda sha: {"c00": 2e10, "c01": 2e10, "c02": 2e10 * 1.001, "c03": 2e10 * 1.03,
                                              "c04": 2e10 * 1.03, "c05": 2e10 * 1.03, "c06": 2e10 * 1.03}[sha], ilim)
check(got == "c03", "bisect on instruction counts: a 3%% step is found past 0.1%% noise (%s)" % got)
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
# Plant a symlink at every predictable temp name (the old "<dest>.tmp.<pid>" scheme, for this pid)
# and write in-process: an implementation that opens a predictable name follows it.
python3 -I - "$PERF" "$TMP/plant" "$TMP/outside/victim" <<'PY' && ok "atomic_write never follows a planted symlink at a predictable temp name" || no "atomic_write followed a planted symlink"
import os, sys
sys.path.insert(0, sys.argv[1])
import store
d = sys.argv[2]; os.makedirs(d)
dest = os.path.join(d, "records.jsonl")
for name in ("%s.tmp.%d" % (dest, os.getpid()), dest + ".tmp", os.path.join(d, ".records.jsonl.tmp")):
    os.symlink(sys.argv[3], name)
store.atomic_write(dest, b"x\n")
assert not os.path.exists(sys.argv[3]) and open(dest).read() == "x\n" and not os.path.islink(dest)
PY
[ -z "$(G status --porcelain)" ] && ok "the data worktree is clean after every write" || no "uncommitted data: $(G status --porcelain | head -3)"

echo "=== detect.py / report.py on the stored data ==="
python3 -I "$PERF/detect.py" --records "$REC" --sha "$(sed -n 12p "$FIX" | python3 -I -c 'import json,sys; print(json.loads(sys.stdin.read())["sha"])')" --out "$TMP/sum11.json" --quiet
[ $? -eq 0 ] && ok "detect.py exit 0 on a noise-only commit" || no "detect.py false alarm on commit 11"
python3 -I "$PERF/detect.py" --records "$REC" --out "$TMP/sum.json" --quiet
[ $? -eq 1 ] && ok "detect.py exit 1 on the newest (regressed) commit" || no "detect.py missed the planted regression"
python3 -I -c '
import json,sys; s=json.load(open(sys.argv[1]))
assert s["schema"]=="concrete.perf.summary.v1" and s["verdict"]=="regression"
assert s["regressions"]==["compile.lox.instr"], s["regressions"]
' "$TMP/sum.json" && ok "summary JSON: schema, verdict and the one regressed metric" || no "summary JSON content"
python3 -I - "$TMP/zero.jsonl" <<'PY'
import json, sys
base = {"schema": "concrete.perf.record.v2", "suite_version": 2, "machine": "z", "measured_at": "x", "status": "ok",
        "counter": "instructions:u", "comparable": True}
with open(sys.argv[1], "w") as f:
    for i, v in enumerate([0.0, 0.0, 0.0, 0.0, 1e8]):
        r = dict(base, sha="%040x" % (i + 1), metrics={"frontend.p.instr": {"unit": "instr", "median": v, "lo": v, "hi": v, "n": 1, "samples": [v]}})
        f.write(json.dumps(r) + "\n")
PY
python3 -I "$PERF/detect.py" --records "$TMP/zero.jsonl" --out "$TMP/zero.json" >"$TMP/zero.txt" 2>&1
zrc=$?
[ "$zrc" -eq 1 ] && ! grep -q Traceback "$TMP/zero.txt" && ok "detect.py CLI: a 0 -> 100M instruction regression exits 1 without crashing" || no "detect.py CLI zero baseline (exit $zrc): $(tail -2 "$TMP/zero.txt")"
python3 -I "$PERF/report.py" --records "$REC" --summary "$TMP/sum.json" --html "$TMP/r.html" > "$TMP/r.txt" \
  && grep -q "compile.lox.instr" "$TMP/r.txt" && grep -q "<regression" "$TMP/r.txt" \
  && ok "report.py prints trends and marks the regression" || no "report.py text"
if [ ! -s "$TMP/r.html" ] || ! grep -q '</html>' "$TMP/r.html" || ! grep -q '<svg' "$TMP/r.html"; then
  no "report HTML missing, truncated or without charts"
elif grep -qiE '(src|href)="?(https?:)?//' "$TMP/r.html"; then no "report HTML references external assets"
else ok "report HTML is a complete page with inline charts and no external assets"; fi

echo "=== suite helpers ==="
python3 -I - "$PERF" <<'PY' && ok "synthetic generators emit every family; timed() measures wall, rss and (where perf counts) instructions" || no "suite helpers"
import sys, json
sys.path.insert(0, sys.argv[1])
import suite
spec = json.load(open(sys.argv[1] + "/suite.json"))
for s in spec["synthetic"]:
    src = suite.gen(s["family"], 8)
    assert src.startswith("mod m") and "fn main" in src, s
w, rss, rc, ins = suite.timed(["true"], "/")
assert rc == 0 and w >= 0
if suite.TIME_BIN:
    w, rss, rc, ins = suite.timed([sys.executable, "-c", "b = bytearray(64 * 1024 * 1024); b[::4096] = b'x' * len(b[::4096])"], "/")
    assert rc == 0 and rss is not None and rss >= 48 * 1024, ("peak RSS not measured", rss)
if suite.counter() == "instructions:u":
    # Instruction counts cover the whole process tree and are near-deterministic.
    a = [suite.timed([sys.executable, "-c", "sum(range(3000000))"], "/")[3] for _ in range(3)]
    assert all(x and x > 1e7 for x in a), ("no instruction count", a)
    assert (max(a) - min(a)) / min(a) < 0.02, ("instruction counts not stable", a)
    child = suite.timed(["bash", "-c", "%s -c 'sum(range(3000000))'" % sys.executable], "/")[3]
    assert child and child > 0.9 * min(a), ("child instructions not counted", child, a)
PY
python3 -I - "$PERF" "$TMP" <<'PY' && ok "merge-gates: keeps the shards' counter, refuses a duplicated shard and mixed counters" || no "merge-gates"
import json, os, subprocess, sys
P, T = sys.argv[1], sys.argv[2]
def shard(name, cmds, ctr="instructions:u"):
    path = os.path.join(T, name)
    json.dump({"discovered_on": "a" * 40, "machine": "m1", "counter": ctr, "gates": [],
               "all": [{"cmd": "bash scripts/tests/%s.sh" % c, "wall_s": 1.0, "instr": i, "rc": 0} for c, i in cmds]},
              open(path, "w"))
    return path
a = shard("s0.json", [("check_a", 9e9), ("check_b", 1e9)])
b = shard("s1.json", [("check_c", 5e9)])
w = shard("s2.json", [("check_d", 7e9)], ctr="wall")
env = dict(os.environ, PERF_COUNTER="wall")   # the merging host cannot count: provenance must not change
run = lambda *f: subprocess.run([sys.executable, P + "/suite.py", "merge-gates", "--top", "2", "--out", os.path.join(T, "m.json")] + list(f),
                                capture_output=True, env=env).returncode
assert run(a, b) == 0
m = json.load(open(os.path.join(T, "m.json")))
assert m["counter"] == "instructions:u" and [g["id"] for g in m["gates"]] == ["check_a", "check_c"], m
assert run(a, a) == 2 and run(a, w) == 2
PY
python3 -I - "$PERF" "$TMP" <<'PY' && ok "timed(): a timeout kills the benchmark's whole process tree" || no "timed() timeout left a descendant running"
import os, sys, time
sys.path.insert(0, sys.argv[1])
import suite
mark = os.path.join(sys.argv[2], "survivor")
w, rss, rc, ins = suite.timed(["bash", "-c", "(sleep 3; touch %s) & wait" % mark], "/", timeout=1)
time.sleep(3.5)
assert rc == 124 and not os.path.exists(mark), (rc, os.path.exists(mark))
mark2 = os.path.join(sys.argv[2], "stubborn")
w, rss, rc, ins = suite.timed(["bash", "-c", "(trap '' TERM; sleep 13; touch %s) & sleep 30" % mark2], "/", timeout=1)
time.sleep(13)
assert rc == 124 and not os.path.exists(mark2), ("a TERM-ignoring descendant survived", rc)
PY

echo
echo "check_perf_tracking: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
