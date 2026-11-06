#!/usr/bin/env bash
# Compiler-profile gate (scripts/perf/leanprof_profile.sh, R-0551).
#
# The profile job is worth running only if a hot spot that grows, or a new one, is reported, and
# host noise or a uniformly slower machine is not. This gate checks that decision logic and the
# driver's wiring with SYNTHETIC leanprof reports (tests/fixtures/leanprof_profile/synth.py) and a
# FAKE leanprof and compiler, never with a real profile, so it is build-free and runs anywhere:
#   A  report reader    a leanprof v3 report is read; malformed ones (wrong version, no scopes, a
#                       negative or inconsistent time, zero total) are refused
#   B  summary          the median of the repetitions; each stage through its declaration, the
#                       fallback declaration when the first is absent, `null` when none is there;
#                       a report of the other mode is refused
#   C  comparison       no finding on noise, on a uniform 1.5x slowdown, or on a 0.3% function that
#                       doubles; a `grown` finding on a hot spot that doubles; the R-0551 control
#                       (a deliberate quadratic loop in the lower stage) gives a `new` finding and
#                       lower's `stage-grown`; a stage timed through another declaration is a note,
#                       not a finding; sample vs auto is refused (exit 2); thresholds are recorded
#   D  driver           with the fake leanprof: refuses outside the devshell; profiles lox, std and
#                       the built-in scaling program in COPIES (the checkout is not written); one
#                       workload per --scaling-corpus file; a baseline with the control exits 1
#                       with comparison.md; a failing workload exits 3 with status failed; a hang is
#                       INCOMPLETE under bounded_run (exit 3), never a pass; auto mode
#   F  runner           scripts/perf/leanprof_runner.sh against a local origin (fake nix and lake):
#                       first cycle profiles main only; queued landings oldest first, each against
#                       the previous profiled commit; the control at a landed commit is recorded in
#                       findings.jsonl and notified; three failures give failed.json and the next
#                       commit's baseline skips it
#   E  shell syntax     bash -n on the driver, the runner and the fakes
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PY="python3 scripts/perf/leanprof_profile.py"
DRV="scripts/perf/leanprof_profile.sh"
FIX="$ROOT_DIR/tests/fixtures/leanprof_profile"
SYN="python3 $FIX/synth.py"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
export PYTHONDONTWRITEBYTECODE=1
jq_py() {  # jq_py FILE EXPR: print a Python expression over the JSON in FILE (bound to d)
  python3 -c 'import json,sys; d=json.load(open(sys.argv[1])); print(eval(sys.argv[2]))' "$1" "$2"
}

echo "A  report reader"
$SYN --variant base "$TMP/base.json"
$PY validate "$TMP/base.json" && ok "a v3 report is read" || no "a v3 report is refused"
bad() {  # bad LABEL PYTHON-MUTATION (d is the report)
  python3 -c 'import json,sys; d=json.load(open(sys.argv[1])); exec(sys.argv[2]); json.dump(d, open(sys.argv[3], "w"))' \
    "$TMP/base.json" "$2" "$TMP/bad.json"
  if $PY validate "$TMP/bad.json" 2>/dev/null; then no "malformed report accepted: $1"; else ok "refused: $1"; fi
}
bad "version 2" 'd["version"] = 2'
bad "no scopes" 'd["scopes"] = []'
bad "zero total" 'd["wall_ns"] = 0'
bad "negative self time" 'd["scopes"][0]["self_ns"] = -1'
bad "self above total" 'd["scopes"][0]["self_ns"] = d["scopes"][0]["total_ns"] * 2 + 10'
bad "unnamed scope" 'd["scopes"][0]["name"] = ""'
echo '{"version": 3' > "$TMP/trunc.json"
$PY validate "$TMP/trunc.json" 2>/dev/null && no "truncated JSON accepted" || ok "refused: truncated JSON"

echo "B  summary"
mkprof() {  # mkprof NAME MODE VARIANT [REPS]: a profile of one synthetic `lox` workload
  local n="$1" m="$2" v="$3" reps="${4:-3}" r files=()
  for r in $(seq 1 "$reps"); do $SYN --mode "$m" --variant "$v" --rep "$r" "$TMP/$n-$r.json"; files+=("$TMP/$n-$r.json"); done
  $PY summarize --workload lox --mode "$m" "${files[@]}" > "$TMP/$n.wl.json" || return 1
  echo "{\"mode\": \"$m\", \"sha\": \"$n\", \"reps\": $reps, \"rate\": 2000}" > "$TMP/$n.meta.json"
  $PY assemble --meta "$TMP/$n.meta.json" --out "$TMP/$n.profile.json" "$TMP/$n.wl.json"
}
mkprof base sample base && ok "a sample profile is assembled" || no "sample profile not assembled"
mkprof noise sample noise
[ "$(jq_py "$TMP/noise.wl.json" 'd["reps"]')" = 3 ] && ok "three repetitions summarized" || no "repetition count"
# The median: rep 2 of three noisy reps is not the mean.
want="$(python3 - "$TMP" <<'PY'
import json, statistics, sys
t = sys.argv[1]; n = "Concrete.Symbols.isRuntimeHelperName (in List.elem)"
v = []
for r in (1, 2, 3):
    d = json.load(open("%s/noise-%d.json" % (t, r)))
    v.append(sum(s["self_ns"] for s in d["scopes"] if s["name"] == n) / d["wall_ns"] * 100)
print(round(statistics.median(v), 2))
PY
)"
got="$(jq_py "$TMP/noise.wl.json" '[h["self_pct"] for h in d["hotspots"] if h["name"].startswith("Concrete.Symbols.isRuntimeHelperName")][0]')"
[ "$want" = "$got" ] && ok "a hot spot's share is the median of the repetitions ($got%)" || no "median share: want $want got $got"
[ "$(jq_py "$TMP/base.wl.json" 'd["hotspots"][0]["name"]')" = "Concrete.Symbols.isRuntimeHelperName (in List.elem)" ] \
  && ok "hot spots ranked by self share" || no "hot-spot ranking"
[ "$(jq_py "$TMP/base.wl.json" '{s["stage"]: s["decl"] for s in d["stages"]}["lower"]')" = "Concrete.Pipeline.lower" ] \
  && ok "the lower stage is timed through Concrete.Pipeline.lower" || no "lower stage declaration"
st="$(jq_py "$TMP/base.wl.json" '[(s["pct"] is not None and s["pct"] > 0) for s in d["stages"]].count(False)')"
[ "$st" = 0 ] && ok "every stage seen in the synthetic build" || no "$st stages not seen"
lp="$(jq_py "$TMP/base.wl.json" '{s["stage"]: s["pct"] for s in d["stages"]}')"
python3 -c 'import sys; s=eval(sys.argv[1]); fe=["parse","resolve-files","resolve","check","elaborate","core-check","proof-core"]; sys.exit(0 if s["load-project"] >= sum(s[x] for x in fe) else 1)' "$lp" \
  && ok "load-project encloses the front-end stages" || no "load-project does not enclose the front end"
mkprof ren sample renamed-stage 1
[ "$(jq_py "$TMP/ren.wl.json" '{s["stage"]: s["decl"] for s in d["stages"]}["emit"]')" = "Concrete.emitSSAProgram" ] \
  && ok "a stage falls back to its next declaration" || no "stage fallback"
python3 -c 'import json,sys; d=json.load(open(sys.argv[1])); d["scopes"]=[s for s in d["scopes"] if s["name"]!="Concrete.Pipeline.monomorphize"]; json.dump(d,open(sys.argv[2],"w"))' "$TMP/base-1.json" "$TMP/nomono.json"
[ "$($PY summarize --workload x --mode sample "$TMP/nomono.json" | python3 -c 'import json,sys; print({s["stage"]: s["pct"] for s in json.load(sys.stdin)["stages"]}["monomorphize"])')" = None ] \
  && ok "a stage with no declaration in the report is null, not zero" || no "absent stage"
$SYN --mode auto --variant base "$TMP/auto-1.json"
$PY summarize --workload lox --mode sample "$TMP/auto-1.json" >/dev/null 2>&1 \
  && no "an auto report summarized as sample" || ok "a report of the other mode is refused"

echo "C  comparison"
cmpv() {  # cmpv VARIANT -> exit status of compare(base, VARIANT); comparison in $TMP/VARIANT.cmp.json
  [ -f "$TMP/$1.profile.json" ] || mkprof "$1" sample "$1" >/dev/null
  $PY compare "$TMP/base.profile.json" "$TMP/$1.profile.json" --json "$TMP/$1.cmp.json" --md "$TMP/$1.cmp.md" >/dev/null
}
kinds() { jq_py "$TMP/$1.cmp.json" '",".join(sorted(f["kind"] + ":" + f["name"] for f in d["findings"]))'; }
for v in noise uniform small-double; do
  cmpv "$v"; rc=$?
  [ "$rc" = 0 ] && [ -z "$(kinds "$v")" ] && ok "no finding on $v" || no "$v: exit $rc, findings $(kinds "$v")"
done
cmpv grown; rc=$?
[ "$rc" = 1 ] && [ "$(kinds grown)" = "grown:Concrete.moduleCallContext (in List.filterTR.loop)" ] \
  && ok "a hot spot that doubles is a grown finding (its stage, +7 points but x1.23, is not)" || no "grown: exit $rc, findings $(kinds grown)"
cmpv quadratic; rc=$?
[ "$rc" = 1 ] && [ "$(kinds quadratic)" = "new:Concrete.lowerQuadraticControl,stage-grown:lower" ] \
  && ok "control: a deliberate quadratic loop in lower opens a new + stage-grown finding" || no "quadratic control: exit $rc, findings $(kinds quadratic)"
grep -q "lowerQuadraticControl" "$TMP/quadratic.cmp.md" && grep -q "finding" "$TMP/quadratic.cmp.md" \
  && ok "comparison.md names the finding" || no "comparison.md does not name the finding"
[ "$(jq_py "$TMP/quadratic.cmp.json" 'd["thresholds"]["grow_pts"]')" = 2.0 ] && ok "thresholds recorded in the comparison" || no "thresholds not recorded"
$PY compare "$TMP/base.profile.json" "$TMP/quadratic.profile.json" --new-pts 50 --grow-pts 50 >/dev/null \
  && ok "thresholds are the comparison's (raised: no finding)" || no "threshold override"
cmpv renamed-stage; rc=$?
[ "$rc" = 0 ] && jq_py "$TMP/renamed-stage.cmp.json" 'd["notes"]' | has_match "stage emit timed through" \
  && ok "a stage timed through another declaration is a note, not a finding" || no "renamed stage: exit $rc"
mkprof autob auto base 1 >/dev/null
$PY compare "$TMP/base.profile.json" "$TMP/autob.profile.json" >/dev/null 2>&1; rc=$?
[ "$rc" = 2 ] && ok "a sample profile is not compared with an auto profile (exit 2)" || no "mode mismatch: exit $rc"
$PY compare "$TMP/base.profile.json" "$TMP/base-1.json" >/dev/null 2>&1; rc=$?
[ "$rc" = 2 ] && ok "a raw report is not a profile (exit 2)" || no "raw report as profile: exit $rc"

python3 - "$TMP" <<'PY'
import json, sys
t = sys.argv[1]
def prof(sha, pct):
    w = {"name": "lox", "status": "ok", "stages": [], "self_pct_all": {"f": pct},
         "hotspots": [{"name": "f", "pos": "", "self_pct": pct, "self_ms": 1, "total_pct": pct, "calls": 0}]}
    json.dump({"schema": "concrete-leanprof-profile/1", "mode": "auto", "sha": sha, "workloads": [w]},
              open("%s/edge-%s.json" % (t, sha), "w"))
prof("b", 2.1); prof("h", 4.1)
PY
$PY compare "$TMP/edge-b.json" "$TMP/edge-h.json" >/dev/null; rc=$?
[ "$rc" = 1 ] && ok "a share at exactly the threshold (2.1% -> 4.1%: +2.0 points) is a finding" || no "threshold edge: exit $rc"

echo "D  driver"
FAKEBIN="$TMP/fakebin"; mkdir -p "$FAKEBIN"
printf '#!/bin/sh\n[ -z "$FAKE_LAKE_FAIL" ] || exit 1\n' > "$FAKEBIN/lake"; chmod +x "$FAKEBIN/lake"
LPF="$FIX/fake_leanprof.sh" CC="$FIX/fake_compiler.sh"
drive() {  # drive OUT [args]: the driver inside a (fake) devshell
  local out="$1"; shift
  env IN_NIX_SHELL=impure PATH="$FAKEBIN:$PATH" FAKE_CC_LOG="$TMP/cc.log" \
    bash "$DRV" profile --leanprof "$LPF" --compiler "$CC" --out "$out" --mode sample "$@" > "$TMP/drive.out" 2>&1
}
env -u IN_NIX_SHELL bash "$DRV" profile --leanprof "$LPF" --compiler "$CC" --out "$TMP/nonix" >/dev/null 2>&1; rc=$?
[ "$rc" = 2 ] && ! [ -e "$TMP/nonix/profile.json" ] && ok "profile refuses to run outside the devshell" || no "outside the devshell: exit $rc"
before="$(git status --porcelain -- examples/lox std 2>/dev/null)"
rm -f "$TMP/cc.log"
drive "$TMP/p1" --reps 2 --scaling-size 50; rc=$?
[ "$rc" = 0 ] && ok "profile of lox, std and scaling exits 0" || { no "profile exit $rc"; sed 's/^/      /' "$TMP/drive.out" | tail -20; }
[ "$(jq_py "$TMP/p1/profile.json" '",".join(w["name"] + "=" + w["status"] for w in d["workloads"])')" = "lox=ok,std=ok,scaling=ok" ] \
  && ok "three workloads profiled" || no "workloads: $(jq_py "$TMP/p1/profile.json" '[w["name"] for w in d["workloads"]]' 2>&1)"
[ "$(grep -c '|build --emit-llvm|$' "$TMP/cc.log" 2>/dev/null)" = 4 ] && [ "$(grep -c '|scaling.con --emit-llvm|$' "$TMP/cc.log")" = 2 ] \
  && ok "the compiler ran build --emit-llvm (lox, std) and FILE --emit-llvm (scaling), twice each" || no "compiler invocations: $(cat "$TMP/cc.log" 2>/dev/null)"
grep -q "^$ROOT_DIR/\(examples/lox\|std\)|" "$TMP/cc.log" && no "a workload was built inside the checkout" || ok "workloads are built in copies, not in the checkout"
[ "$before" = "$(git status --porcelain -- examples/lox std 2>/dev/null)" ] && ok "the checkout is unchanged" || no "the profile wrote into examples/lox or std"
[ "$(ls "$TMP/p1/raw" | wc -l | tr -d ' ')" = 6 ] && ok "raw/ keeps every leanprof report" || no "raw/: $(ls "$TMP/p1/raw")"
grep -q '^## lox' "$TMP/p1/summary.md" && grep -q '| lower |' "$TMP/p1/summary.md" && ok "summary.md has stages and hot spots" || no "summary.md"
[ "$(jq_py "$TMP/p1/profile.json" 'd["mode"] + " " + str(d["leanprof"].startswith(("git:", "sha256:"))) + " " + d["compiler"]')" = "sample True concrete fake" ] \
  && ok "the profile records mode, leanprof and compiler versions" || no "profile metadata"
cp "$TMP/p1/profile.json" "$TMP/baseline.json"
drive "$TMP/p2" --reps 1 --workloads lox --baseline "$TMP/baseline.json"; rc=$?
[ "$rc" = 0 ] && [ -f "$TMP/p2/comparison.md" ] && ok "unchanged against the baseline: exit 0, comparison written" || no "baseline, unchanged: exit $rc"
FAKE_LP_VARIANT=quadratic drive "$TMP/p3" --reps 1 --workloads lox --baseline "$TMP/baseline.json"; rc=$?
[ "$rc" = 1 ] && grep -q lowerQuadraticControl "$TMP/p3/comparison.md" && ok "the control against the baseline: exit 1, finding in comparison.md" || no "control via the driver: exit $rc"
FAKE_LP_FAIL=std- drive "$TMP/p4" --reps 1 --scaling-size 50; rc=$?
[ "$rc" = 3 ] && [ "$(jq_py "$TMP/p4/profile.json" '{w["name"]: w["status"] for w in d["workloads"]}["std"]')" = failed ] \
  && ok "a failing workload: exit 3, status failed, the profile still written" || no "failing workload: exit $rc"
FAKE_LP_HANG=lox- drive "$TMP/p5" --reps 1 --workloads lox,std --budget 1; rc=$?
[ "$rc" = 3 ] && [ "$(jq_py "$TMP/p5/profile.json" '",".join(w["status"] for w in d["workloads"])')" = "incomplete,ok" ] \
  && ok "a hang is INCOMPLETE under bounded_run (exit 3), the next workload still runs" || no "hang: exit $rc"
mkdir -p "$TMP/corpus"; printf 'fn main() -> Int { return 0; }\n' > "$TMP/corpus/a.con"; cp "$TMP/corpus/a.con" "$TMP/corpus/b.con"
drive "$TMP/p6" --reps 1 --workloads scaling --scaling-corpus "$TMP/corpus"; rc=$?
[ "$rc" = 0 ] && [ "$(jq_py "$TMP/p6/profile.json" '",".join(w["name"] for w in d["workloads"])')" = "scaling:a,scaling:b" ] \
  && ok "one workload per scaling-corpus file (R-0550's corpus)" || no "scaling corpus: exit $rc"
drive "$TMP/p7" --mode auto --workloads lox; rc=$?
[ "$rc" = 0 ] && [ "$(jq_py "$TMP/p7/profile.json" 'd["mode"] + str(d["workloads"][0]["reps"])')" = auto1 ] \
  && ok "auto mode: one leanprof run --auto per workload" || no "auto mode: exit $rc"
mkdir -p "$TMP/src/Concrete" "$TMP/src/examples/lox" "$TMP/src/std"; touch "$TMP/src/lakefile.toml"
for d in examples/lox std; do touch "$TMP/src/$d/Concrete.toml"; echo from-source > "$TMP/src/$d/.marker"; done
rm -f "$TMP/cc.log"
drive "$TMP/p9" --reps 1 --workloads lox,std --source "$TMP/src"; rc=$?
[ "$rc" = 0 ] && [ "$(grep -c '|from-source$' "$TMP/cc.log" 2>/dev/null)" = 2 ] \
  && ok "--source: lox and std come from the given source tree" || no "--source: exit $rc, $(cat "$TMP/cc.log" 2>/dev/null)"
drive "$TMP/p10" --source "$TMP/corpus"; rc=$?
[ "$rc" = 2 ] && ok "--source that is not a concrete tree is refused" || no "bad --source: exit $rc"
env IN_NIX_SHELL=impure PATH="$FAKEBIN:$PATH" FAKE_LP_LOG="$TMP/lp.log" bash "$DRV" profile --leanprof "$LPF" --out "$TMP/p11" --workloads lox >/dev/null 2>&1; rc=$?
[ "$rc" = 0 ] && [ "$(cut -d'|' -f1 "$TMP/lp.log")" = auto ] && ok "the default mode is auto (any perf_event_paranoid)" || no "default mode: exit $rc"
cp -R "$TMP/src" "$TMP/src-nostd"; rm -rf "$TMP/src-nostd/std"
drive "$TMP/p12" --reps 1 --workloads lox,std --source "$TMP/src-nostd"; rc=$?
[ "$rc" = 3 ] && [ "$(jq_py "$TMP/p12/profile.json" '",".join(w["name"] + "=" + w["status"] for w in d["workloads"])')" = "lox=ok,std=failed" ] \
  && ok "a workload that cannot be copied is failed (exit 3), never left out" || no "uncopyable workload: exit $rc"
mkdir -p "$TMP/corpus2"; printf 'fn main() -> Int { return 0; }\n' > "$TMP/corpus2/a+b.con"; cp "$TMP/corpus2/a+b.con" "$TMP/corpus2/a_b.con"
drive "$TMP/p13" --reps 1 --workloads scaling --scaling-corpus "$TMP/corpus2"; rc=$?
[ "$rc" = 0 ] && [ "$(ls "$TMP/p13/raw" | wc -l | tr -d ' ')" = 2 ] \
  && [ "$(jq_py "$TMP/p13/profile.json" '",".join(w["name"] for w in d["workloads"])')" = "scaling:a+b,scaling:a_b" ] \
  && ok "two corpus names that sanitize alike keep separate reports" || no "name collision: exit $rc"
( env IN_NIX_SHELL=impure PATH="$FAKEBIN:$PATH" timeout 20 bash "$DRV" profile --leanprof "$LPF" --out ) >/dev/null 2>&1; rc=$?
[ "$rc" = 2 ] && ok "an option without its value is a usage error, not a loop" || no "trailing option: exit $rc"
drive "$TMP/p8" --workloads nope; rc=$?
[ "$rc" = 2 ] && ok "an unknown workload is refused" || no "unknown workload: exit $rc"
a="$(bash "$DRV" gen-corpus 40 | cksum)"; b="$(bash "$DRV" gen-corpus 40 | cksum)"
[ "$a" = "$b" ] && bash "$DRV" gen-corpus 40 | has_match '^fn main() -> Int {' && ok "gen-corpus is deterministic and has a main" || no "gen-corpus"

echo "F  runner (one cycle per landed commit)"
RUN="scripts/perf/leanprof_runner.sh"
printf '#!/bin/sh\nwhile [ $# -gt 0 ] && [ "$1" != --command ]; do shift; done; shift; exec "$@"\n' > "$FAKEBIN/nix"; chmod +x "$FAKEBIN/nix"
ORIGIN="$TMP/origin"; mkdir -p "$ORIGIN/Concrete" "$ORIGIN/examples/lox" "$ORIGIN/std"
touch "$ORIGIN/lakefile.toml" "$ORIGIN/examples/lox/Concrete.toml" "$ORIGIN/std/Concrete.toml"
git -C "$ORIGIN" init -q -b main && git -C "$ORIGIN" -c user.name=t -c user.email=t@t add -A
commit() { echo "$1" > "$ORIGIN/Concrete/c.txt"; git -C "$ORIGIN" -c user.name=t -c user.email=t@t add -A; git -C "$ORIGIN" -c user.name=t -c user.email=t@t commit -q -m "$1"; git -C "$ORIGIN" rev-parse HEAD; }
LH="$TMP/lphome"
cycle() { env IN_NIX_SHELL=impure PATH="$FAKEBIN:$PATH" LEANPROF="$LPF" LP_HOME="$LH" LP_REMOTE_URL="$ORIGIN" \
  CONCRETE_NOTIFY_ENV=/nonexistent LP_PROFILE_ARGS="--workloads lox" "$@" bash "$RUN" > "$TMP/cycle.out" 2>&1; }
C0="$(commit c0)"; C1="$(commit c1)"
cycle; rc=$?
[ "$rc" = 0 ] && [ -f "$LH/history/$C1/profile.json" ] && ! [ -e "$LH/history/$C0" ] && ! [ -f "$LH/history/$C1/comparison.json" ] \
  && ok "first cycle: origin/main only, no baseline" || { no "first cycle: exit $rc"; tail -5 "$TMP/cycle.out" | sed 's/^/      /'; }
[ "$(readlink "$LH/out/latest")" = "$LH/history/$C1" ] && grep -q "${C1:0:12}" "$LH/out/index.md" && ok "index.md and latest published" || no "publish"
cycle; rc=$?; [ "$rc" = 0 ] && grep -q "nothing to profile" "$TMP/cycle.out" && ok "nothing new: exit 0" || no "idle cycle: exit $rc"
C2="$(commit c2)"; C3="$(commit c3)"
cycle; [ -f "$LH/history/$C2/profile.json" ] && ! [ -e "$LH/history/$C3" ] && [ "$(jq_py "$LH/history/$C2/comparison.json" 'd["base_sha"]')" = "$C1" ] \
  && ok "two landings: the older one first, compared with the previous profiled commit" || no "queue order"
cycle FAKE_LP_VARIANT=quadratic; rc=$?
[ "$rc" = 0 ] && [ "$(jq_py "$LH/history/$C3/comparison.json" 'd["base_sha"]')" = "$C2" ] \
  && grep -q "\"sha\": \"$C3\"" "$LH/findings.jsonl" && grep -q lowerQuadraticControl "$LH/findings.jsonl" \
  && grep -q "profile finding ${C3:0:12}" "$TMP/cycle.out" \
  && ok "control at a landed commit: finding recorded in findings.jsonl and notified" || no "runner control: exit $rc"
C4="$(commit c4)"
for i in 1 2 3; do cycle FAKE_LAKE_FAIL=1; done
[ -f "$LH/history/$C4/failed.json" ] && grep -q "FAILED" "$LH/out/index.md" && ok "a commit that does not build, three times: failed.json, shown in the index" || no "failed commit"
C5="$(commit c5)"; cycle
[ "$(jq_py "$LH/history/$C5/comparison.json" 'd["base_sha"]')" = "$C3" ] && ok "after a failed commit the baseline is the last profiled one" || no "baseline after failure"
cycle env -u LEANPROF; rc=$?; [ "$rc" = 1 ] && ok "no LEANPROF: harness error (exit 1)" || no "missing LEANPROF: exit $rc"

echo "E  shell syntax"
for f in "$DRV" "$RUN" "$FIX/fake_leanprof.sh" "$FIX/fake_compiler.sh"; do
  bash -n "$f" && ok "bash -n $(basename "$f")" || no "bash -n $f"
done

echo "check_leanprof_profile: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
