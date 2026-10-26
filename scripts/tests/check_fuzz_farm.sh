#!/usr/bin/env bash
# Fuzz-farm gate (scripts/fuzz/farm/). Build-free: it drives the farm engine with a FAKE
# compiler and FAKE generators (tests/fixtures/fuzz_farm/), so it runs in CI's grammar job and
# says nothing about the real compiler. What it pins is the farm's own logic:
#   A  classification   silent / crash / unsound / timeout / refusal / agree, on fixture programs
#   B  campaign + dedup  repeated planted bugs collapse to ONE signature each, with counts;
#                        a crash reached by two generators is one signature
#   C  minimizer         every minimized.con re-classifies to its finding's pre-signature, is no
#                        larger than repro.con, and the linearity pair-minimizer keeps the
#                        planted `pass(` step while collapsing the rest
#   D  notify            one notify.sh call per NEW silent/crash/unsound signature, none else
#   E  re-check          a main that now refuses the program keeps it open (refused-on-main);
#                        against a "fixed" compiler the silent finding becomes
#                        `fixed-on-main <sha>`; the still-crashing one stays open; a repeat
#                        after the fix reopens it
#   F  promote           byte-exact against tests/fixtures/fuzz_farm/promote.golden, and --write
#                        puts the fixture where it says, body == minimized.con
#   G  drift             a refusal-heavy generator raises a drift alert (status JSON too) and
#                        does NOT file one finding per refusal; a clean batch clears it
#   H  status JSON       the status-page contract fields exist
#   I  shell syntax      farm.sh, install-farm.sh, promote.sh, predicate.sh parse
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
FARM="python3 scripts/fuzz/farm/farm.py"
FIX="$ROOT_DIR/tests/fixtures/fuzz_farm"
CC="$FIX/fake_compiler.py"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
export PYTHONDONTWRITEBYTECODE=1

# Bug 263: coverage comes from completed batches, not elapsed wall time. The generous
# deadline is a watchdog; an incomplete bounded campaign must fail, even if it found bugs.
if python3 scripts/tests/check_fuzz_farm_batches.py; then
  ok "bounded campaigns stop at complete work and reject incomplete coverage"
else no "bounded campaign controls"; fi

cls_of() {  # cls_of <oracle> <file> [params-json]  -> prints the class (or None)
  local params=()
  [ $# -ge 3 ] && params=(--params "$3")
  $FARM classify --compiler "$CC" --oracle "$1" "${params[@]}" "$2" 2>/dev/null |
    python3 -c 'import json,sys; print(json.load(sys.stdin)["class"])'
}
expect_cls() {  # expect_cls <label> <want> <oracle> <file> [params]
  local label="$1" want="$2"; shift 2
  local got; got="$(cls_of "$@")"
  [ "$got" = "$want" ] && ok "$label -> $want" || no "$label: want $want, got '$got'"
}

echo "=== A: classification ==="
expect_cls "value/value disagreement" silent diff "$FIX/cases/silent.con"
expect_cls "compiler panic" crash diff "$FIX/cases/crash.con"
expect_cls "checker refusal of a generated program" refusal diff "$FIX/cases/refusal.con"
expect_cls "agreeing program" None diff "$FIX/cases/agree.con"
FARM_T_RUN=0.2 expect_cls "native hang (re-run at 4x first)" timeout diff "$FIX/cases/hang.con"
expect_cls "native SIGSEGV is a native crash, not trap agreement" silent diff "$FIX/cases/segv.con"
expect_cls "runaway output hits the cap: resource limit, not a finding" None diff "$FIX/cases/spam.con"
expect_cls "accepted leak" unsound lin "$FIX/cases/lin_leak_accepted.con" '{"must_compile": false, "kind": "leak"}'
expect_cls "rejected leak (E0208)" None lin "$FIX/cases/lin_leak_rejected.con" '{"must_compile": false, "kind": "leak"}'
expect_cls "mutation oracle ignores clean refusals" None mut "$FIX/cases/refusal.con"
expect_cls "mutation oracle still catches a crash" crash mut "$FIX/cases/crash.con"
if $FARM classify --compiler "$TMP/no-such-compiler" "$FIX/cases/agree.con" >/dev/null 2>&1; then
  no "a missing compiler must be an infrastructure error, not a finding"
else ok "a missing compiler is an infrastructure error (nonzero, no class)"; fi

echo "=== B/C/D: campaign, dedup, minimizer, notify ==="
H="$TMP/home"; SNAP="$TMP/snap"
mkdir -p "$SNAP/scripts/land"
cat > "$SNAP/scripts/land/notify.sh" <<EOF
#!/usr/bin/env bash
echo "\$1|\$2" >> "$TMP/notify.log"
EOF
chmod +x "$SNAP/scripts/land/notify.sh"
FARM_HOME="$H" FARM_PUBLIC="$H/pub" FAKE_GEN_MODE=bugs $FARM campaign --compiler "$CC" \
  --gen-dir "$FIX/gen" --sha 1111111 --snapshot "$SNAP" --seconds 180 --batches 3 --batch 20 --seed 7 \
  --generators diff,lin,mut --no-lean-reduce > "$TMP/campaign.log" 2>&1
rc=$?
[ "$rc" -eq 0 ] && ok "campaign exits 0" || { no "campaign exit $rc"; tail -20 "$TMP/campaign.log"; }
python3 - "$H" "$CC" <<'PY' > "$TMP/bc.txt" 2>&1
import json, os, subprocess, sys
h, cc = sys.argv[1:3]
idx = json.load(open(os.path.join(h, "findings", "findings.json")))
sigs = idx["signatures"]
by = {}
for s, e in sigs.items():
    by.setdefault(e["class"], []).append(s)
def say(ok, msg): print(("OK " if ok else "NO ") + msg)
for c in ("silent", "crash", "unsound"):
    n = len(by.get(c, []))
    say(n == 1, f"exactly one {c} signature (got {n})")
    if n:
        say(sigs[by[c][0]]["count"] >= 2, f"{c} repeats counted on one signature (count={sigs[by[c][0]]['count']})")
crash = sigs[by["crash"][0]] if by.get("crash") else {}
gens = {o["generator"] for o in crash.get("occurrences", [])}
say(gens >= {"diff", "mut"}, f"one crash signature across generators {sorted(gens)}")
for s, e in sigs.items():
    d = os.path.join(h, "findings", s)
    meta = json.load(open(os.path.join(d, "meta.json")))
    mn, rp = open(os.path.join(d, "minimized.con")).read(), open(os.path.join(d, "repro.con")).read()
    say(len(mn) <= len(rp), f"{s}: minimized ({len(mn)}B) no larger than repro ({len(rp)}B)")
    r = subprocess.run([sys.executable, "scripts/fuzz/farm/farm.py", "classify", "--compiler", cc,
                        "--oracle", meta["oracle"], "--params", json.dumps(meta["params"]),
                        os.path.join(d, "minimized.con")], capture_output=True, text=True)
    got = json.loads(r.stdout)["presig"] if r.stdout else None
    say(got == meta["presig"], f"{s}: minimized.con keeps the signature ({got})")
    for f in ("repro.con", "minimized.con", "meta.json", "log"):
        say(os.path.exists(os.path.join(d, f)), f"{s}: has {f}")
if by.get("unsound"):
    mn = open(os.path.join(h, "findings", by["unsound"][0], "minimized.con")).read()
    lets = [l for l in mn.splitlines() if "let v" in l]
    say(mn.count("pass(") == 1 and len(lets) == 2,
        f"linearity pair-minimizer: one planted pass( step and 2 lets left ({len(lets)})")
PY
while read -r verdict msg; do
  [ "$verdict" = OK ] && ok "$msg" || no "$msg"
done < "$TMP/bc.txt"
grep -q "Traceback" "$TMP/bc.txt" && { no "dedup/minimizer checker crashed"; cat "$TMP/bc.txt"; }
nn="$(wc -l < "$TMP/notify.log" 2>/dev/null || echo 0)"
[ "$nn" -eq 3 ] && ok "notify.sh called once per new silent/crash/unsound signature (3)" \
  || no "notify.sh calls: want 3, got $nn"
grep -q "^error|fuzz-farm: NEW silent" "$TMP/notify.log" 2>/dev/null && ok "silent notifies at level error" \
  || no "silent notification missing or wrong level"

echo "=== E: re-check against a fixed main ==="
FARM_HOME="$H" FAKE_REJECT_ALL=1 $FARM recheck --compiler "$CC" --sha 1500000 > "$TMP/recheck0.log" 2>&1
python3 - "$H" <<'PY' > "$TMP/e0.txt"
import json, os, sys
idx = json.load(open(os.path.join(sys.argv[1], "findings", "findings.json")))
st = {e["class"]: e for e in idx["signatures"].values()}
s = st.get("silent", {})
print(("OK " if s.get("status") == "open" and s.get("last_recheck", {}).get("refused_on_main") else "NO ")
      + f"a main that now REFUSES the program does not mark it fixed -> {s.get('status')} {s.get('last_recheck')}")
PY
while read -r verdict msg; do [ "$verdict" = OK ] && ok "$msg" || no "$msg"; done < "$TMP/e0.txt"
FARM_HOME="$H" FAKE_FIXED=1 $FARM recheck --compiler "$CC" --sha 2222222 > "$TMP/recheck.log" 2>&1
python3 - "$H" <<'PY' > "$TMP/e.txt"
import json, os, sys
idx = json.load(open(os.path.join(sys.argv[1], "findings", "findings.json")))
st = {e["class"]: e["status"] for e in idx["signatures"].values()}
print(("OK " if st.get("silent") == "fixed-on-main 2222222" else "NO ") + f"silent finding -> {st.get('silent')}")
print(("OK " if st.get("crash") == "open" else "NO ") + f"crash finding stays open -> {st.get('crash')}")
print(("OK " if st.get("unsound") == "open" else "NO ") + f"unsound finding stays open -> {st.get('unsound')}")
PY
while read -r verdict msg; do [ "$verdict" = OK ] && ok "$msg" || no "$msg"; done < "$TMP/e.txt"
# main is now 2222222 (the snapshot the re-check used): a worker still on the OLD snapshot
# 1111111 reproduces the bug, which must NOT reopen it; one on the current main must.
mkdir -p "$H/current"; echo 2222222 > "$H/current/SHA"
FARM_HOME="$H" FARM_PUBLIC="$H/pub" FAKE_GEN_MODE=bugs $FARM campaign --compiler "$CC" \
  --gen-dir "$FIX/gen" --sha 1111111 --seconds 180 --batches 1 --batch 10 --seed 9 --generators diff \
  --no-lean-reduce > /dev/null 2>&1 || no "recheck campaign incomplete"
python3 - "$H" <<'PY' > "$TMP/e2.txt"
import json, os, sys
idx = json.load(open(os.path.join(sys.argv[1], "findings", "findings.json")))
e = [e for e in idx["signatures"].values() if e["class"] == "silent"][0]
print(("OK " if e["status"] == "fixed-on-main 2222222" else "NO ") +
      f"an occurrence on an older snapshot does not reopen a fixed finding -> {e['status']}")
PY
FARM_HOME="$H" FARM_PUBLIC="$H/pub" FAKE_GEN_MODE=bugs $FARM campaign --compiler "$CC" \
  --gen-dir "$FIX/gen" --sha 2222222 --seconds 180 --batches 1 --batch 10 --seed 9 --generators diff \
  --no-lean-reduce > /dev/null 2>&1 || no "recheck campaign incomplete"
python3 - "$H" <<'PY' >> "$TMP/e2.txt"
import json, os, sys
idx = json.load(open(os.path.join(sys.argv[1], "findings", "findings.json")))
e = [e for e in idx["signatures"].values() if e["class"] == "silent"][0]
print(("OK " if e["status"] == "open" and "reopened" in e else "NO ") +
      f"an occurrence on the current main reopens it -> {e['status']}")
PY
rm -rf "$H/current"
while read -r verdict msg; do [ "$verdict" = OK ] && ok "$msg" || no "$msg"; done < "$TMP/e2.txt"

echo "=== F: promote ==="
if FARM_HOME="$FIX" $FARM promote silent-000000f1x7e0 | diff -u "$FIX/promote.golden" - > "$TMP/promote.diff"; then
  ok "promote output matches promote.golden"
else no "promote output drifted:"; head -20 "$TMP/promote.diff"; fi
mkdir -p "$TMP/repo"
FARM_HOME="$FIX" $FARM promote silent-000000f1x7e0 --repo "$TMP/repo" --write > /dev/null
f="$TMP/repo/tests/regressions/fuzz_silent_000000f1/repro.con"
if [ -f "$f" ] && head -1 "$f" | grep -q '^// Fuzz-farm finding silent-000000f1x7e0' \
   && [ "$(grep -v '^//' "$f")" = "$(cat "$FIX/findings/silent-000000f1x7e0/minimized.con")" ]; then
  ok "--write places the fixture under tests/regressions/ with header + minimized body"
else no "--write fixture missing or malformed ($f)"; fi
rp="$(sed -n 's|^// Replay: python3 \([^ ]*\) .*|\1|p' "$FIX/promote.golden")"
[ -n "$rp" ] && [ -f "$rp" ] && ok "the replay command names a real script ($rp)" \
  || no "the replay command names no real script ('$rp')"
if FARM_HOME="$FIX" $FARM promote silent-doesnotexist >/dev/null 2>&1; then
  no "promote of an unknown signature must fail"; else ok "promote of an unknown signature fails"; fi

echo "=== G: generator drift ==="
D="$TMP/drift"
FARM_HOME="$D" FARM_PUBLIC="$D/pub" FAKE_GEN_MODE=drift $FARM campaign --compiler "$CC" \
  --gen-dir "$FIX/gen" --sha 4444444 --seconds 180 --batches 1 --batch 30 --seed 3 --generators diff \
  --no-lean-reduce > "$TMP/drift.log" 2>&1 || no "drift campaign incomplete"
FARM_HOME="$D" FARM_PUBLIC="$D/pub" $FARM status >/dev/null
python3 - "$D" <<'PY' > "$TMP/g.txt"
import json, os, sys
d = sys.argv[1]
dr = json.load(open(os.path.join(d, "state", "drift.json")))
idx = json.load(open(os.path.join(d, "findings", "findings.json")))
st = json.load(open(os.path.join(d, "pub", "fuzz-status.json")))
a = dr.get("diff", {})
print(("OK " if a.get("alert") and a.get("rate", 0) > 0.3 else "NO ") + f"drift alert raised (rate {a.get('rate')})")
print(("OK " if a.get("top_codes") and a["top_codes"][0][0] == "E0100" else "NO ") + f"drift names the refusing code {a.get('top_codes')}")
ref = [e for e in idx["signatures"].values() if e["class"] == "refusal"]
tot = sum(e["count"] for e in ref)
import glob
stats = [json.load(open(f)) for f in glob.glob(os.path.join(d, "state", "stats", "*", "*.json"))]
batches = sum(s.get("drift_batches", 0) for s in stats)
refused = sum(s.get("outcomes", {}).get("diff:refusal", 0) for s in stats)
print(("OK " if batches >= 1 and tot <= batches and refused > 3 * tot else "NO ") +
      f"refusals under drift: one sample per drift batch, not one per case "
      f"(filed {tot}, drift batches {batches}, refused cases {refused})")
print(("OK " if "diff" in st.get("drift_alerts", {}) else "NO ") + "status JSON carries the drift alert")
PY
while read -r verdict msg; do [ "$verdict" = OK ] && ok "$msg" || no "$msg"; done < "$TMP/g.txt"
FARM_HOME="$D" FARM_PUBLIC="$D/pub" FAKE_GEN_MODE=clean $FARM campaign --compiler "$CC" \
  --gen-dir "$FIX/gen" --sha 4444444 --seconds 180 --batches 1 --batch 30 --seed 4 --generators diff \
  --no-lean-reduce > /dev/null 2>&1 || no "clean campaign incomplete"
python3 -c 'import json,sys; a=json.load(open(sys.argv[1]))["diff"]; sys.exit(0 if not a["alert"] else 1)' \
  "$D/state/drift.json" && ok "a clean batch clears the drift alert" || no "drift alert did not clear"

echo "=== H: status JSON contract ==="
FARM_HOME="$H" FARM_PUBLIC="$H/pub" $FARM status >/dev/null
python3 - "$H/pub/fuzz-status.json" <<'PY' > "$TMP/h.txt"
import json, sys
s = json.load(open(sys.argv[1]))
need = ["schema", "generated_at", "compiler_sha", "classes", "totals", "today", "drift_alerts", "paused"]
miss = [k for k in need if k not in s]
print(("OK " if not miss else "NO ") + f"top-level fields present (missing {miss})")
cls = s.get("classes", {})
ok = all(set(cls.get(c, {})) >= {"open", "fixed", "new_today"} for c in ["silent", "crash", "unsound", "timeout", "refusal"])
print(("OK " if ok else "NO ") + "per-class open/fixed/new_today counts")
print(("OK " if s.get("totals", {}).get("fixed") == 0 and s["totals"]["open"] >= 3 else "NO ") + f"totals {s.get('totals')}")
PY
while read -r verdict msg; do [ "$verdict" = OK ] && ok "$msg" || no "$msg"; done < "$TMP/h.txt"
FARM_HOME="$H" $FARM summary > /dev/null && [ -s "$H/reports/summary-$(date -u +%F).md" ] \
  && ok "daily summary written" || no "daily summary missing"

echo "=== I: shell syntax ==="
for s in farm.sh install-farm.sh promote.sh predicate.sh; do
  bash -n "scripts/fuzz/farm/$s" && ok "$s parses" || no "$s does not parse"
done

echo ""
echo "FUZZ-FARM: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
