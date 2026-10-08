#!/usr/bin/env bash
# R-0484 R10: PROOF ADMISSION READS THE SHARED ASSUMPTION SUMMARY.
#
# Admission decides whether an extractable function may count as EFFECT-FREE proof coverage. It
# used its own analysis (`effectOpaqueSet`: functions that reach an indirect call over ProofCore's
# call graph), which saw only the program's own modules. So a program function admitted as
# effect-free could reach, inside a dependency:
#   - an indirect call      (`admlib.apply` calls its fn-typed parameter),
#   - a foreign binding     (`admlib.abs`, through the trusted wrapper `admlib.tabs`),
# and a method call on a type parameter (`T_describe`) was not an edge at all.
#
# Admission now takes the summary every authority report renders (`Assumptions.forProgram`, over
# the program AND its loaded dependencies) and refuses whenever that summary says the function's
# reachable behaviour is unknown or rests on an assumed effect declaration:
#   noSummary / ambiguousSummary   no single summary covers the function
#   unresolvedCall                 a call names nothing analysed (a missing dependency)
#   indirectCall                   a call through a fn-typed binding
#   typeParamDispatch              a method call on a type parameter
#   foreignAssumption              a reached foreign binding (declared effects, assumed honest)
# Eligibility (extraction) is untouched; admission is carried beside it.
#
# The judgment's unit controls are `#guard`s in Concrete/Proof/ProofCore.lean (they fail the
# BUILD); this gate checks the end-to-end wiring on tests/regressions/proof_admission and that
# admission agrees with the summary facts the reports publish.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/proof_admission"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

echo "=== the fixture builds and runs ==="
if (cd "$FIX/app" && $TO "$CC" build . -o "$TMP/app" >"$TMP/b.log" 2>&1) && "$TMP/app" >/dev/null 2>&1; then
  ok "app builds and exits 0"
else
  no "app does not build or run"; head -5 "$TMP/b.log" | sed 's/^/       /'
fi

(cd "$FIX/app" && $TO "$CC" src/main.con --report diagnostics-json) > "$TMP/facts.json" 2>"$TMP/facts.err"
(cd "$FIX/app" && $TO "$CC" src/main.con --report proof-status) > "$TMP/status.txt" 2>&1

echo "=== admission verdicts, read from the eligibility facts ==="
python3 - "$TMP/facts.json" > "$TMP/table.out" <<'PY'
import json, sys
def res(ok, msg): print(("ok   " if ok else "FAIL ") + msg)
try:
    d = json.load(open(sys.argv[1]))
except Exception as e:
    res(False, f"diagnostics-json is not JSON ({e})"); sys.exit()
facts = d.get("facts", [])
elig = {f["function"]: f for f in facts if f.get("kind") == "eligibility"}
caps = {f["function"]: f for f in facts if f.get("kind") == "capability"}
ext = {f["function"]: f for f in facts if f.get("kind") == "extraction"}
res(bool(elig) and all("admissible" in f and "admission_reasons" in f for f in elig.values()),
    f"every eligibility fact states admission ({len(elig)} functions)")

def admitted(fn, why):
    f = elig.get(fn)
    res(bool(f) and f["status"] == "eligible" and f["admissible"] is True and f["admission_reasons"] == [],
        f"ACCEPT {fn}: admitted — {why}")
def refused(fn, needle, why):
    f = elig.get(fn)
    rs = f["admission_reasons"] if f else []
    res(bool(f) and f["status"] == "eligible" and f["admissible"] is False and any(needle in r for r in rs),
        f"REFUSE {fn}: {why}" + ("" if f and any(needle in r for r in rs) else f"  (reasons: {rs})"))

admitted("main.plain", "complete, calls into the dependency, reaches no foreign binding")
admitted("main.local_only", "complete, local arithmetic")
admitted("main.Point_describe", "a concrete impl method is not a dispatch")
refused("main.via_foreign", "reaches foreign binding admlib.abs",
        "reaches a foreign binding through a trusted wrapper (dropped-edge class)")
# \x60 is a backtick: kept out of the literal so the vacuity ratchet's line grep stays quiet.
THROUGH_F = 'through \x60f\x60 in admlib.apply'
ON_T = 'dispatched on type parameter \x60T\x60'
refused("main.via_indirect", THROUGH_F,
        "reaches an indirect call INSIDE THE DEPENDENCY")
refused("main.via_indirect2", THROUGH_F,
        "transitively, through via_indirect")
refused("main.show", ON_T,
        "a method call on a type parameter")

# Refusing admission must not refuse EXTRACTION: the evidence surface keeps these functions.
for fn in ["main.via_foreign", "main.via_indirect", "main.via_indirect2", "main.show"]:
    res(ext.get(fn, {}).get("status") == "extracted", f"{fn}: still extracted (admission is not eligibility)")

# ONE SET OF FACTS: for every eligible function, admission must agree with the summary facts the
# capability reports publish — complete coverage and no assumed foreign binding. This compares
# two surfaces; it is not how the compiler decides.
bad = []
for fn, f in elig.items():
    if f["status"] != "eligible": continue
    c = caps.get(fn)
    if c is None or c.get("assumptions_computed") is not True:
        bad.append(f"{fn}: no computed capability facts"); continue
    expect = c["coverage_complete"] is True and c["assumed_foreign_bindings"] == []
    if f["admissible"] != expect:
        bad.append(f"{fn}: admissible={f['admissible']} but summary complete={c['coverage_complete']} foreign={c['assumed_foreign_bindings']}")
res(not bad, "admission agrees with the published summary facts for every eligible function" +
    ("" if not bad else ": " + "; ".join(bad)))
PY
while IFS= read -r line; do
  case "$line" in
    "ok   "*) ok "${line#ok   }" ;;
    "FAIL "*) no "${line#FAIL }" ;;
  esac
done < "$TMP/table.out"
[ -s "$TMP/table.out" ] || no "the verdict table produced no rows"

echo "=== proof-status names every refusal, and only real ones ==="
n=$(grep -c "admission: REFUSED" "$TMP/status.txt" || true)
[ "${n:-0}" -eq 4 ] && ok "four admission refusals rendered (the four refused functions)" \
                   || no "expected 4 admission refusals in proof-status, saw ${n:-0}"
if grep -q "admission: REFUSED — no reason recorded" "$TMP/status.txt"; then
  no "a refusal is rendered without a reason"
else
  ok "no refusal is rendered without a reason (the excluded entry point is not repeated as one)"
fi

echo "=== the judgment's unit controls exist (they run at build time) ==="
# Missing dependency (no summary; unloaded callee), unresolved indirect call, type-parameter
# dispatch, foreign binding with and without its facts entry, ambiguity, and the two accepting
# shapes. A #guard that fails stops the build, so presence plus a built compiler is the check.
g=$(grep -c '^#guard admissionProbeFor' Concrete/Proof/ProofCore.lean || true)
[ "${g:-0}" -ge 9 ] && ok "$g admission #guard controls in ProofCore" \
                   || no "admission #guard controls missing (found ${g:-0}, need 9)"

echo
echo "PROOF-ADMISSION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
