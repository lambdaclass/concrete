#!/usr/bin/env bash
# R-0484 R10: CAPABILITY EXPLANATIONS FOLLOW DEPENDENCY SUMMARIES.
#
# "Why does this function need Console?" was answered from the program's own modules only. A
# capability supplied by a dependency callee read `<- declared` (as if nothing supplied it), and a
# dependency definition whose spelling happens to match an intrinsic read `Vec_pop (intrinsic)`.
#
# One producer (`capSuppliers`) now resolves each callee the way a call resolves — a program
# definition, then a DEPENDENCY definition from the summary's index (through import aliases, as the
# summary does), then an intrinsic — and the caps text, the authority text and the diagnostics-json
# `why` suppliers all render from it.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
APP="$ROOT_DIR/tests/regressions/assumption_summary/app"
B64="$ROOT_DIR/examples/base64_cli"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

for p in app:"$APP" b64:"$B64"; do
  n="${p%%:*}"; d="${p#*:}"
  (cd "$d" && $TO "$CC" src/main.con --report caps) > "$TMP/$n.caps" 2>&1
  (cd "$d" && $TO "$CC" src/main.con --report authority) > "$TMP/$n.auth" 2>&1
  (cd "$d" && $TO "$CC" src/main.con --report diagnostics-json) > "$TMP/$n.diag" 2>/dev/null
done

python3 - "$TMP" > "$TMP/rows.out" <<'PY'
import json, re, sys
T = sys.argv[1]
def res(ok, msg): print(("ok   " if ok else "FAIL ") + msg)

def caps_why(path, module):
    """(fn, cap) -> why string, from --report caps."""
    out, fn = {}, None
    for line in open(path).read().splitlines():
        m = re.match(r"\s{6}(\w+) : ", line)
        if m: fn = f"{module}.{m.group(1)}"; continue
        m = re.match(r"\s{4}(\w+)\s+(<- .*)$", line)
        if m and fn: out[(fn, m.group(1))] = m.group(2)
    return out

app = caps_why(f"{T}/app.caps", "main")
res(app.get(("main.via_helpers", "Console")) == "<- calls factlib.helper_a (dependency)",
    "via_helpers: Console comes from factlib.helper_a, through the import alias 'ha' (was: declared)")
w = app.get(("main.via_recursion", "Console"), "")
res("factlib.countdown (dependency)" in w and "factlib.even (dependency)" in w,
    "via_recursion: both dependency callees are named")
auth = open(f"{T}/app.auth").read()
res(re.search(r"via_helpers\s+<- calls factlib\.helper_a \(dependency\)", auth) is not None,
    "the authority report names the same dependency supplier")

# JSON `why` suppliers agree with the caps text for every (function, capability).
def diag_why(path):
    d = json.load(open(path)); out = {}
    for f in d["facts"]:
        if f.get("kind") != "capability" or f.get("is_extern"): continue
        for w in f.get("why", []):
            out[(f["function"], w["capability"])] = w.get("suppliers")
    return out
bad = []
for name, module in (("app", "main"), ("b64", "base64_cli")):
    text = caps_why(f"{T}/{name}.caps", module); js = diag_why(f"{T}/{name}.diag")
    for key, why in text.items():
        sup = js.get(key)
        if sup is None: bad.append(f"{key}: no JSON why"); continue
        tdeps = set(re.findall(r"([\w.]+) \(dependency\)", why))
        for amb in re.findall(r"\(dependency: ([^)]*)\)", why): tdeps |= {x.strip() for x in amb.split(" or ")}
        jdeps = {t for s in sup if s["kind"] == "dependency" for t in s["targets"]}
        if tdeps != jdeps: bad.append(f"{key}: text {sorted(tdeps)} json {sorted(jdeps)}")
        if (why == "<- declared") != (sup == []): bad.append(f"{key}: declared-ness differs")
res(not bad, "caps text and diagnostics-json suppliers agree for every function and capability" + ("" if not bad else ": " + "; ".join(bad[:4])))
js = diag_why(f"{T}/app.diag").get(("main.via_helpers", "Console")) or []
res(any(s["kind"] == "dependency" and s["targets"] == ["factlib.helper_a"] for s in js),
    "diagnostics-json: via_helpers' supplier is kind 'dependency', target factlib.helper_a")

b = open(f"{T}/b64.caps").read()
res("(unknown)" not in b, "base64_cli: no callee is left '(unknown)'")
res("Vec_pop (intrinsic)" not in b and "Vec_new (intrinsic)" not in b,
    "base64_cli: a std definition is not labelled an intrinsic")
# base64_cli imports std.base64.{decode}; std.hex also defines decode. The import alias must pick
# the imported one, not both and not the other. (A spelling that stays ambiguous renders every
# candidate joined by "or"; no program in the corpus produces one, so that branch is untested here.)
res("std.base64.base64_decode (dependency)" in b and "std.hex.hex_decode" not in b,
    "an imported spelling resolves to the imported definition (decode -> std.base64.base64_decode, not std.hex)")
PY
while IFS= read -r line; do
  case "$line" in
    "ok   "*) ok "${line#ok   }" ;;
    "FAIL "*) no "${line#FAIL }" ;;
  esac
done < "$TMP/rows.out"
[ -s "$TMP/rows.out" ] || no "the row table produced nothing"

echo "=== CONTROL: with no dependency loaded, the query trace still ends at an intrinsic ==="
cat > "$TMP/q.con" <<'EOF'
fn shout() with(Console) -> i32 { print_int(1); return 0; }
fn main() with(Console) -> i32 { return shout(); }
EOF
q="$($TO "$CC" "$TMP/q.con" --query why-capability:main:Console 2>&1)"
if printf '%s' "$q" | grep -q '"origin": "intrinsic"' && printf '%s' "$q" | grep -q '"answer": "transitive"'; then
  ok "single-file why-capability: main -> shout -> print_int (intrinsic), transitive"
else
  no "single-file why-capability trace changed: $(printf '%s' "$q" | head -c 200)"
fi

echo
echo "CAPABILITY-EXPLANATIONS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
