#!/usr/bin/env bash
# The browser compiler's root builds and answers like the compiler (bug 316).
#
# Browser.lean is the root of the `concrete-browser-native` executable, the native twin of the
# site's Wasm compiler (scripts/browser/README.md). It drives the pipeline passes itself, so a change
# to the pipeline's API breaks it, and for a week nothing noticed: it sat outside lakefile.toml's
# defaultTargets, so no `lake build`, CI job or hook ever compiled it (it called Pipeline.desugar,
# folded into resolve by 6269002b). It is a default target now, and this gate runs it:
#
#   1. the executable builds;
#   2. on scripts/browser/parity_cases.json, the cases test-parity.mjs compares with the Wasm build,
#      the native adapter
#      exits 0 with JSON whose verdict is the expected one, every accepted program shows the four
#      stages Core, Specialized Core, SSA and LLVM, and every refused one carries a diagnostic.
#
# The Wasm half of the parity test needs Emscripten and the committed Wasm snapshot; it is not run
# here. This gate holds the native half, which is what keeps the root from rotting.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
BIN=".lake/build/bin/concrete-browser-native"
BROWSER_ROOT="Browser.lean"
[ -f "$BROWSER_ROOT" ] || { echo "FAIL: $BROWSER_ROOT is missing"; exit 1; }

echo "=== the browser root builds ==="
if lake build concrete-browser-native > "$TMP/build.log" 2>&1 && [ -x "$BIN" ]; then
  ok "lake build concrete-browser-native"
else
  tail -20 "$TMP/build.log"; no "concrete-browser-native does not build"
  echo "BROWSER-NATIVE: PASS=$PASS FAIL=$FAIL"; exit 1
fi

echo "=== the native adapter on the parity cases ==="
# The cases scripts/browser/test-parity.mjs compares with the Wasm build: one list, one file.
CASES="scripts/browser/parity_cases.json"
# The runner's own exit status counts: a crash after some cases must not read as a partial pass.
N_CASES="$(python3 -I -c 'import json,sys; print(len(json.load(open(sys.argv[1]))))' "$CASES")"
python3 -I - "$CASES" "$BIN" "$TMP" <<'PY' > "$TMP/results.txt"
import json, subprocess, sys
cases, binary, tmp = json.load(open(sys.argv[1])), sys.argv[2], sys.argv[3]
if len(cases) < 5:
    print(f"FAIL only {len(cases)} parity cases"); sys.exit()
for name, source, want in cases:
    try:
        r = subprocess.run([binary], input=source, capture_output=True, text=True, timeout=60)
    except subprocess.TimeoutExpired:
        print(f"FAIL {name}: timed out"); continue
    if r.returncode != 0:
        print(f"FAIL {name}: the adapter exited {r.returncode}: {r.stderr[:200]}"); continue
    try:
        d = json.loads(r.stdout)
    except ValueError:
        print(f"FAIL {name}: not JSON: {r.stdout[:200]}"); continue
    names = [s.get("name") for s in d.get("stages", [])]
    if d.get("ok") is not want:
        print(f"FAIL {name}: verdict ok={d.get('ok')}, expected {want}")
    elif want and names != ["Core", "Specialized Core", "SSA", "LLVM"]:
        print(f"FAIL {name}: stages {names}")
    elif not want and not d.get("diagnostics"):
        print(f"FAIL {name}: refused with no diagnostic")
    else:
        print(f"ok {name}")
PY
runner_rc=$?
[ "$runner_rc" -eq 0 ] || no "the case runner exited $runner_rc (a crash, not a verdict)"
while IFS= read -r line; do
  case "$line" in ok\ *) ok "${line#ok }" ;; FAIL\ *) no "${line#FAIL }" ;; *) no "unexpected: $line" ;; esac
done < "$TMP/results.txt"
[ "$(grep -cE '^(ok|FAIL) ' "$TMP/results.txt")" = "$N_CASES" ] \
  || no "the case runner reported $(grep -cE '^(ok|FAIL) ' "$TMP/results.txt") of $N_CASES cases"

echo "BROWSER-NATIVE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
