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
#   2. on the cases scripts/browser/test-parity.mjs compares with the Wasm build, the native adapter
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
# name <TAB> expected ok <TAB> source; the same cases as scripts/browser/test-parity.mjs.
cases_file="$TMP/cases.tsv"
cat > "$cases_file" <<'EOF'
constant	true	mod demo { fn main() -> Int { return 42; } }
arithmetic	true	mod demo { fn add(a: i64, b: i64) -> i64 { return a + b; } fn main() -> Int { return add(20, 22); } }
generic	true	mod demo { fn identity<T>(x: T) -> T { return x; } fn main() -> Int { return identity::<Int>(42); } }
struct	true	mod demo { struct Copy Point { x: Int, y: Int } fn main() -> Int { let p = Point { x: 20, y: 22 }; return p.x + p.y; } }
capability	true	mod demo { fn report() with(Console) { println("Hello from Concrete"); } fn main() with(Console) -> Int { report(); return 0; } }
ownership error	false	mod demo { struct Point { x: Int } fn main() -> Int { let p = Point { x: 42 }; return p.x; } }
branch	true	mod demo { fn main() -> Int { if true { return 42; } else { return 0; } } }
type error	false	mod demo { fn main() -> Int { return true; } }
parse error	false	mod demo { fn main(
unknown name	false	mod demo { fn main() -> Int { return missing; } }
EOF
while IFS=$'\t' read -r name want source; do
  if ! printf '%s' "$source" | timeout 60 "$BIN" > "$TMP/out.json" 2> "$TMP/err.txt"; then
    no "$name: the adapter exited non-zero: $(head -c 200 "$TMP/err.txt")"; continue
  fi
  verdict="$(python3 -I - "$TMP/out.json" "$want" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
want = sys.argv[2] == "true"
if d.get("ok") is not want:
    print(f"verdict ok={d.get('ok')}, expected {want}"); sys.exit()
names = [s.get("name") for s in d.get("stages", [])]
if want and names != ["Core", "Specialized Core", "SSA", "LLVM"]:
    print(f"stages {names}"); sys.exit()
if not want and not d.get("diagnostics"):
    print("refused with no diagnostic"); sys.exit()
print("ok")
PY
)"
  [ "$verdict" = "ok" ] && ok "$name" || no "$name: $verdict"
done < "$cases_file"

echo "BROWSER-NATIVE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
