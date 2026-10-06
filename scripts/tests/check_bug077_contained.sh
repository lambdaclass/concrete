#!/usr/bin/env bash
# Bug 077 (OPEN): a relative call into a nested submodule (`util::poke` from `tb.a`) lowers to an
# undefined symbol. It may stay separately owned while R-0484 closes only if three things hold,
# and this gate checks them:
#   1. the build REFUSES the reproducer (non-zero exit at LLVM validation, no binary) every time;
#   2. reports are not falsely complete: the relative call still resolves for the assumption
#      summary, so a foreign binding reached through it is listed for the caller;
#   3. no R-0484 acceptance case depends on it: each acceptance project builds and runs.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/bug077_nested_submodule_call"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

echo "=== 1. the build refuses the reproducer, every time ==="
refused=0
for i in 1 2 3; do
  rm -f "$TMP/out"
  if $TO "$CC" "$FIX/repro.con" -o "$TMP/out" >"$TMP/b$i.log" 2>&1; then :; else
    if grep -c "undefined value '@util_poke'" "$TMP/b$i.log" >/dev/null && [ ! -e "$TMP/out" ]; then refused=$((refused+1)); fi
  fi
done
[ "$refused" -eq 3 ] && ok "3/3 builds refused at LLVM validation (undefined @util_poke), no binary" \
                     || no "the reproducer was not refused consistently ($refused/3): $(head -2 "$TMP/b1.log" | tr '\n' ' ')"

echo "=== 2. reports are not falsely complete ==="
$TO "$CC" "$FIX/reach.con" --report assumptions > "$TMP/reach.json" 2>/dev/null
r="$(python3 - "$TMP/reach.json" <<'PY'
import json, sys
fs = {f["fn"]: f for f in json.load(open(sys.argv[1]))["functions"]}
c = fs.get("tb.a.a_call", {})
names = {a["declaration"] for a in c.get("assumes", [])}
print("ok" if "tb.a.util.abs" in names and "tb.a.util.a_util_poke" in names else "no " + json.dumps(c))
PY
)"
[ "$r" = ok ] && ok "a_call lists the foreign binding and trusted boundary it reaches through the relative call" \
             || no "a_call does not list what it reaches through the relative call: $r"

echo "=== 3. no R-0484 acceptance case depends on it ==="
# Direct evidence rather than a source heuristic: a fixture that needed the broken lowering would
# fail to build. Each R-0484 acceptance project builds and runs.
for d in tests/regressions/proof_admission/app tests/regressions/trusted_boundaries/tb \
         tests/regressions/assumption_summary/app tests/regressions/intrinsic_classification/app; do
  if (cd "$d" && $TO "$CC" build . -o "$TMP/acc" >"$TMP/acc.log" 2>&1) && "$TMP/acc" >/dev/null 2>&1; then
    ok "$d builds and runs"
  else
    no "$d does not build or run: $(head -2 "$TMP/acc.log" | tr '\n' ' ')"
  fi
done

echo
echo "BUG077-CONTAINED: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
