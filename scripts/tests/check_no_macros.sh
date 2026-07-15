#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Macro-stance gate (ROADMAP Phase 6 #11; docs/MACRO_STANCE.md).
#
# Concrete has no language macro system: no unrestricted macros (`macro …`,
# `foo!(…)`) and no `#[derive(…)]`. This gate pins that all three stay clean
# parse errors (never silently accepted), so a macro-shaped feature cannot creep
# in as a derive/helper exception.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

rejected(){ printf '%s' "$2" > "$TMP/m.con"
  if "$C" "$TMP/m.con" -o "$TMP/m.bin" >"$TMP/m.out" 2>&1; then
    no "$1: ACCEPTED (macro-shaped syntax must stay rejected in V1)"
  else ok "$1: rejected ($(grep -oE '\([A-Z0-9]+\)' "$TMP/m.out" | head -1))"; fi; }

echo "=== no language macro system (all macro/derive syntax stays rejected) ==="
rejected "macro definition"   'mod m { macro foo { } fn main() -> Int { return 0; } }'
rejected "bang invocation"    'mod m { fn main() -> Int { return foo!(); } }'
rejected "#[derive(...)]"     'mod m { #[derive(Eq)] struct S { x: i32 } fn main() -> Int { return 0; } }'

echo ""
echo "NO-MACROS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
