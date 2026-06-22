#!/usr/bin/env bash
# Nested-patterns decision gate (ROADMAP Phase 6 #5; docs/NESTED_PATTERNS.md).
#
# Concrete V1 destructures one level per match arm; nested patterns
# (`E::W { P { x, y } }`, `Some(Some(n))`) are deferred (workload-gated). This
# gate pins that deeper-than-one-level pattern syntax stays a clean parse error
# (never silently half-accepted) and that the documented one-level workarounds
# (field access; nested `match`) compile and run.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

rejected(){ printf '%s' "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.out" 2>&1; then
    no "$1: nested pattern was ACCEPTED (should be rejected in V1)"
  else ok "$1: rejected ($(grep -oE '\([A-Z0-9]+\)' "$TMP/t.out" | head -1))"; fi; }
run_expect(){ printf '%s' "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.out" 2>&1; then
    local g; g="$("$TMP/t.bin" 2>/dev/null)"
    [ "$g" = "$3" ] && ok "$1 -> $g" || no "$1: got '$g' want '$3'"
  else no "$1: expected to compile"; sed 's/^/        /' "$TMP/t.out" | head -3; fi; }

echo "=== nested pattern syntax stays rejected (V1: one level per arm) ==="
rejected "struct-in-enum pattern" 'mod m { struct Copy P { x: i32, y: i32 } enum E { W { p: P } } fn f(e: E) -> i32 { match e { E::W { P { x, y } } => { return x + y; } } } fn main() -> Int { return 0; } }'

echo "=== one-level + field access workaround (structs) ==="
run_expect "field-access workaround" 'mod m { struct Copy P { x: i32, y: i32 } enum E { W { p: P } } fn f(e: E) -> i32 { match e { E::W { p } => { return p.x + p.y; } } } fn main() -> Int { return f(E::W { p: P { x: 5, y: 6 } }) as Int; } }' 11

echo "=== one-level + nested match workaround (enums) ==="
run_expect "nested-match workaround" 'mod m { fn f(o: Option<Option<i32>>) -> i32 { match o { Option::Some { inner } => { match inner { Option::Some { n } => { return n; }, Option::None => { return -1; } } }, Option::None => { return -2; } } } fn main() -> Int { return f(Option::<Option<i32>>::Some { value: Option::<i32>::Some { value: 7 } }) as Int; } }' 7

echo ""
echo "NESTED-PATTERNS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
