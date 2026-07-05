#!/usr/bin/env bash
# Struct functional-update gate (ROADMAP Phase 6 #5 — struct update syntax).
#
# `S { f: x, ..base }` builds a struct value, taking the listed fields from the
# literal and every other field from `base` (which must be the same struct type).
# Desugared in Elab to `base.field` for each omitted field. Locks:
#   - override-some + copy-the-rest produces the right field values,
#   - `S { ..base }` copies all fields,
#   - a base of a different struct type is rejected (E0220).
#
# Fixtures: tests/programs/struct_update/. See docs/PATTERN_ERGONOMICS.md
# (struct update is recorded alongside the Phase 6 #5 surface).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/struct_update"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

run_expect(){ local n="$1" e="$2"
  if ! "$C" "$D/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  local g; g="$("$TMP/$n.bin" 2>/dev/null)"
  [ "$g" = "$e" ] && ok "$n -> $g" || no "$n: got '$g' want '$e'"; }

reject_with(){ local n="$1" c="$2"
  local out; out="$("$C" "$D/$n.con" -o "$TMP/$n.bin" 2>&1)"
  if grep <<<"$out" -qE 'error\['; then
    grep <<<"$out" -q "($c)" && ok "$n rejected with $c" \
      || { no "$n rejected, wrong code"; grep <<<"$out" -oE '\([A-Z0-9]+\)' | head -1 | sed 's/^/        got: /'; }
  else no "$n: expected rejection ($c), compiled"; fi; }

echo "=== struct functional update (..base) ==="
run_expect override_fields 1
run_expect all_from_base 1
reject_with neg_wrong_type_base E0220

echo ""
echo "STRUCT-UPDATE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
