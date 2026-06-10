#!/usr/bin/env bash
# Nested field-write known-hole gate (miscompile).
#
# `o.inner.v = x` (a field assignment whose object is itself a field access) is
# silently dropped: Lower's `.fieldAssign` value-struct path mutates a temporary
# COPY of `o.inner` and only writes it back when the object is a plain `.ident`.
# Single-level `o.v = x` works. This is a fail-open miscompile — it compiles and
# runs with the stale value.
#
# Tracks the hole without pretending it is fixed:
#   1. The fixture builds.
#   2. It returns the WRONG value (109), proving the nested write was dropped.
# When nested place/lvalue lowering lands, the program returns 7709; flip the
# expected value then (and move this to a regression gate).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

F="examples/known_holes/nested_field_write/src/main.con"

echo "=== known miscompile reproduces: nested field write is dropped ==="
if "$COMPILER" "$F" -o "$TMP/nfw" >/dev/null 2>&1; then
  OUT="$("$TMP/nfw" 2>/dev/null)"; RC=$?
  VAL="$OUT"; [ -z "$VAL" ] && VAL="$RC"
  if [ "$VAL" = "109" ]; then
    ok "o.inner.v=77 dropped: returns 109 (stale) instead of 7709 (KNOWN HOLE)"
  elif [ "$VAL" = "7709" ]; then
    no "nested field write now works (got 7709) — FIXED; flip this gate to expect 7709"
  else
    no "unexpected value '$VAL' (expected 109 while broken, 7709 when fixed)"
  fi
else
  no "fixture failed to compile"
fi

echo "=== single-level field write is unaffected (sanity, expect 77) ==="
cat > "$TMP/single.con" <<'EOF'
struct Copy Box { v: i64 }
fn main() -> i64 { let mut o: Box = Box { v: 1 }; o.v = 77; return o.v; }
EOF
if "$COMPILER" "$TMP/single.con" -o "$TMP/single" >/dev/null 2>&1; then
  OUT="$("$TMP/single" 2>/dev/null)"; RC=$?; VAL="$OUT"; [ -z "$VAL" ] && VAL="$RC"
  [ "$VAL" = "77" ] && ok "single-level field write works (77)" \
    || no "single-level field write also broken (got '$VAL') — wider than nested"
else
  no "single-level fixture failed to compile"
fi

echo ""
echo "NESTED-FIELD-WRITE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
