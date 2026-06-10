#!/usr/bin/env bash
# Raw-pointer-to-local known-hole gate (unsafe path).
#
# `&mut x as *mut i64` does not alias the local `x`: locals are SSA register
# values, not addressable stack slots, so taking a raw pointer materializes a
# pointer to a COPY. Store-then-load through the SAME pointer is consistent, but
# the local read directly is unchanged. Requires trusted + raw pointers
# (audit-responsibility), and shares the addressability root with the nested
# place-write bug (now fixed); the fix here is promoting address-taken locals to
# stack allocas.
#
# Tracks the hole: the fixture builds and returns 1 (stale). Also asserts that
# store+load through the SAME pointer is internally consistent (returns 99),
# isolating the bug to local aliasing rather than deref-store itself.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

F="examples/known_holes/raw_ptr_to_local/src/main.con"

echo "=== known hole reproduces: raw ptr to local does not alias the local ==="
if "$COMPILER" "$F" -o "$TMP/rpl" >/dev/null 2>&1; then
  OUT="$("$TMP/rpl" 2>/dev/null)"; RC=$?; VAL="$OUT"; [ -z "$VAL" ] && VAL="$RC"
  if [ "$VAL" = "1" ]; then
    ok "store through &mut-local-ptr does not reach the local: returns 1 (KNOWN HOLE)"
  elif [ "$VAL" = "99" ]; then
    no "raw ptr to local now aliases (got 99) — FIXED; flip this gate to expect 99"
  else
    no "unexpected value '$VAL' (1 while broken, 99 when fixed)"
  fi
else
  no "fixture failed to compile"
fi

echo "=== deref store/load are internally consistent (isolates the bug to aliasing) ==="
cat > "$TMP/rw.con" <<'EOF'
trusted fn rw(p: *mut i64, val: i64) -> i64 { *p = val; return *p; }
fn main() -> i64 { let mut x: i64 = 1; return rw(&mut x as *mut i64, 99); }
EOF
if "$COMPILER" "$TMP/rw.con" -o "$TMP/rw" >/dev/null 2>&1; then
  OUT="$("$TMP/rw" 2>/dev/null)"; RC=$?; VAL="$OUT"; [ -z "$VAL" ] && VAL="$RC"
  [ "$VAL" = "99" ] && ok "store+load through the same pointer is consistent (99)" \
    || no "deref store/load inconsistent (got '$VAL') — wider than local aliasing"
else
  no "rw fixture failed to compile"
fi

echo ""
echo "RAW-PTR-TO-LOCAL: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
