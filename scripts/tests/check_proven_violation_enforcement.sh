#!/usr/bin/env bash
# Proven-violation enforcement regression gate (ROADMAP Phase 12 #0 —
# FIXED 2026-06-11).
#
# A runtime-safety obligation the compiler discharges to `violation` is a
# compile-time PROOF the program is wrong (constant index proven OOB, constant
# zero divisor), categorically different from an `unproven` obligation. In SAFE
# code these now FAIL the build (E0900); `trusted`/`with(Unsafe)` code is
# exempt; `unproven` obligations are NOT swept in.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

OOB="examples/known_holes/proven_oob_index/src/main.con"
DIV="examples/known_holes/proven_div_zero/src/main.con"

echo "=== proven violations are rejected in safe code ==="
OUT="$("$COMPILER" "$OOB" -o "$TMP/oob" 2>&1)"
if printf '%s' "$OUT" | grep -q "E0900" && printf '%s' "$OUT" | grep -qi "out of bounds"; then
  ok "constant out-of-bounds index a[5] is rejected (E0900)"
else
  no "constant OOB no longer rejected — H2 enforcement regressed"
fi
OUT="$("$COMPILER" "$DIV" -o "$TMP/div" 2>&1)"
if printf '%s' "$OUT" | grep -q "E0900" && printf '%s' "$OUT" | grep -qi "zero"; then
  ok "literal divide-by-zero 10/0 is rejected (E0900)"
else
  no "literal div-by-zero no longer rejected — H2 enforcement regressed"
fi

echo "=== trusted / Unsafe code is exempt (audit-responsibility) ==="
cat > "$TMP/trusted.con" <<'EOF'
trusted fn risky() -> i64 { let a: [i64; 3] = [1,2,3]; return a[5]; }
fn main() -> i64 { return risky(); }
EOF
"$COMPILER" "$TMP/trusted.con" -o "$TMP/trusted" >/dev/null 2>&1 \
  && ok "trusted fn with a constant OOB still compiles (exempt)" \
  || no "trusted exemption broken — proven-violation gate fired on trusted code"

cat > "$TMP/unsafe.con" <<'EOF'
fn risky() with(Unsafe) -> i64 { let a: [i64; 3] = [1,2,3]; return a[5]; }
fn main() with(Unsafe) -> i64 { return risky(); }
EOF
"$COMPILER" "$TMP/unsafe.con" -o "$TMP/unsafe" >/dev/null 2>&1 \
  && ok "with(Unsafe) fn with a constant OOB still compiles (exempt)" \
  || no "Unsafe exemption broken — proven-violation gate fired on with(Unsafe) code"

echo "=== unproven obligations are NOT swept into the hard error ==="
cat > "$TMP/unproven.con" <<'EOF'
fn main() -> i64 { let a: [i64; 3] = [10,20,30]; let i: i64 = 1; return a[i]; }
EOF
"$COMPILER" "$TMP/unproven.con" -o "$TMP/unproven" >/dev/null 2>&1 \
  && ok "variable-index (unproven, not violation) still compiles" \
  || no "unproven obligation wrongly rejected — H2 over-fires"

echo ""
echo "PROVEN-VIOLATION-ENFORCEMENT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
