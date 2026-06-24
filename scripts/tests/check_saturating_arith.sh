#!/usr/bin/env bash
# Saturating-arithmetic gate — ROADMAP #10 Stage 2.2 (ARITHMETIC_POLICY.md §13 step 2).
#
# `saturating_add` / `saturating_sub` are the explicit spelling for intentional
# clamp-to-range arithmetic. They lower to the LLVM `llvm.{s,u}{add,sub}.sat`
# intrinsics. They must:
#   - clamp at the type's bounds (unsigned high/low, signed high/low),
#   - agree between interpreter and compiled code (the oracle invariant),
#   - be integer-only: reject float/bool operands and mixed widths.
# (saturating_mul arrives in Stage 2.2b on the shared overflow-intrinsic infra.)

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

oracle(){ local n="$1"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  "$TMP/$n.bin" >/dev/null 2>&1; local ce=$?
  "$C" "$TMP/$n.con" --interp >/dev/null 2>&1; local ie=$?
  if [ "$ce" = 0 ] && [ "$ie" = 0 ]; then ok "$n: interp == compiled == 0 (agree)"
  else no "$n: disagreement/failure (compiled=$ce interp=$ie)"; fi; }

reject(){ local n="$1" want="$2"
  if "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then no "$n: expected rejection"; return; fi
  grep -qF "$want" "$TMP/$n.err" && ok "$n: rejected ('$want')" || { no "$n: wrong diagnostic"; sed 's/^/        /' "$TMP/$n.err" | head -2; }; }

echo "=== saturating clamps at type bounds (signed + unsigned), interp == compiled ==="
cat > "$TMP/sat.con" <<'EOF'
pub fn main() -> Int {
    let umax: u8 = 255;
    let one8: u8 = 1;
    if saturating_add(umax, one8) != 255 { return 1; }          // unsigned high clamp
    let z8: u8 = 0;
    if saturating_sub(z8, one8) != 0 { return 2; }              // unsigned low clamp (underflow)
    let imax: i32 = 2147483647;
    let onei: i32 = 1;
    if saturating_add(imax, onei) != 2147483647 { return 3; }   // signed high clamp
    let imin: i32 = -2147483648;
    if saturating_sub(imin, onei) != -2147483648 { return 4; }  // signed low clamp
    let a: u8 = 100;
    let b: u8 = 50;
    if saturating_add(a, b) != 150 { return 5; }                // in-range, no clamp
    return 0;
}
EOF
oracle sat

echo "=== integer-only: reject float, mixed-width, wrong arity ==="
cat > "$TMP/float.con" <<'EOF'
pub fn main() -> Int { let x: f64 = saturating_add(1.0, 2.0); return 0; }
EOF
reject float "requires integer operands"
cat > "$TMP/mixed.con" <<'EOF'
pub fn main() -> Int { let a: u8 = 1; let b: i32 = 2; let r: u8 = saturating_add(a, b); return 0; }
EOF
reject mixed "same integer type"
cat > "$TMP/arity.con" <<'EOF'
pub fn main() -> Int { let a: u8 = 1; let r: u8 = saturating_add(a); return 0; }
EOF
reject arity "exactly 2 arguments"

echo ""
echo "SATURATING-ARITH: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
