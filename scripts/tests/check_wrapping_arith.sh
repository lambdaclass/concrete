#!/usr/bin/env bash
# Wrapping-arithmetic gate — ROADMAP #10 Stage 2.1 (ARITHMETIC_POLICY.md §13 step 1).
#
# `wrapping_add` / `wrapping_sub` / `wrapping_mul` are the explicit, visible
# spelling for intentional two's-complement modular arithmetic. They must:
#   - wrap correctly for unsigned AND signed fixed-width integers,
#   - agree between the interpreter and compiled code (the oracle invariant),
#   - be integer-only: reject float / bool operands and mixed widths,
#   - be DISTINCT from ordinary `+ - *`, which are now checked and trap on
#     overflow (Stage 2.3) — wrapping_* is the explicit way to opt into wrap.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# A self-checking program (returns 0 on success, a distinct nonzero per failed
# assertion) must compile, run to 0, AND interpret to 0 — interp == compiled.
oracle(){ local n="$1"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  "$TMP/$n.bin" >/dev/null 2>&1; local ce=$?
  "$C" "$TMP/$n.con" --interp >/dev/null 2>&1; local ie=$?
  if [ "$ce" = 0 ] && [ "$ie" = 0 ]; then ok "$n: interp == compiled == 0 (agree)"
  else no "$n: disagreement/failure (compiled=$ce interp=$ie)"; fi; }

reject(){ local n="$1" want="$2"
  if "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected rejection"; return; fi
  grep -qF "$want" "$TMP/$n.err" && ok "$n: rejected ('$want')" || { no "$n: wrong diagnostic"; sed 's/^/        /' "$TMP/$n.err" | head -2; }; }

echo "=== wrapping_* wrap correctly (unsigned + signed), interp == compiled ==="
cat > "$TMP/wrap.con" <<'EOF'
pub fn main() -> Int {
    let u_max: u8 = 255;
    let one8: u8 = 1;
    if wrapping_add(u_max, one8) != 0 { return 1; }      // 255 + 1 -> 0
    let z8: u8 = 0;
    if wrapping_sub(z8, one8) != 255 { return 2; }       // 0 - 1 -> 255
    let big8: u8 = 200;
    let two8: u8 = 2;
    if wrapping_mul(big8, two8) != 144 { return 3; }     // 400 mod 256 -> 144
    let i_max: i32 = 2147483647;
    let onei: i32 = 1;
    if wrapping_add(i_max, onei) != -2147483648 { return 4; }  // signed wrap
    let i_min: i32 = -2147483648;
    if wrapping_sub(i_min, onei) != 2147483647 { return 5; }   // signed wrap down
    return 0;
}
EOF
oracle wrap

# Ordinary `+ - *` are now CHECKED (Stage 2.3 flipped them) — `wrapping_add` is the
# ONLY way to get wrap. Verify the two differ: 255 + 1 traps, but wrapping_add wraps
# to 0. (The trap itself is owned by check_checked_arith.sh.)
echo "=== ordinary + traps where wrapping_add wraps (they are distinct) ==="
cat > "$TMP/distinct.con" <<'EOF'
fn add(a: u8, b: u8) -> u8 { return a + b; }
pub fn main() -> Int { let x: u8 = add(255, 1); return 0; }
EOF
"$C" "$TMP/distinct.con" -o "$TMP/distinct.bin" >/dev/null 2>&1; "$TMP/distinct.bin" >/dev/null 2>&1
if [ "$?" -ne 0 ]; then ok "ordinary + traps on 255+1 (checked), unlike wrapping_add"; else no "ordinary + did NOT trap (flip regressed?)"; fi

echo "=== integer-only: reject float and mixed-width operands ==="
cat > "$TMP/float.con" <<'EOF'
pub fn main() -> Int { let x: f64 = wrapping_add(1.0, 2.0); return 0; }
EOF
reject float "requires integer operands"
cat > "$TMP/mixed.con" <<'EOF'
pub fn main() -> Int {
    let a: u8 = 1;
    let b: i32 = 2;
    let r: u8 = wrapping_add(a, b);
    return 0;
}
EOF
reject mixed "same integer type"

echo "=== wrong arity rejected ==="
cat > "$TMP/arity.con" <<'EOF'
pub fn main() -> Int { let a: u8 = 1; let r: u8 = wrapping_add(a); return 0; }
EOF
reject arity "exactly 2 arguments"

echo ""
echo "WRAPPING-ARITH: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
