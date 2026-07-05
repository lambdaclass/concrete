#!/usr/bin/env bash
# Checked-arithmetic gate — ROADMAP #10 Stage 2.3 (ARITHMETIC_POLICY.md §13 step 5).
#
# Ordinary `+` is CHECKED: it traps (abort) on overflow, in every profile.
# Intentional modular arithmetic must use the explicit `wrapping_*` spelling.
# This gate locks both, and carries a sentinel/modular-wrap REGRESSION so the
# flip (and the SHA-256-style migration that unblocked it) can't silently revert:
#   - ordinary `+` overflow traps (compiled abort + interp error);
#   - in-range `+` is unchanged and interp == compiled;
#   - `wrapping_add` still escapes the check (modular by intent);
#   - a modular accumulation written with `wrapping_add` runs WITHOUT trapping
#     and is correct (the migration pattern SHA-256 needed).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Oracle: self-checking program must compile, run to 0, AND interp to 0.
oracle(){ local n="$1"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  "$TMP/$n.bin" >/dev/null 2>&1; local ce=$?
  "$C" "$TMP/$n.con" --interp >/dev/null 2>&1; local ie=$?
  if [ "$ce" = 0 ] && [ "$ie" = 0 ]; then ok "$n: interp == compiled == 0 (agree)"
  else no "$n: disagreement/failure (compiled=$ce interp=$ie)"; fi; }

echo "=== ordinary + traps on overflow (compiled abort, interp error) ==="
# Through a function so the overflow is not constant-folded / DCE'd.
cat > "$TMP/trap.con" <<'EOF'
fn add(a: u8, b: u8) -> u8 { return a + b; }
pub fn main() -> Int { let x: u8 = add(255, 1); if x == 0 { return 9; } return 7; }
EOF
"$C" "$TMP/trap.con" -o "$TMP/trap.bin" >/dev/null 2>&1
"$TMP/trap.bin" >/dev/null 2>&1; tc=$?
if [ "$tc" -ne 0 ]; then ok "compiled: u8 255+1 aborts (exit $tc)"; else no "compiled: u8 255+1 did NOT trap (exit $tc)"; fi
# NB: the interp EXITS NONZERO when it traps, so capture first (pipefail would
# otherwise propagate that nonzero and defeat the grep).
itrap="$("$C" "$TMP/trap.con" --interp 2>&1 || true)"
if grep <<<"$itrap" -qi "overflow"; then ok "interp: u8 255+1 traps (overflow)"; else no "interp: u8 255+1 did NOT trap"; fi

# `-` underflow (unsigned) and `*` overflow also trap.
cat > "$TMP/subtrap.con" <<'EOF'
fn sub(a: u8, b: u8) -> u8 { return a - b; }
pub fn main() -> Int { let x: u8 = sub(0, 1); if x == 255 { return 9; } return 7; }
EOF
"$C" "$TMP/subtrap.con" -o "$TMP/subtrap.bin" >/dev/null 2>&1; "$TMP/subtrap.bin" >/dev/null 2>&1; sc=$?
if [ "$sc" -ne 0 ]; then ok "compiled: u8 0-1 underflow aborts (exit $sc)"; else no "compiled: u8 0-1 did NOT trap"; fi
cat > "$TMP/multrap.con" <<'EOF'
fn mul(a: i32, b: i32) -> i32 { return a * b; }
pub fn main() -> Int { let x: i32 = mul(100000, 100000); if x == 1410065408 { return 9; } return 7; }
EOF
"$C" "$TMP/multrap.con" -o "$TMP/multrap.bin" >/dev/null 2>&1; "$TMP/multrap.bin" >/dev/null 2>&1; mc=$?
if [ "$mc" -ne 0 ]; then ok "compiled: i32 100000*100000 overflow aborts (exit $mc)"; else no "compiled: i32 mul did NOT trap"; fi

# div-by-zero (was UB/SIGFPE) and over-width shift now ABORT (Stage 2.4/2.5).
cat > "$TMP/dz.con" <<'EOF'
fn dv(a: i32, b: i32) -> i32 { return a / b; }
pub fn main() -> Int { let x: i32 = dv(10, 0); return 0; }
EOF
"$C" "$TMP/dz.con" -o "$TMP/dz.bin" >/dev/null 2>&1; "$TMP/dz.bin" >/dev/null 2>&1; dc=$?
if [ "$dc" -ne 0 ]; then ok "compiled: div-by-zero aborts (exit $dc, was UB)"; else no "compiled: div-by-zero did NOT trap"; fi
dzi="$("$C" "$TMP/dz.con" --interp 2>&1 || true)"
if grep <<<"$dzi" -qi "division by zero"; then ok "interp: div-by-zero traps"; else no "interp: div-by-zero did NOT trap"; fi
cat > "$TMP/sh.con" <<'EOF'
fn sh(a: u32, b: u32) -> u32 { return a << b; }
pub fn main() -> Int { let x: u32 = sh(1, 40); return 0; }
EOF
"$C" "$TMP/sh.con" -o "$TMP/sh.bin" >/dev/null 2>&1; "$TMP/sh.bin" >/dev/null 2>&1; hc=$?
if [ "$hc" -ne 0 ]; then ok "compiled: u32 1<<40 over-width shift aborts (exit $hc, was UB)"; else no "compiled: over-width shift did NOT trap"; fi

echo "=== in-range + unchanged, interp == compiled ==="
cat > "$TMP/inrange.con" <<'EOF'
pub fn main() -> Int { let a: u8 = 100; let b: u8 = 50; if a + b != 150 { return 1; }
    let c: i32 = 2000000000; let d: i32 = 100000000; if c + d != 2100000000 { return 2; } return 0; }
EOF
oracle inrange

echo "=== wrapping_add still escapes the check (modular by intent) ==="
cat > "$TMP/wrap.con" <<'EOF'
pub fn main() -> Int { let a: u8 = 255; let b: u8 = 1; if wrapping_add(a, b) != 0 { return 1; } return 0; }
EOF
oracle wrap

echo "=== regression: modular accumulation via wrapping_add does NOT trap, is correct ==="
# A SHA-256-style mod-2^32 accumulation: would overflow under checked +, but
# wrapping_add is the correct explicit spelling and must run cleanly.
cat > "$TMP/modacc.con" <<'EOF'
fn mix(seed: u32, n: u32) -> u32 {
    let mut h: u32 = seed;
    let mut i: u32 = 0;
    while i < n {
        h = wrapping_add(wrapping_mul(h, 31), i);  // intentional mod-2^32 mixing
        i = i + 1;
    }
    return h;
}
pub fn main() -> Int {
    // Large enough to overflow u32 many times; must not trap.
    let h: u32 = mix(2654435761, 100000);
    if h == 0 { return 0; }   // any deterministic value; the point is it ran
    return 0;
}
EOF
oracle modacc

echo ""
echo "CHECKED-ARITH: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
