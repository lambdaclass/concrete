#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Mixed-width binop gate (E0228) + `as`-cast value normalization.
#
# 1. Numeric binop operands must agree EXACTLY (width and signedness). The old
#    `typesCompatible` leniency let `i8 < i32` etc. through check AND interp,
#    only to die at SSA-verify (E0715, an internal-error class) — a violation
#    of "check-accept implies compile". Now E0228 fires at check time, for
#    every operator family (arith, comparison, bitwise/shift).
# 2. Literal-only operands stay FLEXIBLE: they adopt the other operand's type
#    (`inner[64 + i]` with i: i32 types 64 as i32, not Int-from-the-index-hint).
# 3. The interpreter normalizes `as`-cast values into the target range
#    (silent-truncation policy, ARITHMETIC_POLICY.md): `(-11) as u32` is the
#    two's-complement reinterpretation, matching the compiled binary — it used
#    to keep -11 and diverge (wrong `%` results, spurious checked-add traps).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

rejects_e0228(){ local label="$1" F="$2"
  local OUT; OUT="$("$COMPILER" "$F" --interp 2>&1)"
  if [ $? -ne 0 ] && printf '%s' "$OUT" | grep -q "E0228"; then ok "$label"
  else no "$label (got: $(printf '%s' "$OUT" | head -1))"; fi; }

# agree <label> <file> <expected>: interp and compiled both print <expected>.
agree(){ local label="$1" F="$2" want="$3"
  local I C
  I="$("$COMPILER" "$F" --interp 2>&1)"
  "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1 && C="$("$F.bin")" || C="<compile/run failed>"
  if [ "$I" = "$want" ] && [ "$C" = "$want" ]; then ok "$label"
  else no "$label (want $want, interp=$I compiled=$C)"; fi; }

echo "=== mixed-width operands are E0228 at check time (per operator family) ==="

cat > "$TMPDIR/cmp.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut a: i8 = 0 - 1;
        let mut b: i32 = 0;
        if a < b { return 1; } else { return 0; }
    }
}
EOF
rejects_e0228 "comparison i8 < i32" "$TMPDIR/cmp.con"

cat > "$TMPDIR/arith.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut a: i8 = 3;
        let mut b: i32 = 5;
        let c = a + b;
        return 0;
    }
}
EOF
rejects_e0228 "arithmetic i8 + i32" "$TMPDIR/arith.con"

cat > "$TMPDIR/bit.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut a: u8 = 3;
        let mut b: i64 = 5;
        let c = a & b;
        return 0;
    }
}
EOF
rejects_e0228 "bitwise u8 & i64" "$TMPDIR/bit.con"

echo "=== literal operands adopt the sibling operand's type ==="

cat > "$TMPDIR/flex.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut i: i32 = 3;
        let mut a: [i32; 8] = [0,1,2,3,4,5,6,7];
        let v: i32 = a[(4 + i) as Int];
        let mut b: i8 = 3;
        let c: i8 = b + 5;
        let w: i32 = match i { 0 => i, _ => (4 - a[2]) };
        return (v + w) as Int + (c as Int);
    }
}
EOF
agree "index/lhs/arm literals unify (a[64+i] class)" "$TMPDIR/flex.con" "17"

echo "=== interp normalizes as-cast values (matches compiled truncation) ==="

cat > "$TMPDIR/negu32.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut y: i32 = 0 - 11;
        let c: u32 = (y) as u32;
        return (c % 1000) as Int;
    }
}
EOF
agree "(-11) as u32 reinterprets" "$TMPDIR/negu32.con" "285"

cat > "$TMPDIR/negadd.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut x: i32 = 6;
        let mut y: i32 = 0 - 11;
        let s: u32 = (((x) as u32) + ((y) as u32)) % 1000;
        return s as Int;
    }
}
EOF
agree "u32 add over wrapped negative" "$TMPDIR/negadd.con" "291"

cat > "$TMPDIR/narrow.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut w: i32 = 300;
        let n: i8 = (w) as i8;
        return n as Int;
    }
}
EOF
agree "narrowing 300 as i8 truncates to 44" "$TMPDIR/narrow.con" "44"

# A left-shift result that overflows the SIGNED width truncates two's-complement,
# same as LLVM `shl` (`100 << 1` at i8 = 200 -> -56). The interpreter used to wrap
# unsigned only (`maskWidth`) and left the signed result un-truncated (200),
# diverging from the compiled binary — now both go through `IntArith.wrapToWidth`.
cat > "$TMPDIR/shl.con" <<'EOF'
mod m {
    fn main() -> Int {
        let a: i8 = 100;
        let x: i8 = a << 1;
        return x as Int;
    }
}
EOF
agree "i8 100 << 1 truncates to -56 (signed shl)" "$TMPDIR/shl.con" "-56"

echo
echo "check_mixed_width_binops: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
