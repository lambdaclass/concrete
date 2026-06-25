#!/usr/bin/env bash
# Arithmetic red-team gate — ROADMAP #10 checked-arithmetic hardening.
#
# Adversarial coverage for the checked-arithmetic model and the bugs the
# 2026-06-25 red-team sweep found. Every class below is either a defined-abort
# obligation (overflow/UB MUST trap on BOTH the interpreter and the compiled
# binary) or a value-agreement obligation (interp stdout == compiled stdout).
# The value-agreement oracle is the strong one: it caught a signed div/mod
# divergence (interp used Lean's floored `/`/`%`, the backend uses truncated
# sdiv/srem) that the algebraic "div/mod identity" test could not, because the
# identity holds under either rounding convention.
#
# Bugs this gate locks down (do not regress):
#   - unary `-(MIN)` must trap (was a silent wrap in codegen + an out-of-range
#     value in interp; the checked flip covered binops but missed unary neg)
#   - signed `/` and `%` truncate toward zero (interp + constant folder both
#     used floored division → disagreed with the backend on negative operands)
#   - signed `MIN % -1` must trap (interp did not; the srem helper does)
#   - `x * pow2` overflow must trap (the unsound mul→shl strength reduction that
#     dropped the trap stays removed)
#   - constant-fold of an overflowing op must trap, not wrap
#   - the formatter must re-escape string/char literals (round-trips, no
#     parse-breaking `"a"b"` output)
#
# New arithmetic work should add a fixture here: a trap case, a wrap/clamp
# value-agreement case, and (if it touches literals) a constant-fold case.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
write(){ printf '%s\n' "$2" > "$TMP/$1.con"; }

# trap: program must ABORT (defined-overflow/UB) on BOTH sides — compiled exits
# nonzero with no stdout value, interp exits nonzero. (A defined abort, not UB.)
trap_both(){ local n="$1"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.cerr" 2>&1; then
    no "$n: expected to compile then trap"; sed 's/^/        /' "$TMP/$n.cerr" | head -3; return; fi
  local cout ce ie; cout="$("$TMP/$n.bin" 2>/dev/null)"; ce=$?
  "$C" "$TMP/$n.con" --interp >/dev/null 2>&1; ie=$?
  if [ -z "$cout" ] && [ "$ce" -ne 0 ] && [ "$ie" -ne 0 ]; then
    ok "$n: traps on both sides (compiled=$ce interp=$ie)"
  else no "$n: expected a trap both sides — compiled(out='$cout' rc=$ce) interp(rc=$ie)"; fi; }

# agree: interp stdout == compiled stdout (and both run to success). The strong
# oracle for value-level correctness (wrap/clamp/sign-of-div).
agree(){ local n="$1" want="$2"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.cerr" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.cerr" | head -3; return; fi
  local cout iout; cout="$("$TMP/$n.bin" 2>/dev/null)"
  iout="$("$C" "$TMP/$n.con" --interp 2>/dev/null | tail -1)"
  if [ "$cout" = "$iout" ] && [ "$cout" = "$want" ]; then ok "$n: interp == compiled == $want"
  else no "$n: compiled='$cout' interp='$iout' want='$want'"; fi; }

echo "=== checked overflow traps (+ - *) across widths and signedness ==="
write ov_u8_add  'fn main() -> i64 { let a: u8 = 255; let b: u8 = 1; return (a + b) as i64; }'
write ov_i8_add  'fn main() -> i64 { let a: i8 = 127; let b: i8 = 1; return (a + b) as i64; }'
write ov_i8_sub  'fn main() -> i64 { let a: i8 = -128; let b: i8 = 1; return (a - b) as i64; }'
write ov_u8_sub  'fn main() -> i64 { let a: u8 = 0; let b: u8 = 1; return (a - b) as i64; }'
write ov_u32_mul 'fn main() -> i64 { let a: u32 = 65536; let b: u32 = 65536; return (a * b) as i64; }'
write ov_i64_mul 'fn main() -> i64 { let a: i64 = 9223372036854775807; let b: i64 = 2; return a * b; }'
for t in ov_u8_add ov_i8_add ov_i8_sub ov_u8_sub ov_u32_mul ov_i64_mul; do trap_both "$t"; done

echo "=== unary negation of MIN traps (checked flip covered binops; neg too) ==="
write neg_i8   'fn main() -> i64 { let x: i8 = -128; return (-x) as i64; }'
write neg_i32  'fn main() -> i64 { let x: i32 = -2147483648; return (-x) as i64; }'
write neg_i64  'fn main() -> i64 { let x: i64 = -9223372036854775808; return -x; }'
for t in neg_i8 neg_i32 neg_i64; do trap_both "$t"; done
write neg_ok 'fn main() -> i64 { let x: i32 = 5; return (-x) as i64; }'
agree neg_ok -5
# Boundary: the negative LITERAL at the signed minimum is a VALID value and must
# NOT trap — `-128 : i8` is `neg 128`, range-checked on the RESULT (-128 fits),
# folded to a constant. This is the flip side of the runtime `-(MIN)` trap above;
# a too-eager checked-neg made `-128` itself trap (numeric-literals CI failure).
write neg_lit_i8  'fn main() -> i64 { let a: i8 = -128; return a as i64; }'
write neg_lit_i32 'fn main() -> i64 { let a: i32 = -2147483648; return a as i64; }'
agree neg_lit_i8 -128
agree neg_lit_i32 -2147483648

echo "=== mul by power-of-2 overflow traps (unsound mul->shl reduction stays gone) ==="
write mulp2_i32 'fn main() -> i64 { let x: i32 = 1073741824; return (x * 4) as i64; }'  # 2^30 * 4 = 2^32 > i32::MAX
write mulp2_i64 'fn main() -> i64 { let x: i64 = 4611686018427387904; return x * 4; }'   # 2^62 * 4 overflows i64
for t in mulp2_i32 mulp2_i64; do trap_both "$t"; done

echo "=== constant-fold overflow traps (foldBinOpConst must not fold-and-wrap) ==="
write cfold_i8   'fn main() -> i64 { let y: i8 = 100 + 100; return y as i64; }'
write cfold_u8   'fn main() -> i64 { let y: u8 = 200 + 100; return y as i64; }'
write cfold_mul  'fn main() -> i64 { let y: i32 = 100000 * 100000; return y as i64; }'
for t in cfold_i8 cfold_u8 cfold_mul; do trap_both "$t"; done

echo "=== div/mod by zero + signed MIN/-1 trap (div AND mod) ==="
write div0   'fn main() -> i64 { let a: i32 = 7; let b: i32 = 0; return (a / b) as i64; }'
write mod0   'fn main() -> i64 { let a: i32 = 7; let b: i32 = 0; return (a % b) as i64; }'
write divmin 'fn main() -> i64 { let a: i32 = -2147483648; let b: i32 = -1; return (a / b) as i64; }'
write modmin 'fn main() -> i64 { let a: i32 = -2147483648; let b: i32 = -1; return (a % b) as i64; }'
for t in div0 mod0 divmin modmin; do trap_both "$t"; done

echo "=== over-width shift traps ==="
write shl_w  'fn main() -> i64 { let x: u32 = 1; let s: u32 = 32; return (x << s) as i64; }'
write shr_w  'fn main() -> i64 { let x: u32 = 1; let s: u32 = 40; return (x >> s) as i64; }'
for t in shl_w shr_w; do trap_both "$t"; done

echo "=== signed div/mod TRUNCATE toward zero (interp == compiled, both correct) ==="
# Lean's floored `/`/`%` would give -4 and 3 here; the backend truncates to -3 / -2.
write dvar_div 'fn main() -> i64 { let a: i32 = -17; let b: i32 = 5; return (a / b) as i64; }'
write dvar_mod 'fn main() -> i64 { let a: i32 = -17; let b: i32 = 5; return (a % b) as i64; }'
write dvar_mod2 'fn main() -> i64 { let a: i32 = -7; let b: i32 = 3; return (a % b) as i64; }'
agree dvar_div -3
agree dvar_mod -2
agree dvar_mod2 -1
# Same, but constant-folded (the folder must also truncate, not floor).
write dconst_div 'fn main() -> i64 { return (-17 / 5) as i64; }'
write dconst_mod 'fn main() -> i64 { return (-17 % 5) as i64; }'
agree dconst_div -3
agree dconst_mod -2

echo "=== wrapping_* wrap correctly at boundaries (interp == compiled) ==="
write wadd 'fn main() -> i64 { let a: u8 = 255; let b: u8 = 1; return wrapping_add(a, b) as i64; }'
write wsub 'fn main() -> i64 { let a: u8 = 0; let b: u8 = 1; return wrapping_sub(a, b) as i64; }'
write wmul 'fn main() -> i64 { let a: i32 = 2147483647; let b: i32 = 2; return wrapping_mul(a, b) as i64; }'
agree wadd 0
agree wsub 255
agree wmul -2

echo "=== saturating_* clamp at boundaries (interp == compiled) ==="
write sadd 'fn main() -> i64 { let a: u8 = 250; let b: u8 = 50; return saturating_add(a, b) as i64; }'
write ssub 'fn main() -> i64 { let a: u8 = 10; let b: u8 = 50; return saturating_sub(a, b) as i64; }'
write smul 'fn main() -> i64 { let a: i8 = 100; let b: i8 = 100; return saturating_mul(a, b) as i64; }'
agree sadd 255
agree ssub 0
agree smul 127

echo "=== mixed wrapping + checked in one expression (no cross-contamination) ==="
# wrapping_add wraps to 0; then + 5 is checked and in range → 5.
write mixed 'fn main() -> i64 { let a: u8 = 255; let b: u8 = 1; let w: u8 = wrapping_add(a, b); return (w + 5) as i64; }'
agree mixed 5

echo "=== formatter re-escapes string/char literals (round-trips, re-parses) ==="
write fmt_esc 'fn f() -> Int {
    let s: ByteView = "say \"hi\"\tand\nbye\\done";
    let c: char = '"'"'\n'"'"';
    return 0;
}'
"$C" "$TMP/fmt_esc.con" --fmt > "$TMP/fmt_esc.1" 2>&1
if "$C" "$TMP/fmt_esc.1" --fmt > "$TMP/fmt_esc.2" 2>&1 && cmp -s "$TMP/fmt_esc.1" "$TMP/fmt_esc.2"; then
  ok "fmt_esc: formatter output re-parses + is idempotent (escapes preserved)"
else
  no "fmt_esc: formatter output does not re-parse (escape bug)"; sed 's/^/        /' "$TMP/fmt_esc.2" | head -3
fi
# The raw escape sequences must survive in the formatted source.
if grep -qF '\"hi\"' "$TMP/fmt_esc.1" && grep -qF '\t' "$TMP/fmt_esc.1" && grep -qF '\\done' "$TMP/fmt_esc.1"; then
  ok "fmt_esc: \\\" \\t \\\\ all re-escaped in output"
else
  no "fmt_esc: escapes missing from formatted output"; sed 's/^/        /' "$TMP/fmt_esc.1" | head -5
fi

echo ""
echo "ARITH-REDTEAM: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
