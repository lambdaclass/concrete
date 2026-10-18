#!/usr/bin/env bash
# Cast-matrix differential gate: one row per DOCUMENTED cast-semantics claim.
#
# ARITHMETIC_POLICY says `as` keeps the low bits (silent truncation /
# bit-pattern reinterpretation). The interpreter is a hand-written second
# implementation of that claim, and nothing forced agreement until the
# width-lattice fuzzer caught `evalCast` retagging without normalizing
# ((-11) as u32 stayed -11). This gate makes the agreement MECHANICAL: for
# every (source type × edge value), cast to EVERY width and compare a digest
# between interpreter and compiled binary. Edge values cover zero, one, a
# mid-range value, and (for signed) -1 and MIN — so sign-extension,
# zero-extension, truncation, and negative-to-unsigned reinterpretation are
# all pinned, per pair.

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/gate.sh"
gate_init "check_cast_matrix"

DSTS=(i8 i16 i32 i64 u8 u16 u32 u64)

# gen <src> <literal>: program casting the value to every width, digesting all.
gen() {
  local src="$1" lit="$2" f="$3"
  {
    echo "mod m {"
    echo "    fn main() -> Int {"
    echo "        let mut x: $src = $lit;"
    echo "        let mut d: Int = 0;"
    local k=3
    for dst in "${DSTS[@]}"; do
      echo "        d = ((d + ((((x) as $dst) as Int) % 99991) * $k) % 99999989);"
      k=$((k+2))
    done
    echo "        return d;"
    echo "    }"
    echo "}"
  } > "$f"
}

echo "=== every (source width x edge value) casts to every width identically ==="

run_src() {
  local src="$1"; shift
  for lit in "$@"; do
    local f="$GATE_TMP/${src}_${lit//[^0-9a-z]/_}.con"
    gen "$src" "$lit" "$f"
    agree_both "$src = $lit -> all widths" "$f"
  done
}

run_src i8  0 1 107 "-1" "-128"
run_src i16 0 1 30000 "-1" "-32768"
run_src i32 0 1 2000000000 "-1" "-2147483648"
run_src i64 0 1 9000000000000000000 "-1"
run_src u8  0 1 200 255
run_src u16 0 1 60000 65535
run_src u32 0 1 4000000000 4294967295
run_src u64 0 1 18000000000000000000

# ---------------------------------------------------------------------------
# Bug 088: CONSTANT operands. The matrix above casts a variable, so the SSA
# constant folder never sees a constant cast. The folder had no cast fold, and
# identity-cast elimination forwarded the operand unnormalized: with the
# unhinted literal 0xFFFF_FFFF_FFFF_FFFF typed `Int` (2^64-1, never
# range-checked), `0xFFFFFFFFFFFFFFFF as i64 < 0` folded to false while the
# interpreter said true. Each row is a constant expression whose cast, compare,
# division, shift or remainder the folder evaluates; interp and binary must both
# print the pinned value.
# ---------------------------------------------------------------------------
echo
echo "=== constant casts fold to the same value the interpreter computes (bug 088) ==="

crow() {
  local label="$1" body="$2" want="$3"
  local f="$GATE_TMP/c_${label//[^0-9a-z]/_}.con"
  printf 'mod m {\n    fn main() -> Int {\n        %s\n    }\n}\n' "$body" > "$f"
  agree "$label" "$f" "$want"
}

crow "u64max as i64 < 0"          'if 0xFFFFFFFFFFFFFFFF as i64 < 0 { return 1; } return 0;' 1
crow "u64max as i64"              'return 0xFFFFFFFFFFFFFFFF as i64;' -1
crow "u64max as i64 == -1"        'if (0xFFFFFFFFFFFFFFFF as i64) == -1 { return 1; } return 0;' 1
crow "u64max as u8"               'return (0xFFFFFFFFFFFFFFFF as u8) as Int;' 255
crow "u64 unsigned compare"       'if (0xFFFFFFFFFFFFFFFF as u64) > (9223372036854775807 as u64) { return 1; } return 0;' 1
crow "u64 unsigned division"      'return ((0xFFFFFFFFFFFFFFFF as u64) / 2) as Int;' 9223372036854775807
crow "u64 unsigned remainder"     'return ((0xFFFFFFFFFFFFFFFF as u64) % 10) as Int;' 5
crow "u64 logical shift right"    'return ((0xFFFFFFFFFFFFFFFF as u64) >> 60) as Int;' 15
crow "i64 -1 arithmetic shift"    'return (0xFFFFFFFFFFFFFFFF as i64) >> 1;' -1
crow "-1 as u32 compare"          'if (-1 as u32) > 5 { return 1; } return 0;' 1
crow "-1 as u32 division"         'return ((-1 as u32) / 16) as Int;' 268435455
crow "-1 as i32 as u64 shift"     'return (((-1 as i32) as u64) >> 32) as Int;' 4294967295
crow "0xFFFFFFFF as i32"          'return (0xFFFFFFFF as i32) as Int;' -1
crow "0xFFFFFFFF as i32 division" 'return ((0xFFFFFFFF as i32) as Int) / 1;' -1
crow "200 as i8 < 0"              'if (200 as i8) < 0 { return 1; } return 0;' 1
crow "200 as i8"                  'return (200 as i8) as Int;' -56
crow "300 as u8"                  'return (300 as u8) as Int;' 44
crow "128 as u8 as i8"            'return ((128 as u8) as i8) as Int;' -128
crow "70000 as u16 remainder"     'return ((70000 as u16) % 7) as Int;' 5
crow "2^63 as i64"                'return 9223372036854775808 as i64;' -9223372036854775808

# A literal must fit the type it ends up with — the DEFAULTED type too (bug 088:
# an unhinted literal was `Int` with no range check, so the interpreter kept
# 2^64-1 and the binary emitted -1), and a type it is INSTANTIATED at (Mono
# substituted `T` silently: `let y: T = 300` at u8 was 300 interpreted, 44
# compiled). Both paths must refuse, with E0227.
echo
echo "=== an out-of-range literal is rejected by the compiler AND the interpreter (bug 088) ==="

# The STAGE is pinned too: a literal whose type Check knows is refused by Check
# (with a source span); only a literal at a type variable is left to post-mono.
# Without the stage, the post-mono backstop would hide a Check that stopped
# range-checking the defaulted type.
rejects_both() {
  local label="$1" f="$2" code="$3" stage="$4" c i
  c="$("$GATE_COMPILER" "$f" -o "$GATE_TMP/rb.bin" 2>&1)"; local crc=$?
  i="$("$GATE_COMPILER" "$f" --interp 2>&1)"; local irc=$?
  if [ "$crc" -ne 0 ] && [ "$irc" -ne 0 ] \
     && grep -qF "error[$stage]: ($code)" <<<"$c" && grep -qF "error[$stage]: ($code)" <<<"$i"; then ok "$label"
  else no "$label (want error[$stage] $code from both; compile rc=$crc '$(printf '%s' "$c" | awk 'NR<=1')', interp rc=$irc '$(printf '%s' "$i" | awk 'NR<=1')')"; fi
}

rrow() {
  local label="$1" stage="$2" src="$3"
  local f="$GATE_TMP/r_${label//[^0-9a-z]/_}.con"
  printf '%s\n' "$src" > "$f"
  rejects_both "$label" "$f" E0227 "$stage"
}

rrow "unhinted 2^64-1"        check     'mod m { fn main() -> Int { let x = 18446744073709551615; return x; } }'
rrow "unhinted 2^63"          check     'mod m { fn main() -> Int { let x = 9223372036854775808; return 0; } }'
rrow "cast operand > u64 max" check     'mod m { fn main() -> Int { return 18446744073709551616 as Int; } }'
rrow "generic literal at u8"  post-mono 'mod m { fn k<T: Copy>(x: T) -> T { let y: T = 300; return y; } fn main() -> Int { let a: u8 = k::<u8>(1); return a as Int; } }'

gate_finish
