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

gate_finish
