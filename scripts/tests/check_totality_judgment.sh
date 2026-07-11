#!/usr/bin/env bash
# TotalityJudgment matrix (Phase 6.5).
#
# Audit outcome: the totality/trap/divergence facts are ALREADY single-sourced —
# divergence in CheckHelpers (blockDiverges / exprDiverges / blockNonTerminating
# / blockExitsFunction), arithmetic-trap in IntArith.evalIntBinOp (the const
# folder routes through IntArith.foldIntBinOp + isSideEffecting, so a fold cannot
# drop a documented trap — the seed-20260703 fix), cast-trap in
# IntArith.checkedToType, bounds in Lower.emitBoundsCheck. Nothing reimplements
# them. So TotalityJudgment is a GATE over existing facts, not a new helper or
# decision record (building one would be speculative — no consumer or gate pulls
# it).
#
# This matrix proves the totality classification agrees interp == compiled: a
# total expression yields its value on both; a trap fires on both; a divergent
# branch (return) yields the taken value on both. A mismatch would be a real
# trap/divergence-fact drift between the reference (interp) and codegen.
#
# (The interpreter's documented coverage gaps — floats, alloc, etc. — are NOT
# totality-fact drift: the interp refuses LOUDLY rather than giving a wrong
# answer, so they are excluded here and tracked as oracle-coverage work.)

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
emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

# total <label> <name> <expected>: TOTAL — both interp and compiled yield <expected>.
total(){ local label="$1" F="$TMPDIR/$2.con" want="$3"
  local I C
  I="$("$COMPILER" "$F" --interp 2>&1)"
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then C="$("$F.bin" 2>&1)"; else C="<compile-fail>"; fi
  if [ "$I" = "$want" ] && [ "$C" = "$want" ]; then ok "$label"
  else no "$label (want $want, interp=$(printf '%s' "$I"|head -1) compiled=$(printf '%s' "$C"|head -1))"; fi; }

# traps <label> <name>: PARTIAL — the operation traps on BOTH interp and the
# compiled binary (interp exits non-zero; the binary exits non-zero).
traps(){ local label="$1" F="$TMPDIR/$2.con"
  local IRC CRC _o
  "$COMPILER" "$F" --interp >/dev/null 2>&1; IRC=$?
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then _o="$("$F.bin" 2>&1)"; CRC=$?; else CRC=200; fi
  if [ $IRC -ne 0 ] && [ $CRC -ne 0 ]; then ok "$label"
  else no "$label (expected both to trap; interp rc=$IRC compiled rc=$CRC)"; fi; }

echo "=== total: yields a value on both ==="
emit tot 'mod m { fn main() -> Int { return 2 + 3; } }'
total "pure arithmetic is total (=> 5)" tot "5"

emit ret 'mod m { fn main() -> Int { let c: Bool = true; let x: Int = if c { return 7; } else { 0 }; return x; } }'
total "divergent branch (return) yields the taken value (=> 7)" ret "7"

echo "=== partial: traps on both (checked arithmetic / bounds) ==="
emit ovf 'mod m { fn main() -> Int { let x: i32 = 2000000000 + 2000000000; return x as Int; } }'
traps "checked add overflow traps" ovf

emit dz 'mod m { fn main() -> Int { let a: Int = 10; let b: Int = 0; return a / b; } }'
traps "division by zero traps" dz

emit oob 'mod m { fn main() -> Int { let arr: [i32; 3] = [1,2,3]; let i: i32 = 5; return arr[(i) as Int] as Int; } }'
traps "array index out of bounds traps" oob

emit btr 'mod m { fn main() -> Int { let c: Bool = true; let x: i32 = if c { 2000000000 + 2000000000 } else { 0 }; return x as Int; } }'
traps "a taken branch that overflows traps" btr

echo
echo "check_totality_judgment: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
