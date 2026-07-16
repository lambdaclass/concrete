#!/usr/bin/env bash
# Integer-arithmetic semantics gate (ROADMAP Phase 6.5 #1).
#
# One source of truth for integer arithmetic (Concrete/Semantics/IntArith.lean)
# only matters if it keeps three views of a program in lockstep:
#
#   interpret  ==  fold-then-interpret  ==  compiled
#
# The interpreter is the oracle; constant folding (SSACleanup) and the backend
# checked helpers (EmitSSA) must agree with it on BOTH the value AND the trap of
# every integer operation. The scary regression is a fold that turns a trapping
# expression into a (wrapped) constant, or DCE deleting a documented trap — so
# this gate leads with trap preservation under folding (the red-team), then
# checks value agreement between the constant-folded and dynamic paths.
#
# "Folded path"  = both operands are compile-time constants (SSACleanup folds).
# "Dynamic path" = an operand is threaded through a loop so it is not a
#                   provable constant (forces the runtime helper / interp eval).
#
# Needs the compiler built; runs in the compiler test job.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
# run_prog <file> -> sets globals RC (exit code) and OUT (stdout, last line).
# Compiled side runs the self-printing wrapper (MAIN_EXIT_MODEL stage 2).
run_prog(){ gate_selfprint_wrap "$1" "$TMP/b_wrapped.con"; "$C" "$TMP/b_wrapped.con" -o "$TMP/b.bin" >/dev/null 2>&1 && { OUT="$("$TMP/b.bin" 2>/dev/null)"; RC=$?; } || { OUT="<compile-failed>"; RC=255; }; }
interp_prog(){ IOUT="$("$C" "$1" --interp 2>&1)"; IRC=$?; }

# trap_preserved <label> <program>: the program's integer op TRAPS. Both the
# compiled binary AND the interpreter must trap (nonzero) — a fold must NOT turn
# it into a value. This is the red-team: a constant, foldable, overflowing op.
trap_preserved(){
  local n="$1"; printf '%s' "$2" > "$TMP/$n.con"
  run_prog "$TMP/$n.con"; interp_prog "$TMP/$n.con"
  if [ "$RC" -ne 0 ] && [ "$IRC" -ne 0 ]; then ok "$n: trap preserved (compiled rc=$RC, interp rc=$IRC)"
  else no "$n: TRAP ERASED (compiled rc=$RC out='$OUT', interp rc=$IRC) — a fold or helper dropped the trap"; fi
}

# agree <label> <expected> <program>: compiled and interp both produce <expected>
# (value on stdout, exit 0). Used for both folded and dynamic variants.
agree(){
  local n="$1" want="$2"; printf '%s' "$3" > "$TMP/$n.con"
  run_prog "$TMP/$n.con"; interp_prog "$TMP/$n.con"
  local il; il="$(printf '%s' "$IOUT" | tail -1)"
  if [ "$RC" -eq 0 ] && [ "$OUT" = "$want" ] && [ "$il" = "$want" ]; then ok "$n: = $want (compiled == interp)"
  else no "$n: want $want; compiled rc=$RC out='$OUT'; interp rc=$IRC out='$il'"; fi
}

echo "=== TRAP PRESERVATION under constant folding (red-team: a fold must not erase a trap) ==="
# Both operands are let-bound constants → SSACleanup can fold. The fold must
# refuse (result out of range) and leave the op live so it traps at runtime,
# matching the interpreter. i32: 2e9 + 2e9 = 4e9 > i32::MAX.
trap_preserved fold_add_ovf 'mod m { fn main() -> Int { let a: i32 = 2000000000; let b: i32 = 2000000000; return (a + b) as Int; } }'
# u8: 200 + 100 = 300 > 255.
trap_preserved fold_add_u8  'mod m { fn main() -> Int { let a: u8 = 200; let b: u8 = 100; return (a + b) as Int; } }'
# u8: 0 - 1 underflow.
trap_preserved fold_sub_u8  'mod m { fn main() -> Int { let a: u8 = 0; let b: u8 = 1; return (a - b) as Int; } }'
# i32: 100000 * 100000 = 1e10 overflow.
trap_preserved fold_mul_ovf 'mod m { fn main() -> Int { let a: i32 = 100000; let b: i32 = 100000; return (a * b) as Int; } }'
# constant divide-by-zero must trap, not fold.
trap_preserved fold_div_zero 'mod m { fn main() -> Int { let a: i32 = 5; let b: i32 = 0; return (a / b) as Int; } }'
# DCE red-team: the overflowing product is UNUSED (0 * x). It must NOT be DCE'd
# away — the documented trap must still fire (found by fuzz seed 20260703).
trap_preserved dce_unused_ovf 'mod m { fn main() -> Int { let a: i32 = 100000; let b: i32 = 100000; let p: i32 = a * b; let z: i32 = 0; return (z * p) as Int; } }'

echo "=== VALUE AGREEMENT: folded path == dynamic path == interp, in range ==="
# Folded: constant operands. Dynamic: same values threaded through a loop so the
# op is not a provable constant. Both must equal the interpreter.
agree add_fold   "42" 'mod m { fn main() -> Int { let a: i32 = 40; let b: i32 = 2; return (a + b) as Int; } }'
agree add_dyn    "42" 'mod m { fn main() -> Int { let mut a: i32 = 0; while a < 40 { a = a + 1; } return (a + 2) as Int; } }'
# signed negative truncated division: -17 / 5 = -3 (toward zero), NOT -4.
agree div_neg_fold "-3" 'mod m { fn main() -> Int { let a: i32 = -17; let b: i32 = 5; return (a / b) as Int; } }'
agree div_neg_dyn  "-3" 'mod m { fn main() -> Int { let mut a: i32 = 0; while a > -17 { a = a - 1; } return (a / 5) as Int; } }'
# signed negative truncated remainder: -17 % 5 = -2 (sign of dividend).
agree mod_neg_fold "-2" 'mod m { fn main() -> Int { let a: i32 = -17; let b: i32 = 5; return (a % b) as Int; } }'
# wrapping (explicit): i32::MAX + 1 wraps to i32::MIN, no trap.
agree wrap_add   "-2147483648" 'mod m { fn main() -> Int { let a: i32 = 2147483647; return wrapping_add(a, 1) as Int; } }'
# saturating (explicit): u8 200 + 100 clamps to 255.
agree sat_add    "255" 'mod m { fn main() -> Int { let a: u8 = 200; return saturating_add(a, 100) as Int; } }'
# negative bitwise at width: -1 & 255 = 255 (must NOT clamp negative to 0).
agree band_neg   "255" 'mod m { fn main() -> Int { let a: i32 = -1; let b: i32 = 255; return (a & b) as Int; } }'

echo ""
echo "INT-ARITH-SEMANTICS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
