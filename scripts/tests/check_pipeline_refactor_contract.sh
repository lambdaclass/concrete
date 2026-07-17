#!/usr/bin/env bash
# Phase 6B / 6.5 validation artifact (ROADMAP item 32).
#
# The capstone contract for the compiler-pipeline refactor: a compact corpus that
# exercises every Phase 6B refactor axis end-to-end through the ONE composed
# pipeline, and proves the two properties the phase promises:
#
#   (A) behavior-preserving for ACCEPTED programs — a well-formed program yields
#       the SAME observable result on the interpreter (the reference semantics)
#       and the compiled binary (same value, or both trap); and
#   (B) fail-closed for MALFORMED programs — each per-axis violation is rejected
#       with its owning diagnostic BEFORE Lower, never miscompiled.
#
# Axes covered (each has its own focused gate too; this artifact is the single
# entrypoint that asserts they hold together and stay behavior-preserving):
# arithmetic reference semantics, mixed-width/type policy, CopyJudgment,
# OwnershipJudgment, TotalityJudgment, CapabilityJudgment, CoreCheck boundary,
# the returned-reference boundary, the pure-discard rule + discard escape, and the
# control-flow builder (value-bearing if/while, and the unit-typed if-expr that
# must not emit `alloca void`).
#
# This is a contract, not new truth: it reuses the shipped rules and just pins
# that the composed pipeline is behavior-preserving + fail-closed over the corpus.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

# (A) agree <label> <name> <expected>: ACCEPTED — interp AND compiled both exit 0
# with <expected> stdout (behavior-preserving value; the reference == the codegen).
# Compiled side runs the self-printing wrapper (MAIN_EXIT_MODEL stage 2).
agree(){ local label="$1" F="$TMPDIR/$2.con" want="$3" I C
  I="$("$COMPILER" "$F" --interp 2>&1)"
  gate_selfprint_wrap "$F" "$F.w.con"
  if "$COMPILER" "$F.w.con" -o "$F.bin" >/dev/null 2>&1; then C="$("$F.bin" 2>&1)"; else C="<compile-fail>"; fi
  if [ "$I" = "$want" ] && [ "$C" = "$want" ]; then ok "$label (=> $want)"
  else no "$label (want $want; interp=$(printf '%s' "$I"|head -1) compiled=$(printf '%s' "$C"|head -1))"; fi; }

# (A) both_trap <label> <name>: ACCEPTED shape whose evaluation TRAPS identically —
# interp exits non-zero AND the compiled binary exits non-zero (shared trap:
# checked overflow, div-by-zero, bounds). A one-sided trap is a miscompile.
both_trap(){ local label="$1" F="$TMPDIR/$2.con" IRC CRC _o
  "$COMPILER" "$F" --interp >/dev/null 2>&1; IRC=$?
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then _o="$("$F.bin" 2>&1)"; CRC=$?; else CRC=200; fi
  if [ $IRC -ne 0 ] && [ $CRC -ne 0 ]; then ok "$label (both trap)"
  else no "$label (expected both trap; interp rc=$IRC compiled rc=$CRC)"; fi; }

# (B) closed <label> <name> <code>: MALFORMED — rejected with the owning diagnostic
# (fail-closed before Lower, never compiled).
closed(){ local label="$1" F="$TMPDIR/$2.con" want="$3" out
  out="$("$COMPILER" "$F" -o "$F.bin" 2>&1)"
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then no "$label (compiled — $want not raised, MISCOMPILE RISK)"
  elif grep -q <<<"$out" "$want"; then ok "$label (fail-closed $want)"
  else no "$label (rejected but not $want: $(printf '%s' "$out" | head -1))"; fi; }

echo "=== (A) behavior-preserving: interp (reference) == compiled ==="

emit arith 'mod m { fn main() -> Int { return (2 + 3) * 4 - 1; } }'
agree "arithmetic reference semantics" arith "19"

emit widths 'mod m { fn main() -> Int { let a: i8 = 5; let b: i16 = 300; return (a as Int) + (b as Int); } }'
agree "mixed-width via explicit casts" widths "305"

emit copydup 'mod m { struct Copy P { x: i32, y: i32 } fn main() -> Int { let p: P = P { x: 7, y: 2 }; let q: P = p; return (p.x + q.y) as Int; } }'
agree "CopyJudgment: Copy struct duplicated" copydup "9"

emit vif 'mod m { fn main() -> Int { let c: Bool = true; let x: Int = if c { 40 + 2 } else { 0 }; return x; } }'
agree "control-flow builder: value-bearing if-expr" vif "42"

emit vwhile 'mod m { fn main() -> Int { let mut i: Int = 0; let mut x: Int = 7; while i < 10 { i = i + 1; if i == 4 { x = 42; break; } } return x; } }'
agree "control-flow builder: loop result via break (stmt form; value while removed 6D#2)" vwhile "42"

emit unitif 'mod m { fn main() -> Int { let mut acc: Int = 0; let u = if acc == 0 { acc = 5; } else { acc = 9; }; return acc; } }'
agree "control-flow builder: unit if-expr (no alloca void)" unitif "5"

emit discok 'mod m { fn main() -> Int { discard(2 + 3); return 0; } }'
agree "discard escape acknowledges a pure Copy discard" discok "0"

echo "=== (A) behavior-preserving traps: interp and compiled trap together ==="

emit ovf 'mod m { fn main() -> Int { let x: i32 = 2000000000 + 2000000000; return x as Int; } }'
both_trap "TotalityJudgment: checked overflow" ovf

emit dz 'mod m { fn main() -> Int { let a: Int = 10; let b: Int = 0; return a / b; } }'
both_trap "TotalityJudgment: division by zero" dz

emit oob 'mod m { fn main() -> Int { let arr: [i32; 3] = [1,2,3]; let i: i32 = 5; return arr[(i) as Int] as Int; } }'
both_trap "array bounds" oob

echo "=== (B) fail-closed: each axis violation rejected before Lower ==="

emit mixw 'mod m { fn main() -> Int { let a: i8 = 5; let b: i16 = 3; let c: i8 = a + b; return c as Int; } }'
closed "type policy: mixed-width binop" mixw "E0228"

emit uam 'mod m { struct Box { v: i32 } fn sink(b: Box) -> i32 { let Box { v } = b; return v; } fn main() -> Int { let b: Box = Box { v: 3 }; let x: i32 = sink(b); let y: i32 = sink(b); return (x + y) as Int; } }'
closed "OwnershipJudgment: use-after-move" uam "E0205"

emit lindrop 'mod m { struct Box { v: i32 } fn main() -> Int { let b: Box = Box { v: 3 }; return 0; } }'
closed "OwnershipJudgment: linear value never consumed" lindrop "E0208"

emit puredisc 'mod m { fn main() -> Int { 2 + 3; return 0; } }'
closed "discard rule: pure Copy value discarded" puredisc "E0294"

emit retref 'mod m { fn bad(x: &Int) -> &Int { return x; } fn main() -> Int { return 0; } }'
closed "returned-reference boundary" retref "reference"

emit misscap 'mod m { fn needs() with(File) -> Int { return 0; } fn main() -> Int { return needs(); } }'
closed "CapabilityJudgment: missing capability" misscap "File"

echo
echo "check_pipeline_refactor_contract: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
