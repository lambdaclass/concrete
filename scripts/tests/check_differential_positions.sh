#!/usr/bin/env bash
# Differential position gate: a flexible-literal overflow (or a width-sensitive
# shift) placed in many SYNTACTIC POSITIONS, asserting the interpreter and the
# compiled binary agree (same value, or both trap on the checked overflow).
#
# Why this gate exists. The 2026-07 differential sweep found two real
# interp-vs-compiled soundness bugs of ONE class — "a context/width exists, but
# one path failed to apply it":
#   1. if/match branch bodies were elaborated with no result hint, so a flexible
#      branch typed Int (i64) while the node was i32 — interp kept 4000000000
#      while the compiled binary truncated to -294967296.
#   2. the interpreter's signed left-shift did not truncate to the width
#      (`100 << 1` at i8 gave interp 200 vs compiled -56).
# Both are the same shape as the type-axis (E0228) family. The random fuzzer
# CANNOT see this class because it bounds arithmetic to avoid overflow, so this
# targeted gate is the permanent guard: each case puts the overflow where the
# width must be applied, so a regression (a position that stops flowing the
# width, or a shift that stops wrapping) re-opens the divergence and fails here.
#
# Agreement rule: interp and compiled must reach the same observable state —
# either both exit 0 with the same stdout, or both exit non-zero (a shared trap
# on checked overflow / bounds). A one-sided value is the bug.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; DIV=0

# agree_or_trap <label> <file>: interp and compiled reach the same state.
# IMPORTANT: capture the compiler exit code WITHOUT a pipe (a `| head` would
# report the pipe's exit, not the compiler's — the flaw that produced false
# results during the sweep).
agree_or_trap(){ local label="$1" F="$2"
  local IOUT IRC COUT CRC istate cstate
  IOUT="$("$COMPILER" "$F" --interp 2>&1)"; IRC=$?
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then COUT="$("$F.bin" 2>&1)"; CRC=$?; else COUT="<compile-fail>"; CRC=250; fi
  [ $IRC -eq 0 ] && istate="ok:$IOUT" || istate="trap"
  [ $CRC -eq 0 ] && cstate="ok:$COUT" || cstate="trap"
  if [ "$istate" = "$cstate" ]; then PASS=$((PASS+1)); echo "  ok   $label ($istate)"
  else DIV=$((DIV+1)); echo "  DIVERGE  $label"
    echo "      interp:   rc=$IRC $(printf '%s' "$IOUT" | head -1)"
    echo "      compiled: rc=$CRC $(printf '%s' "$COUT" | head -1)"; fi; }

emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

echo "=== flexible overflow across syntactic positions (interp == compiled) ==="

emit p1  'mod m { fn main() -> Int { let x: i32 = 2000000000 + 2000000000; return x as Int; } }'
agree_or_trap "let-RHS i32 overflow" "$TMPDIR/p1.con"

emit p2  'mod m { fn f() -> i32 { return 2000000000 + 2000000000; } fn main() -> Int { return f() as Int; } }'
agree_or_trap "return-position i32 overflow" "$TMPDIR/p2.con"

emit p3  'mod m { fn main() -> Int { let c: Bool = true; let d: Bool = true; let x: i32 = if c { if d { 2000000000 + 2000000000 } else { 0 } } else { 0 }; return x as Int; } }'
agree_or_trap "nested-if branch i32 overflow" "$TMPDIR/p3.con"

emit p4  'mod m { fn main() -> Int { let i: i32 = 0; let c: Bool = true; let x: i32 = match i { 0 => if c { 2000000000 + 2000000000 } else { 0 }, _ => 0 }; return x as Int; } }'
agree_or_trap "if-inside-match-arm i32 overflow" "$TMPDIR/p4.con"

emit p5  'mod m { fn main() -> Int { let i: i32 = 0; let c: Bool = true; let x: i32 = if c { match i { _ => 2000000000 + 2000000000 } } else { 0 }; return x as Int; } }'
agree_or_trap "match-inside-if-branch i32 overflow" "$TMPDIR/p5.con"

emit p6  'mod m { fn main() -> Int { let x: i8 = 100 + 100; return x as Int; } }'
agree_or_trap "let-RHS i8 overflow (100+100)" "$TMPDIR/p6.con"

emit p7  'mod m { fn main() -> Int { let x: i16 = 30000 + 30000; return x as Int; } }'
agree_or_trap "let-RHS i16 overflow (30000+30000)" "$TMPDIR/p7.con"

emit p8  'mod m { fn main() -> Int { let x: u8 = 200 + 100; return x as Int; } }'
agree_or_trap "let-RHS u8 overflow (200+100)" "$TMPDIR/p8.con"

emit p9  'mod m { fn main() -> Int { let x: i32 = 100000 * 100000; return x as Int; } }'
agree_or_trap "let-RHS i32 mul overflow" "$TMPDIR/p9.con"

emit p10 'mod m { fn main() -> Int { let x: i32 = ((1000000000 + 1000000000) + 1000000000) + 1000000000; return x as Int; } }'
agree_or_trap "deep binop tree i32 overflow" "$TMPDIR/p10.con"

emit p11 'mod m { fn main() -> Int { let x: u8 = 10 - 20; return x as Int; } }'
agree_or_trap "let-RHS u8 underflow (10-20)" "$TMPDIR/p11.con"

echo "=== signed shift width wrapping (interp == compiled two's-complement) ==="

emit s1  'mod m { fn main() -> Int { let x: i8 = 100 << 1; return x as Int; } }'
agree_or_trap "i8 100 << 1 -> -56 (signed shl truncates)" "$TMPDIR/s1.con"

emit s2  'mod m { fn main() -> Int { let x: i16 = 20000 << 2; return x as Int; } }'
agree_or_trap "i16 20000 << 2 (signed shl truncates)" "$TMPDIR/s2.con"

emit s3  'mod m { fn main() -> Int { let a: i8 = 0 - 50; let x: i8 = a << 1; return x as Int; } }'
agree_or_trap "i8 (-50) << 1 via variable" "$TMPDIR/s3.con"

emit s4  'mod m { fn main() -> Int { let x: u16 = 40000 << 1; return x as Int; } }'
agree_or_trap "u16 40000 << 1 (unsigned shl wraps)" "$TMPDIR/s4.con"

echo "=== labeled break/continue reach the TARGETED loop (interp == compiled) ==="

# `break 'outer` / `continue 'outer` from an inner loop must act on the OUTER
# loop (the \047 in printf is the label apostrophe). The interpreter used to
# discard the label and catch every labeled signal at the innermost loop,
# diverging from the compiled binary (continue: interp 50 vs compiled 0; nested
# break: interp 32 vs compiled 25).
printf 'mod m { fn main() -> Int { let mut sum: Int = 0; let mut i: Int = 0; \047outer: while i < 5 { i = i + 1; let mut j: Int = 0; while j < 5 { j = j + 1; if j == 3 { continue \047outer; } } sum = sum + 10; } return sum; } }\n' > "$TMPDIR/lc.con"
agree_or_trap "continue outer skips the outer-body tail (=> 0)" "$TMPDIR/lc.con"

printf 'mod m { fn main() -> Int { let mut sum: Int = 0; let mut i: Int = 0; \047outer: while i < 10 { let mut j: Int = 0; while j < 10 { sum = sum + 1; j = j + 1; if sum >= 25 { break \047outer; } } i = i + 1; } return sum; } }\n' > "$TMPDIR/lb.con"
agree_or_trap "break outer exits the outer loop (=> 25)" "$TMPDIR/lb.con"

echo "=== clean sanity (values fit; not overflow) ==="

emit c1  'mod m { fn main() -> Int { let c: Bool = true; let x: i32 = if c { 40 + 2 } else { 0 }; return x as Int; } }'
agree_or_trap "clean nested value (42)" "$TMPDIR/c1.con"

emit c2  'mod m { fn main() -> Int { let i: i32 = 1; let x: i32 = match i { 0 => 5, _ => 1000000000 + 1000000000 }; return (x / 2) as Int; } }'
agree_or_trap "match-arm value then divide" "$TMPDIR/c2.con"

echo
echo "check_differential_positions: PASS=$PASS DIVERGE=$DIV"
[ "$DIV" -eq 0 ]
