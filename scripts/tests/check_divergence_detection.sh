#!/usr/bin/env bash
# Divergence-detection gate.
#
# `blockDiverges` (Concrete/Check/CheckHelpers.lean) decides whether a block can fall through.
# It is what lets a linear value be consumed in an `if` without `else` whose then-branch never
# reaches the code after it; without it that program is E0213. Each ACCEPTED fixture below is
# legal only because one arm of `stmtDiverges` recognises its then-branch as diverging, and each
# REJECTED fixture is the same shape with the divergence removed, so the rule is pinned from both
# sides: answering "never diverges" fails the accepted rows, answering "always diverges" fails the
# rejected ones.
#
# Accepted fixtures are also COMPILED AND RUN, and the result is compared with the interpreter.
# That is how bug 078 was found: the nested if/else row passed checking and then failed SSA
# verification (E0703), because lowering left the merge of an if/else whose arms both return open
# as if it were reachable. Checking alone would not have seen it.
#
# This is the gate the `divergence-detection` mutation family names. It previously named
# run_tests.sh, which the mutant aborts partway (an unguarded compile of a program that needs
# divergence), so the campaign could only report "never reached the end" — INVALID, not killed.

set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

HDR='struct Bar { x: i32, }
impl Bar { fn drop(self) { let Bar { x } = self; } }
'
emit(){ printf '%s%s\n' "$HDR" "$2" > "$TMPDIR/$1.con"; }

# accepted <name> <expected-exit> <label>: compiles, runs, exits as expected, and agrees with --interp.
accepted(){ local f="$TMPDIR/$1.con" want="$2" label="$3" out rc got interp
  out="$("$COMPILER" "$f" -o "$f.bin" 2>&1)"; rc=$?
  if [ $rc -ne 0 ]; then
    no "$label — rejected: $(printf '%s' "$out" | grep -oE '\(E[0-9]+\)[^|]*' | head -1)"; return
  fi
  "$f.bin"; got=$?
  interp="$("$COMPILER" "$f" --interp 2>/dev/null | tail -1)"
  if [ "$got" = "$want" ] && [ "$interp" = "$want" ]; then
    ok "$label (exit $got, interpreter agrees)"
  else
    no "$label — compiled exit $got, interpreter '$interp', expected $want"
  fi
}

# rejected <name> <label>: refused with E0213, the error divergence detection exists to lift.
rejected(){ local f="$TMPDIR/$1.con" label="$2" out rc
  out="$("$COMPILER" "$f" -o "$f.bin" 2>&1)"; rc=$?
  if [ $rc -ne 0 ] && printf '%s' "$out" | grep -q 'E0213'; then
    ok "$label (E0213)"
  else
    no "$label — expected E0213, got rc=$rc: $(printf '%s' "$out" | head -1)"
  fi
}

echo "=== a then-branch that diverges lets the fall-through path own the value ==="
emit ret 'fn main() -> i32 { let b: Bar = Bar { x: 1 }; if false { b.drop(); return 1; } b.drop(); return 0; }'
accepted ret 0 "return diverges"
emit brk 'fn main() -> i32 { let mut i: i32 = 0; while i < 3 { let b: Bar = Bar { x: i }; if i == 1 { b.drop(); break; } b.drop(); i = i + 1; } return i; }'
accepted brk 1 "break diverges"
emit cont 'fn main() -> i32 { let mut i: i32 = 0; let mut n: i32 = 0; while i < 4 { i = i + 1; let b: Bar = Bar { x: i }; if i == 2 { b.drop(); continue; } b.drop(); n = n + 1; } return n; }'
accepted cont 3 "continue diverges"
emit loop 'fn main() -> i32 { let b: Bar = Bar { x: 1 }; if false { b.drop(); while true { } } b.drop(); return 4; }'
accepted loop 4 "while true without break diverges"
emit ifelse 'fn main() -> i32 { let b: Bar = Bar { x: 1 }; let c: bool = false; if false { b.drop(); if c { return 1; } else { return 2; } } b.drop(); return 5; }'
accepted ifelse 5 "if/else whose arms both diverge diverges (bug 078: lowered and verified)"

echo "=== bug 078: an if/else whose arms both diverge leaves no reachable merge ==="
cat > "$TMPDIR/b078_stmt.con" <<'E'
fn main() -> i32 { let a: [i32; 2] = [7, 8]; let c: bool = false; if false { if c { return 1; } else { return 2; } } return a[0]; }
E
accepted b078_stmt 7 "statement form, branch not taken"
cat > "$TMPDIR/b078_taken.con" <<'E'
fn main() -> i32 { let a: [i32; 2] = [7, 8]; let c: bool = true; if a[1] == 8 { if c { return 1; } else { return 2; } } return a[0]; }
E
accepted b078_taken 1 "statement form, branch taken"
cat > "$TMPDIR/b078_expr.con" <<'E'
fn main() -> i32 { let a: [i32; 2] = [7, 8]; let c: bool = false; if a[0] == 7 { let v: i32 = if c { return 1; } else { return 2; }; return v; } return a[0]; }
E
accepted b078_expr 2 "expression form (if-expression whose arms both return)"
cat > "$TMPDIR/b078_loop.con" <<'E'
fn main() -> i32 { let a: [i32; 2] = [7, 8]; let mut i: i32 = 0; while i < 5 { if i == 3 { if a[0] == 7 { break; } else { return 9; } } i = i + 1; } return i + a[1]; }
E
accepted b078_loop 11 "break/return arms inside a loop"

echo "=== the same shapes without divergence stay refused ==="
emit neg_plain 'fn main() -> i32 { let b: Bar = Bar { x: 1 }; if false { b.drop(); } b.drop(); return 0; }'
rejected neg_plain "a then-branch that falls through"
emit neg_loopbrk 'fn main() -> i32 { let b: Bar = Bar { x: 1 }; if false { b.drop(); while true { break; } } b.drop(); return 0; }'
rejected neg_loopbrk "while true WITH a break does not diverge"
emit neg_halfif 'fn main() -> i32 { let b: Bar = Bar { x: 1 }; let c: bool = false; if false { b.drop(); if c { return 1; } else { } } b.drop(); return 0; }'
rejected neg_halfif "an if/else with one falling-through arm does not diverge"

echo ""
echo "DIVERGENCE-DETECTION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
