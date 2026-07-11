#!/usr/bin/env bash
# OwnershipJudgment agreement matrix (Phase 6.5).
#
# Ownership/linearity is decided in Check (accept/reject), realized by Lower as a
# move/drop plan, and modeled by the interpreter as consumption. This gate proves
# the three agree, as a MATRIX of scenarios rather than a data model:
#
#   ACCEPT rows  — the program type-checks; interp and the compiled binary must
#                  produce the SAME result. This is the load-bearing tri-agreement:
#                  a move/drop mismatch between Lower and the interpreter (double
#                  free, missed drop, wrong move) shows up as a value divergence.
#   REJECT rows  — Check rejects (both paths share Check, so both fail with the
#                  same ownership diagnostic). Confirms the linear rule fires.
#
# If an ACCEPT row diverges, that is a real Lower-vs-interp ownership bug; build
# the shared move/drop record where the drift is.

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

# agree <label> <name> <expected>: type-checks; interp == compiled == expected.
agree(){ local label="$1" F="$TMPDIR/$2.con" want="$3"
  local I C
  I="$("$COMPILER" "$F" --interp 2>&1)"
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then C="$("$F.bin" 2>&1)"; else C="<compile-fail>"; fi
  if [ "$I" = "$want" ] && [ "$C" = "$want" ]; then ok "$label"
  else no "$label (want $want, interp=$(printf '%s' "$I"|head -1) compiled=$(printf '%s' "$C"|head -1))"; fi; }

# both_reject <label> <name> <code>: Check rejects on BOTH paths with <code>.
both_reject(){ local label="$1" F="$TMPDIR/$2.con" code="$3"
  local IOUT CRC
  IOUT="$("$COMPILER" "$F" --interp 2>&1)"
  "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; CRC=$?
  if [ $CRC -ne 0 ] && printf '%s' "$IOUT" | grep -q "$code"; then ok "$label"
  else no "$label (want reject $code; compiled rc=$CRC, interp=$(printf '%s' "$IOUT"|head -1))"; fi; }

echo "=== ACCEPT: interp move/drop/consumption == compiled runtime ==="

emit mv 'mod m { fn main() -> Int { let a: String = "hi"; let b: String = a; let n: Int = string_length(&b); drop_string(b); return n; } }'
agree "value move: String a->b, use b (=> 2)" mv "2"

emit dup 'mod m { fn main() -> Int { let a: i32 = 5; let b: i32 = a; let c: i32 = a; return (b + c) as Int; } }'
agree "Copy duplicate: i32 used twice (=> 10)" dup "10"

emit br 'mod m { fn main() -> Int { let c: Bool = true; let s: String = "x"; if c { drop_string(s); } else { drop_string(s); } return 0; } }'
agree "branch consume agreement (both arms consume) (=> 0)" br "0"

emit mt 'mod m { fn main() -> Int { let i: i32 = 1; let s: String = "x"; match i { 0 => { drop_string(s); } _ => { drop_string(s); } } return 0; } }'
agree "match consume agreement (all arms consume) (=> 0)" mt "0"

emit rc 'mod m { fn take(s: String) -> Int { let n: Int = string_length(&s); drop_string(s); return n; } fn main() -> Int { let s: String = "abc"; return take(s); } }'
agree "return/param consume: move into callee (=> 3)" rc "3"

emit bw 'mod m { fn main() -> Int { let s: String = "hello"; let n: Int = string_length(&s); drop_string(s); return n; } }'
agree "borrow does not consume: &s then drop (=> 5)" bw "5"

emit ow 'mod m { fn main() -> Int { let mut s: String = "aa"; drop_string(s); s = "bbbb"; let n: Int = string_length(&s); drop_string(s); return n; } }'
agree "assignment overwrite: mut String reassigned (=> 4)" ow "4"

emit bv 'mod m { fn main() -> Int { let mut i: i32 = 0; let s: String = while true { break "hey"; } else { "" }; let n: Int = string_length(&s); drop_string(s); return n; } }'
agree "break value: while yields a String, consumed after (=> 3)" bv "3"

echo "=== REJECT: Check enforces linearity on both paths ==="

emit dupr 'mod m { fn main() -> Int { let a: String = "hi"; let b: String = a; let c: String = a; drop_string(b); drop_string(c); return 0; } }'
both_reject "non-Copy duplicate (use after move)" dupr "E0205"

emit brd 'mod m { fn main() -> Int { let c: Bool = true; let s: String = "x"; if c { drop_string(s); } return 0; } }'
both_reject "branch consume disagreement (one arm only)" brd "E021"

emit leak 'mod m { fn main() -> Int { let s: String = "x"; return 0; } }'
both_reject "non-Copy value never consumed (leak)" leak "E020"

emit proj 'mod m { struct Box { s: String } fn main() -> Int { let box: Box = Box { s: "hi" }; let taken: String = box.s; drop_string(taken); return 0; } }'
both_reject "non-Copy projection: move field out by value" proj "E0290"

echo
echo "check_ownership_judgment: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
