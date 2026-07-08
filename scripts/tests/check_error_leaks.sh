#!/usr/bin/env bash
# Error-leak gate (pipeline second-tier: panic-to-diagnostic boundary).
#
# A user-triggerable mistake must surface as a clean COMPILER DIAGNOSTIC
# (`error[pass]: ...` with a source location), never as an internal leak from a
# lower layer — an `llvm-as`/LLVM-IR-validation failure, a `clang`/`ld` linker
# error, a Lean `PANIC`, or a crash. Those leaks are the "impossible compiler
# bug" surface escaping to the user; every one of them is either a checker gap
# (reject earlier) or a genuine ICE.
#
# This gate feeds a corpus of malformed / edge programs and asserts each is
# rejected by a clean diagnostic with no leak markers. It caught bug 024
# (recursive structs with infinite size reached llvm-as as a recursive LLVM
# struct type instead of a checker error). Extend the corpus whenever a new
# "ugly error instead of a diagnostic" case turns up.
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

LEAK='llvm-as|LLVM IR validation|Undefined symbols|^ld:|clang: error|llc:|PANIC|panic|Segmentation|stack overflow|uncaught exception'

# clean_reject <label> <source> [expected-substring]
# compile must FAIL, output must contain a clean `error[` diagnostic and NONE of
# the internal-leak markers. If given, output must also contain <substring>.
clean_reject(){
  local n="$1"; printf '%s' "$2" > "$TMP/$n.con"
  local out rc
  out="$("$C" "$TMP/$n.con" -o "$TMP/$n.bin" 2>&1)" && rc=0 || rc=$?
  if [ "$rc" -eq 0 ]; then no "$n: expected rejection, but it compiled"; return; fi
  if printf '%s' "$out" | grep -qE "$LEAK"; then
    no "$n: LEAKED a lower-layer error: $(printf '%s' "$out" | grep -oE "$LEAK" | head -1)"; return; fi
  if ! printf '%s' "$out" | grep -q "error\["; then
    no "$n: rejected but with no clean 'error[...]' diagnostic: $(printf '%s' "$out" | head -1)"; return; fi
  if [ $# -ge 3 ] && ! printf '%s' "$out" | grep -qi "$3"; then
    no "$n: diagnostic missing expected text '$3': $(printf '%s' "$out" | head -1)"; return; fi
  ok "$n"
}

# accepts <label> <source>: a valid program (esp. the not-a-false-positive
# counterexamples) must still compile.
accepts(){
  local n="$1"; printf '%s' "$2" > "$TMP/$n.con"
  if "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >/dev/null 2>&1; then ok "$n"
  else no "$n: expected to compile"; fi
}

echo "=== recursive/infinite-size types are a checker diagnostic, not an llvm-as leak (bug 024) ==="
clean_reject rec_direct  'mod m { struct S { x: S } fn main() -> Int { return 0; } }'                                    "recursive type"
clean_reject rec_mutual  'mod m { struct A { b: B } struct B { a: A } fn main() -> Int { return 0; } }'                  "recursive type"
clean_reject rec_array   'mod m { struct S { xs: [S; 2] } fn main() -> Int { return 0; } }'                              "recursive type"

echo "=== an executable with no entry point is a diagnostic, not an ld leak (bug 025) ==="
clean_reject no_main    'mod m { fn helper() -> Int { return 1; } }'  "no \`main\` function"
clean_reject empty_file ''                                            "no \`main\` function"

echo "=== indirection breaks the cycle — valid recursive shapes still compile (no false positive) ==="
accepts ll_ptr    'mod m { struct Node { val: i32, next: *const Node } fn main() -> Int { return 0; } }'
accepts ll_mutual 'mod m { struct A { b: *mut B } struct B { a: *const A } fn main() -> Int { return 0; } }'
accepts nested_ok 'mod m { struct P { x: i32 } struct Q { p: P } fn main() -> Int { return 0; } }'

echo ""
echo "ERROR-LEAKS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
