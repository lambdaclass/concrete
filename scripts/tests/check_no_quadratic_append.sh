#!/usr/bin/env bash
# Anti-pattern ratchet: `xs ++ [x]` in the hot pipeline files.
#
# The 2026-07 Phase 6C #2 complexity-guard sweep found 5+ real O(n²) bugs of ONE
# shape — `acc ++ [x]` appended per element inside a fold/loop (O(len acc) each,
# O(n²) total) — across the lexer, parser, mono, resolve, SSACleanup, and
# SSAVerify. The fix in every case: accumulate REVERSED (prepend is O(1)) and
# `.reverse` once, or use `Std.HashMap`/`Std.HashSet` for membership/lookup.
#
# This is a RATCHET, not a blanket ban: many `++ [x]` are over small, fixed-size
# lists (params, type args) and are harmless. Distinguishing "inside a loop over n
# elements" by grep is unreliable, so instead we pin the CURRENT count per hot
# file and fail if it INCREASES — i.e. a NEW `xs ++ [x]` must be justified. When
# you fix one (convert to prepend+reverse / a map), RATCHET THE BASELINE DOWN in
# this file. New per-element appends in a loop should never be added; a genuinely
# one-shot `++ [x]` (not in a loop) may bump the baseline with a one-line note.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0

# file  baseline-count-of-'++ ['  (ratchet: current count must be <= baseline).
# Pin the values from the committed tree; lower them as appends are eliminated.
check() {
  local f="$1" base="$2"
  [ -f "$f" ] || { echo "  ok   $f (absent — skipped)"; return; }
  local n; n=$(grep -cE '\+\+ \[' "$f")
  if [ "$n" -le "$base" ]; then
    echo "  ok   $f ($n <= $base)"; PASS=$((PASS+1))
  else
    echo "  FAIL $f: $n '++ [' occurrences > baseline $base"
    echo "        A new 'xs ++ [x]' was added to a hot pipeline file. If it is inside"
    echo "        a fold/loop over n elements it is O(n^2) — accumulate reversed"
    echo "        (prepend + .reverse once) or use Std.HashMap/HashSet. If it is a"
    echo "        one-shot (not in a loop), raise this baseline with a note."
    FAIL=$((FAIL+1))
  fi
}

echo "=== O(n^2) '++ [x]' ratchet over hot pipeline files ==="
check Concrete/Frontend/Lexer.lean       1
check Concrete/Frontend/Parser.lean      64
check Concrete/IR/Lower.lean             64
check Concrete/IR/Mono.lean              5
check Concrete/IR/SSACleanup.lean        1
check Concrete/IR/SSAVerify.lean         2
check Concrete/Backend/EmitSSA.lean      29
check Concrete/Resolve/Resolve.lean      8
check Concrete/Check/Check.lean          4
check Concrete/Check/CheckHelpers.lean   8
check Concrete/Elab/Elab.lean            57
check Concrete/Interp/Interp.lean        6

echo
echo "check_no_quadratic_append: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
