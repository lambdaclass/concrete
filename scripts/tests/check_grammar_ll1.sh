#!/usr/bin/env bash
# The grammar is LL(1), and three independent checkers agree on why.
#
# grammar/concrete.ebnf is checked by three deliberately independent implementations,
# scripts/check_ll1.{py,c,rs} (added together in ced8b114 as a diverse-implementation cross-check:
# one LL(1) claim, three FIRST/nullable computations that share no code). Three checkers that each
# only print "passed" are three opinions nobody compares, and the C and Rust ones ran only as
# ad-hoc CI steps the local runner could not see. This gate is the one place they run:
#
#   1. each checker's --canonical output on the real grammar (every rule's nullability and FIRST
#      set, then every conflict) is byte-identical across the three, and reports no conflict;
#   2. on each planted grammar under tests/fixtures/grammar_ll1/, all three exit 1 and agree
#      byte-for-byte on the conflicts they report, so a checker that silently passes is caught.
#
# Requires python3, a C compiler (cc) and rustc; the nix devshell provides all three.
# Usage: scripts/tests/check_grammar_ll1.sh
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

GRAMMAR="grammar/concrete.ebnf"
FIXTURES="tests/fixtures/grammar_ll1"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/check_grammar_ll1.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
FAIL=0
no() { echo "FAIL: $*" >&2; FAIL=1; }

for tool in python3 cc rustc; do
  command -v "$tool" >/dev/null 2>&1 || { echo "FAIL: $tool not found (run inside nix develop)" >&2; exit 2; }
done
cc -Wall -Wextra -Werror -O2 -o "$WORK/check_ll1_c" scripts/check_ll1.c || { echo "FAIL: check_ll1.c does not build" >&2; exit 1; }
rustc -O -o "$WORK/check_ll1_rs" scripts/check_ll1.rs || { echo "FAIL: check_ll1.rs does not build" >&2; exit 1; }

# run_all <grammar> <tag>: each checker's canonical output in $WORK/<tag>.<impl>, its exit in $WORK/<tag>.<impl>.rc
run_all() {
  python3 scripts/check_ll1.py --canonical "$1" >"$WORK/$2.py"; echo $? >"$WORK/$2.py.rc"
  "$WORK/check_ll1_c" --canonical "$1" >"$WORK/$2.c"; echo $? >"$WORK/$2.c.rc"
  "$WORK/check_ll1_rs" --canonical "$1" >"$WORK/$2.rs"; echo $? >"$WORK/$2.rs.rc"
}

# agree <tag> <expected exit>
agree() {
  local impl
  for impl in py c rs; do
    [ "$(cat "$WORK/$1.$impl.rc")" = "$2" ] || no "$1: check_ll1.$impl exited $(cat "$WORK/$1.$impl.rc"), expected $2"
  done
  for impl in c rs; do
    cmp -s "$WORK/$1.py" "$WORK/$1.$impl" || {
      no "$1: check_ll1.py and check_ll1.$impl disagree (< py, > $impl):"
      diff "$WORK/$1.py" "$WORK/$1.$impl" | head -20 >&2
    }
  done
}

run_all "$GRAMMAR" real
agree real 0
grep -q '^rule program ' "$WORK/real.py" || no "real: the canonical dump has no 'program' rule (vacuous output)"

n=0
for g in "$FIXTURES"/*.ebnf; do
  [ -e "$g" ] || continue
  t="planted_$(basename "$g" .ebnf)"
  run_all "$g" "$t"
  agree "$t" 1
  grep -q '^conflict ' "$WORK/$t.py" || no "$t: no conflict line reported"
  n=$((n + 1))
done
[ "$n" -ge 4 ] || no "expected at least four planted grammars under $FIXTURES, found $n"

if [ "$FAIL" -ne 0 ]; then exit 1; fi
echo "grammar LL(1): 3 checkers agree on $(grep -c '^rule ' "$WORK/real.py") rules, no conflict; $n planted grammars refused by all three"
