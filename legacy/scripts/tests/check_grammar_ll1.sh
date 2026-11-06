#!/usr/bin/env bash
# The grammar is LL(1) except for its listed conflicts, and three independent checkers agree on why.
#
# grammar/concrete.ebnf is checked by three deliberately independent implementations,
# scripts/check_ll1.{py,c,rs} (added together in ced8b114 as a diverse-implementation cross-check:
# one LL(1) claim, three FIRST/nullable computations that share no code). Three checkers that each
# only print "passed" are three opinions nobody compares, and the C and Rust ones ran only as
# ad-hoc CI steps the local runner could not see. This gate is the one place they run:
#
#   1. each checker's --canonical output on the real grammar (every rule's nullability, FIRST and
#      FOLLOW set, then every FIRST/FIRST and FIRST/FOLLOW conflict) is byte-identical across the
#      three, and its conflicts are exactly grammar/ll1_known_conflicts.tsv (bug 329);
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

# THE REAL GRAMMAR'S CONFLICTS ARE EXACTLY THE KNOWN ONES (bug 329). Since the checkers compute
# FOLLOW sets, the grammar shows FIRST/FOLLOW conflicts that no checker reported before. They are
# not resolved here (every grammar edit is the owner's); grammar/ll1_known_conflicts.tsv lists each
# with its disposition, and may only shrink: an unlisted conflict fails, and so does a listed one
# that no longer occurs. With an empty list the real grammar must check clean.
KNOWN="grammar/ll1_known_conflicts.tsv"
run_all "$GRAMMAR" real
grep -v '^#' "$KNOWN" | grep -v '^$' | cut -f1 | LC_ALL=C sort > "$WORK/known"
grep -E '^conflict(-follow)? ' "$WORK/real.py" | LC_ALL=C sort > "$WORK/real.conflicts"
if [ -s "$WORK/known" ]; then agree real 1; else agree real 0; fi
if cmp -s "$WORK/known" "$WORK/real.conflicts"; then
  :
else
  no "the real grammar's LL(1) conflicts differ from $KNOWN (< listed, > found):"
  diff "$WORK/known" "$WORK/real.conflicts" >&2
fi
bad_rows="$(awk -F'\t' '!/^#/ && NF > 0 && (NF != 3 || $2 == "" || $3 == "")' "$KNOWN")"
[ -z "$bad_rows" ] || no "$KNOWN: every row needs the conflict, a disposition and a reason (3 tab-separated columns)"
# THE LIST ONLY SHRINKS, against the base revision (merge-base with origin/main; the parent on
# main; LL1_BASE when set, e.g. github.event.before in CI). Listing a new conflict does not admit it.
BASE="${LL1_BASE:-}"
case "$BASE" in *[!0]*) ;; *) BASE="" ;; esac
if [ -z "$BASE" ]; then
  MB="$(git merge-base HEAD origin/main 2>/dev/null || true)"
  if [ -z "$MB" ]; then no "no merge-base with origin/main (fetch it, or set LL1_BASE)"
  elif [ "$MB" != "$(git rev-parse HEAD)" ]; then BASE="$MB"
  else BASE="HEAD~1"; fi
fi
if [ -n "$BASE" ]; then
  if ! git rev-parse --verify -q "$BASE^{commit}" >/dev/null; then
    no "base revision '$BASE' does not resolve (fetch history, or set LL1_BASE)"
  elif ! git cat-file -e "$BASE:$KNOWN" 2>/dev/null; then
    # The base revision has no list: this change introduces it (bug 329), so there is nothing to
    # shrink from. Said, not silent.
    echo "  note: $(git rev-parse --short "$BASE") has no $KNOWN; this change introduces the list"
  elif git show "$BASE:$KNOWN" > "$WORK/known_base.tsv" 2>/dev/null; then
    grep -v '^#' "$WORK/known_base.tsv" | grep -v '^$' | cut -f1 | LC_ALL=C sort > "$WORK/known_base"
    added="$(LC_ALL=C comm -13 "$WORK/known_base" "$WORK/known")"
    [ -z "$added" ] || no "$KNOWN gained conflicts relative to $(git rev-parse --short "$BASE"); the list only shrinks: $added"
  fi
fi
grep -q '^rule program ' "$WORK/real.py" || no "real: the canonical dump has no 'program' rule (vacuous output)"
grep -qxF 'follow program <eof>' "$WORK/real.py" || no "real: FOLLOW(program) is not exactly end of input (the bug 329 regression)"

n=0
for g in "$FIXTURES"/*.ebnf; do
  [ -e "$g" ] || continue
  t="planted_$(basename "$g" .ebnf)"
  run_all "$g" "$t"
  agree "$t" 1
  grep -qE '^conflict(-follow)? ' "$WORK/$t.py" || no "$t: no conflict line reported"
  n=$((n + 1))
done
[ "$n" -ge 10 ] || no "expected at least ten planted grammars under $FIXTURES, found $n"
# Bug 329's regression: the FIRST/FOLLOW fixtures must fail as FOLLOW conflicts, not by accident.
# EVERY PLANTED GRAMMAR IS PINNED TO ITS EXACT CONFLICTS (tests/fixtures/grammar_ll1/
# expected_conflicts.tsv): no more and no fewer lines than listed, so neither a shared omission nor
# a spurious extra report can pass. Between them the follow_* grammars cover every FIRST/FOLLOW kind
# (an optional, a repetition, an alternative against a nullable one, two nullable alternatives, a
# nullable body under [ ] and { }, and a conflict reached only through FOLLOW propagated from the
# enclosing rule); the others cover FIRST/FIRST shapes.
EXPECTED="$FIXTURES/expected_conflicts.tsv"
for g in "$FIXTURES"/*.ebnf; do
  b="$(basename "$g" .ebnf)"
  awk -F'\t' -v f="$b" '!/^#/ && $1 == f { print $2 }' "$EXPECTED" | LC_ALL=C sort > "$WORK/want.$b"
  grep -E '^conflict(-follow)? ' "$WORK/planted_$b.py" | LC_ALL=C sort > "$WORK/got.$b"
  if [ ! -s "$WORK/want.$b" ]; then no "planted_$b: no expected conflicts listed in $EXPECTED"
  elif ! cmp -s "$WORK/want.$b" "$WORK/got.$b"; then
    no "planted_$b: conflicts differ from $EXPECTED (< expected, > reported):"
    diff "$WORK/want.$b" "$WORK/got.$b" >&2
  fi
done

if [ "$FAIL" -ne 0 ]; then exit 1; fi
echo "grammar LL(1): 3 checkers agree on $(grep -c '^rule ' "$WORK/real.py") rules and their FIRST and FOLLOW sets; $(wc -l < "$WORK/real.conflicts" | tr -d ' ') conflicts, all known ($KNOWN); $n planted grammars refused by all three"
