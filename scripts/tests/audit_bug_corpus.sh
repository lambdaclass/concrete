#!/usr/bin/env bash
# Bug-to-regression corpus audit.
# Checks that every numbered bug in docs/bugs/ has regression test coverage.
#
# Each bug must either:
#   1. Have an entry in BUG_TEST_MAP (bug number -> test file name)
#   2. Be listed in SKIP_BUGS with a justification
#
# THE RECORDS OWN THESE MAPS. Since the ledger migration (scripts/gen/migrate_ledgers.sh) each bug's
# coverage is the `fixtures:` or `coverage:` line of the ledger block in its record,
# docs/bugs/NNN_*.md, and the maps below are built from them at run time by
# `scripts/gen/bug_ledger.py corpus` (never committed, so two branches filing bugs cannot conflict
# here). The inline arrays are the pre-migration form; once any record carries a ledger block they
# must be empty, and an entry left in them fails this audit.
#
# Run as a CI gate or manually:
#   bash scripts/tests/audit_bug_corpus.sh

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BUGS_DIR="$ROOT_DIR/docs/bugs"
TESTS_DIR="$ROOT_DIR/tests/programs"
RUN_TESTS="$ROOT_DIR/scripts/tests/run_tests.sh"

PASS=0
FAIL=0
SKIP=0

# Explicit mapping: bug number -> regression test file(s) in tests/programs/.
# Multiple files separated by space.
declare -A BUG_TEST_MAP=(
  # Empty since the ledger migration: a bug's fixtures are the `fixtures:` line of its record's
  # ledger block (docs/bugs/NNN_*.md). Do not add entries here.
)

# Bugs that don't need a .con regression test (with reason).
declare -A SKIP_BUGS=(
  # Empty since the ledger migration: a bug without a fixture names its coverage in the
  # `coverage:` line of its record's ledger block. Do not add entries here.
)

# The records' coverage lines (see the header). A record-borne entry and a legacy inline entry never
# coexist: the migration moves every inline entry into its record in one step.
LEGACY_ENTRIES=$(( ${#BUG_TEST_MAP[@]} + ${#SKIP_BUGS[@]} ))
if ! LEDGER_ROWS="$(python3 -I "$ROOT_DIR/scripts/gen/bug_ledger.py" corpus --root "$ROOT_DIR")"; then
  echo "FATAL: scripts/gen/bug_ledger.py corpus failed; the bug records' ledger blocks cannot be read" >&2
  exit 2
fi
LEDGER_FAIL=0
if [ -n "$LEDGER_ROWS" ]; then
  if [ "$LEGACY_ENTRIES" -gt 0 ]; then
    echo "  FAIL the bug records carry ledger blocks but this script still holds $LEGACY_ENTRIES inline entries:" \
      "$(printf '%s\n' "${!BUG_TEST_MAP[@]}" "${!SKIP_BUGS[@]}" | sort | tr '\n' ' ')— move each into its record's" \
      "ledger block (fixtures: or coverage:) and delete it here"
    LEDGER_FAIL=1
  fi
  while IFS=$'\t' read -r lnum lkind _lstatus lval; do
    [ -n "$lnum" ] || continue
    case "$lkind" in
      fixtures) BUG_TEST_MAP[$lnum]="$lval" ;;
      coverage) SKIP_BUGS[$lnum]="$lval" ;;
      *) echo "FATAL: bug_ledger.py corpus printed an unknown kind '$lkind' for $lnum" >&2; exit 2 ;;
    esac
  done <<< "$LEDGER_ROWS"
fi

echo "=== Bug-to-Regression Corpus Audit ==="
echo

for bugfile in "$BUGS_DIR"/[0-9][0-9][0-9]_*.md; do
  [[ -f "$bugfile" ]] || continue
  base=$(basename "$bugfile" .md)
  num=${base%%_*}

  # Skip exempted bugs
  if [[ -n "${SKIP_BUGS[$num]:-}" ]]; then
    echo "  skip  $base (${SKIP_BUGS[$num]})"
    SKIP=$((SKIP + 1))
    continue
  fi

  # Check explicit mapping
  if [[ -z "${BUG_TEST_MAP[$num]:-}" ]]; then
    echo "  FAIL  $base -- no entry in BUG_TEST_MAP"
    FAIL=$((FAIL + 1))
    continue
  fi

  issues=""
  for testfile in ${BUG_TEST_MAP[$num]}; do
    # Check test file exists
    if [[ ! -f "$TESTS_DIR/$testfile" ]]; then
      issues="${issues}missing $testfile; "
      continue
    fi
    # Check test is registered in run_tests.sh
    testbase="${testfile%.con}"
    if ! grep -q "$testbase" "$RUN_TESTS" 2>/dev/null; then
      issues="${issues}$testfile not in run_tests.sh; "
    fi
  done

  if [[ -z "$issues" ]]; then
    echo "  ok    $base -> ${BUG_TEST_MAP[$num]}"
    PASS=$((PASS + 1))
  else
    echo "  FAIL  $base -- $issues"
    FAIL=$((FAIL + 1))
  fi
done

echo
echo "=== Reverse check: bug_*.con without a numbered bug ==="

for testfile in "$TESTS_DIR"/bug_*.con; do
  [[ -f "$testfile" ]] || continue
  testbase=$(basename "$testfile")
  # Check if any BUG_TEST_MAP value references this file
  found=false
  for mapped in "${BUG_TEST_MAP[@]}"; do
    for f in $mapped; do
      [[ "$f" == "$testbase" ]] && found=true && break 2
    done
  done
  if ! $found; then
    echo "  warn  $testbase -- not mapped to any numbered bug"
  fi
done

# ============================================================================
# STATUS AGREEMENT: the bug doc and this corpus must not contradict each other.
#
# Bug 063 was recorded three ways at once — the doc said Fixed, this corpus said
# OPEN with "no fixture yet", and the roadmap task still described it as pending
# — and nothing in CI objected. A reader asking "is 063 closed?" got a different
# answer depending on which file they opened, which is the same defect class as
# ABSENCE_IS_NOT_A_FACT.md: a record of a fact that is not tied to the fact.
#
# ONLY THE FIXED/NOT-FIXED BINARY IS ENFORCED, deliberately. The vocabularies do
# not line up beyond that and should not be forced to: bug 058 is CONTAINED in
# its doc and OPEN here, which is an honest pair — contained is not fixed. What
# must never happen is one side reading FIXED while the other does not.
#
# Legacy entries (001-028, 047-057) hold a fixture path rather than a status
# word; they predate the convention and are COUNTED AND REPORTED rather than
# silently skipped, so the coverage of this check stays visible instead of
# looking total.
# ============================================================================
# NO DUPLICATE BUG NUMBERS — in the docs OR in the tables below.
#
# THIS IS THE ONE THAT ACTUALLY BIT. Two unrelated bugs were filed under 064 and
# two more under 065. The tables here are bash ASSOCIATIVE ARRAYS, where a
# repeated key silently OVERWRITES instead of colliding, so one entry of each
# pair simply ceased to exist — and both discarded entries were FIXED bugs, which
# were also missing from the README index. Three records of the same fact and all
# three lost it, with nothing in CI objecting. They are now 069 and 070.
#
# A duplicate is detected by counting SOURCE LINES, not array entries: by the
# time the array exists the evidence is gone, which is exactly why this went
# unnoticed for so long.
echo
echo "=== every bug number is used once ==="
DUP_DOCS="$(ls "$BUGS_DIR"/[0-9][0-9][0-9]_*.md 2>/dev/null | xargs -n1 basename 2>/dev/null | cut -c1-3 | sort | uniq -d || true)"
DUP_KEYS="$(grep -oE '^[[:space:]]*\[[0-9]{3}\]=' "${BASH_SOURCE[0]}" | tr -d ' []=' | sort | uniq -d || true)"
if [ -n "$DUP_DOCS" ]; then
  echo "  FAIL two bug docs share a number: $(echo $DUP_DOCS) — renumber one; the ledger keys by number"
  FAIL=$((FAIL+1))
else
  ok_dup=1
fi
if [ -n "$DUP_KEYS" ]; then
  echo "  FAIL a corpus key is assigned twice: $(echo $DUP_KEYS) — bash keeps the LAST silently, so an entry is being discarded"
  FAIL=$((FAIL+1))
elif [ "${ok_dup:-0}" = "1" ]; then
  echo "  ok   no duplicate numbers in docs or corpus keys"
fi

echo
echo "=== bug doc status agrees with the corpus (FIXED vs not) ==="
if [ -n "$LEDGER_ROWS" ]; then
  # ONE OWNER: since the migration the record's **Status:** line is the only status there is, and
  # bug_ledger.py refuses a coverage line that restates it, so there is no second copy to compare.
  echo "  ok   the record's **Status:** line is the only status (bug_ledger.py refuses a coverage line restating it)"
else
STATUS_CHECKED=0; STATUS_LEGACY=0; STATUS_BAD=0
for doc in "$BUGS_DIR"/[0-9][0-9][0-9]_*.md; do
  [ -e "$doc" ] || continue
  num="$(basename "$doc" | cut -c1-3)"
  # SKIP_BUGS is where the status-bearing rows live (a bug with no fixture of its
  # own, annotated with why); BUG_TEST_MAP holds the legacy fixture paths.
  entry="${SKIP_BUGS[$num]:-${BUG_TEST_MAP[$num]:-}}"
  [ -n "$entry" ] || continue
  # The corpus word, upper-cased. Legacy rows start with a path, not a status.
  cword="$(printf '%s' "$entry" | awk '{print toupper($1)}')"
  case "$cword" in
    FIXED*|OPEN*|HALF*|PARTIAL*|CONTAINED*) : ;;
    *) STATUS_LEGACY=$((STATUS_LEGACY+1)); continue ;;
  esac
  dline="$(grep -m1 '^\*\*Status:\*\*' "$doc" || true)"
  if [ -z "$dline" ]; then
    echo "  FAIL $num: the bug doc has no '**Status:**' line to compare"
    STATUS_BAD=$((STATUS_BAD+1)); continue
  fi
  dword="$(printf '%s' "$dline" | sed 's/^\*\*Status:\*\*[[:space:]]*//' | awk '{print toupper($1)}')"
  STATUS_CHECKED=$((STATUS_CHECKED+1))
  dfixed=no; cfixed=no
  case "$dword" in FIXED*) dfixed=yes ;; esac
  case "$cword" in FIXED*) cfixed=yes ;; esac
  if [ "$dfixed" != "$cfixed" ]; then
    echo "  FAIL $num: doc says '$dword' but the corpus says '$cword' — one of them is wrong"
    STATUS_BAD=$((STATUS_BAD+1))
  fi
done
echo "  checked $STATUS_CHECKED status-bearing entries; $STATUS_LEGACY legacy entries carry a fixture path instead of a status and are not compared"
if [ "$STATUS_CHECKED" -lt 20 ]; then
  echo "  FAIL only $STATUS_CHECKED entries were comparable — the convention is eroding, not holding"
  STATUS_BAD=$((STATUS_BAD+1))
fi
if [ "$STATUS_BAD" -eq 0 ]; then
  echo "  ok   every status-bearing entry agrees with its bug doc"
else
  FAIL=$((FAIL+STATUS_BAD))
fi
fi

FAIL=$((FAIL+LEDGER_FAIL))

echo
echo "=== Results ==="
echo "  pass:    $PASS"
echo "  fail:    $FAIL"
echo "  skip:    $SKIP"

if [[ "$FAIL" -gt 0 ]]; then
  echo
  echo "FAILED: $FAIL bug(s) missing regression coverage."
  echo "Fix: give the bug's record (docs/bugs/NNN_*.md) a ledger block with a fixtures: or coverage: line"
  echo "     (before the ledger migration: an entry in BUG_TEST_MAP or SKIP_BUGS in this script)."
  exit 1
fi

echo
echo "All numbered bugs have regression coverage."
