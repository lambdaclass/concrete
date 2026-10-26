#!/usr/bin/env bash
# ONE OWNER PER FACT: the shared ledgers are generated from records nobody else edits.
#
# Run by check_docs_drift.sh (leg 4b). The facts and their owners:
#
#   a bug's status, summary, coverage, classes, silent/loud, exits   its record, docs/bugs/NNN_*.md
#   a change log entry                                               its fragment, changelog.d/*.md
#   the CI gate inventory                                            scripts/tests/ci_gate_inventory.txt
#
# and the views generated from them: the bug lists in docs/bugs/README.md, the bugs column of
# STRUCTURAL_CLASSES.md's summary table and the exit bug spans in ROADMAP.md (scripts/gen/bug_ledger.py),
# the coverage maps of audit_bug_corpus.sh (built at run time, never committed) and CHANGELOG.md
# (fragments folded by scripts/gen/changelog.py --fold, stamped so a hand edit is visible).
#
# Legs:
#   1. the committed views match the records, and CHANGELOG.md matches its stamp (before the migration,
#      scripts/gen/migrate_ledgers.sh, both report "not migrated" and pass);
#   2. negative controls against a MIGRATED COPY: before the migration the copy is made by running the
#      migration itself on a scratch tree, so the migration is exercised on every run until it lands;
#      a hand edit of each view, a record changed without regenerating, malformed records and missing
#      markers must each be refused, with the exit code that distinguishes stale (1) from malformed (2);
#   3. positive controls: a new record regenerates and checks; a rebase conflict inside a generated
#      block, or one confined to a ROADMAP exit span, is resolved by --write; fragments fold newest
#      first and the folded file checks;
#   4. the copy's audit_bug_corpus.sh and check_bug_filing_complete.sh read coverage from the records:
#      they pass, and fail on a record whose fixture is missing or on a legacy inline entry left behind.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
BL=scripts/gen/bug_ledger.py
CL=scripts/gen/changelog.py
TMP="$(mktemp -d)" || { echo "FATAL: mktemp -d failed" >&2; exit 2; }
case "$TMP" in /?*/?*) [ -d "$TMP" ] || { echo "FATAL: mktemp gave no directory" >&2; exit 2; } ;;
  *) echo "FATAL: unusable temporary directory '$TMP'" >&2; exit 2 ;; esac
trap 'rm -rf "$TMP"' EXIT

echo "=== 1. the committed views match their records ==="
if python3 -I "$BL" --check; then ok "bug ledger views match the records"
else no "bug ledger views differ from the records (run python3 $BL --write)"; fi
if python3 -I "$CL" --check; then ok "CHANGELOG.md matches its stamp and every changelog.d/ fragment is well-formed"
else no "CHANGELOG.md was hand-edited or a fragment is malformed (see above)"; fi
if [ -f scripts/tests/ci_gate_inventory.txt ]; then
  if [ "$(LC_ALL=C sort -u scripts/tests/ci_gate_inventory.txt)" = "$(cat scripts/tests/ci_gate_inventory.txt)" ]; then
    ok "the CI gate inventory is sorted and duplicate-free (run_ci_gates_local.sh compares it with the workflow)"
  else no "scripts/tests/ci_gate_inventory.txt is not sorted (LC_ALL=C sort -u) or holds a duplicate"; fi
fi

# ----------------------------------------------------------------------------- the migrated copy
# BASE is built once: a copy of the inputs, migrated by scripts/gen/migrate_ledgers.py when this tree
# is not migrated yet, plus CONTROL RECORD 997, which every record-level control mutates. The controls
# own their record instead of borrowing a real bug's, so fixing or reclassifying a real bug can never
# break a control.
BASE="$TMP/base"; C="$TMP/tree"
mkdir -p "$BASE/docs" "$BASE/scripts/tests" "$BASE/scripts/gen" "$BASE/.github/workflows"
cp -R docs/bugs "$BASE/docs/bugs"
cp ROADMAP.md CHANGELOG.md "$BASE/"
cp -R changelog.d "$BASE/changelog.d"
cp scripts/gen/bug_ledger.py scripts/gen/changelog.py scripts/gen/migrate_ledgers.py "$BASE/scripts/gen/"
cp scripts/tests/audit_bug_corpus.sh scripts/tests/check_bug_filing_complete.sh \
   scripts/tests/run_tests.sh scripts/tests/run_ci_gates_local.sh "$BASE/scripts/tests/"
# The copied gates start by sourcing the gate-shell guard from beside themselves.
mkdir -p "$BASE/scripts/tests/lib" && cp scripts/tests/lib/gate_env.sh "$BASE/scripts/tests/lib/"
[ -f scripts/tests/ci_gate_inventory.txt ] && cp scripts/tests/ci_gate_inventory.txt "$BASE/scripts/tests/"
cp .github/workflows/lean_action_ci.yml "$BASE/.github/workflows/"
ln -s "$ROOT_DIR/tests" "$BASE/tests"
MIGRATED_HERE=0; [ -n "$(python3 -I "$BL" corpus)" ] && MIGRATED_HERE=1
: > "$TMP/migrate.log"
if [ "$MIGRATED_HERE" = 0 ]; then
  python3 -I "$BASE/scripts/gen/migrate_ledgers.py" "$BASE" > "$TMP/migrate.log" 2>&1 \
    && python3 -I "$BASE/scripts/gen/bug_ledger.py" --write --root "$BASE" >> "$TMP/migrate.log" 2>&1 \
    && python3 -I "$BASE/scripts/gen/changelog.py" --stamp-initial --root "$BASE" >> "$TMP/migrate.log" 2>&1
fi
if [ "$MIGRATED_HERE" = 1 ]; then
  ok "the tree is migrated; the controls run on a copy of it"
elif python3 -I "$BL" --check --root "$BASE" >/dev/null 2>&1 && [ -n "$(python3 -I "$BL" corpus --root "$BASE")" ] \
     && python3 -I "$CL" --check --root "$BASE" 2>/dev/null | grep -q 'matches its stamp'; then
  ok "the migration (scripts/gen/migrate_ledgers.py) runs on this tree and its result checks; the controls run on it"
else
  no "the migration does not run cleanly on this tree (log below); scripts/gen/migrate_ledgers.sh would fail"
  sed 's/^/      /' "$TMP/migrate.log" | awk 'NR<=40'
fi
# The first generated exit span, if the roadmap has one; exit-span controls need it.
EXIT_ID="$(grep -oE 'BEGIN GENERATED: exit [0-9][a-z] bugs' "$BASE/ROADMAP.md" | awk 'NR==1 {print $4}')"
cat > "$BASE/docs/bugs/997_ledger_control.md" <<'EOF'
# Bug 997 — the ledger gate's control record (scripts/tests/check_ledgers.sh)

<!-- bug-ledger
summary: control record
coverage: control record, covered by scripts/tests/check_ledgers.sh
class: 1 — control note
silent: no
-->

**Status:** OPEN

## Structural cause

control

## Structural owner

control
EOF
python3 -I "$BL" --write --root "$BASE" >/dev/null 2>&1 || no "the control record 997 does not regenerate into the migrated copy"
make_copy() { rm -rf "$C"; cp -RP "$BASE" "$C"; }

edit() { # file (relative to the copy), python edit over s
  python3 -I - "$C/$1" "$2" <<'PY'
import sys, re
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
exec(sys.argv[2])
open(p, 'w', encoding='utf-8').write(s)
PY
}
expect() { # name, expected exit, command...
  local name="$1" want="$2"; shift 2
  local rc=0
  "$@" >/dev/null 2>&1 || rc=$?
  [ "$rc" = "$want" ] && ok "control $name is refused (exit $rc)" || no "control $name: exit $rc, expected $want"
}
neg() { # name, file, python edit, expected exit
  make_copy
  if ! edit "$2" "$3"; then no "control $1: could not build its copy"; return; fi
  expect "$1" "$4" python3 -I "$BL" --check --root "$C"
}
R=docs/bugs/997_ledger_control.md
add() { printf "old='summary: '; assert s.count(old)==1; s=s.replace(old, '%s\\\\nsummary: ')" "$1"; }

echo "=== 2. hand edits, stale views and malformed records are refused ==="
make_copy
expect clean_copy_checks 0 python3 -I "$BL" --check --root "$C"
neg readme_hand_edit docs/bugs/README.md "old='## Open Numbered Bugs\n\n'; assert s.count(old)==1; s=s.replace(old, old+'- [999_x.md](999_x.md) — added by hand\n')" 1
neg class_cell_hand_edit docs/bugs/STRUCTURAL_CLASSES.md "import re; s,n=re.subn(r'^(\| 1\. [^|]*\| )', r'\g<1>999, ', s, flags=re.M); assert n==1" 1
neg status_changed_not_regenerated "$R" "import re; s,n=re.subn(r'^\*\*Status:\*\* OPEN', '**Status:** FIXED', s, count=1, flags=re.M); assert n==1" 1
neg class_removed_not_regenerated "$R" "import re; s,n=re.subn(r'^class: 1 .*\n', '', s, count=1, flags=re.M); assert n==1" 1
neg class_note_changed_not_regenerated "$R" "old='control note'; assert s.count(old)==1; s=s.replace(old, 'another note')" 1
neg summary_changed_not_regenerated "$R" "old='summary: control record'; assert s.count(old)==1; s=s.replace(old, 'summary: CHANGED')" 1
neg unknown_key "$R" "$(add 'severity: high')" 2
neg fixtures_and_coverage "$R" "$(add 'fixtures: bug_print_builtins.con')" 2
neg no_coverage_at_all "$R" "import re; s,n=re.subn(r'^coverage: .*\n', '', s, count=1, flags=re.M); assert n==1" 2
neg no_summary "$R" "import re; s,n=re.subn(r'^summary: .*\n', '', s, count=1, flags=re.M); assert n==1" 2
neg key_given_twice "$R" "$(add 'silent: yes')" 2
neg class_twice "$R" "$(add 'class: 1')" 2
neg record_without_block "$R" "import re; s,n=re.subn(r'^<!-- bug-ledger\n.*?^-->\n', '', s, count=1, flags=re.M|re.S); assert n==1" 2
neg unclosed_block "$R" "import re; s,n=re.subn(r'^-->\n', '', s, count=1, flags=re.M); assert n==1" 2
neg two_blocks "$R" "s = s + '\n<!-- bug-ledger\nsummary: x\ncoverage: OPEN -- y.sh\n-->\n'" 2
neg class_not_in_table "$R" "$(add 'class: 999')" 2
neg exit_without_span "$R" "$(add 'exit: 9z')" 2
neg silent_bad_value "$R" "import re; s,n=re.subn(r'^silent: no', 'silent: maybe', s, count=1, flags=re.M); assert n==1" 2
neg status_word_unknown "$R" "import re; s,n=re.subn(r'^\*\*Status:\*\* OPEN', '**Status:** OPNE', s, count=1, flags=re.M); assert n==1" 2
neg readme_markers_missing docs/bugs/README.md "old='<!-- END GENERATED: bug list -->'; assert s.count(old)==1; s=s.replace(old,'')" 2
neg class_markers_missing docs/bugs/STRUCTURAL_CLASSES.md "old='<!-- END GENERATED: class table -->'; assert s.count(old)==1; s=s.replace(old,'')" 2
neg summary_restates_status "$R" "old='summary: control record'; assert s.count(old)==1; s=s.replace(old, 'summary: OPEN: control record')" 2
neg coverage_restates_status "$R" "old='coverage: control'; assert s.count(old)==1; s=s.replace(old, 'coverage: FIXED -- control')" 2
neg summary_lowercase_status_lead "$R" "old='summary: control record'; assert s.count(old)==1; s=s.replace(old, 'summary: Contained: control record')" 2
neg coverage_lowercase_status_lead "$R" "old='coverage: control'; assert s.count(old)==1; s=s.replace(old, 'coverage: Fixed -- control')" 2
neg summary_status_midtext "$R" "old='summary: control record'; assert s.count(old)==1; s=s.replace(old, 'summary: control record (OPEN since 2099)')" 2
neg class_note_with_pipe "$R" "old='control note'; assert s.count(old)==1; s=s.replace(old, 'a | b')" 2
neg class_row_unreadable docs/bugs/STRUCTURAL_CLASSES.md "import re; s,n=re.subn(r'^\| 1\. ', '|1. ', s, count=1, flags=re.M); assert n==1" 2
neg summary_status_no_punctuation "$R" "old='summary: control record'; assert s.count(old)==1; s=s.replace(old, 'summary: Fixed in R-1234')" 2
neg value_closes_comment "$R" "old='summary: control record'; assert s.count(old)==1; s=s.replace(old, 'summary: control --> visible')" 2
neg class_block_prose docs/bugs/STRUCTURAL_CLASSES.md "old='<!-- END GENERATED: class table -->'; assert s.count(old)==1; s=s.replace(old, 'prose inside the block\n'+old)" 2
neg duplicate_number "$R" "import shutil, os; d=os.path.dirname(p); shutil.copy(p, os.path.join(d, '997_duplicate.md'))" 2
if [ -n "$EXIT_ID" ]; then
  neg exit_span_hand_edit ROADMAP.md "import re; s,n=re.subn(r'(<!-- BEGIN GENERATED: exit $EXIT_ID bugs[^>]*-->)', r'\g<1>999, ', s); assert n==1" 1
  neg exit_added_not_regenerated "$R" "$(add "exit: $EXIT_ID")" 1
  neg exit_span_orphan_end ROADMAP.md "old='<!-- END GENERATED: exit $EXIT_ID bugs -->'; assert s.count(old)==1; s=s.replace(old, old+' x '+old)" 2
  neg exit_span_unclosed ROADMAP.md "old='<!-- END GENERATED: exit $EXIT_ID bugs -->'; assert s.count(old)==1; s=s.replace(old,'')" 2
else
  ok "ROADMAP.md has no generated exit bug span; the exit-span controls have nothing to mutate"
fi

echo "=== 3. regeneration: a new record, rebase conflicts, fragments ==="
make_copy
cat > "$C/docs/bugs/998_control_bug.md" <<EOF
# Bug 998 — a second control record

<!-- bug-ledger
summary: control: a new open bug
coverage: control record, covered by scripts/tests/check_ledgers.sh
class: 1 — the control's note
silent: yes
${EXIT_ID:+exit: $EXIT_ID}
-->

**Status:** OPEN
EOF
if python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 && python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 \
   && grep -qF '[998_control_bug.md](998_control_bug.md) — control: a new open bug' "$C/docs/bugs/README.md" \
   && grep -qE '^\| 1\. .*998 \(the control.s note\)' "$C/docs/bugs/STRUCTURAL_CLASSES.md" \
   && { [ -z "$EXIT_ID" ] || grep -qE "BEGIN GENERATED: exit $EXIT_ID bugs[^>]*-->[^<]*998" "$C/ROADMAP.md"; }; then
  ok "positive control: a new record regenerates into every view and checks"
else no "positive control: a new record did not regenerate into the README, class table and exit span"; fi
# A rebase conflict inside the README block: both sides regenerated the list.
make_copy
edit docs/bugs/README.md "old='## Open Numbered Bugs\n\n'; assert s.count(old)==1; s=s.replace(old, old+'<<<<<<< HEAD\n- [997_a.md](997_a.md) — ours\n=======\n- [996_b.md](996_b.md) — theirs\n>>>>>>> theirs\n')"
expect conflicted_readme_is_stale 1 python3 -I "$BL" --check --root "$C"
if python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 && python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 \
   && ! grep -qE '^(<{7}|>{7})' "$C/docs/bugs/README.md"; then
  ok "positive control: --write resolves a conflict inside the README's generated block"
else no "positive control: --write left a README block conflict unresolved"; fi
if [ -n "$EXIT_ID" ]; then
  # A conflict confined to a ROADMAP exit span: the whole table row conflicts, the sides differ only there.
  make_copy
  edit ROADMAP.md "import re
m = re.search(r'^.*BEGIN GENERATED: exit $EXIT_ID bugs.*\n', s, re.M); row = m.group(0)
ours = re.sub(r'(BEGIN GENERATED: exit $EXIT_ID bugs[^>]*-->)', r'\g<1>997, ', row); theirs = re.sub(r'(BEGIN GENERATED: exit $EXIT_ID bugs[^>]*-->)', r'\g<1>996, ', row)
s = s[:m.start()] + '<<<<<<< HEAD\n' + ours + '=======\n' + theirs + '>>>>>>> theirs\n' + s[m.end():]"
  if python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 && python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 \
     && ! grep -qE '^(<{7}|>{7})' "$C/ROADMAP.md"; then
    ok "positive control: --write resolves a conflict confined to a ROADMAP exit span"
  else no "positive control: --write left an exit-span conflict unresolved"; fi
  # ...but a conflict that also differs OUTSIDE the span is a real one and is left for a human.
  make_copy
  edit ROADMAP.md "import re
m = re.search(r'^.*BEGIN GENERATED: exit $EXIT_ID bugs.*\n', s, re.M); row = m.group(0)
other = row.replace('BEGIN GENERATED', 'OUTSIDE EDIT -->X<!-- BEGIN GENERATED', 1)
s = s[:m.start()] + '<<<<<<< HEAD\n' + row + '=======\n' + other + '>>>>>>> theirs\n' + s[m.end():]"
  python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 || true
  if grep -qE '^<{7}' "$C/ROADMAP.md"; then
    ok "control: a conflict outside the generated span is left in place for a human"
  else no "control: --write silently resolved a conflict outside a generated span"; fi
fi
# A conflict in the class table whose sides disagree on a class-owned cell (one side drops the last
# class row) is a real conflict: --write must refuse, not union the rows back.
make_copy
edit docs/bugs/STRUCTURAL_CLASSES.md "import re
rows = [m for m in re.finditer(r'^\| \d+\. .*\n', s, re.M)]
last = rows[-1]
s = s[:last.start()] + '<<<<<<< HEAD\n' + last.group(0) + '=======\n' + '>>>>>>> theirs\n' + s[last.end():]"
expect class_row_deleted_on_one_side 2 python3 -I "$BL" --write --root "$C"
make_copy
edit docs/bugs/STRUCTURAL_CLASSES.md "import re
m = re.search(r'^\| 1\. ([^|]*)\| ([^|]*)\| (.*)\n', s, re.M); row = m.group(0)
other = row.replace(m.group(2), m.group(2).rstrip() + ', 996 ', 1)
s = s[:m.start()] + '<<<<<<< HEAD\n' + row + '=======\n' + other + '>>>>>>> theirs\n' + s[m.end():]"
if python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 && python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 \
   && ! grep -qE '^(<{7}|>{7})' "$C/docs/bugs/STRUCTURAL_CLASSES.md"; then
  ok "positive control: --write resolves a class-table conflict confined to the generated bugs column"
else no "positive control: --write left a bugs-column conflict in the class table unresolved"; fi
# Changelog: a stamped copy, a hand edit, malformed fragments, a fold.
make_copy
python3 -I "$CL" --check --root "$C" >/dev/null 2>&1 || python3 -I "$CL" --stamp-initial --root "$C" >/dev/null 2>&1
edit CHANGELOG.md "old='## Major Milestones\n'; i=s.index(old); j=s.index('\n', i+len(old))+1; s=s[:j]+'\n### Added by hand (2099-01-01)\n\nx\n'+s[j:]"
expect changelog_hand_edit 1 python3 -I "$CL" --check --root "$C"
make_copy
edit CHANGELOG.md "import re; s,n=re.subn(r'^<!-- changelog stamp: .*\n', '', s, flags=re.M); assert n==1"
expect changelog_stamp_deleted 1 python3 -I "$CL" --check --root "$C"
make_copy
edit CHANGELOG.md "import re; s,n=re.subn(r'(<!-- changelog stamp: )[0-9a-f]{32}', r'\g<1>not-a-digest', s, flags=re.M); assert n==1"
expect changelog_stamp_damaged 2 python3 -I "$CL" --check --root "$C"
make_copy
edit CHANGELOG.md "old='## Major Milestones\n'; i=s.index(old); j=s.index('\n', i+len(old))+1; s=s[:j]+'\n### Completed Task R-9998\n\nx\n'+s[j:]"
expect changelog_print_refuses_hand_edit 2 python3 -I "$CL" --print --root "$C"
if python3 -I "$CL" --restamp --root "$C" >/dev/null 2>&1 && python3 -I "$CL" --check --root "$C" >/dev/null 2>&1; then
  ok "positive control: --restamp repairs a damaged stamp"
else no "positive control: --restamp could not repair a damaged stamp"; fi
make_copy
edit CHANGELOG.md "import re; s,n=re.subn(r'^<!-- changelog stamp: .*\n', '', s, flags=re.M); assert n==1"
if python3 -I "$CL" --restamp --root "$C" >/dev/null 2>&1 && python3 -I "$CL" --check --root "$C" >/dev/null 2>&1; then
  ok "positive control: --restamp repairs a deleted stamp"
else no "positive control: --restamp could not repair a deleted stamp"; fi
cl_neg() { # name, fragment file name, content
  make_copy
  python3 -I "$CL" --check --root "$C" >/dev/null 2>&1 || python3 -I "$CL" --stamp-initial --root "$C" >/dev/null 2>&1
  printf '%s' "$3" > "$C/changelog.d/$2"
  expect "$1" 2 python3 -I "$CL" --check --root "$C"
}
cl_neg fragment_bad_name "Bug175.md" $'### A heading\n\nbody\n'
cl_neg fragment_no_heading "2099-01-01-x.md" $'just text\n'
cl_neg fragment_level2_heading "2099-01-01-x.md" $'### A heading\n\n## Not allowed\n'
cl_neg fragment_conflict_marker "2099-01-01-x.md" $'### A heading\n<<<<<<< HEAD\nx\n'
make_copy
python3 -I "$CL" --check --root "$C" >/dev/null 2>&1 || python3 -I "$CL" --stamp-initial --root "$C" >/dev/null 2>&1
printf '### Control Older (2099-01-01)\n\nolder body\n' > "$C/changelog.d/2099-01-01-older.md"
printf '### Control Newer (2099-01-02)\n\nnewer body\n\n### Completed Task R-9999\n\ndone\n' > "$C/changelog.d/2099-01-02-newer.md"
python3 -I "$CL" --print --root "$C" > "$TMP/printed.md" 2>/dev/null
if python3 -I "$CL" --fold --root "$C" >/dev/null 2>&1 && python3 -I "$CL" --check --root "$C" >/dev/null 2>&1 \
   && [ -z "$(ls "$C/changelog.d" | grep -v '^README.md$')" ] \
   && python3 -I - "$C/CHANGELOG.md" <<'PY'
import sys
s = open(sys.argv[1], encoding='utf-8').read()
i, j, k = s.find('### Control Newer'), s.find('### Completed Task R-9999'), s.find('### Control Older')
sys.exit(0 if 0 < i < j < k and s.index('## Major Milestones') < i else 1)
PY
then
  ok "positive control: fragments fold newest first under Major Milestones, are removed, and the result checks"
else no "positive control: the fold misordered, kept fragments or left an unstamped file"; fi
python3 -I - "$TMP/printed.md" "$C/CHANGELOG.md" <<'PY' && ok "control: --print before the fold equals the folded file (minus its stamp)" || no "control: --print disagrees with the fold"
import re, sys
strip = lambda t: re.sub(r'^<!-- changelog stamp: .*\n', '', t, flags=re.M)
sys.exit(0 if strip(open(sys.argv[1]).read()) == strip(open(sys.argv[2]).read()) else 1)
PY

echo "=== 4. the audits read coverage from the records ==="
make_copy
if bash "$C/scripts/tests/audit_bug_corpus.sh" >/dev/null 2>&1; then ok "audit_bug_corpus.sh passes on the migrated copy"
else no "audit_bug_corpus.sh fails on the migrated copy"; fi
if bash "$C/scripts/tests/check_bug_filing_complete.sh" >/dev/null 2>&1; then ok "check_bug_filing_complete.sh passes on the migrated copy"
else no "check_bug_filing_complete.sh fails on the migrated copy"; fi
edit "$R" "import re; s,n=re.subn(r'^coverage: .*', 'fixtures: no_such_fixture_999.con', s, count=1, flags=re.M); assert n==1"
expect audit_missing_fixture 1 bash "$C/scripts/tests/audit_bug_corpus.sh"
expect filing_missing_fixture 1 bash "$C/scripts/tests/check_bug_filing_complete.sh"
make_copy
edit scripts/tests/audit_bug_corpus.sh "old='declare -A SKIP_BUGS=(\n'; assert s.count(old)==1; s=s.replace(old, old+'  [996]=\"OPEN -- a legacy entry left behind\"\n')"
expect audit_legacy_entry_left 1 bash "$C/scripts/tests/audit_bug_corpus.sh"

echo
echo "LEDGERS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
