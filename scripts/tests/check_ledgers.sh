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
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
BL=scripts/gen/bug_ledger.py
CL=scripts/gen/changelog.py
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

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
C="$TMP/tree"
make_copy() {
  rm -rf "$C"; mkdir -p "$C/docs" "$C/scripts/tests" "$C/scripts/gen" "$C/.github/workflows"
  cp -R docs/bugs "$C/docs/bugs"
  cp ROADMAP.md CHANGELOG.md "$C/"
  cp -R changelog.d "$C/changelog.d"
  cp scripts/gen/bug_ledger.py scripts/gen/changelog.py scripts/gen/migrate_ledgers.py "$C/scripts/gen/"
  cp scripts/tests/audit_bug_corpus.sh scripts/tests/check_bug_filing_complete.sh \
     scripts/tests/run_tests.sh scripts/tests/run_ci_gates_local.sh "$C/scripts/tests/"
  [ -f scripts/tests/ci_gate_inventory.txt ] && cp scripts/tests/ci_gate_inventory.txt "$C/scripts/tests/"
  cp .github/workflows/lean_action_ci.yml "$C/.github/workflows/"
  ln -s "$ROOT_DIR/tests" "$C/tests"
  if [ -z "$(python3 -I "$BL" corpus)" ]; then
    python3 -I "$C/scripts/gen/migrate_ledgers.py" "$C" > "$TMP/migrate.log" 2>&1 \
      && python3 -I "$C/scripts/gen/bug_ledger.py" --write --root "$C" >> "$TMP/migrate.log" 2>&1 \
      && python3 -I "$C/scripts/gen/changelog.py" --stamp-initial --root "$C" >> "$TMP/migrate.log" 2>&1
  fi
}
make_copy
MIGRATED_HERE=0; [ -n "$(python3 -I "$BL" corpus)" ] && MIGRATED_HERE=1
if [ "$MIGRATED_HERE" = 1 ]; then
  ok "the tree is migrated; the controls run on a copy of it"
elif python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 && [ -n "$(python3 -I "$BL" corpus --root "$C")" ]; then
  ok "the migration (scripts/gen/migrate_ledgers.py) runs on this tree and its result checks; the controls run on it"
else
  no "the migration does not run cleanly on this tree (log below); scripts/gen/migrate_ledgers.sh would fail"
  sed 's/^/      /' "$TMP/migrate.log" | awk 'NR<=40'
fi

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
REC124="$(cd docs/bugs && ls 124_*.md)"
REC102="$(cd docs/bugs && ls 102_*.md)"

echo "=== 2. hand edits, stale views and malformed records are refused ==="
neg readme_hand_edit docs/bugs/README.md "old='## Open Numbered Bugs\n\n'; assert s.count(old)==1; s=s.replace(old, old+'- [999_x.md](999_x.md) — added by hand\n')" 1
neg class_cell_hand_edit docs/bugs/STRUCTURAL_CLASSES.md "import re; s,n=re.subn(r'^(\| 15\. [^|]*\| )', r'\g<1>999, ', s, flags=re.M); assert n==1" 1
neg exit_span_hand_edit ROADMAP.md "import re; s,n=re.subn(r'(<!-- BEGIN GENERATED: exit 0b bugs[^>]*-->)', r'\g<1>999, ', s); assert n==1" 1
neg status_changed_not_regenerated "docs/bugs/$REC124" "import re; s,n=re.subn(r'^\*\*Status:\*\* OPEN', '**Status:** FIXED', s, count=1, flags=re.M); assert n==1" 1
neg class_changed_not_regenerated "docs/bugs/$REC124" "import re; s,n=re.subn(r'^(class: 2\b)', 'class: 15\nclass: 2', s, count=1, flags=re.M); assert n==1" 1
neg exit_changed_not_regenerated "docs/bugs/$REC124" "old='summary: '; assert s.count(old)==1; s=s.replace(old, 'exit: 0b\nsummary: ')" 1
neg summary_changed_not_regenerated "docs/bugs/$REC102" "import re; s,n=re.subn(r'^summary: ', 'summary: CHANGED ', s, count=1, flags=re.M); assert n==1" 1
neg unknown_key "docs/bugs/$REC124" "old='summary: '; assert s.count(old)==1; s=s.replace(old, 'severity: high\nsummary: ')" 2
neg fixtures_and_coverage "docs/bugs/$REC102" "old='summary: '; assert s.count(old)==1; s=s.replace(old, 'coverage: also a gate\nsummary: ')" 2
neg no_coverage_at_all "docs/bugs/$REC102" "import re; s,n=re.subn(r'^fixtures: .*\n', '', s, count=1, flags=re.M); assert n==1" 2
neg no_summary "docs/bugs/$REC124" "import re; s,n=re.subn(r'^summary: .*\n', '', s, count=1, flags=re.M); assert n==1" 2
neg record_without_block "docs/bugs/$REC124" "import re; s,n=re.subn(r'^<!-- bug-ledger\n.*?^-->\n', '', s, count=1, flags=re.M|re.S); assert n==1" 2
neg unclosed_block "docs/bugs/$REC124" "import re; s,n=re.subn(r'^-->\n', '', s, count=1, flags=re.M); assert n==1" 2
neg class_not_in_table "docs/bugs/$REC124" "old='summary: '; assert s.count(old)==1; s=s.replace(old, 'class: 99\nsummary: ')" 2
neg exit_without_span "docs/bugs/$REC124" "old='summary: '; assert s.count(old)==1; s=s.replace(old, 'exit: 9z\nsummary: ')" 2
neg silent_bad_value "docs/bugs/$REC124" "old='summary: '; assert s.count(old)==1; s=s.replace(old, 'silent: maybe\nsummary: ')" 2
neg readme_markers_missing docs/bugs/README.md "old='<!-- END GENERATED: bug list -->'; assert s.count(old)==1; s=s.replace(old,'')" 2
neg class_markers_missing docs/bugs/STRUCTURAL_CLASSES.md "old='<!-- END GENERATED: class table -->'; assert s.count(old)==1; s=s.replace(old,'')" 2
neg exit_span_unclosed ROADMAP.md "old='<!-- END GENERATED: exit 0b bugs -->'; assert s.count(old)==1; s=s.replace(old,'')" 2
neg duplicate_number docs/bugs/README.md "import shutil, os; d=os.path.dirname(p); shutil.copy(os.path.join(d, '$REC124'), os.path.join(d, '124_duplicate.md'))" 2

echo "=== 3. regeneration: a new record, rebase conflicts, fragments ==="
make_copy
cat > "$C/docs/bugs/998_control_bug.md" <<'EOF'
# Bug 998 — a control record

<!-- bug-ledger
summary: control: a new open bug
fixtures: bug_print_builtins.con
class: 8 — the control's note
silent: yes
exit: 0b
-->

**Status:** OPEN
EOF
if python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 && python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 \
   && grep -qF '[998_control_bug.md](998_control_bug.md) — control: a new open bug' "$C/docs/bugs/README.md" \
   && grep -qE '^\| 8\. .*998 \(the control.s note\)' "$C/docs/bugs/STRUCTURAL_CLASSES.md" \
   && grep -qE 'exit 0b bugs[^>]*-->[^<]*998' "$C/ROADMAP.md"; then
  ok "positive control: a new record regenerates into all three views and checks"
else no "positive control: a new record did not regenerate into the README, class table and exit span"; fi
# A rebase conflict inside the README block: both sides regenerated the list.
make_copy
edit docs/bugs/README.md "old='## Open Numbered Bugs\n\n'; assert s.count(old)==1; s=s.replace(old, old+'<<<<<<< HEAD\n- [997_a.md](997_a.md) — ours\n=======\n- [996_b.md](996_b.md) — theirs\n>>>>>>> theirs\n')"
expect conflicted_readme_is_stale 1 python3 -I "$BL" --check --root "$C"
if python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 && python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 \
   && ! grep -qE '^(<{7}|>{7})' "$C/docs/bugs/README.md"; then
  ok "positive control: --write resolves a conflict inside the README's generated block"
else no "positive control: --write left a README block conflict unresolved"; fi
# A conflict confined to a ROADMAP exit span: the whole table row conflicts, the sides differ only there.
make_copy
edit ROADMAP.md "import re
m = re.search(r'^\| 0b \|.*\n', s, re.M); row = m.group(0)
ours = re.sub(r'(BEGIN GENERATED: exit 0b bugs[^>]*-->)', r'\g<1>997, ', row); theirs = re.sub(r'(BEGIN GENERATED: exit 0b bugs[^>]*-->)', r'\g<1>996, ', row)
s = s[:m.start()] + '<<<<<<< HEAD\n' + ours + '=======\n' + theirs + '>>>>>>> theirs\n' + s[m.end():]"
if python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 && python3 -I "$BL" --check --root "$C" >/dev/null 2>&1 \
   && ! grep -qE '^(<{7}|>{7})' "$C/ROADMAP.md"; then
  ok "positive control: --write resolves a conflict confined to a ROADMAP exit span"
else no "positive control: --write left an exit-span conflict unresolved"; fi
# ...but a conflict that also differs OUTSIDE the span is a real one and is left for a human.
make_copy
edit ROADMAP.md "import re
m = re.search(r'^\| 0b \|.*\n', s, re.M); row = m.group(0)
s = s[:m.start()] + '<<<<<<< HEAD\n' + row + '=======\n' + row.replace('| R-0482 |', '| R-0483 |', 1) + '>>>>>>> theirs\n' + s[m.end():]"
python3 -I "$BL" --write --root "$C" >/dev/null 2>&1 || true
if grep -qE '^<{7}' "$C/ROADMAP.md"; then
  ok "control: a conflict outside the generated span is left in place for a human"
else no "control: --write silently resolved a conflict outside a generated span"; fi
# Changelog: a stamped copy, a hand edit, malformed fragments, a fold.
make_copy
python3 -I "$CL" --check --root "$C" >/dev/null 2>&1 || python3 -I "$CL" --stamp-initial --root "$C" >/dev/null 2>&1
edit CHANGELOG.md "old='## Major Milestones\n'; i=s.index(old); j=s.index('\n', i+len(old))+1; s=s[:j]+'\n### Added by hand (2099-01-01)\n\nx\n'+s[j:]"
expect changelog_hand_edit 1 python3 -I "$CL" --check --root "$C"
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
edit "docs/bugs/$REC102" "import re; s,n=re.subn(r'^fixtures: ', 'fixtures: no_such_fixture_999.con ', s, count=1, flags=re.M); assert n==1"
expect audit_missing_fixture 1 bash "$C/scripts/tests/audit_bug_corpus.sh"
expect filing_missing_fixture 1 bash "$C/scripts/tests/check_bug_filing_complete.sh"
make_copy
edit scripts/tests/audit_bug_corpus.sh "old='declare -A SKIP_BUGS=(\n'; assert s.count(old)==1; s=s.replace(old, old+'  [124]=\"OPEN -- a legacy entry left behind\"\n')"
expect audit_legacy_entry_left 1 bash "$C/scripts/tests/audit_bug_corpus.sh"

echo
echo "LEDGERS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
