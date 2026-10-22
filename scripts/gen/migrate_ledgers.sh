#!/usr/bin/env bash
# THE ONE-TIME LEDGER MIGRATION: one owner per fact (docs/bugs/README.md, "Filing a bug").
#
# Moves every per-bug fact out of the shared, hand-edited views and into the bug's own record, then
# turns those views into generated blocks:
#
#   audit_bug_corpus.sh maps  -> each record's ledger block (fixtures: / coverage:), maps emptied
#   README bug lists          -> summary:; the lists become a generated block
#   STRUCTURAL_CLASSES table  -> class:; the bugs column becomes generated
#   ROADMAP exit 0b bug list  -> exit: 0b; the list becomes a generated span
#   discovery.tsv silent      -> silent: (only when docs/bugs/discovery.tsv exists)
#   CHANGELOG.md              -> stamped; new entries are changelog.d/ fragments from now on
#   EXPECTED_GATE_COMMANDS    -> scripts/tests/ci_gate_inventory.txt (one gate command per line)
#
# It reads the views as they are, so it is re-run on whatever main it lands on, not replayed from an
# old diff. Run it on a clean tree at the commit that will land it, review the NOTE lines (facts it
# could not place), then commit the result as one migration commit:
#
#   bash scripts/gen/migrate_ledgers.sh
#   git add -A docs/bugs scripts/tests ROADMAP.md CHANGELOG.md && git commit -m "ledgers: migrate ..."
#
# It refuses a dirty tree and an already-migrated tree, and leaves the tree as it was if any step
# fails before the checks (the checks themselves only report).
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "migrate_ledgers: the tree has uncommitted changes; run it on a clean checkout" >&2
  exit 2
fi
restore() {
  echo "migrate_ledgers: a step failed; restoring the tracked files" >&2
  git checkout -- . 2>/dev/null || true
  rm -f scripts/tests/ci_gate_inventory.txt
}
trap restore ERR

python3 -I scripts/gen/migrate_ledgers.py "$ROOT_DIR"
python3 -I scripts/gen/bug_ledger.py --write
python3 -I scripts/gen/changelog.py --stamp-initial
if [ -f scripts/gen/bug_dashboard.py ]; then
  python3 -I scripts/gen/bug_dashboard.py --write
fi
trap - ERR

echo
echo "=== checks on the migrated tree ==="
rc=0
python3 -I scripts/gen/bug_ledger.py --check || rc=1
python3 -I scripts/gen/changelog.py --check || rc=1
if [ -f scripts/gen/bug_dashboard.py ]; then python3 -I scripts/gen/bug_dashboard.py --check || rc=1; fi
audit_log="$(mktemp)"
bash scripts/tests/audit_bug_corpus.sh > "$audit_log" 2>&1 || { rc=1; tail -20 "$audit_log"; }
rm -f "$audit_log"
bash scripts/tests/check_bug_filing_complete.sh || rc=1
bash scripts/tests/check_roadmap_linear.sh || rc=1
echo
git status --short
if [ "$rc" -eq 0 ]; then
  echo "migrate_ledgers: done; review the NOTE lines above and the diff, then commit"
else
  echo "migrate_ledgers: the migrated tree FAILS a check above; fix the migration before committing" >&2
fi
exit "$rc"
