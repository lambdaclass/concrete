#!/usr/bin/env bash
# Regenerate every committed view that is derived from records, in dependency order.
#
# Run it after a rebase or merge that conflicted in a generated file, instead of merging that file by
# hand: each generator replaces its block whole (conflict markers included) from the merged records,
# so the result is what the records say, never a union of two stale views. Then `git add` the files it
# names and continue. A conflict in a record, or in prose outside a generated block, is a real
# conflict and is resolved by hand first; a generator refuses (exit 2) rather than guess when its
# markers or inputs are damaged.
#
#   bug ledger      docs/bugs/README.md lists, STRUCTURAL_CLASSES.md table column, ROADMAP exit spans,
#                   docs/bugs/RULINGS.md (the spec rulings),
#                   docs/bugs/AREAS.md (scripts/gen/bug_ledger.py, from docs/bugs/NNN_*.md; AREAS.md
#                   also from git history, so rerun this after committing a fix and commit the result)
#   bug dashboard   ROADMAP.md's bug dashboard (scripts/gen/bug_dashboard.py), when present
#   candidates      ROADMAP.md's "Implemented, not yet accepted" table (scripts/gen/candidate_table.py)
#   build identity  Concrete/BuildIdentity.lean (scripts/gen/build_identity.sh, from the compiler sources)
#
# CHANGELOG.md is not here: entries are changelog.d/ fragments, folded on main by
# scripts/gen/changelog.py --fold, so it never conflicts.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

if grep -lE '^(<{7}|>{7})( |$)' docs/bugs/[0-9][0-9][0-9]_*.md 2>/dev/null; then
  echo "refresh_generated: the bug records above hold conflict markers; resolve them by hand first" >&2
  exit 2
fi

python3 -I scripts/gen/bug_ledger.py --write
if [ -f scripts/gen/bug_dashboard.py ]; then
  python3 -I scripts/gen/bug_dashboard.py --write
fi
if [ -f scripts/gen/candidate_table.py ]; then
  python3 -I scripts/gen/candidate_table.py --write
fi
bash scripts/gen/build_identity.sh

left="$(grep -lE '^(<{7}|>{7})( |$)' docs/bugs/README.md docs/bugs/STRUCTURAL_CLASSES.md docs/bugs/AREAS.md docs/bugs/RULINGS.md ROADMAP.md \
          Concrete/BuildIdentity.lean 2>/dev/null || true)"
if [ -n "$left" ]; then
  echo "refresh_generated: conflict markers remain OUTSIDE the generated blocks in:" >&2
  printf '  %s\n' $left >&2
  echo "resolve those by hand, then rerun" >&2
  exit 1
fi
echo "refresh_generated: every generated view is current; git add the files above (or: git status)"
