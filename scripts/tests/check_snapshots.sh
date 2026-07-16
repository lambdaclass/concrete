#!/usr/bin/env bash
# Snapshot baseline gate (Phase 2 E.23 surface).
#
# Walks every examples/*/snapshot/ directory. For each example, runs
# the same reports the snapshot directory contains, and asserts the
# output is byte-identical to the snapshot. Drift is real signal:
# either the source changed in a way that should be reflected in the
# snapshot, or a compiler change altered the report.
#
# Updating a snapshot is a deliberate action — see UPDATE_SNAPSHOTS
# at the bottom of this script.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

UPDATE=${UPDATE_SNAPSHOTS:-0}

# A snapshot file's stem is the --report kind it captures.
report_kind_from() {
  basename "$1" .txt
}

mapfile -t SNAPSHOT_DIRS < <(find examples -type d -name snapshot | sort)

if [ ${#SNAPSHOT_DIRS[@]} -eq 0 ]; then
  echo "No snapshot/ directories under examples/. Nothing to check."
  exit 0
fi

PASS=0
FAIL=0
UPDATED=0
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

for dir in "${SNAPSHOT_DIRS[@]}"; do
  example_dir=$(dirname "$dir")
  source="$example_dir/src/main.con"
  if [ ! -f "$source" ]; then
    echo "  SKIP $dir — source not found at $source"
    continue
  fi

  label="${example_dir#examples/}"
  echo "=== $label ==="

  # Flatten nested example labels (e.g. evidence_classes/proved_by_lean) for the
  # temp file path, which is a flat directory.
  flat_label="${label//\//_}"

  mapfile -t SNAPSHOTS < <(find "$dir" -maxdepth 1 -name '*.txt' | sort)
  for snap in "${SNAPSHOTS[@]}"; do
    kind=$(report_kind_from "$snap")
    actual_file="$TMP/$flat_label-$kind.txt"
    "$COMPILER" "$source" --report "$kind" > "$actual_file" 2>&1

    if cmp -s "$snap" "$actual_file"; then
      echo "  ok   $kind"
      PASS=$((PASS + 1))
    else
      if [ "$UPDATE" = "1" ]; then
        cp "$actual_file" "$snap"
        echo "  UPD  $kind (snapshot updated)"
        UPDATED=$((UPDATED + 1))
      else
        echo "  FAIL $kind — snapshot drift"
        diff -u "$snap" "$actual_file" | head -20 | sed 's/^/    /'
        FAIL=$((FAIL + 1))
      fi
    fi
  done
done

echo ""
if [ "$UPDATE" = "1" ]; then
  echo "SNAPSHOTS: PASS=$PASS  UPDATED=$UPDATED"
  exit 0
fi
echo "SNAPSHOTS: PASS=$PASS  FAIL=$FAIL"
if [ "$FAIL" -gt 0 ]; then
  echo ""
  echo "To update snapshots intentionally:"
  echo "  UPDATE_SNAPSHOTS=1 bash scripts/tests/check_snapshots.sh"
  exit 1
fi
exit 0
