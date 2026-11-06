#!/usr/bin/env bash
# Differential oracle for evidence_classes/tested_by_oracle (demo.clamp).
#
# Runs the Python reference across N seeded cases. For each, generates a tiny
# Concrete driver that calls clamp with the literal inputs and returns the
# result as the process exit code, compiles it natively, and compares the exit
# code to the reference. Disagreement is real signal.
#
# Usage: run_oracle.sh [seed]   (default 0)
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
SRC="examples/evidence_classes/tested_by_oracle/src/main.con"
REFERENCE="examples/evidence_classes/tested_by_oracle/oracle/reference.py"
SEED="${1:-0}"
[ -x "$COMPILER" ] || { echo "error: compiler not found at $COMPILER. Run 'make build'." >&2; exit 2; }

# Prelude = everything up to `pub fn main` (keeps clamp, drops the example main).
PRELUDE=$(mktemp)
awk '/pub fn main/{exit} {print}' "$SRC" > "$PRELUDE"
TMPDIR=$(mktemp -d)
trap 'rm -f "$PRELUDE"; rm -rf "$TMPDIR"' EXIT

TOTAL=0; PASS=0; FAIL=0
while IFS= read -r line; do
  [ -z "$line" ] && continue
  TOTAL=$((TOTAL + 1))
  x=$(echo "$line"  | awk -F'|' '{print $1}' | xargs)
  lo=$(echo "$line" | awk -F'|' '{print $2}' | xargs)
  hi=$(echo "$line" | awk -F'|' '{print $3}' | xargs)
  expected=$(echo "$line" | awk -F'|' '{print $4}' | xargs)
  drv="$TMPDIR/case_$TOTAL.con"; bin="$TMPDIR/case_$TOTAL"
  { cat "$PRELUDE"; cat <<EOF
    pub fn main() -> i32 {
        return clamp(${x}, ${lo}, ${hi});
    }
}
EOF
  } > "$drv"
  if ! "$COMPILER" "$drv" -o "$bin" >/dev/null 2>&1; then
    FAIL=$((FAIL + 1)); echo "  FAIL case $TOTAL — compile failed: $line"; continue
  fi
  actual=$("$bin" 2>/dev/null | tr -d "[:space:]")
  if [ "$actual" = "$expected" ]; then PASS=$((PASS + 1));
  else FAIL=$((FAIL + 1)); echo "  FAIL case $TOTAL — expected=$expected actual=$actual ($line)"; fi
done <<< "$("$REFERENCE" "$SEED")"

echo ""
echo "ORACLE (tested_by_oracle, seed=$SEED): PASS=$PASS  FAIL=$FAIL  TOTAL=$TOTAL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
