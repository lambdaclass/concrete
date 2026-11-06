#!/usr/bin/env bash
# Differential oracle for constant_time_tag.
#
# Runs the Python reference across N seeded random cases.  For
# each case, generates a tiny Concrete driver that constructs the
# same two [u8; 16] tags as array literals, calls ct_compare, and
# prints the resulting i32.  Compares to the reference.
#
# Usage: run_oracle.sh [seed]   (default seed: 0)
#
# This is the oracle beyond hand-written tests — bar #5 of the
# constant_time_tag graduation contract.  The 6 hand-written
# cases in src/main.con cover representative inputs; this harness
# pushes 200 cases biased toward each per-position byte-diff plus
# all-equal / multiple-differ / high-bit cases.
#
# Independent confidence for the negative direction the proof
# does NOT cover yet: ct_compare a b = 1 iff a = b.  The
# theorem only covers a = b (positive direction); the oracle
# pins the negative direction (any byte differs → return 0)
# across 200 seeded cases per seed.
#
# Disagreements are real: either the Concrete source is wrong,
# the reference is wrong, or the spec is ambiguous.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
REFERENCE="examples/constant_time_tag/oracle/reference.py"
CT_SOURCE="examples/constant_time_tag/src/main.con"
SEED="${1:-0}"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi
if [ ! -f "$REFERENCE" ]; then
  echo "error: reference $REFERENCE missing." >&2
  exit 2
fi

# Strip pub fn main from the source; keep ct_compare as prelude.
PRELUDE=$(mktemp)
awk '/pub fn main/{exit} {print}' "$CT_SOURCE" > "$PRELUDE"

TMPDIR=$(mktemp -d)
trap 'rm -f "$PRELUDE"; rm -rf "$TMPDIR"' EXIT

CASES=$("$REFERENCE" "$SEED")

TOTAL=0
PASS=0
FAIL=0

while IFS= read -r line; do
  [ -z "$line" ] && continue
  TOTAL=$((TOTAL + 1))

  # Parse: "<16 a bytes> | <16 b bytes> | <expected>"
  a_str=$(echo "$line" | awk -F'|' '{print $1}' | xargs)
  b_str=$(echo "$line" | awk -F'|' '{print $2}' | xargs)
  expected=$(echo "$line" | awk -F'|' '{print $3}' | xargs)

  # Convert space-separated to comma-separated for array literal.
  a_arr=$(echo "$a_str" | tr ' ' ',')
  b_arr=$(echo "$b_str" | tr ' ' ',')

  drv="$TMPDIR/case_$TOTAL.con"
  bin="$TMPDIR/case_$TOTAL"
  {
    cat "$PRELUDE"
    cat <<EOF
    pub fn main() -> i32 {
        let a: [u8; 16] = [${a_arr}];
        let b: [u8; 16] = [${b_arr}];
        return ct_compare(a, b);
    }
}
EOF
  } > "$drv"

  if ! "$COMPILER" "$drv" -o "$bin" >/dev/null 2>&1; then
    FAIL=$((FAIL + 1))
    echo "  FAIL case $TOTAL — compilation failed"
    echo "    $line"
    continue
  fi
  actual=$("$bin" 2>/dev/null)
  if [ "$actual" = "$expected" ]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
    echo "  FAIL case $TOTAL — expected=$expected actual=$actual"
    echo "    $line"
  fi
done <<< "$CASES"

echo ""
echo "ORACLE (constant_time_tag, seed=$SEED): PASS=$PASS  FAIL=$FAIL  TOTAL=$TOTAL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
