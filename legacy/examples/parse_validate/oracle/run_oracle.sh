#!/usr/bin/env bash
# Differential oracle for parse_validate.
#
# Runs the Python reference implementation across N random test
# cases (seeded). For each case, generates a tiny Concrete driver
# that calls parse_header with the same input, compiles + runs,
# and asserts the resulting error code matches the reference.
#
# Usage: run_oracle.sh [seed]   (default seed: 0)
#
# This is the oracle beyond hand-written tests — bar #5 of the
# parse_validate graduation contract. The 8 hand-written cases in
# src/main.con cover one example per error variant; this harness
# pushes ~200 cases biased toward each variant boundary, plus
# adversarial inputs.
#
# Disagreements are real: either the Concrete source is wrong, or
# the reference is wrong, or the spec is ambiguous. Whichever it
# is, you found it.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
REFERENCE="examples/parse_validate/oracle/reference.py"
PV_SOURCE="examples/parse_validate/src/main.con"
SEED="${1:-0}"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi
if [ ! -f "$REFERENCE" ]; then
  echo "error: reference $REFERENCE missing." >&2
  exit 2
fi

# Strip parse_validate's main() and keep the validator definitions
# as the prelude. We can't easily import a Concrete module yet
# (Phase 9 work), so the cheap shortcut is to concatenate the
# function bodies with our own main() that drives one test case.
PRELUDE=$(mktemp)
# Take everything before `pub fn main()`.
awk '/pub fn main/{exit} {print}' "$PV_SOURCE" > "$PRELUDE"
# Close the parse_validate { ... } block — main() was inside it.
# Don't actually close — we'll add main() inside the mod.

TMPDIR=$(mktemp -d)
trap 'rm -f "$PRELUDE"; rm -rf "$TMPDIR"' EXIT

CASES=$("$REFERENCE" "$SEED")

TOTAL=0
PASS=0
FAIL=0

while IFS= read -r line; do
  [ -z "$line" ] && continue
  TOTAL=$((TOTAL + 1))

  # Parse: "<8 ints> | <len> | <expected>"
  data=$(echo "$line" | awk -F'|' '{print $1}' | xargs)
  length=$(echo "$line" | awk -F'|' '{print $2}' | xargs)
  expected=$(echo "$line" | awk -F'|' '{print $3}' | xargs)

  # Build a Concrete driver that calls parse_header(data, length).
  drv="$TMPDIR/case_$TOTAL.con"
  bin="$TMPDIR/case_$TOTAL"
  {
    cat "$PRELUDE"
    cat <<EOF
    pub fn main() -> Int {
        let d: [i32; 8] = [$(echo "$data" | tr ' ' ','| sed 's/-/0-/g' )];
        let len: i32 = $length;
        match parse_header(d, len) {
            Result::Ok { value } => { return 0; },
            Result::Err { error } => { return error_code(error) as Int; },
        }
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
echo "ORACLE (parse_validate, seed=$SEED): PASS=$PASS  FAIL=$FAIL  TOTAL=$TOTAL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
