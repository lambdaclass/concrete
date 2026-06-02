#!/usr/bin/env bash
# Differential oracle for fixed_capacity.
#
# Runs the Python reference across N seeded random cases.  For each
# case, generates a tiny Concrete driver that builds the same MsgBuf
# + ring state, calls validate_message, and prints the resulting
# error code.  Compares to the reference.
#
# Usage: run_oracle.sh [seed]   (default seed: 0)
#
# This is the oracle beyond hand-written tests — bar #5 of the
# fixed_capacity graduation contract.  The 8 hand-written cases in
# src/main.con cover representative inputs; this harness pushes
# 200 cases biased toward each error path (1..7) plus replay tests.
#
# Disagreements are real: either the Concrete source is wrong, or
# the reference is wrong, or the spec is ambiguous.  Whichever it
# is, you found it.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
REFERENCE="examples/fixed_capacity/oracle/reference.py"
FC_SOURCE="examples/fixed_capacity/src/main.con"
SEED="${1:-0}"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi
if [ ! -f "$REFERENCE" ]; then
  echo "error: reference $REFERENCE missing." >&2
  exit 2
fi

# Strip fixed_capacity's pub fn main and keep the function defs as
# prelude.  We append our own driver main and a trusted constructor
# for the MsgBuf with case-specific bytes.
PRELUDE=$(mktemp)
awk '/pub fn main/{exit} {print}' "$FC_SOURCE" > "$PRELUDE"

TMPDIR=$(mktemp -d)
trap 'rm -f "$PRELUDE"; rm -rf "$TMPDIR"' EXIT

CASES=$("$REFERENCE" "$SEED")

TOTAL=0
PASS=0
FAIL=0

while IFS= read -r line; do
  [ -z "$line" ] && continue
  TOTAL=$((TOTAL + 1))

  # Parse: "<8 bytes> | <len> | <comma-separated pre-pushed seqs or '-'> | <expected>"
  bytes_str=$(echo "$line" | awk -F'|' '{print $1}' | xargs)
  length=$(echo "$line" | awk -F'|' '{print $2}' | xargs)
  pushes=$(echo "$line" | awk -F'|' '{print $3}' | xargs)
  expected=$(echo "$line" | awk -F'|' '{print $4}' | xargs)

  # 8 bytes for the test buf header.
  read -r b0 b1 b2 b3 b4 b5 b6 b7 <<< "$bytes_str"

  # Build pre-push sequence if any.
  pre_push_lines=""
  if [ "$pushes" != "-" ]; then
    IFS=',' read -ra ps <<< "$pushes"
    for s in "${ps[@]}"; do
      pre_push_lines="${pre_push_lines}        ring = ring_push(ring, ${s});\n"
    done
  fi

  drv="$TMPDIR/case_$TOTAL.con"
  bin="$TMPDIR/case_$TOTAL"
  {
    cat "$PRELUDE"
    cat <<EOF
    trusted fn driver_buf() -> MsgBuf {
        let mut data: [u8; 256] = [0; 256];
        data[0] = $b0;
        data[1] = $b1;
        data[2] = $b2;
        data[3] = $b3;
        data[4] = $b4;
        data[5] = $b5;
        data[6] = $b6;
        data[7] = $b7;
        return MsgBuf { data: data, len: $length };
    }

    pub fn main() -> i32 {
        let buf: MsgBuf = driver_buf();
        let mut ring: RingBuf = ring_new();
$(printf "%b" "$pre_push_lines")        let r: ValidateResult = validate_message(buf, ring);
        return r.error;
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
echo "ORACLE (fixed_capacity, seed=$SEED): PASS=$PASS  FAIL=$FAIL  TOTAL=$TOTAL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
