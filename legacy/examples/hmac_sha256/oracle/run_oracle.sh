#!/usr/bin/env bash
# Differential oracle for hmac_sha256 (graduation bar #5).
#
# For each seeded case from reference.py (Python stdlib hmac as the
# independent oracle), generates a tiny Concrete driver that embeds
# the key, message, and EXPECTED tag as fixed-size array literals,
# calls verify_hmac (which recomputes the tag and constant-time
# compares it to the expected), and prints the i32 result.  The
# expected output is always 1 — a 0 (or non-1) means Concrete's
# HMAC diverged from Python's on the full 32-byte tag.
#
# Usage: run_oracle.sh [seed]   (default seed: 0)
#
# This pushes the primitive across every length regime (empty
# key/message, key </=/> block size, message at/under/over the
# 64-byte boundary, multi-block) far beyond the two hand-written
# RFC vectors in src/main.con.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
REFERENCE="examples/hmac_sha256/oracle/reference.py"
HMAC_SOURCE="examples/hmac_sha256/src/main.con"
SEED="${1:-0}"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi
if [ ! -f "$REFERENCE" ]; then
  echo "error: reference $REFERENCE missing." >&2
  exit 2
fi

# Keep every function up to (but not including) `pub fn main` as a
# prelude; the driver supplies its own main.
PRELUDE=$(mktemp)
awk '/pub fn main/{exit} {print}' "$HMAC_SOURCE" > "$PRELUDE"

TMPDIR=$(mktemp -d)
trap 'rm -f "$PRELUDE"; rm -rf "$TMPDIR"' EXIT

CASES=$("$REFERENCE" "$SEED")

TOTAL=0
PASS=0
FAIL=0

while IFS= read -r line; do
  [ -z "$line" ] && continue
  TOTAL=$((TOTAL + 1))

  key_arr=$(echo "$line" | awk -F'|' '{print $1}' | xargs | tr ' ' ',')
  k_len=$(echo "$line"   | awk -F'|' '{print $2}' | xargs)
  msg_arr=$(echo "$line" | awk -F'|' '{print $3}' | xargs | tr ' ' ',')
  m_len=$(echo "$line"   | awk -F'|' '{print $4}' | xargs)
  tag_arr=$(echo "$line" | awk -F'|' '{print $5}' | xargs | tr ' ' ',')

  drv="$TMPDIR/case_$TOTAL.con"
  bin="$TMPDIR/case_$TOTAL"
  {
    cat "$PRELUDE"
    cat <<EOF
    pub fn main() -> i32 {
        let k: [u8; 128] = [${key_arr}];
        let m: [u8; 256] = [${msg_arr}];
        let e: [u8; 32] = [${tag_arr}];
        return verify_hmac(k, ${k_len}, m, ${m_len}, e);
    }
}
EOF
  } > "$drv"

  if ! "$COMPILER" "$drv" -o "$bin" >/dev/null 2>&1; then
    FAIL=$((FAIL + 1))
    echo "  FAIL case $TOTAL (k_len=$k_len m_len=$m_len) — compilation failed"
    continue
  fi
  actual=$("$bin" 2>/dev/null)
  if [ "$actual" = "1" ]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
    echo "  FAIL case $TOTAL (k_len=$k_len m_len=$m_len) — verify_hmac returned '$actual', expected 1"
  fi
done <<< "$CASES"

echo ""
echo "ORACLE (hmac_sha256, seed=$SEED): PASS=$PASS  FAIL=$FAIL  TOTAL=$TOTAL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
