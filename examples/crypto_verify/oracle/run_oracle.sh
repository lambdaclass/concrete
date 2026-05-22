#!/usr/bin/env bash
# Differential oracle for crypto_verify.
#
# Runs the Python reference across N seeded random cases. For each,
# generates a Concrete driver that calls verify_message with the
# same inputs and asserts the result matches the reference.
#
# Usage: run_oracle.sh [seed]

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
REFERENCE="examples/crypto_verify/oracle/reference.py"
SEED="${1:-0}"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

CASES=$("$REFERENCE" "$SEED")

TOTAL=0
PASS=0
FAIL=0

# Negative literals need parens in Concrete: write `(0 - 5)` for -5.
lit() { case "$1" in -*) echo "(0 - ${1#-})";; *) echo "$1";; esac; }

while IFS= read -r line; do
  [ -z "$line" ] && continue
  TOTAL=$((TOTAL + 1))

  args=$(echo "$line" | awk -F'|' '{print $1}' | xargs)
  expected=$(echo "$line" | awk -F'|' '{print $2}' | xargs)
  key=$(echo "$args" | awk '{print $1}')
  msg=$(echo "$args" | awk '{print $2}')
  nonce=$(echo "$args" | awk '{print $3}')
  tag=$(echo "$args" | awk '{print $4}')
  max_n=$(echo "$args" | awk '{print $5}')

  k=$(lit "$key"); m=$(lit "$msg"); n=$(lit "$nonce")
  t=$(lit "$tag"); mn=$(lit "$max_n")

  drv="$TMPDIR/case_$TOTAL.con"
  bin="$TMPDIR/case_$TOTAL"
  cat > "$drv" <<EOF
fn compute_tag(key: Int, message: Int, nonce: Int) -> Int {
    return key * message + nonce;
}
fn verify_tag(key: Int, message: Int, nonce: Int, expected_tag: Int) -> Int {
    let computed: Int = compute_tag(key, message, nonce);
    if computed == expected_tag { return 1; } else { return 0; }
}
fn check_nonce(nonce: Int, max_nonce: Int) -> Int {
    if nonce > 0 {
        if nonce <= max_nonce { return 1; } else { return 0; }
    } else { return 0; }
}
fn verify_message(key: Int, message: Int, nonce: Int,
                  expected_tag: Int, max_nonce: Int) -> Int {
    if verify_tag(key, message, nonce, expected_tag) != 1 { return 0; }
    if check_nonce(nonce, max_nonce) != 1 { return 0; }
    return 1;
}
fn main() -> Int {
    return verify_message($k, $m, $n, $t, $mn);
}
EOF

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
echo "ORACLE (crypto_verify, seed=$SEED): PASS=$PASS  FAIL=$FAIL  TOTAL=$TOTAL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
