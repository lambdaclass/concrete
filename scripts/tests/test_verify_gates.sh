#!/usr/bin/env bash
# Run `concrete --report verify` across the curated corpora (oracle
# vectors + wrong-code runtime cases) and assert every per-gate run
# is error-free.
#
# Warnings ARE tolerated — the post-elab gate has a documented
# exception for Ty.placeholder leaks via `?` (try) and `defer`, and
# the harness reports them but does not fail on them. Any new warning
# class added in the future should be triaged through the contract in
# docs/VERIFY_GATES.md before the corpus accepts it silently.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

PASS=0
FAIL=0
WARN_TOTAL=0
SKIP=0
FAIL_LINES=()

run_one() {
  local file="$1"
  local out
  out=$("$COMPILER" "$file" --report verify 2>&1)
  local first
  first=$(echo "$out" | head -1)
  case "$first" in
    VERIFY-GATES:\ ok*)
      PASS=$((PASS + 1))
      ;;
    VERIFY-GATES:\ warn*)
      PASS=$((PASS + 1))
      local wcount
      wcount=$(grep <<<"$first" -oE '[0-9]+' | head -1)
      WARN_TOTAL=$((WARN_TOTAL + ${wcount:-0}))
      ;;
    VERIFY-GATES:\ FAIL*)
      FAIL=$((FAIL + 1))
      FAIL_LINES+=("FAIL $file — $first")
      # Capture per-gate detail for the log
      echo "$out" | head -10 | sed 's/^/    /' >> /tmp/verify_gates_fail.log 2>/dev/null || true
      ;;
    *)
      # Couldn't even produce a verify report — most likely a
      # frontend error (parse/resolve/check) blocking the gate
      # input. The curated corpora here (oracle vectors +
      # wrong-code kind=runtime cases) all reach ValidatedCore in
      # the production compile path, so a skip here is a frontend
      # regression and must fail the run.
      SKIP=$((SKIP + 1))
      FAIL_LINES+=("SKIP $file — verify report not produced; frontend regression?")
      ;;
  esac
}

# --- Corpus 1: oracle vectors (everything that's currently a live
# differential check between compiled binary and --interp).
ORACLE_VECTORS="tests/oracle/vectors.txt"
if [ -f "$ORACLE_VECTORS" ]; then
  while IFS= read -r line || [ -n "$line" ]; do
    trimmed="${line%%#*}"
    trimmed="${trimmed#"${trimmed%%[![:space:]]*}"}"
    trimmed="${trimmed%"${trimmed##*[![:space:]]}"}"
    [ -z "$trimmed" ] && continue
    [ -f "$trimmed" ] && run_one "$trimmed"
  done < "$ORACLE_VECTORS"
fi

# --- Corpus 2: wrong-code runtime cases (skip compile-error cases —
# they aren't expected to reach ValidatedCore, so the gates don't
# apply).
if [ -f tests/wrong-code/manifest.toml ]; then
  python3 - tests/wrong-code/manifest.toml > /tmp/verify_gates_repros.txt <<'PYEOF'
import re, sys
src = open(sys.argv[1]).read()
chunks = re.split(r'^\[\[case\]\]\s*$', src, flags=re.MULTILINE)[1:]
for c in chunks:
    fields = {}
    for m in re.finditer(r'^\s*(\w+)\s*=\s*"((?:\\.|[^"\\])*)"\s*$', c, flags=re.MULTILINE):
        fields[m.group(1)] = m.group(2)
    if fields.get('kind') == 'runtime':
        print(fields.get('repro', ''))
PYEOF
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    [ -f "$line" ] && run_one "$line"
  done < /tmp/verify_gates_repros.txt
fi

# Report any failures.
for line in "${FAIL_LINES[@]+"${FAIL_LINES[@]}"}"; do
  echo "$line"
done

echo ""
echo "VERIFY-GATES: PASS=$PASS  FAIL=$FAIL  SKIP=$SKIP  warnings=$WARN_TOTAL"
if [ "$FAIL" -gt 0 ] || [ "$SKIP" -gt 0 ]; then
  exit 1
fi
exit 0
