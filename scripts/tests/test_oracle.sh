#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase A.1 semantic-oracle differential harness.
#
# For each vector in tests/oracle/vectors.txt: compile to a native binary, run
# it, run the same source through `--interp`, and compare trimmed stdout.
# Mismatches are failures (named semantic regressions per Phase A.3). Programs
# whose interp output starts with `interp: ` are recorded as PENDING (the
# interpreter does not yet support that construct) — these are not failures
# but are visible so the gap is tracked.
#
# Contract: the harness only handles `fn main() -> Int`. Both the compiled
# binary (via emitMainWrapper's %lld\n format) and the interpreter
# (Main.lean's IO.println of the int return) print the same `<value>\n`.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
VECTORS="tests/oracle/vectors.txt"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi
if [ ! -f "$VECTORS" ]; then
  echo "error: vectors file $VECTORS missing." >&2
  exit 2
fi

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

PASS=0
FAIL=0
PENDING=0
TOTAL=0
FAIL_LINES=()
PENDING_LINES=()

run_vector() {
  local file="$1"
  local name
  name=$(echo "$file" | tr '/' '_' | sed 's/\.con$//')
  local bin="$TMPDIR/$name"
  local compiled_out interp_out interp_status

  if ! "$COMPILER" "$file" -o "$bin" >/dev/null 2>"$TMPDIR/$name.cerr"; then
    FAIL=$((FAIL + 1))
    FAIL_LINES+=("FAIL $file — compilation failed; see $TMPDIR/$name.cerr")
    return
  fi
  compiled_out=$("$bin" 2>/dev/null) || true

  # Capture --interp; treat "interp: ..." as a pending skip, not a failure.
  interp_out=$("$COMPILER" "$file" --interp 2>&1)
  interp_status=$?

  case "$interp_out" in
    "interp: "*)
      PENDING=$((PENDING + 1))
      PENDING_LINES+=("PEND $file — $interp_out")
      return
      ;;
  esac

  if [ "$interp_status" -ne 0 ]; then
    FAIL=$((FAIL + 1))
    FAIL_LINES+=("FAIL $file — --interp exited $interp_status: $interp_out")
    return
  fi

  # Both contracts: print the int return + newline. Trim trailing whitespace
  # (newline) before comparison so we compare values, not formatting noise.
  local c_trim i_trim
  c_trim=$(printf '%s' "$compiled_out" | sed -e 's/[[:space:]]*$//')
  i_trim=$(printf '%s' "$interp_out"  | sed -e 's/[[:space:]]*$//')

  if [ "$c_trim" = "$i_trim" ]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
    FAIL_LINES+=("FAIL $file — compiled='$c_trim' interp='$i_trim'")
  fi
}

while IFS= read -r line || [ -n "$line" ]; do
  # Strip comments and blank lines.
  trimmed="${line%%#*}"
  trimmed="${trimmed#"${trimmed%%[![:space:]]*}"}"
  trimmed="${trimmed%"${trimmed##*[![:space:]]}"}"
  [ -z "$trimmed" ] && continue

  if [ ! -f "$trimmed" ]; then
    FAIL=$((FAIL + 1))
    FAIL_LINES+=("FAIL $trimmed — file not found")
    TOTAL=$((TOTAL + 1))
    continue
  fi

  TOTAL=$((TOTAL + 1))
  run_vector "$trimmed"
done < "$VECTORS"

# Print pending first so the failures stay near the bottom of the log.
for line in "${PENDING_LINES[@]+"${PENDING_LINES[@]}"}"; do
  echo "$line"
done
for line in "${FAIL_LINES[@]+"${FAIL_LINES[@]}"}"; do
  echo "$line"
done

echo ""
echo "ORACLE: PASS=$PASS  FAIL=$FAIL  PENDING=$PENDING  TOTAL=$TOTAL"

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
exit 0
