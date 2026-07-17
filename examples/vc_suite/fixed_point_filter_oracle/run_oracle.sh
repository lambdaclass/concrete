#!/usr/bin/env bash
# Differential oracle for vc_suite/fixed_point_filter (ROADMAP Phase 2 #6).
# Runs the Python reference across N seeded cases; for each, generates a Concrete
# driver that calls dsp.scale with the case inputs, compiles, runs, and compares
# the program's output to the reference. Disagreement = a real codegen/spec bug.
#   Usage: run_oracle.sh [seed]   (default 0)
set -uo pipefail
# knob-proof: drivers self-print; the legacy result echo must stay off even
# when a caller exports it for its own unconverted corpus
unset CONCRETE_ECHO_RESULT
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
REFERENCE="examples/vc_suite/fixed_point_filter_oracle/reference.py"
SRC="examples/vc_suite/fixed_point_filter.con"
SEED="${1:-0}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

# prelude = the module minus its closing brace; we inject a per-case main.
PRELUDE=$(mktemp); sed '$d' "$SRC" > "$PRELUDE"
TMPDIR=$(mktemp -d); trap 'rm -f "$PRELUDE"; rm -rf "$TMPDIR"' EXIT

TOTAL=0; PASS=0; FAIL=0
while IFS= read -r line; do
  [ -z "$line" ] && continue
  TOTAL=$((TOTAL + 1))
  s=$(echo "$line" | awk -F'|' '{print $1}' | xargs)
  c=$(echo "$line" | awk -F'|' '{print $2}' | xargs)
  expected=$(echo "$line" | awk -F'|' '{print $3}' | xargs)
  drv="$TMPDIR/case_$TOTAL.con"; bin="$TMPDIR/case_$TOTAL"
  # self-printing driver (MAIN_EXIT_MODEL stage 2): the binary prints its own
  # result at full width; nothing depends on the legacy result echo.
  { cat "$PRELUDE"; printf '    pub fn main() with(Console) -> i32 { print_int((scale(%s, %s)) as Int); print_char(10); return 0; }\n}\n' "$s" "$c"; } > "$drv"
  if ! "$COMPILER" "$drv" -o "$bin" >/dev/null 2>&1; then
    FAIL=$((FAIL + 1)); echo "  FAIL case $TOTAL ($s * $c) — compilation failed"; continue
  fi
  actual=$("$bin" 2>/dev/null)
  if [ "$actual" = "$expected" ]; then PASS=$((PASS + 1));
  else FAIL=$((FAIL + 1)); echo "  FAIL case $TOTAL — scale($s,$c) expected=$expected actual=$actual"; fi
done <<< "$(python3 "$REFERENCE" "$SEED")"

echo ""
echo "ORACLE (fixed_point_filter, seed=$SEED): PASS=$PASS  FAIL=$FAIL  TOTAL=$TOTAL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
