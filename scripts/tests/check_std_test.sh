#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase 7 step 5 gate: the std.test oracle/helper layer discipline.
#
#   - basic expect_* helpers are SILENT, capability-free, allocation-free
#     (bool-returning; no output on pass or fail — messaging is the caller's,
#     via assert_* which carry visible Console authority)
#   - assert_* failures report a STABLE message shape; passes are silent
#   - the oracle path (expect_sink/sink_matches) byte-compares a Writer sink —
#     test output and documented output are one mechanism
#   - NOT an xUnit framework: no suites/fixtures/setup-teardown surface

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
M="docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== basic expectations: silent, cap-free, alloc-free ==="
for f in expect_ok expect_err expect_some expect_none sink_matches; do
  r=$(grep -P "^test\t$f\t" "$M")
  echo "$r" | awk -F'\t' '$3=="no" && $6=="none"' | grep -q . \
    && ok "test.$f: no Alloc, no caps" || no "test.$f facts wrong ($r)"
done

echo "=== messaging assertions: authority VISIBLE (Console) ==="
for f in assert_eq assert_true assert_false expect_sink; do
  r=$(grep -P "^test\t$f\t" "$M")
  echo "$r" | awk -F'\t' '$6 ~ /Console/' | grep -q . \
    && ok "test.$f carries Console" || no "test.$f missing Console ($r)"
done

echo "=== stable failure-message shapes (source-pinned) ==="
grep -q '"EXPECT-SINK MISMATCH: "' std/src/test.con \
  && ok "expect_sink failure prefix stable" || no "expect_sink message drifted"
grep -qE 'ASSERTION FAILED|FAIL' std/src/test.con \
  && ok "assert failure marker present" || no "assert failure marker missing"

echo "=== not an xUnit framework ==="
grep -qEi "setup|teardown|suite|before_each" std/src/test.con \
  && no "xUnit surface creeping into std.test" || ok "no suites/fixtures/setup-teardown"

echo
echo "STD-TEST: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
