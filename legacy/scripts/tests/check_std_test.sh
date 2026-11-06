#!/usr/bin/env bash
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
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
M="docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== basic expectations: silent, operationally cap-free, alloc-free ==="
# "cap-free" here means free of OPERATIONAL authority — silent (no Console) and
# allocation-free (no Alloc). That is the property under test: an expectation helper must
# not print or allocate behind the author's back. It is a different axis from a caller
# SAFETY OBLIGATION: `sink_matches(buf: *const u8, len, ..)` takes a caller-supplied raw
# pointer and dereferences it, so the caller owes validity. Before the two-axis work
# there was no reason to distinguish the two, so this read `$6=="none"`.
OPERATIONAL='File|Console|Network|Alloc|Env|Time|Process|Random'
for f in expect_ok expect_err expect_some expect_none; do
  r=$(grep -P "^test\t$f\t" "$M")
  echo "$r" | awk -F'\t' '$3=="no" && $6=="none"' | has_match . \
    && ok "test.$f: no Alloc, no caps" || no "test.$f facts wrong ($r)"
done
r=$(grep -P "^test\tsink_matches\t" "$M")
if echo "$r" | awk -F'\t' -v op="$OPERATIONAL" '$3=="no" && $6 !~ op' | has_match .; then
  ok "test.sink_matches: no Alloc, no operational authority (Unsafe is the caller's buffer)"
else
  no "test.sink_matches facts wrong ($r)"
fi

echo "=== messaging assertions: authority VISIBLE (Console) ==="
for f in assert_eq assert_true assert_false expect_sink; do
  r=$(grep -P "^test\t$f\t" "$M")
  echo "$r" | awk -F'\t' '$6 ~ /Console/' | has_match . \
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
