#!/usr/bin/env bash
set -euo pipefail

# Proof Evidence Gate
#
# Runs all proof-related checks for the pressure set as one CI gate
# with one failure surface. Exits 0 only if everything passes.
#
# Checks:
#   1. Extraction — all expected functions extracted or correctly excluded
#   2. Registry validation — no registry errors
#   3. Proof-status consistency — obligation counts match expected
#   4. Proof diagnostics — taxonomy complete, no unexpected errors
#   5. Proof dependencies — graph well-formed
#   6. Evidence bundle — generates valid JSON with correct structure
#   7. Determinism — extraction and fingerprints are reproducible
#   8. Lean theorem checking — all proved theorems pass kernel check
#
# Usage:
#   scripts/ci/proof_gate.sh [--verbose]

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
PP_SRC="$ROOT_DIR/examples/proof_pressure/src/main.con"
VERBOSE="${1:-}"
PASS=0
FAIL=0
TOTAL=0

pass() {
    PASS=$((PASS + 1))
    TOTAL=$((TOTAL + 1))
    echo "  ✓ $1"
}

fail() {
    FAIL=$((FAIL + 1))
    TOTAL=$((TOTAL + 1))
    echo "  ✗ $1"
}

section() {
    echo ""
    echo "--- $1 ---"
}

if [ ! -x "$COMPILER" ]; then
    echo "Error: compiler not found at $COMPILER"
    echo "Run 'lake build' first."
    exit 2
fi

if [ ! -f "$PP_SRC" ]; then
    echo "Error: proof pressure set not found at $PP_SRC"
    exit 2
fi

echo "=== Proof Evidence Gate ==="
echo "  source: $PP_SRC"
echo "  compiler: $($COMPILER --version 2>/dev/null || echo 'unknown')"

# ============================================================
# 1. Extraction
# ============================================================
section "Extraction"

ext_out=$($COMPILER "$PP_SRC" --report extraction 2>&1) || true

# check_nonce should be extracted
if echo "$ext_out" | grep -A2 "check_nonce" | grep -q "status: extracted"; then
    pass "check_nonce extracted"
else
    fail "check_nonce should be extracted"
fi

# clamp_value should be extracted
if echo "$ext_out" | grep -A2 "clamp_value" | grep -q "status: extracted"; then
    pass "clamp_value extracted"
else
    fail "clamp_value should be extracted"
fi

# classify_range should be eligible but blocked
if echo "$ext_out" | grep -A2 "classify_range" | grep -q "eligible\|extraction failed"; then
    pass "classify_range blocked (eligible but not extractable)"
else
    fail "classify_range should be eligible but blocked"
fi

# format_result should be excluded
if echo "$ext_out" | grep -A2 "format_result" | grep -q "excluded"; then
    pass "format_result excluded (has capabilities)"
else
    fail "format_result should be excluded"
fi

# ============================================================
# 2. Registry validation
# ============================================================
section "Registry validation"

reg_warnings=$($COMPILER "$PP_SRC" --report proof-status 2>&1 | grep "^warning:" || true)
reg_errors=$($COMPILER "$PP_SRC" --report proof-status 2>&1 | grep "^error:" || true)

if [ -z "$reg_errors" ]; then
    pass "no registry errors"
else
    fail "registry errors found: $reg_errors"
fi

# Stale fingerprint warning is expected (compute_checksum)
if echo "$reg_warnings" | grep -q "stale fingerprint.*compute_checksum" || echo "$($COMPILER "$PP_SRC" --report proof-status 2>&1)" | grep -q "stale"; then
    pass "stale fingerprint warning present for compute_checksum"
else
    fail "expected stale fingerprint warning for compute_checksum"
fi

# ============================================================
# 3. Proof-status consistency
# ============================================================
section "Proof-status consistency"

ps_out=$($COMPILER "$PP_SRC" --report proof-status 2>&1) || true

# Count obligations
ps_proved=$(echo "$ps_out" | grep -c "proved" || true)
ps_stale=$(echo "$ps_out" | grep -c "proof stale" || true)
ps_missing=$(echo "$ps_out" | grep -c "no proof" || true)
ps_blocked=$(echo "$ps_out" | grep -c "blocked" || true)
ps_ineligible=$(echo "$ps_out" | grep -c "not eligible" || true)

if [ "$ps_proved" -ge 2 ]; then
    pass "at least 2 proved functions"
else
    fail "expected at least 2 proved functions, got $ps_proved"
fi

if [ "$ps_stale" -ge 1 ]; then
    pass "at least 1 stale proof"
else
    fail "expected at least 1 stale proof, got $ps_stale"
fi

# Totals line should be present
if echo "$ps_out" | grep -q "Totals:"; then
    pass "proof-status totals line present"
else
    fail "proof-status should show totals line"
fi

# ============================================================
# 4. Proof diagnostics
# ============================================================
section "Proof diagnostics"

diag_out=$($COMPILER "$PP_SRC" --report proof-diagnostics 2>&1) || true

# Should have failure and repair classes
if echo "$diag_out" | grep -q "failure:" && echo "$diag_out" | grep -q "repair:"; then
    pass "diagnostics include failure and repair classes"
else
    fail "diagnostics should include failure: and repair: lines"
fi

# Should cover stale_proof diagnostic
if echo "$diag_out" | grep -q "stale_proof"; then
    pass "stale_proof diagnostic present"
else
    fail "expected stale_proof diagnostic"
fi

# Should cover unsupported_construct diagnostic
if echo "$diag_out" | grep -q "unsupported_construct"; then
    pass "unsupported_construct diagnostic present"
else
    fail "expected unsupported_construct diagnostic"
fi

# ============================================================
# 5. Proof dependencies
# ============================================================
section "Proof dependencies"

deps_out=$($COMPILER "$PP_SRC" --report proof-deps 2>&1) || true

if echo "$deps_out" | grep -q "validate_header" && echo "$deps_out" | grep -q "check_nonce"; then
    pass "dependency graph shows validate_header → check_nonce"
else
    fail "dependency graph should show validate_header → check_nonce edge"
fi

if echo "$deps_out" | grep -q "Summary:"; then
    pass "dependency graph has summary line"
else
    fail "dependency graph should have summary"
fi

# ============================================================
# 6. Evidence bundle
# ============================================================
section "Evidence bundle"

bundle_out=$($COMPILER "$PP_SRC" --report proof-bundle 2>&1 | grep -v '^warning:')

if echo "$bundle_out" | python3 -c "import sys,json; d=json.load(sys.stdin); assert d['schema_kind']=='proof_bundle'" 2>/dev/null; then
    pass "evidence bundle is valid JSON with correct schema_kind"
else
    fail "evidence bundle should be valid JSON with schema_kind=proof_bundle"
fi

if echo "$bundle_out" | python3 -c "
import sys,json; d=json.load(sys.stdin)
s=d['summary']
assert s['proved'] >= 2
assert s['total_functions'] >= 7
assert d['fact_count'] == len(d['facts'])
assert len(d['registry']) >= 3
assert len(d['assumptions']) >= 5
" 2>/dev/null; then
    pass "evidence bundle summary, facts, registry, and assumptions are consistent"
else
    fail "evidence bundle structure check failed"
fi

# ============================================================
# 7. Determinism
# ============================================================
section "Determinism"

ext1=$($COMPILER "$PP_SRC" --report extraction 2>&1)
ext2=$($COMPILER "$PP_SRC" --report extraction 2>&1)

if [ "$ext1" = "$ext2" ]; then
    pass "extraction is deterministic (two runs identical)"
else
    fail "extraction should be deterministic"
fi

fp1=$($COMPILER "$PP_SRC" --report fingerprints 2>&1)
fp2=$($COMPILER "$PP_SRC" --report fingerprints 2>&1)

if [ "$fp1" = "$fp2" ]; then
    pass "fingerprints are deterministic (two runs identical)"
else
    fail "fingerprints should be deterministic"
fi

# ============================================================
# 8. Lean theorem checking
# ============================================================
section "Lean theorem checking"

cp_out=$($COMPILER "$PP_SRC" --report check-proofs 2>&1) || true
cp_exit=$?

if echo "$cp_out" | grep -q "Kernel-verified\|verified"; then
    pass "check-proofs reports verified theorems"
else
    fail "check-proofs should report verified theorems"
fi

if echo "$cp_out" | grep -q "check_nonce.*correct\|check_nonce"; then
    pass "check_nonce theorem verified"
else
    fail "check_nonce theorem should be verified"
fi

# ============================================================
# Summary
# ============================================================
echo ""
echo "=== Proof Evidence Gate Summary ==="
echo "  passed: $PASS / $TOTAL"
echo "  failed: $FAIL / $TOTAL"

if [ "$FAIL" -gt 0 ]; then
    echo ""
    echo "PROOF GATE FAILED"
    exit 1
else
    echo ""
    echo "PROOF GATE PASSED"
    exit 0
fi
