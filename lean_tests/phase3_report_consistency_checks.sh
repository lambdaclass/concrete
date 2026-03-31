#!/usr/bin/env bash
# Phase 3: Report consistency cross-checks
# Paste these into run_tests.sh inside the report section.
# All checks use $TESTDIR and the check_report / check_report_multi helpers.

# ============================================================
# Cross-check 1: proof-eligible functions have no capabilities in caps report
# ============================================================

# proof says pure_compute is eligible
check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✓ pure_compute" \
    "consistency: proof-eligible pure_compute" \
    "consistency: pure_compute not proof-eligible"

# caps confirms pure_compute is pure (no caps)
check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "pure_compute.*(pure)" \
    "consistency: caps confirms pure_compute is pure" \
    "consistency: caps does not confirm pure_compute is pure"

# proof says pure_multiply is eligible
check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✓ pure_multiply" \
    "consistency: proof-eligible pure_multiply" \
    "consistency: pure_multiply not proof-eligible"

# caps confirms pure_multiply is pure
check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "pure_multiply.*(pure)" \
    "consistency: caps confirms pure_multiply is pure" \
    "consistency: caps does not confirm pure_multiply is pure"

# ============================================================
# Cross-check 2: trusted functions in unsafe report AND excluded from proof
# ============================================================

# unsafe report lists trusted_read
check_report "$TESTDIR/phase3_report_consistency.con" unsafe \
    "trusted_read" \
    "consistency: unsafe report shows trusted_read" \
    "consistency: unsafe report missing trusted_read"

# unsafe report lists safe_abs (trusted extern)
check_report "$TESTDIR/phase3_report_consistency.con" unsafe \
    "safe_abs" \
    "consistency: unsafe report shows safe_abs" \
    "consistency: unsafe report missing safe_abs"

# proof excludes trusted_read (trusted boundary)
check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✗ trusted_read.*trusted boundary" \
    "consistency: proof excludes trusted_read" \
    "consistency: proof does not exclude trusted_read"

# proof excludes safe_abs (trusted extern)
check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "✗ safe_abs.*trusted" \
    "consistency: proof excludes safe_abs" \
    "consistency: proof does not exclude safe_abs"

# ============================================================
# Cross-check 3: functions with capabilities appear in authority report
# ============================================================

# authority report shows Alloc section (needed by needs_alloc)
check_report "$TESTDIR/phase3_report_consistency.con" authority \
    "capability Alloc" \
    "consistency: authority shows Alloc section" \
    "consistency: authority missing Alloc section"

# authority traces needs_alloc -> vec_new
check_report "$TESTDIR/phase3_report_consistency.con" authority \
    "needs_alloc.*vec_new" \
    "consistency: authority traces needs_alloc -> vec_new" \
    "consistency: authority missing needs_alloc chain"

# authority report shows Console section (needed by needs_console)
check_report "$TESTDIR/phase3_report_consistency.con" authority \
    "capability Console" \
    "consistency: authority shows Console section" \
    "consistency: authority missing Console section"

# caps report confirms needs_alloc has Alloc
check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "needs_alloc.*Alloc" \
    "consistency: caps confirms needs_alloc has Alloc" \
    "consistency: caps missing needs_alloc Alloc"

# caps report confirms needs_console has Console
check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "needs_console.*Console" \
    "consistency: caps confirms needs_console has Console" \
    "consistency: caps missing needs_console Console"

# ============================================================
# Cross-check 4: generic functions appear in mono with specializations
# ============================================================

# mono report lists identity with specialization
check_report "$TESTDIR/phase3_report_consistency.con" mono \
    "identity.*i32" \
    "consistency: mono shows identity<i32> specialization" \
    "consistency: mono missing identity<i32> specialization"

# mono report lists generic_add_one with specialization
check_report "$TESTDIR/phase3_report_consistency.con" mono \
    "generic_add_one.*i32" \
    "consistency: mono shows generic_add_one<i32> specialization" \
    "consistency: mono missing generic_add_one<i32> specialization"

# mono shows generic function count (at least 2)
check_report "$TESTDIR/phase3_report_consistency.con" mono \
    "Generic functions:" \
    "consistency: mono shows Generic functions count" \
    "consistency: mono missing Generic functions count"

# ============================================================
# Cross-check 5: allocating functions appear in alloc report
# ============================================================

# alloc report shows needs_alloc allocates via vec_new
check_report "$TESTDIR/phase3_report_consistency.con" alloc \
    "allocates: vec_new" \
    "consistency: alloc shows vec_new allocation" \
    "consistency: alloc missing vec_new allocation"

# alloc report shows alloc_and_free uses defer free
check_report_multi "$TESTDIR/phase3_report_consistency.con" alloc \
    "consistency: alloc shows alloc_and_free defer free" \
    "consistency: alloc missing alloc_and_free defer free" \
    "fn alloc_and_free" "defer free"

# alloc report shows alloc_returned returns allocation (caller responsible)
check_report_multi "$TESTDIR/phase3_report_consistency.con" alloc \
    "consistency: alloc shows alloc_returned caller-responsible" \
    "consistency: alloc missing alloc_returned caller-responsible" \
    "fn alloc_returned" "caller responsible for cleanup"

# ============================================================
# Cross-check 6: repr(C) struct appears in layout report with fields
# ============================================================

# layout shows CPoint as repr(C) struct
check_report_multi "$TESTDIR/phase3_report_consistency.con" layout \
    "consistency: layout shows repr(C) CPoint" \
    "consistency: layout missing repr(C) CPoint" \
    "struct CPoint" "repr(C)"

# layout shows CPoint fields x, y, z
check_report_multi "$TESTDIR/phase3_report_consistency.con" layout \
    "consistency: layout shows CPoint fields" \
    "consistency: layout missing CPoint fields" \
    "struct CPoint" "size: 12"

# layout shows NormalPair
check_report_multi "$TESTDIR/phase3_report_consistency.con" layout \
    "consistency: layout shows NormalPair" \
    "consistency: layout missing NormalPair" \
    "struct NormalPair" "size: 8"

# layout totals line includes both structs
check_report "$TESTDIR/phase3_report_consistency.con" layout \
    "Totals:.*struct" \
    "consistency: layout totals present" \
    "consistency: layout totals missing"

# ============================================================
# Cross-check 7: proof eligible count matches number of pure functions in caps
# ============================================================

# proof shows 2 eligible (pure_compute, pure_multiply) — the only pure public fns
check_report "$TESTDIR/phase3_report_consistency.con" proof \
    "2 eligible for ProofCore" \
    "consistency: proof shows 2 eligible" \
    "consistency: proof wrong eligible count"

# caps should show exactly 2 pure functions (pure_compute, pure_multiply)
# Both must appear as (pure) in caps; identity and wrap_pair are also pure but
# the eligible count matches the public pure non-generic fns
check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "pure_compute.*(pure)" \
    "consistency: caps cross-check pure_compute is pure for count" \
    "consistency: caps cross-check pure_compute not pure"

check_report "$TESTDIR/phase3_report_consistency.con" caps \
    "pure_multiply.*(pure)" \
    "consistency: caps cross-check pure_multiply is pure for count" \
    "consistency: caps cross-check pure_multiply not pure"
