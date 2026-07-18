#!/usr/bin/env bash
# Bug-to-regression corpus audit.
# Checks that every numbered bug in docs/bugs/ has regression test coverage.
#
# Each bug must either:
#   1. Have an entry in BUG_TEST_MAP below (bug number -> test file name)
#   2. Be listed in SKIP_BUGS with a justification
#
# Run as a CI gate or manually:
#   bash scripts/tests/audit_bug_corpus.sh

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BUGS_DIR="$ROOT_DIR/docs/bugs"
TESTS_DIR="$ROOT_DIR/tests/programs"
RUN_TESTS="$ROOT_DIR/scripts/tests/run_tests.sh"

PASS=0
FAIL=0
SKIP=0

# Explicit mapping: bug number -> regression test file(s) in tests/programs/.
# Multiple files separated by space.
declare -A BUG_TEST_MAP=(
  [001]="bug_cross_module_struct_field.con"
  [002]="bug_i32_literal_type.con"
  [003]="bug_cross_module_mut_borrow.con"
  [004]="bug_array_var_index_assign.con"
  [005]="bug_enum_in_struct.con"
  [007]="bug_print_builtins.con"
  [008]="bug_if_expression.con"
  [011]="bug_string_building.con"
  [012]="bug_clock_builtin.con"
  [013]="test_string_literal_in_loop.con"
  [014]="test_string_literal_in_loop.con"
  [016]="bug_cross_module_mut_borrow.con"
  [018]="bug_stack_array_borrow_copy.con"
  [019]="bug_array_struct_field_mutation.con"
  [020]="bug_int_match_consume.con"
  [021]="bug_int_match_disagree.con"
  [022]="submodule_linear_consume/src/main.con"
  [023]="scand_aggregate_in_scope/src/main.con"
)

# Bugs that don't need a .con regression test (with reason).
declare -A SKIP_BUGS=(
  [006]="string literal collision -- fixed in EmitSSA, covered by cross-module tests"
  [009]="const decls not lowered -- fixed in Lower.lean, no standalone repro needed"
  [010]="missing string_substr -- stdlib addition"
  [015]="O0 default perf -- compiler flag default change, not a .con test"
  [017]="socket constants linux-only -- platform-specific stdlib"
  [024]="recursive-struct infinite size -- covered by check_error_leaks.sh gate corpus"
  [025]="no-main linker leak -- covered by check_error_leaks.sh (no_main, empty_file cases)"
  [026]="huge array-repeat count hang -- covered by check_error_leaks.sh (huge_array case)"
  [027]="EmitSSA O(n^2) rendering -- OPEN perf item; no .con regression (codegen perf, not correctness)"
  [028]="reserved-name collision -- covered by check_error_leaks.sh (clash_*, extern_argc cases)"
  [030]="FIXED general (not trusted-only) -- error_030_nonmut_array_write.con (run_err E0217); Check arrayIndexAssign mut rule"
  [029]="FIXED both sites -- regress_029_if_merge_array_addr.con + regress_029_loop_exit_array_addr.con in tests/programs (run_ok 42/7)"
  [033]="FIXED -- regress_033_discard_live_string.con (run_ok live-across); Lower ifExpr merge aggregate alloca path"
  [032]="FIXED -- regress_032_multibyte_str_literal.con (run_ok 42); EmitSSA byte-based literal sizes+escapes"
  [031]="FIXED all 3 sites -- regress_031_if_branch_borrow.con + regress_031_ifexpr_branch_borrow.con + regress_031_match_arm_borrow.con (run_ok 101/107/8); Lower prePromoteAddrTaken"
  [034]="FIXED -- regress_034_shortcircuit_borrow_promotion.con (run_ok 133) guards the shape; LOAD-BEARING regression = check_cli_helpers.sh two-positionals leg (pre-fix compiler aborts 134, verified); Lower &&/|| prePromoteAddrTaken"
  [035]="FIXED -- enum_generic_payload_layout/src/main.con (project test, exit 0); lowerModule now receives program-wide struct/enum/newtype defs (own-module priority, additive fill)"
  [036]="FIXED -- import_closure_metadata/src/main.con (project test, exit 0); resolveImports closes over public types reachable through imported signatures"
  [037]="FIXED -- error_repr_align_exceeds_natural.con (run_err E0585); repr_align.con moved to the legal no-op case (run_ok 8); CoreCheck reprAlignExceedsNatural"
  [038]="FIXED -- regress_038_if_merge_promoted_aggregate.con (run_ok qm); Lower merge loops skip ANY promoted var (aggregate included); extended fuzzer is the class gate"
  [039]="FIXED -- regress_039_import_alias_collision/src/main.con (project test, exit 0); emitSModule puts the module's own bare->qualified import aliases ahead of the program-wide pool"
  [040]="FIXED -- regress_040_match_binder_types.con (run_ok 42); CoreCheck addVar shadows (prepend) + match-arm binders arm-scoped (save/restore)"
  [041]="FIXED -- regress_041_match_binder_states.con (run_ok 42) + error_041_match_leak_still_caught.con (run_err E0208); Check post-match merges rebuild from envBefore (arm binders arm-scoped)"
  [042]="FIXED -- regress_042_import_newtype/src/main.con (project test, exit 0) + std-compiled-coverage numeric leg; Resolve import classifier now registers newtypes + public type aliases"
)

echo "=== Bug-to-Regression Corpus Audit ==="
echo

for bugfile in "$BUGS_DIR"/[0-9][0-9][0-9]_*.md; do
  [[ -f "$bugfile" ]] || continue
  base=$(basename "$bugfile" .md)
  num=${base%%_*}

  # Skip exempted bugs
  if [[ -n "${SKIP_BUGS[$num]:-}" ]]; then
    echo "  skip  $base (${SKIP_BUGS[$num]})"
    SKIP=$((SKIP + 1))
    continue
  fi

  # Check explicit mapping
  if [[ -z "${BUG_TEST_MAP[$num]:-}" ]]; then
    echo "  FAIL  $base -- no entry in BUG_TEST_MAP"
    FAIL=$((FAIL + 1))
    continue
  fi

  issues=""
  for testfile in ${BUG_TEST_MAP[$num]}; do
    # Check test file exists
    if [[ ! -f "$TESTS_DIR/$testfile" ]]; then
      issues="${issues}missing $testfile; "
      continue
    fi
    # Check test is registered in run_tests.sh
    testbase="${testfile%.con}"
    if ! grep -q "$testbase" "$RUN_TESTS" 2>/dev/null; then
      issues="${issues}$testfile not in run_tests.sh; "
    fi
  done

  if [[ -z "$issues" ]]; then
    echo "  ok    $base -> ${BUG_TEST_MAP[$num]}"
    PASS=$((PASS + 1))
  else
    echo "  FAIL  $base -- $issues"
    FAIL=$((FAIL + 1))
  fi
done

echo
echo "=== Reverse check: bug_*.con without a numbered bug ==="

for testfile in "$TESTS_DIR"/bug_*.con; do
  [[ -f "$testfile" ]] || continue
  testbase=$(basename "$testfile")
  # Check if any BUG_TEST_MAP value references this file
  found=false
  for mapped in "${BUG_TEST_MAP[@]}"; do
    for f in $mapped; do
      [[ "$f" == "$testbase" ]] && found=true && break 2
    done
  done
  if ! $found; then
    echo "  warn  $testbase -- not mapped to any numbered bug"
  fi
done

echo
echo "=== Results ==="
echo "  pass:    $PASS"
echo "  fail:    $FAIL"
echo "  skip:    $SKIP"

if [[ "$FAIL" -gt 0 ]]; then
  echo
  echo "FAILED: $FAIL bug(s) missing regression coverage."
  echo "Fix: add entry to BUG_TEST_MAP or SKIP_BUGS in this script."
  exit 1
fi

echo
echo "All numbered bugs have regression coverage."
