#!/usr/bin/env bash
# R-0022 — the ONE fast fail-closed surface-gate target.
#
# Every inventory/completeness gate that must fail the instant you ADD an
# E-code, public item, IR/AST constructor, manifest fact, generated file, or
# counted append pattern without a matching ledger/spec/fixture row. This is
# the single checklist: `make test-fast-surface-gates` runs it locally and
# CI's Language-surface job runs the SAME script (no hidden second list). On
# failure it prints the exact constituent command so a red board is
# immediately actionable.
#
# Constituents are FAST (static/grep/tiny-compile, ~10s total) and
# fail-closed. Heavy gates (proof replay, fuzz, differential, oracle) live
# elsewhere — they are not surface-inventory checks.
#
# Usage:
#   run_fast_surface_gates.sh              run all constituents, fail-closed
#   run_fast_surface_gates.sh --list       print the family -> command map
#   run_fast_surface_gates.sh --mutate     self-test: prove the aggregate goes
#                                           RED when a representative row is
#                                           removed from each row-bearing family

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# family | gate command (relative to repo root). Order is stable/documented.
GATES=(
  "diagnostic-code completeness|bash scripts/tests/check_diagnostic_codes_complete.sh"
  "quadratic-append ratchet|bash scripts/tests/check_no_quadratic_append.sh"
  "public API inventory + import firewall|bash scripts/tests/check_compiler_api_boundary.sh"
  "backend interface contract|bash scripts/tests/check_backend_contracts.sh"
  "AST/IR constructor inventory|bash scripts/tests/check_value_flow_spec.sh"
  "walker (consumer) coverage|bash scripts/tests/check_constructor_coverage.sh"
  "stdlib manifest truth (cap/trust/ownership)|bash scripts/tests/check_stdlib_manifest.sh"
  "capability fact source|bash scripts/tests/check_capability_facts.sh"
  "construction rights + fixtures|bash scripts/tests/check_construction_rights.sh"
  "module visibility + import fixtures|bash scripts/tests/check_module_visibility.sh"
  "generated-file / docs drift|bash scripts/tests/check_docs_drift.sh"
)

run_all() {
  local pass=0 fail=0 failed=""
  for entry in "${GATES[@]}"; do
    local fam="${entry%%|*}" cmd="${entry#*|}"
    if eval "$cmd" >/dev/null 2>&1; then
      pass=$((pass+1))
    else
      fail=$((fail+1))
      failed="$failed\n  FAILED SURFACE GATE [$fam]:\n    $cmd"
    fi
  done
  echo "FAST-SURFACE-GATES: PASS=$pass FAIL=$fail"
  if [ "$fail" -ne 0 ]; then
    echo -e "Re-run the exact failing constituent(s):$failed"
    return 1
  fi
  return 0
}

# Remove the first line of $file containing the fixed string $needle (portable:
# grep -F, no sed -i / GNU-vs-BSD hazard). Prints 1 if a line was removed.
remove_first_line_with() {
  local file="$1" needle="$2" tmp removed=0
  tmp="$(mktemp)"
  awk -v n="$needle" 'skip==0 && index($0,n){skip=1; next} {print}' "$file" > "$tmp"
  if ! cmp -s "$file" "$tmp"; then removed=1; fi
  cat "$tmp" > "$file"; rm -f "$tmp"
  echo "$removed"
}

mutate_self_test() {
  local ok=0 bad=0
  # family | file | fixed-string needle identifying one representative row
  local TAB; TAB="$(printf '\t')"
  local -a MUT=(
    "manifest fact|docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv|option${TAB}unwrap_or_else${TAB}"
    "diagnostic-code ledger|Concrete/Report/Report.lean|entry \"E0296\""
    "value-flow constructor row|docs/VALUE_FLOW_SPEC.md|\`letDestructure\`"
  )
  for m in "${MUT[@]}"; do
    local fam="${m%%|*}"; local rest="${m#*|}"; local file="${rest%%|*}"; local needle="${rest#*|}"
    if [ ! -f "$file" ]; then echo "  SKIP $fam (no $file)"; continue; fi
    local bak; bak="$(mktemp)"; cp "$file" "$bak"
    # restore on any exit path
    trap 'cat "$bak" > "$file"; rm -f "$bak"' RETURN 2>/dev/null || true
    local did; did="$(remove_first_line_with "$file" "$needle")"
    if [ "$did" != "1" ]; then
      echo "  WARN $fam: needle matched no row (stale) — NOT proven"
      bad=$((bad+1))
    elif run_all >/dev/null 2>&1; then
      echo "  BAD  $fam: aggregate still PASSED after removing a row (gate is hollow)"
      bad=$((bad+1))
    else
      echo "  ok   $fam: removing a row turns the aggregate RED"
      ok=$((ok+1))
    fi
    cat "$bak" > "$file"; rm -f "$bak"; trap - RETURN 2>/dev/null || true
  done
  echo "FAST-SURFACE-MUTATION: proven=$ok unproven=$bad"
  echo "(structural families — API boundary, visibility, construction rights — carry"
  echo " their own in-gate negative fixtures; nightly check_gate_mutation_coverage"
  echo " mutates compiler source to prove each is load-bearing.)"
  [ "$bad" -eq 0 ] && [ "$ok" -ge 3 ]
}

case "${1:-}" in
  --list)
    printf '%-42s  %s\n' "FAMILY" "COMMAND"
    for entry in "${GATES[@]}"; do printf '%-42s  %s\n' "${entry%%|*}" "${entry#*|}"; done ;;
  --mutate) mutate_self_test ;;
  *) run_all ;;
esac