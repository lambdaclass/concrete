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
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# family | gate command (relative to repo root). Order is stable/documented.
GATES=(
  "diagnostic-code completeness|bash scripts/tests/check_diagnostic_codes_complete.sh"
  "every diagnostic has a code|bash scripts/tests/check_diagnostic_codes_present.sh"
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
  "site pages generated from docs|bash scripts/tests/check_site_generated.sh"
)

# Every constituent's output is kept in its own log. The runner used to discard it, so a
# constituent that failed inside the runner and passed when re-run alone left no evidence of why
# (2026-10-23: docs drift failed once on fede-256gb and the cause could not be recovered). A
# failure now prints the constituent's own FAIL lines, its exit code, its time and its log, and
# the logs of a failing run are kept (FAST_SURFACE_LOG_DIR to choose where).
run_all() {
  local pass=0 fail=0 failed="" logdir keep=0
  logdir="${FAST_SURFACE_LOG_DIR:-$(mktemp -d)}"; mkdir -p "$logdir"
  for entry in "${GATES[@]}"; do
    local fam="${entry%%|*}" cmd="${entry#*|}" log rc t0
    log="$logdir/$(printf '%s' "$fam" | tr -c 'A-Za-z0-9' '_').log"
    t0=$(date +%s)
    eval "$cmd" > "$log" 2>&1; rc=$?
    if [ "$rc" -eq 0 ]; then
      pass=$((pass+1))
    else
      fail=$((fail+1)); keep=1
      failed="$failed\n  FAILED SURFACE GATE [$fam] (exit $rc, $(( $(date +%s) - t0 ))s, log $log):\n    $cmd"
      failed="$failed\n$(grep -E '^\s*(FAIL|INCOMPLETE|error)' "$log" | awk 'NR<=10 {print "      " $0}')"
    fi
  done
  echo "FAST-SURFACE-GATES: PASS=$pass FAIL=$fail"
  if [ "$fail" -ne 0 ]; then
    echo -e "Re-run the exact failing constituent(s):$failed"
    echo "Constituent logs kept in $logdir"
    return 1
  fi
  [ -n "${FAST_SURFACE_LOG_DIR:-}" ] || [ "$keep" -ne 0 ] || rm -rf "$logdir"
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

# THE SELF-TEST MUTATES A PRIVATE COPY, never this checkout (R-0546). It used to edit the tracked
# files below in place, inside a full gate pass that runs ~30 other gates beside it. Every gate's
# require_fresh_binary runs `lake build`, so while Concrete/Report/Report.lean was mutated a
# concurrent gate rebuilt the SHARED compiler from the mutated source; the binary vanished during the
# relink (fuzzers: "No such file ... bin/concrete"), the tracked tree read dirty (check_clean_checkout),
# the build identity moved (check_checker_identity), and the stdlib manifest gate saw a missing row.
# The runner's sequential recheck then passed them all, so a real race in this gate was being
# absorbed as "contention". The copy is lib/private_checkout.sh's: this checkout's tracked content
# (uncommitted edits and deletions included) in its own git repository, with its own .lake seeded from
# this one and verified by lake build, and its own repository lock.
source "$ROOT_DIR/scripts/tests/lib/private_checkout.sh"

mutate_self_test() {
  local ok=0 bad=0 base tree
  # MUT_BASE is GLOBAL: the EXIT trap runs after this function's locals are gone.
  MUT_BASE="$(mktemp -d)" || { echo "FAST-SURFACE-MUTATION: could not create a private copy"; return 1; }
  trap 'rm -rf "${MUT_BASE:-}"' EXIT
  base="$MUT_BASE"
  if ! tree="$(private_checkout "$base")"; then
    echo "FAST-SURFACE-MUTATION: could not make a faithful private copy — nothing was proven"
    sed 's/^/    /' "$base/private-checkout.log" 2>/dev/null | tail -5
    return 1
  fi
  # Everything below runs in the copy, under the copy's own repository lock.
  cd "$tree" || return 1
  unset CONCRETE_GATE_LOCK
  # family | file | fixed-string needle identifying one representative row
  local TAB; TAB="$(printf '\t')"
  local -a MUT=(
    "manifest fact|docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv|option${TAB}unwrap_or_else${TAB}"
    "diagnostic-code ledger|Concrete/Report/Report.lean|entry \"E0296\""
    "value-flow constructor row|docs/spec/ownership.md|\`letDestructure\`"
  )
  for m in "${MUT[@]}"; do
    local fam="${m%%|*}"; local rest="${m#*|}"; local file="${rest%%|*}"; local needle="${rest#*|}"
    if [ ! -f "$file" ]; then echo "  SKIP $fam (no $file)"; continue; fi
    local bak; bak="$(mktemp)"; cp "$file" "$bak"
    local did; did="$(remove_first_line_with "$file" "$needle")"
    if [ "$did" != "1" ]; then
      echo "  WARN $fam: needle matched no row (stale) — NOT proven"
      bad=$((bad+1))
    elif FAST_SURFACE_LOG_DIR="$base/logs/$ok$bad" run_all > "$base/agg.out" 2>&1; then
      echo "  BAD  $fam: aggregate still PASSED after removing a row (gate is hollow)"
      bad=$((bad+1))
    elif ! grep -qE '^      .*FAIL' "$base/agg.out"; then
      # the aggregate must show WHY: the failing constituent's own FAIL lines, not only its name
      echo "  BAD  $fam: the aggregate failed without showing the constituent's FAIL lines"
      bad=$((bad+1))
    else
      echo "  ok   $fam: removing a row turns the aggregate RED and shows the constituent's FAIL lines"
      ok=$((ok+1))
    fi
    cat "$bak" > "$file"; rm -f "$bak"
  done
  cd "$ROOT_DIR" || true
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