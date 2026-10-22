#!/usr/bin/env bash
# IMPACT GATE SELECTION, REPLAYED AGAINST THE FAILURES IT EXISTS TO PREVENT (R-0513).
#
# scripts/tests/select_gates.sh picks the workflow gate commands a diff can break, and the pre-push
# hook runs what it picks. A selector is only worth its speed if it never drops a gate the diff
# breaks, and the cases that matter are ones the old area map already missed. On 2026-10-22 four
# failures were first seen in CI, about 1.5 h each:
#   - bug 129's symbol mangling (c3ae877a) broke check_stack_budget.sh (an extern spelled like an
#     intrinsic became E0118) and the float round-trip judge that check_concrete_fmt.sh runs;
#   - safety-eval2's heap rule (bug 144) broke check_linear_discard.sh;
#   - the symbols rebase broke the trust gate's bug-audit corpus (run_tests.sh --trust-gate).
# Each diff is a FIXED file list in tests/fixtures/gate_selection/, taken from the real commits
# named in its header. The lists are written out so the controls keep their meaning on a shallow
# CI checkout and after history is rewritten; when the commits are present, the lists are
# checked against them.
#
# Over-selection is safe and costs minutes; a selector that selects everything would pass every
# must-select control, so precision is pinned too (a script-only diff, a docs-only diff), and so are
# the fail-safe (an unmapped path selects everything) and the runner wiring (`--impact-files`
# resolves to exactly the selector's list).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0; SKIP=0
ok()   { echo "  ok   $1"; PASS=$((PASS+1)); }
no()   { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
skip() { echo "  skip $1"; SKIP=$((SKIP+1)); }

FIX="tests/fixtures/gate_selection"
SELECT="scripts/tests/select_gates.sh"
for f in "$SELECT" scripts/tests/lib/select_gates.py "$FIX"; do
  [ -e "$f" ] || { echo "FATAL: $f missing" >&2; exit 2; }
done

TMPD="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMPD"' EXIT

bash scripts/tests/run_ci_gates_local.sh --plan > "$TMPD/inventory" 2>"$TMPD/inventory.err" \
  || { echo "FATAL: the runner could not resolve the gate inventory:" >&2; cat "$TMPD/inventory.err" >&2; exit 2; }
NINV="$(grep -c . "$TMPD/inventory")"
[ "$NINV" -gt 100 ] || { echo "FATAL: gate inventory has only $NINV commands" >&2; exit 2; }

# select NAME -> $TMPD/NAME.sel (one command per line); a selector failure is a FAIL, not an empty list
select_fixture() {
  local name="$1" src="$2"
  if ! bash "$SELECT" --files "$src" > "$TMPD/$name.sel" 2>"$TMPD/$name.err"; then
    no "$name: the selector failed: $(head -c 300 "$TMPD/$name.err")"
    return 1
  fi
  return 0
}
# has NAME GATE-TEXT: some selected command runs GATE-TEXT (a script name, optionally with flags)
has() { grep -qE "(^|[ /])$(printf '%s' "$2" | sed 's/[.[\*^$]/\\&/g')( |$)" "$TMPD/$1.sel"; }
must() {
  local name="$1"; shift
  local g
  for g in "$@"; do
    if has "$name" "$g"; then ok "$name selects $g"
    else no "$name does NOT select $g, which this diff broke in CI"; fi
  done
}
count() { grep -c . "$TMPD/$1.sel"; }

echo "=== fixture fidelity: each list is its commits' diff (when history is present) ==="
for p in "$FIX"/*.paths; do
  base="$(sed -n 's/^# base //p' "$p")"; head="$(sed -n 's/^# head //p' "$p")"
  name="$(basename "$p" .paths)"
  if [ -z "$base" ] || [ -z "$head" ]; then no "$name: no '# base'/'# head' header"; continue; fi
  if git cat-file -e "$base^{commit}" 2>/dev/null && git cat-file -e "$head^{commit}" 2>/dev/null; then
    if diff <(git diff --name-only "$base" "$head") <(grep -v '^#' "$p") >/dev/null; then
      ok "$name matches git diff ${base:0:8}..${head:0:8}"
    else
      no "$name differs from git diff ${base:0:8}..${head:0:8}"
    fi
  else
    skip "$name: ${base:0:8}/${head:0:8} not in this clone (shallow checkout); the list is used as written"
  fi
done

echo "=== replay: the 2026-10-22 CI-only failures are selected ==="
select_fixture symbols "$FIX/symbols_c3ae877a.paths" \
  && must symbols check_stack_budget.sh check_concrete_fmt.sh
select_fixture stackleg "$FIX/stack_budget_leg_26c409f8.paths" \
  && must stackleg check_stack_budget.sh
select_fixture float "$FIX/float_judge_1ee7c267.paths" \
  && must float check_concrete_fmt.sh
select_fixture safety "$FIX/safety_eval2_aaa4d0c7.paths" \
  && must safety check_linear_discard.sh
select_fixture rebase "$FIX/symbols_rebase_9757540e_b376b4a2.paths" \
  && must rebase "run_tests.sh --trust-gate"
# The same trust-gate failure without the compiler rule's help: a bug record (docs/bugs, the corpus
# map, a regression) and no Concrete/ change. The audit corpus reads docs/bugs.
select_fixture record "$FIX/bug103_record_a7323992.paths" \
  && must record "run_tests.sh --trust-gate"

echo "=== precision: a selector that selects everything would pass the above ==="
if [ -f "$TMPD/stackleg.sel" ]; then
  n="$(count stackleg)"
  if [ "$n" -lt $((NINV / 5)) ]; then ok "a gate-script + fixture diff selects $n of $NINV"
  else no "a gate-script + fixture diff selects $n of $NINV (>= 20%): the selector does not discriminate"; fi
fi
if select_fixture docs "$FIX/docs_only_92861f4e.paths"; then
  n="$(count docs)"
  if [ "$n" -le 6 ]; then ok "the docs-only diff selects $n gate command(s)"
  else no "the docs-only diff selects $n gate commands (> 6)"; fi
  heavy="$(grep -E 'run_tests\.sh|check_examples\.sh|test_ssa\.sh|check_example_outputs\.sh' "$TMPD/docs.sel" || true)"
  if [ -z "$heavy" ]; then ok "the docs-only diff selects no corpus runner"
  else no "the docs-only diff selects a corpus runner: $heavy"; fi
  sed 's/^/         /' "$TMPD/docs.sel"
fi

echo "=== rules: compiler, shared library, fail-safe ==="
printf 'Concrete/Resolve/BuiltinSigs.lean\n' > "$TMPD/compiler.paths"
if select_fixture compiler "$TMPD/compiler.paths"; then
  must compiler run_tests.sh check_stack_budget.sh check_linear_discard.sh check_concrete_fmt.sh
  n="$(count compiler)"
  if [ "$n" -ge $((NINV * 4 / 5)) ]; then ok "a compiler module selects $n of $NINV (every compiler- or Lean-running gate)"
  else no "a compiler module selects only $n of $NINV"; fi
fi
printf 'scripts/tests/lib/fresh.sh\n' > "$TMPD/lib.paths"
if select_fixture lib "$TMPD/lib.paths"; then
  must lib check_stack_budget.sh check_linear_discard.sh
fi
# Assembled at run time: a path spelled out in this file would be "read" by this gate.
printf '%s/%s\n' zz_unmapped_dir new_input.bin > "$TMPD/unmapped.paths"
if select_fixture unmapped "$TMPD/unmapped.paths"; then
  n="$(count unmapped)"
  if [ "$n" -eq "$NINV" ]; then ok "a path no rule maps selects every gate ($n) — fail-safe"
  else no "a path no rule maps selects $n of $NINV; the fail-safe must select every gate"; fi
fi

printf '.github/workflows/lean_action_ci.yml\n' > "$TMPD/wf.paths"
if SELECT_GATES_BASE=none select_fixture wfnobase "$TMPD/wf.paths"; then
  n="$(count wfnobase)"
  if [ "$n" -eq "$NINV" ]; then ok "a workflow edit with no base to compare selects every gate ($n)"
  else no "a workflow edit with no base selects $n of $NINV"; fi
fi
if SELECT_GATES_BASE=HEAD select_fixture wfhead "$TMPD/wf.paths"; then
  n="$(count wfhead)"
  if [ "$n" -lt "$NINV" ]; then ok "a workflow edit compared with a base selects only what reads it or changed ($n)"
  else no "a workflow edit compared with HEAD still selects every gate"; fi
fi

echo "=== indirect inputs (each one an under-selection found in review) ==="
# A manifest names the example; the gate's text names neither the example nor its expected output.
printf 'examples/base64_cli/expected/encode.stdout\n' > "$TMPD/expected.paths"
select_fixture expected "$TMPD/expected.paths" && must expected check_example_outputs.sh
# check_campaign_summary_golden.sh runs a one-family campaign through the mutation driver, which
# launches its children through this helper: a driver invoked in a bounded mode still runs its helpers.
printf 'scripts/tests/lib/run_campaign_child.py\n' > "$TMPD/child.paths"
select_fixture child "$TMPD/child.paths" && must child check_campaign_summary_golden.sh
# A workflow edit that re-spells a command (new arguments) selects that command: compared against
# main before this gate was registered, the gate's own command is new.
if git cat-file -e "713778e2^{commit}" 2>/dev/null; then
  if SELECT_GATES_BASE=713778e2 select_fixture wfadd "$TMPD/wf.paths"; then
    must wfadd check_gate_selection.sh
  fi
else
  skip "713778e2 not in this clone: the added-command leg needs the base's workflow"
fi

echo "=== wiring: run_ci_gates_local.sh --impact-files resolves to exactly the selector's list ==="
grep -v '^#' "$FIX/stack_budget_leg_26c409f8.paths" > "$TMPD/wire.paths"
if bash scripts/tests/run_ci_gates_local.sh --plan --impact-files "$TMPD/wire.paths" > "$TMPD/wire.plan" 2>/dev/null; then
  if diff <(sort "$TMPD/wire.plan") <(sort "$TMPD/stackleg.sel") >/dev/null; then
    ok "the runner's impact plan equals the selector output ($(grep -c . "$TMPD/wire.plan") commands)"
  else
    no "the runner's impact plan differs from the selector output"
  fi
else
  no "run_ci_gates_local.sh --plan --impact-files failed"
fi

echo
echo "GATE-SELECTION: PASS=$PASS FAIL=$FAIL SKIP=$SKIP"
[ "$FAIL" -eq 0 ]
