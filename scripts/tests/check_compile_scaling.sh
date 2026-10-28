#!/usr/bin/env bash
# COMPILE-TIME SCALING GATE (R-0550, ROADMAP Phase 1 exit 2o; structural class 17, owner R-0422).
#
# Generated programs of 10^4 - 10^5 elements or statements per shape (array literals of constant
# and of distinct elements, array writes in a loop, long functions of assignments, chained `let`s
# and `let`s interleaved with `if`/`while`/`match`, many locals, many declarations, many structs
# and enums, deep statement and expression nesting) are compiled at two sizes a factor of ten
# apart, and every stage is timed: the in-compiler stages from `--emit-trace-json` (parse ... lower, with their
# deterministic allocation heartbeats) and the rest by whole compiles (`--emit-core`,
# `--emit-llvm`, native `-o`, `--interp`). A stage FAILS over its recorded budget, or when its
# growth between sizes is above the recorded near-linear bound. A wall-clock violation is
# re-measured within the recorded retry budget (each stage keeps its fastest run), and on a host
# that can count instructions (perf) a stage is judged against its budget at the host speed the
# budgets were measured at, up to a cap, while each run's instructions and each stage's
# allocations are held exactly (bug 294). Growth between sizes is judged on those exact counts
# where perf counted the run (a wall ratio measures the caches as much as the work, and false-failed
# linear stages near the x63 bound); a run nobody counted keeps the wall-time growth bound. So host
# load cannot fail unchanged work, and a retry cannot hide a quadratic stage: growth is resolved above
# the floors stated in compile_scaling.py (RESOLUTION, SIZES), and below them, or for a quadratic
# part still small at these sizes, only the budget holds the stage.
# Budgets, sizes, bounds and the named exclusions (known filed defects, reported on every run) are
# in scripts/tests/lib/compile_scaling_budgets.tsv, stamped with the commit and host they were
# measured at. Method: scripts/tests/lib/compile_scaling.py.
#
#   check_compile_scaling.sh [--shape NAME]...     judge (all shapes by default)
#   check_compile_scaling.sh --measure [--shape NAME]...
#                                                  re-measure budgets on this host (CI host class)
# CONCRETE_SCALING_COMPILER=<binary> judges another build (a pre-fix compiler) with this tree's
# budgets; the freshness check applies only to this tree's own binary.
#
# HOSTS THAT CANNOT COUNT (bug 315). Where perf cannot count instructions:u, wall growth and wall
# budgets are NOT JUDGED (a wall ratio is the cache and the host's load, not the compiler's work):
# each such stage prints NOT-JUDGED, the summary counts them, and the gate exits 77 when nothing
# failed. That is not a pass: run_ci_gates_local.sh records it as deferred to CI only because
# scripts/tests/lib/host_tools.sh declares this gate judged on CI (judged_on: ci-counted) and CI runs
# it as an unconditional step. Heartbeats, compile results, the run ceiling and a stage over
# uncounted_fail_factor x its budget are still judged there. On CI an uncounted host FAILS: the
# deferred verdicts are made there. Exit: 0 passed, 1 failed, 2 setup, 77 not judged here.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
if [ -z "${CONCRETE_SCALING_COMPILER:-}" ]; then
  source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
  require_fresh_binary || exit 1
fi
COMPILER="${CONCRETE_SCALING_COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
command -v python3 >/dev/null || { echo "error: python3 required" >&2; exit 2; }
# CI MUST COUNT, end to end (bug 315): the real entry point, on CI with no perf on PATH, fails with
# the runner message before it judges a shape (so it needs no compiler). Without this leg a CI host
# that lost its counters would defer every verdict to itself.
case " $* " in *" --measure "*) MEASURING=1 ;; *) MEASURING=0 ;; esac
if [ "$MEASURING" = 0 ]; then
  NOPERF="$(mktemp -d)" || exit 2
  ci_out="$(env -i HOME="$NOPERF" PATH="$NOPERF" GITHUB_ACTIONS=true "$(command -v python3)" -I -u \
    "$ROOT_DIR/scripts/tests/lib/compile_scaling.py" --compiler "$NOPERF/no-compiler" \
    --budgets "$ROOT_DIR/scripts/tests/lib/compile_scaling_budgets.tsv" \
    --bounded "$ROOT_DIR/scripts/tests/lib/bounded.sh" --root "$ROOT_DIR" 2>&1)"; ci_rc=$?
  rm -rf "$NOPERF"
  if [ "$ci_rc" = 1 ] && has_match -F "FAIL CI host must count instructions:u" <<<"$ci_out" \
     && has_match -E '^check_compile_scaling: PASS=[0-9]+ FAIL=1 NOT-JUDGED=0 ' <<<"$ci_out"; then
    echo "  ok   CONTROL (end to end): on CI, a host that cannot count instructions:u fails the gate"
  else
    echo "  FAIL CONTROL (end to end): on CI without perf the gate exited $ci_rc without the refusal:"
    printf '%s\n' "$ci_out" | tail -5 | sed 's/^/    /'
    exit 1
  fi
fi
# Not `exec`: fresh.sh releases the tree lock from this shell's EXIT trap.
python3 -I -u "$ROOT_DIR/scripts/tests/lib/compile_scaling.py" \
  --compiler "$COMPILER" \
  --budgets "$ROOT_DIR/scripts/tests/lib/compile_scaling_budgets.tsv" \
  --bounded "$ROOT_DIR/scripts/tests/lib/bounded.sh" \
  --root "$ROOT_DIR" "$@"
exit $?
