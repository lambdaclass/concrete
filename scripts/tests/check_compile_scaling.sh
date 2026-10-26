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
# re-measured within the recorded retry budget (each stage keeps its fastest run), so host
# noise cannot fail the gate, and a retry cannot hide a quadratic stage: growth is resolved above
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
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
if [ -z "${CONCRETE_SCALING_COMPILER:-}" ]; then
  source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
  require_fresh_binary || exit 1
fi
COMPILER="${CONCRETE_SCALING_COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
command -v python3 >/dev/null || { echo "error: python3 required" >&2; exit 2; }
# Not `exec`: fresh.sh releases the tree lock from this shell's EXIT trap.
python3 -I -u "$ROOT_DIR/scripts/tests/lib/compile_scaling.py" \
  --compiler "$COMPILER" \
  --budgets "$ROOT_DIR/scripts/tests/lib/compile_scaling_budgets.tsv" \
  --bounded "$ROOT_DIR/scripts/tests/lib/bounded.sh" \
  --root "$ROOT_DIR" "$@"
exit $?
