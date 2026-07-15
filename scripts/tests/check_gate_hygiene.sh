#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Gate-hygiene gate (ROADMAP #34b enforcement).
#
# "Fail means fail" must be uniform across the whole gate corpus. Twice (2026-06
# and 2026-07) CI stayed green while a gate was silently masking failures — a
# `cmd | tail` swallowing a nonzero exit, or a fail-counter that never turned
# into a nonzero exit code. `scripts/tests/lib/gate.sh` fixed the ergonomics;
# this gate LOCKS IN the invariant so a new hand-rolled gate can't regress it.
#
# Every shell gate (check_*.sh / test_*.sh) must:
#   1. be pipe-safe   — source lib/gate.sh, OR set `pipefail` itself; and
#   2. propagate fail — contain at least one failure-exit construct
#                       (`exit N`, a trailing `[ "$FAIL" -eq 0 ]`, `gate_finish`,
#                       `|| exit`, `return 1`, …) so a detected failure becomes a
#                       nonzero process exit.
#
# It is a source-structure gate (no compiler build needed) and runs in the
# `grammar` CI job next to the workflow-YAML gate.

set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
GATES_DIR="$ROOT_DIR/scripts/tests"
cd "$GATES_DIR"

PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Gates that legitimately don't fit the shell-gate shape (delegate hygiene, or
# are helpers/libraries rather than standalone gates). Keep this list SHORT and
# justified — it is the audited escape hatch, not a dumping ground.
declare -A EXEMPT=(
  ["lib/gate.sh"]="shared harness, not a standalone gate"
)

echo "=== every shell gate is pipe-safe and propagates failure ==="
shopt -s nullglob
for f in check_*.sh test_*.sh; do
  [[ -n "${EXEMPT[$f]:-}" ]] && { echo "  skip $f (${EXEMPT[$f]})"; continue; }

  pipe_safe=false
  grep -q "lib/gate.sh" "$f" && pipe_safe=true
  grep -qE "set -[a-zA-Z]*o?[[:space:]]+pipefail|pipefail" "$f" && pipe_safe=true

  propagates=false
  # sourcing gate.sh brings gate_finish; otherwise require an explicit construct
  if grep -q "lib/gate.sh" "$f"; then
    propagates=true
  elif grep -qE "exit [1-9]|gate_finish|return 1|-eq 0[[:space:]]*\]|-gt 0|-ne 0|\|\|[[:space:]]*exit" "$f"; then
    propagates=true
  fi

  if $pipe_safe && $propagates; then
    ok "$f"
  elif ! $pipe_safe; then
    no "$f — not pipe-safe (add 'set -euo pipefail' or source lib/gate.sh)"
  else
    no "$f — no failure-exit construct (a detected failure won't set a nonzero exit code)"
  fi
done

echo ""
echo "GATE-HYGIENE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
