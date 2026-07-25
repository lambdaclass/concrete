#!/usr/bin/env bash
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
echo "=== no '| head -N' inside an errexit+pipefail script (SIGPIPE self-abort) ==="
# `cmd | head -N` closes the pipe once N lines arrive. A producer still writing
# then dies of SIGPIPE, and in a script with BOTH `set -e` and `pipefail` that
# failure becomes the script's exit status — so the gate fails for a reason
# unrelated to what it tests, and only when the producer loses the race against
# head's exit. That is exactly how the trust gate died on the R-0001 push: the
# bug-corpus audit capped 55 entries at thirty lines this way, green on one
# commit and exit-2 on the next. `awk "NR<=N"` reads all input and caps
# identically. (This comment avoids spelling the pattern, which the scan below
# would otherwise match in its own source.)
#
# Scoped to errexit scripts because that is where the SIGPIPE is fatal; gates
# using `set -uo pipefail` (no -e) absorb it. If a script here gains `set -e`,
# this check starts covering it.
hazard=0
for f in "$ROOT_DIR"/scripts/tests/*.sh; do
  head -20 "$f" | grep -qE "set -[a-z]*e[a-z]* .*pipefail|set -o errexit" || continue
  # `[h]ead` so this scan does not match its own pattern literal.
  hits="$(grep -nE "\| *[h]ead -[0-9]" "$f" || true)"
  if [ -n "$hits" ]; then
    no "$(basename "$f") — '| head -N' under errexit+pipefail: $(head -1 <<<"$hits" | cut -d: -f1). Use '| awk \"NR<=N\"'"
    hazard=1
  fi
done
[ "$hazard" -eq 0 ] && ok "no errexit gate script caps output with '| head -N'"

echo ""
echo "=== the pre-push hook is installed in this clone ==="
# Advisory, not a failure: core.hooksPath is per-clone local config and cannot be
# versioned, so a gate cannot assert it for anyone else. It CAN tell the person
# running gates right now that their next push is unguarded — which is the moment
# the information is useful. The ritual lived only in a Makefile comment until
# 2026-07-25, and a comment did not stop two red pushes.
hp="$(git -C "$ROOT_DIR" config core.hooksPath 2>/dev/null || true)"
if [ "$hp" = ".githooks" ]; then
  ok "core.hooksPath=.githooks — pre-push runs the CI gate set"
else
  echo "  warn pre-push hook NOT installed in this clone — run 'make setup-hooks'"
  echo "       (advisory: local config, cannot be enforced from a versioned gate)"
fi

echo ""
echo "GATE-HYGIENE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
