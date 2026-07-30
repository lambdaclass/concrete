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
echo "=== no 'lake'/'lean' piped into a short-circuiting grep whose status is used ==="
# Same SIGPIPE family as the check above, but it INVERTS a gate leg instead of
# aborting the script. `lake env lean f.lean 2>&1 | grep -q PAT && ok || no`:
# grep exits at the first match, the elaborator is still writing, it dies of
# SIGPIPE, and under `pipefail` the pipeline's status is that death — so the leg
# reports FAIL against output that plainly contained PAT. Measured: a
# same-name-generator leg failed this way while the pattern was present.
#
# Scoped to lake/lean producers deliberately. `grep -q` after a fast, small
# producer finishes writing before grep exits and is fine; there are ~170 such
# uses in this directory and banning them all would be noise. An elaborator is
# slow and verbose, so it reliably loses the race.
#
# Fix: capture once into a variable, then grep the variable with a here-string.
# That is faster too — these gates elaborated the same file three times.
pipehazard=0
for f in "$ROOT_DIR"/scripts/tests/*.sh; do
  head -20 "$f" | grep -q "pipefail" || continue
  # Scan CODE only. Full-line comments are stripped first: prose describing this
  # very hazard matched it three times, and the same class (a docstring matching
  # a structural scan) has cost this tree several false failures already. Keeping
  # the line numbers means grep -n then filtering, not stripping in place.
  #
  # The producer must be an elaborator INVOCATION. A bare `lean ` alternative
  # also matched `Report.lean | grep -q…`, i.e. a filename followed by a pipe —
  # so the toolchain word must be anchored as a command.
  hits="$(grep -nE '^[0-9]+: *#' -v <<<"$(grep -n "" "$f")" \
    | grep -E "(lake +env +lean|lake +build|(^|[;(|&] *)lean +)[^|]*\| *grep -[a-zA-Z]*[q]" || true)"
  if [ -n "$hits" ]; then
    no "$(basename "$f") — elaborator piped into 'grep -q' under pipefail: line $(head -1 <<<"$hits" | cut -d: -f1). Capture to a variable first."
    pipehazard=1
  fi
done
[ "$pipehazard" -eq 0 ] && ok "no gate pipes an elaborator into a short-circuiting grep"

echo ""
echo "=== no 'local' statement reads a variable it assigns in the same statement ==="
# A builtin's arguments are word-expanded BEFORE the builtin runs, so in
#     local name="$1" f="$TMP/$name.con"
# `$name` is NOT the local being declared on the same line — it resolves against
# the enclosing scope. Two ways that goes wrong, and both happened at once in
# check_trap_inventory.sh:
#   * on CI, where the outer name is unset, `set -u` aborts the whole gate, so
#     it never ran at all;
#   * in the nix devshell, which EXPORTS `name=nix-shell-env`, it silently
#     expanded to that instead — the gate reported PASS=12 FAIL=0 while writing
#     every fixture to one wrong path.
# The second is the dangerous one: green for the wrong reason. The sibling gates
# spell the same line with `$1`, which is always set, which is why this read as
# idiomatic rather than as a hazard.
selfref=0
for f in "$ROOT_DIR"/scripts/tests/*.sh; do
  while IFS=: read -r lineno line; do
    [ -z "${lineno:-}" ] && continue
    body="${line#*local }"
    assigned=""
    bad=""
    # Walk the assignments left to right. A value referencing an ALREADY-listed
    # name on this same line is the hazard.
    for word in $body; do
      case "$word" in
        *=*)
          nm="${word%%=*}"
          val="${word#*=}"
          for prior in $assigned; do
            case "$val" in
              *"\$$prior"*|*"\${$prior}"*|*"\${$prior:"*) bad="$prior" ;;
            esac
          done
          case "$nm" in [A-Za-z_]*) assigned="$assigned $nm" ;; esac
          ;;
      esac
    done
    if [ -n "$bad" ]; then
      no "$(basename "$f"):$lineno — 'local' reads \$$bad, which it assigns in the same statement; split the declaration"
      selfref=1
    fi
  done < <(grep -nE '^[[:space:]]*local[[:space:]]+[A-Za-z_][A-Za-z0-9_]*=' "$f" || true)
done
[ "$selfref" -eq 0 ] && ok "no 'local' statement depends on its own earlier assignment"
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
