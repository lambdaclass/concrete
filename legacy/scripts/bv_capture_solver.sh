#!/usr/bin/env bash
# SAT-solver shim for `set_option sat.solver`.
#
# Lean's `bv_decide` invokes its solver as
#     <solver> <cnf-in> <lrat-out> --lrat --binary=true --quiet --shrink=0 --unsat
# This shim copies the CNF aside and then delegates to the real cadical UNCHANGED,
# so `bv_decide` behaves exactly as it would without the shim. Its only purpose is
# to get hold of the bit-blasted CNF, which is otherwise written to a temp file that
# Lean deletes; there is no Lean option to retain it.
#
# Why: docs/verification/AXIOMS.md records that `bv_decide`'s LRAT certificate checker runs as
# COMPILED LEAN, so a "kernel-checked bitblasting" proof extends trust to
# Lean.trustCompiler. Capturing the CNF lets a SEPARATELY IMPLEMENTED checker
# (drat-trim) verify the same bit-blasting independently — see
# scripts/tests/check_bv_certificates.sh.
#
# CNF_CAPTURE_DIR must be set; if it is not, the shim still delegates correctly and
# simply captures nothing, so it can never break a build.
set -uo pipefail

if [ -n "${CNF_CAPTURE_DIR:-}" ] && [ -d "${CNF_CAPTURE_DIR}" ] && [ -f "${1:-}" ]; then
  cp "$1" "${CNF_CAPTURE_DIR}/cnf.$$.$RANDOM.dimacs" 2>/dev/null || true
fi

# Prefer the cadical Lean ships (the version bv_decide is tested against); fall back
# to PATH. Never substitute a different solver: this must not change bv_decide's
# behaviour, only observe its input.
REAL=""
if [ -n "${LEAN_SYSROOT:-}" ] && [ -x "${LEAN_SYSROOT}/bin/cadical" ]; then
  REAL="${LEAN_SYSROOT}/bin/cadical"
else
  LEANBIN="$(command -v lean 2>/dev/null || true)"
  if [ -n "$LEANBIN" ]; then
    CAND="$(dirname "$(readlink -f "$LEANBIN")")/cadical"
    [ -x "$CAND" ] && REAL="$CAND"
  fi
fi
[ -z "$REAL" ] && REAL="$(command -v cadical || true)"
if [ -z "$REAL" ]; then
  echo "bv_capture_solver: no cadical found" >&2
  exit 127
fi

exec "$REAL" "$@"
