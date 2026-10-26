#!/usr/bin/env bash
# MUTATION ANCHORS — every mutation's target text still exists in its file.
#
# The mutation harness is what establishes that the other gates are load-bearing, so an
# inert mutation costs more than an ordinary test: it removes the evidence that some gate
# does any work, and removes it silently.
#
# A mutation whose MUT_OLD no longer appears in MUT_FILE cannot be applied. The harness
# prints "SKIPPED (pattern not found in file)" and, until 2026-08-06, still exited 0. Two
# things made that invisible for a long time:
#
#   1. the harness rebuilds the compiler twice per mutation, so it is expensive and is NOT
#      run in CI — nothing was checking;
#   2. anchors rot from ordinary refactors. Measured 2026-08-06: 4 of 77 were stale. Three
#      broke when the V2 producer cutover changed `return .call ...` into
#      `return ElaboratedExprV2.mk (CExpr.call ...)`; a fourth broke when R-0455 turned a
#      literal Rocq proof template into an interpolated one.
#
# This gate is the cheap half of that: pure string presence, no builds, no mutations, runs
# in seconds on every push. It cannot tell you a mutation is KILLED — only that it is still
# capable of being applied at all. That is the property that was silently lost.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_mutation_anchors stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR

HARNESS="scripts/tests/test_mutation.sh"
[ -f "$HARNESS" ] || { echo "FATAL: $HARNESS not found" >&2; exit 1; }

# `|| rc=$?` because the ERR trap would otherwise fire on the python exit and kill the
# gate before it can print its own verdict — the failure mode this gate exists to notice,
# reproduced in the gate itself on its first run.
# THE AUTHORITATIVE CHECKER IS INVOKED, NOT REIMPLEMENTED.
#
# This gate used to re-parse the harness's bash source with a python regex and unescape it by hand.
# The harness already ships `--check-patterns`, which iterates the REAL arrays it applies and requires
# each anchor to match EXACTLY ONCE — and its own comment records that a separate re-parsing check
# "mis-handled multi-line MUT_OLD entries and reported 17 false stalenesses". So there were two
# implementations of "does this anchor identify a site", the weaker one was the gate, and on
# 2026-08-21 strengthening the weak copy merely duplicated a check the authority already performed
# correctly. A gate must not be a second opinion about the thing it is checking.
rc=0
DELEG_OUT="$(mktemp)"
bash "$HARNESS" --check-patterns > "$DELEG_OUT" 2>&1 || rc=$?
# A ZERO EXIT IS NOT A VERDICT: a harness that exits early, or whose mode is renamed, would exit 0
# having asserted nothing. The verdict line must be present and name a credible count.
DELEG_VERDICT="$(grep -oE 'all [0-9]+ mutation patterns match their target exactly once' "$DELEG_OUT" | head -1)"
DELEG_COUNT="$(printf '%s' "$DELEG_VERDICT" | grep -oE '[0-9]+' | head -1)"
if [ "$rc" -eq 0 ]; then
  if [ -z "$DELEG_VERDICT" ] || [ -z "$DELEG_COUNT" ] || [ "$DELEG_COUNT" -lt 50 ]; then
    echo "  FAIL $HARNESS --check-patterns exited 0 without a credible verdict"
    echo "       (found: '${DELEG_VERDICT:-<nothing>}') — a delegated check that reports nothing"
    echo "       must not be read as a check that passed."
    rc=1
  else
    echo "  ok   $DELEG_VERDICT ($HARNESS)"
  fi
else
  echo "  FAIL $HARNESS reports inert or ambiguous mutation anchors:"
  grep -E '^  STALE|^FAIL' "$DELEG_OUT" | sed 's/^/    /' | head -12
fi
rm -f "$DELEG_OUT"

# The per-gate coverage floors below are a DIFFERENT fact — how many mutations name each gate — and
# they are declarations in the harness source, so parsing is the only way to read them.
rc2=0
python3 - "$HARNESS" <<'PY' || rc2=$?
import re, sys
harness = sys.argv[1]
src = open(harness).read()
fail = 0
# MUTATION COVERAGE PER GATE. "Every leg is mutation-verified" is a discipline nobody can
# enforce by reading, and I applied it inconsistently across this arc — several legs turned
# out to pass for the wrong reason, and three gates could not fail at all. This is the
# mechanical approximation: the gates that carry the evidence claims must each be the
# target of a minimum number of mutations. It cannot prove a specific leg is covered; it
# does stop a gate accumulating legs while its mutation count stays flat.
FLOORS = {
    'scripts/tests/check_shadow_body_v2.sh': 12,
    'scripts/tests/check_binder_refs.sh': 3,
}
gates = re.findall(r'gate_for_last "([^"]+)"', src)
from collections import Counter
counts = Counter(gates)
for gate, floor in sorted(FLOORS.items()):
    got = counts.get(gate, 0)
    if got < floor:
        print(f"  FAIL {gate} is the target of only {got} mutation(s), floor {floor}")
        print("       A gate accumulating legs without mutations is accumulating assertions")
        print("       nobody has shown can fail. Add mutations, or lower the floor and say why.")
        fail += 1
    else:
        print(f"  ok   {gate}: {got} mutations (floor {floor})")

if fail:
    sys.exit(1)
PY
# The delegated anchor verdict and the per-gate coverage floors are separate results, and both
# must hold. Keeping them separate matters: the floors are about how many mutations NAME a gate,
# which is a declaration; the anchor verdict is about whether those mutations can be APPLIED.
[ "$rc2" -eq 0 ] || rc=1

# BOTH MUTATION CORPORA, and it was one. This gate checked `test_mutation.sh`'s 77 anchors and
# nothing else, while `check_gate_mutation_coverage.sh` carries 78 more with no integrity check at
# all. NINE of those had gone inert — four guarding check_attestation_manifest, two
# check_atomic_flip_entrance, two check_dependency_edges, one check_proof_freshness — and the only
# thing that would have reported it was a multi-hour campaign nobody runs on a change.
#
# Worse, a verified claim about one corpus was reported as a claim about both, in the R-0004 closure
# record. Checking one population and describing two is the failure this pair of checks now prevents.
#
# Delegated to that harness's own ANCHORS_ONLY mode rather than re-parsed here: it already holds the
# arrays, and a second parser for the `add` format would drift from the harness it describes.
COV_RC=0
if [ -x scripts/tests/check_gate_mutation_coverage.sh ] || [ -f scripts/tests/check_gate_mutation_coverage.sh ]; then
  # mktemp, not a predictable /tmp path a same-user process could preplant a symlink at —
  # the same reasoning already applied to the other delegated output below.
  COV_OUT="$(mktemp)" || { echo "  FAIL could not create a temp file for the delegated check"; exit 1; }
  if ANCHORS_ONLY=1 bash scripts/tests/check_gate_mutation_coverage.sh > "$COV_OUT" 2>&1; then
    # A ZERO EXIT IS NOT A VERDICT. This printed `ok` on any successful exit and quoted whatever the
    # grep found — which is the empty string when the child produced no verdict at all. A child
    # replaced by `exit 0`, or one exiting early before its loop, therefore yielded `ok  ` and PASS=2:
    # the delegation reported that 81 anchors were checked when nothing had been. The child's own
    # vacuity floor cannot help, because the failure mode is the child's logic DISAPPEARING.
    COV_VERDICT="$(grep -oE 'all [0-9]+ anchors still match their targets' "$COV_OUT" | head -1)"
    COV_COUNT="$(printf '%s' "$COV_VERDICT" | grep -oE '[0-9]+' | head -1)"
    if [ -z "$COV_VERDICT" ] || [ -z "$COV_COUNT" ] || [ "$COV_COUNT" -lt 50 ]; then
      COV_RC=1
      echo "  FAIL check_gate_mutation_coverage exited 0 but produced no credible anchor verdict"
      echo "       (found: '${COV_VERDICT:-<nothing>}') — a delegated check that reports nothing"
      echo "       must not be read as a check that passed."
    else
      echo "  ok   $COV_VERDICT (check_gate_mutation_coverage)"
    fi
  else
    COV_RC=1
    echo "  FAIL check_gate_mutation_coverage has inert mutation families:"
    grep -E '^  FAIL' "$COV_OUT" | sed 's/^/    /' | head -12
  fi
  rm -f "$COV_OUT"
else
  COV_RC=1
  echo "  FAIL scripts/tests/check_gate_mutation_coverage.sh is missing — its 78 families are unchecked"
fi

if [ "$rc" -eq 0 ] && [ "$COV_RC" -eq 0 ]; then
  echo "MUTATION-ANCHORS: PASS=2 FAIL=0 (both corpora)"
else
  echo "MUTATION-ANCHORS: PASS=$(( (rc == 0 ? 1 : 0) + (COV_RC == 0 ? 1 : 0) )) FAIL=$(( (rc != 0 ? 1 : 0) + (COV_RC != 0 ? 1 : 0) ))"
  exit 1
fi
