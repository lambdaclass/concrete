#!/usr/bin/env bash
# "I COULD NOT DETERMINE THIS" MUST NOT BE STORED AS "THERE IS NOTHING TO DETERMINE".
#
# The rule and its evidence are in docs/project/ABSENCE_IS_NOT_A_FACT.md. Seven instances
# were found across unrelated subsystems written at different times, six of them in one
# session, which is why this is gated rather than left as review discipline: the pattern
# does not correlate with author or subsystem, it correlates with a total function being
# asked a question it cannot answer and having a value available that reads as an answer.
#
# WHAT THIS GATE CAN AND CANNOT DO. It cannot decide in general whether a `none` branch
# means "genuinely absent" (legitimate — `diagsMatch` with no substring matches
# everything) or "could not determine" (the defect). A grep for `| none =>` returns 63
# hits here and nearly all are correct, and a gate that is mostly noise gets ignored,
# which would make it another thing that looks like a check and is not.
#
# So it pins the HIGH-RISK sites specifically: the two capability-inference fallbacks that
# were the defect, and the trusted-extern site that is its root. Each assertion names what
# would have to be true for it to fail.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

DOC="docs/project/ABSENCE_IS_NOT_A_FACT.md"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== the rule is written down where it can be cited ==="
if [ -f "$DOC" ]; then
  ok "$DOC exists"
else
  no "$DOC is missing — the gate pins sites whose reasoning lives nowhere"
fi

echo "=== capability inference contributes NOTHING when it does not know ==="
# Both sites. The first fix took only the call path; the method path is the one real
# callers reach, because std's cap-polymorphic combinators are methods.
for f in Concrete/Check/Check.lean Concrete/Check/CheckHelpers.lean; do
  # The defect is a fallback that YIELDS an empty capability set for an argument whose
  # type is unknown. The repaired form yields `none` and lets `resolveCaps` name the
  # variable, so the giveaway is `argCapSet?` plus no `pure CapSet.empty` beside it.
  if grep -q "argCapSet?" "$f"; then
    if grep -A14 "argCapSet? ← do" "$f" | has_match "pure CapSet.empty"; then
      no "$f: an inference fallback still yields CapSet.empty for an unknown argument"
    else
      ok "$f: unknown argument capability contributes no binding"
    fi
  else
    no "$f: the optional-capset form is gone — a fallback may be fabricating again"
  fi
done

echo "=== and the refusal is the capability leg, not type equality ==="
# Asserted by check_cap_inference.sh on real programs; named here so the connection is
# visible from this gate, which is where someone reading the rule will look.
if [ -x scripts/tests/check_cap_inference.sh ]; then
  ok "check_cap_inference.sh carries the behavioural assertions (E0242, not E0220)"
else
  no "check_cap_inference.sh is missing — nothing checks the behaviour these sites produce"
fi

echo "=== the root site is annotated with what it waits on ==="
# `trusted extern` typed CapSet.empty is CORRECT for required authority and is the point
# where performed effects stop being tracked. It must not be silently "fixed" by putting
# a capability there — that would break authority-at-acquisition without making the
# effect visible — so the annotation is what keeps the next reader from trying.
if grep -q "R-0484 ROOT SITE" Concrete/Resolve/FileSummary.lean; then
  ok "the trusted-extern capset carries its R-0484 annotation"
else
  no "the trusted-extern site lost its annotation — the next reader will mistake it for a bug or for settled"
fi

echo "=== the inventory names files that exist ==="
# A row pointing at a file that has moved is an inventory quietly describing nothing,
# which is the defect this document is about, committed in the document about it.
missing=0
for f in Concrete/Check/Check.lean Concrete/Check/CheckHelpers.lean \
         Concrete/Resolve/FileSummary.lean Concrete/Proof/ProofCore.lean \
         scripts/tests/run_ci_gates_local.sh scripts/tests/check_mutation_anchors.sh; do
  if ! grep -q "$(basename "$f")" "$DOC" 2>/dev/null; then continue; fi
  [ -f "$f" ] || { no "inventory names $f, which does not exist"; missing=1; }
done
[ "$missing" -eq 0 ] && ok "every file the inventory names is present"

echo
echo "ABSENCE-NOT-FACT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
