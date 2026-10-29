#!/usr/bin/env bash
# THE DIFF-TO-GATE MAPPING, TESTED AGAINST A DIFF WHOSE ANSWER IS ALREADY KNOWN.
#
# `gates_for_diff.sh` selects the gates a change can break. A mapping nothing tests is a
# mapping that quietly stops mapping — the same failure as an inert mutation anchor, and
# worse here, because the thing that stops working is the thing you consult INSTEAD of
# running everything.
#
# The fixture is the R-0483 stdlib repair, chosen because its answer was established the
# expensive way: four CI rounds, each surfacing one more derived artifact of one change.
# The stdlib surface manifest went stale; excluding higher-order functions collided with
# shadow-body extraction; renaming symbols left six mutation anchors INERT; and the
# ByteView gate still asserted the design the repair removed. Every one of those is a
# gate the mapping must name for that diff, and the list is not a guess.
#
# A selector that returned every gate would pass all of that and be useless, so precision
# is asserted too: the answer must be a small fraction of the suite, and an unrelated
# change must not drag in the stdlib surface.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

SEL="scripts/tests/lib/gates_for_diff.py"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -f "$SEL" ] || { echo "FATAL: $SEL missing" >&2; exit 2; }

TOTAL_GATES=$(ls scripts/tests/check_*.sh scripts/tests/run_*.sh 2>/dev/null | wc -l)

# The R-0483 stdlib repair, as a file list. Written out rather than read from a commit so
# the fixture cannot silently change meaning if history is rewritten or the commit is
# rebased away.
R0483_FILES=$(cat <<'EOF'
std/src/numeric.con
std/src/text.con
std/src/bytes.con
examples/packet/src/main.con
examples/byte_view/tlv_packet_view/src/main.con
examples/byte_view/utf8_text_slice/src/main.con
Concrete/Proof/GeneratedAttestations.lean
Concrete/Proof/Proof.lean
proofs/Examples/HmacSha256/Proofs.lean
docs/stdlib/BYTE_VIEW.md
EOF
)

sel_out="$(printf '%s\n' "$R0483_FILES" | python3 "$SEL" 2>&1)" || {
  echo "FATAL: selector failed" >&2; printf '%s\n' "$sel_out" >&2; exit 2; }
sel_names="$(printf '%s\n' "$sel_out" | cut -f1)"
nsel=$(printf '%s\n' "$sel_names" | sed '/^$/d' | wc -l)

echo "=== the gates R-0483 actually broke are all selected ==="
# Each of these went red in CI, one round at a time, for that change.
for g in check_stdlib_manifest.sh check_shadow_body_v2.sh check_mutation_anchors.sh \
         check_byte_view.sh check_std_compiled_coverage.sh; do
  if has_match -x "$g" <<<"$sel_names"; then
    ok "$g"
  else
    no "$g is NOT selected — it broke on this diff and the mapping would have missed it"
  fi
done

echo "=== and the derived-artifact set, which is why they broke ==="
for g in check_attestation_manifest.sh check_classification_freshness.sh \
         check_build_identity_freshness.sh; do
  has_match -x "$g" <<<"$sel_names" \
    && ok "$g" \
    || no "$g is not selected for a diff touching std/ and Concrete/"
done

echo "=== a compiler change selects the corpus runner, in both its CI forms ==="
# This gap was real: an R-0484 edit to `Concrete/Report/Report.lean` broke the trust
# gate's self-consistency section, and the mapping missed it because `run_tests.sh`
# NAMES no compiler source — it compiles the whole corpus. A script is also not a gate:
# CI runs the plain form and `--trust-gate`, and only the latter checks consistency.
compiler_sel="$(printf 'Concrete/Report/Report.lean\n' | python3 "$SEL" 2>/dev/null | cut -f1)"
if has_match -x "run_tests.sh" <<<"$compiler_sel"; then
  ok "run_tests.sh is selected for a compiler change"
else
  no "a compiler change does not select the corpus runner"
fi
if has_match -x "run_tests.sh --trust-gate" <<<"$compiler_sel"; then
  ok "its --trust-gate form is named too (different sections, different failures)"
else
  no "the flagged CI form is not surfaced; a script is being treated as one gate"
fi

echo "=== the answer is a selection, not the whole suite ==="
# Without this every assertion above would also pass on `return every gate`.
if [ "$nsel" -gt 0 ] && [ "$nsel" -lt $((TOTAL_GATES / 3)) ]; then
  ok "selected $nsel of $TOTAL_GATES gates (under a third)"
else
  no "selected $nsel of $TOTAL_GATES — too many to be a selection, or none at all"
fi

echo "=== an unrelated change does not drag in the stdlib surface ==="
unrelated="$(printf 'docs/project/STYLE.md\n' | python3 "$SEL" 2>/dev/null | cut -f1)"
if has_match -x "check_stdlib_manifest.sh" <<<"$unrelated"; then
  no "editing a style document selected the stdlib surface manifest"
else
  ok "a docs-only change does not select the stdlib surface manifest"
fi

echo "=== CONTROL: the selector can answer NO ==="
# A selector that never excludes anything would satisfy every check above.
empty="$(printf 'README.md\n' | python3 "$SEL" 2>/dev/null | cut -f1 | sed '/^$/d' | wc -l)"
if [ "$empty" -lt "$nsel" ]; then
  ok "an unrelated file selects fewer gates ($empty) than the stdlib repair ($nsel)"
else
  no "an unrelated file selected $empty gates — the selector does not discriminate"
fi

echo
echo "GATES-FOR-DIFF-MAP: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
