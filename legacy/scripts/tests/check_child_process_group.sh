#!/usr/bin/env bash
# PROCESS IDENTITY BY CONSTRUCTION, ATTACKED DIRECTLY.
#
# The campaign supervisor may publish only when nothing from the run is still executing: evidence
# written after the census would be described by an artifact that never saw it. Four attempts at
# that check shipped broken, and every one of them was found by RUNNING a campaign, not by reading:
#
#   1. `pgrep -P $$` sees one generation, and the campaign child is already reaped by then, so a
#      surviving grandchild is reparented away and never appears.
#   2. matching command lines for the repository path also matched the LAUNCHER of the run.
#   3. matching them for the workspace path matched the supervisor itself, which runs
#      `bash /tmp/concrete-mut.<pid>.../driver.sh`, and any shell whose command line merely
#      CONTAINED the search string.
#   4. /proc ancestry is correct on Linux and ABSENT on macOS — and this repository has already
#      taken a macOS outage from depending on /proc.
#
# Attempts 2, 3 and 4 refused clean runs. A check that always fires carries no more information than
# one that never does; both are indistinguishable from an absent check to anything reading results.
#
# The rule is now structural: the child is launched into its own session, so its process-group id is
# its pid, the supervisor and every ancestor are outside that group, and liveness is whatever the
# kernel says about that group. No patterns, no exclusion lists, nothing depending on how a worker
# spells its command line. Designed using portable POSIX mechanisms; whether Linux and macOS agree
# is established by running this gate on both in CI, not by claiming it here.
set -uEo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
LAUNCH="$ROOT_DIR/scripts/tests/lib/run_campaign_child.py"
# The decoder under test is the supervisor's, loaded from where the supervisor loads it.
. "$ROOT_DIR/scripts/tests/lib/campaign_supervise.sh" \
  || { echo "error: cannot load campaign_supervise.sh; there would be no decoder to test" >&2; exit 2; }
[ -f "$LAUNCH" ] || { echo "error: $LAUNCH missing" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# run_case <label> <expected-rc> <expected-group-empty> <command...>
run_case() {
  local label="$1" want_rc="$2" want_empty="$3"; shift 3
  local rep="$TMP/report.$$.$RANDOM"
  # THE PRODUCER AND THE DECODER ARE TESTED TOGETHER.
  #
  # These cases discarded the launcher's own exit status and then read two fields with sed, so the
  # real report was never put through the real decoder. The synthetic cases below hard-code launcher
  # status 0, which means a launcher that writes plausible fields and exits nonzero — or emits an
  # undeclared key — kept this gate green while the actual supervisor refused every run. Each real
  # launch is now decoded by the production consumer, with the launcher's true status.
  python3 "$LAUNCH" --report "$rep" --run-id "GATE-$$" -- "$@" >/dev/null 2>&1
  local _lrc=$?
  local _bad; _bad="$(decode_launch_report "$rep" "GATE-$$" "$_lrc")"
  if [ -n "$_bad" ]; then
    no "$label — the launcher's own report does not decode:$_bad"
    rm -f "$rep"; return
  fi
  local rc empty
  rc="$(sed -n 's/^child_rc=//p' "$rep" | head -1)"
  local state; state="$(sed -n 's/^process_group_state=//p' "$rep" | head -1)"
  case "$state" in empty) empty=1 ;; nonempty) empty=0 ;; *) empty="state:$state" ;; esac
  if [ "$rc" = "$want_rc" ] && [ "$empty" = "$want_empty" ]; then
    ok "$label"
  else
    no "$label — expected rc=$want_rc group_empty=$want_empty, got rc=${rc:-<none>} group_empty=${empty:-<none>}"
  fi
  rm -f "$rep"
}

echo "=== a clean child leaves an empty group, and its status is preserved ==="
run_case "a child exiting 0 leaves no group members"      0  1 sh -c 'exit 0'
run_case "a nonzero exit is reported, group still empty"   7  1 sh -c 'exit 7'

echo "=== a survivor keeps the group alive, whatever it is called ==="
# THE CHILD EXITS FIRST while its own child keeps running: this is the case `pgrep -P $$` could never
# see, because by the time the supervisor looks, the campaign child has been reaped and the survivor
# has been reparented away from it.
run_case "a grandchild outliving the child is detected"    0  0 sh -c 'sleep 5 & exit 0'
# NAME-INDEPENDENCE, asserted directly rather than by trying to rewrite argv: the check never
# consults a name, so a survivor with an entirely unrelated one is still a member of the group.
run_case "a survivor with an unrelated name is detected"   0  0 sh -c 'exec -a totally-unrelated sleep 5 & exit 0'

echo "=== processes outside the group are not the supervisor's concern ==="
# A decoy that looks exactly like campaign work but was never in this group. Under the command-line
# matching this replaced, this is precisely what produced false refusals.
sh -c 'exec -a concrete-mut.99999.decoy sleep 4' >/dev/null 2>&1 &
_decoy=$!
sleep 0.3
run_case "an unrelated similarly-named process is ignored" 0  1 sh -c 'exit 0'
kill "$_decoy" 2>/dev/null || true
wait "$_decoy" 2>/dev/null || true

echo "=== the launcher never reports on itself or its caller ==="
# If the launcher counted its own process or this gate, every case above would have said non-empty.
# That it does not is the property the previous three attempts lacked.
run_case "the launcher and this gate are outside the group" 0 1 sh -c 'true'

echo "=== the rule is the kernel's, so it is the same rule everywhere ==="
if python3 - <<'PY'
import os, sys
# killpg on a group that cannot exist must raise ProcessLookupError on any POSIX system; if this
# platform reported something else, the emptiness answer would not mean what the supervisor thinks.
try:
    os.killpg(0x7FFFFFFF, 0)
except ProcessLookupError:
    sys.exit(0)
except Exception:
    sys.exit(1)
sys.exit(1)
PY
then ok "an absent process group reports absent (POSIX semantics available here)"
else no "this platform does not report an absent process group as absent"
fi

echo "=== a signalled child is reported unambiguously ==="
# Popen.wait() returns -N for signal N. A shell comparing that reads a negative, or coerces it: the
# status must say plainly that the child was killed, and child_rc must still look like a failure to
# a consumer that reads nothing else.
_rep="$TMP/sig"
python3 "$LAUNCH" --report "$_rep" --run-id "GATE-$$" -- sh -c 'kill -TERM $$' >/dev/null 2>&1
_rc="$(sed -n 's/^child_rc=//p' "$_rep")"; _sg="$(sed -n 's/^child_signalled=//p' "$_rep")"
_sn="$(sed -n 's/^child_signal=//p' "$_rep")"
{ [ "$_rc" = "143" ] && [ "$_sg" = "1" ] && [ "$_sn" = "15" ]; } \
  && ok "a SIGTERMed child reports child_rc=143 child_signalled=1 child_signal=15" \
  || no "signalled child misreported (rc=$_rc signalled=$_sg signal=$_sn)"

echo "=== the report is a strict channel ==="
# Each of these is what a partial write, a double write, or a stray key looks like. The supervisor
# decodes strictly, so every one must be distinguishable from a clean report rather than read as
# whichever line came first.
# THE PRODUCTION DECODER IS THE ONE UNDER TEST.
#
# This gate used to carry its own copy of the decoding rules, which is the failure it exists to
# prevent: the copy was LOOSER than the supervisor's — its `^[a-z_]*=` scan cannot match `PGID=0` or
# a line with no `=` at all, so an injected uppercase or malformed line was not reported as an
# unknown key, it was invisible — and a gate that is weaker than the consumer certifies a strictness
# the consumer does not have. Every case below now runs the function that actually gates publication.
_mk() { printf '%s\n' "$@" > "$TMP/probe"; }
_decodes_clean() { [ -z "$(decode_launch_report "$TMP/probe" "X" 0)" ]; }
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1'
_decodes_clean && ok "a complete report decodes cleanly (positive control)" || no "a complete report was rejected"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'pgid=1'
_decodes_clean && no "a report missing process_group_state was accepted" || ok "a MISSING field is refused"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_rc=1' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1'
_decodes_clean && no "a contradictory duplicate was accepted" || ok "a CONTRADICTORY duplicate is refused"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1'
_decodes_clean && no "an identical duplicate was accepted" || ok "an IDENTICAL duplicate is refused"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1' 'extra=1'
_decodes_clean && no "an unknown key was accepted" || ok "an UNKNOWN key is refused"
# The three the old gate-local decoder could not see at all.
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1' 'PGID=0'
_decodes_clean && no "an UPPERCASE key was accepted" || ok "an uppercase key is refused, not skipped"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1' 'garbage'
_decodes_clean && no "a line with no assignment was accepted" || ok "a malformed line is refused, not skipped"
# A signalled child exits 128+N; a report saying otherwise is contradicting itself, and a consumer
# reading child_rc alone would conclude the campaign exited cleanly.
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=1' 'child_signal=9' 'process_group_state=empty' 'pgid=1'
_decodes_clean && no "child_rc=0 beside child_signal=9 was accepted" || ok "a status that disagrees with the signal is refused"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=137' 'child_signalled=1' 'child_signal=9' 'process_group_state=empty' 'pgid=1'
_decodes_clean && ok "...and the consistent 128+9=137 form is accepted" || no "a correctly signalled report was rejected"
# An unrecognised state must never reach the comparison that only lets `empty` publish.
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=probably_fine' 'pgid=1'
_decodes_clean && no "an unrecognised process_group_state was accepted" || ok "an unknown group state is refused"
# THE LAUNCHER'S OWN EXIT STATUS IS PART OF THE ANSWER. A launcher that wrote a perfect report and
# then died was previously read as a clean run.
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1'
[ -z "$(decode_launch_report "$TMP/probe" "X" 0)" ] \
  && ok "a clean report with launcher rc=0 decodes (positive control for the status check)" \
  || no "the positive control for launcher status failed"
case "$(decode_launch_report "$TMP/probe" "X" 3)" in
  *launcher_exit\(3\)*) ok "the same bytes are refused when the launcher itself exited 3" ;;
  *) no "a nonzero launcher status was ignored" ;;
esac
case "$(decode_launch_report "$TMP/probe" "OTHER-RUN" 0)" in
  *stale_run_id*) ok "a report from another run is refused" ;;
  *) no "a stale report answered for this run" ;;
esac
# AN UNSUPPORTED CONTRACT, ATTACKED RATHER THAN OBSERVED. Confirming that the producer currently
# writes version 1 shows the two agree today; it does not show the consumer would refuse a version
# it cannot interpret. The refusal is what protects a future reader, so the refusal is what is tested.
for _v in 2 0 '' 1.0 v1; do
  _mk "protocol_version=$_v" 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1'
  case "$(decode_launch_report "$TMP/probe" "X" 0)" in
    *unsupported_protocol*) ok "protocol_version='$_v' is refused as uninterpretable" ;;
    *) no "protocol_version='$_v' was accepted as if it were version 1" ;;
  esac
done

echo "=== the stated guarantee is process GROUP, not descendants ==="
# MEASURED, NOT ASSUMED, and recorded here so the limitation cannot quietly become a claim: a
# descendant that starts its own session leaves the original group empty while it is still running.
# That escape is out of the threat model — the campaign starts its own gates, lake and the compiler,
# none of which leave their group — and the field is named process_group_empty precisely so no
# reader infers containment that was never established.
_rep2="$TMP/escape"
python3 "$LAUNCH" --report "$_rep2" --run-id "GATE-$$" -- python3 -c 'import subprocess,sys; subprocess.Popen(["sleep","4"], start_new_session=True); sys.exit(0)' >/dev/null 2>&1
_esc="$(sed -n 's/^process_group_state=//p' "$_rep2")"
if [ "$_esc" = "empty" ]; then
  ok "a session-escaping descendant is NOT detected — documented limitation, not a containment claim"
else
  no "a session-escaping descendant was detected; the documented limitation is now wrong and the comments must be corrected"
fi

echo "=== the v2 contract: version, run binding, and coherent signal fields ==="
# AN UNSUPPORTED CONTRACT IS NOT A REPORT. Reading fields whose meaning a consumer does not know is
# interpreting bytes, not decoding a record.
_rep3="$TMP/proto"
python3 "$LAUNCH" --report "$_rep3" --run-id "PROTO-1" -- sh -c 'exit 0' >/dev/null 2>&1
[ "$(sed -n 's/^protocol_version=//p' "$_rep3")" = "1" ] \
  && ok "the report declares its protocol version" || no "no protocol_version in the report"
# A STALE REPORT BELONGS TO ANOTHER RUN. Binding it to the run id is what stops one being reused.
[ "$(sed -n 's/^run_id=//p' "$_rep3")" = "PROTO-1" ] \
  && ok "the report is bound to the run that requested it" || no "run_id not echoed"
python3 "$LAUNCH" --report "$_rep3" --run-id "PROTO-2" -- sh -c 'exit 0' >/dev/null 2>&1
[ "$(sed -n 's/^run_id=//p' "$_rep3")" = "PROTO-2" ] \
  && ok "a later run overwrites the binding rather than inheriting it" || no "run_id not rebound"
# THE DISTINCTION child_rc ALONE CANNOT MAKE: a child that EXITED 143 and one KILLED by signal 15
# both report child_rc=143. Only child_signalled separates them, so it is asserted directly.
_rep4="$TMP/exit143"
python3 "$LAUNCH" --report "$_rep4" --run-id "X" -- sh -c 'exit 143' >/dev/null 2>&1
_a="$(sed -n 's/^child_rc=//p' "$_rep4")|$(sed -n 's/^child_signalled=//p' "$_rep4")|$(sed -n 's/^child_signal=//p' "$_rep4")"
python3 "$LAUNCH" --report "$_rep4" --run-id "X" -- sh -c 'kill -TERM $$' >/dev/null 2>&1
_b="$(sed -n 's/^child_rc=//p' "$_rep4")|$(sed -n 's/^child_signalled=//p' "$_rep4")|$(sed -n 's/^child_signal=//p' "$_rep4")"
{ [ "$_a" = "143|0|0" ] && [ "$_b" = "143|1|15" ]; } \
  && ok "exit 143 and signal 15 share child_rc but are distinguished (got $_a vs $_b)" \
  || no "exit 143 vs signal 15 not distinguished (got $_a vs $_b)"
# SIGNAL FIELDS MUST AGREE. Either alone would lead a consumer to the opposite conclusion.
_mk 'protocol_version=1' 'run_id=X' 'child_rc=143' 'child_signalled=1' 'child_signal=0' 'process_group_state=empty' 'pgid=1'
# THE SAME DECODER AGAIN, not a third rule. This helper had its own case expression for signal
# coherence, which made it a second decoder of the same fields inside the gate that exists to prove
# there is only one — and a looser one, since it never checked the 128+N status agreement.
_coh() { [ -z "$(decode_launch_report "$TMP/probe" "X" 0)" ]; }
_coh && no "signalled=1 with signal=0 was accepted" || ok "signalled=1 with signal=0 is incoherent"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=9' 'process_group_state=empty' 'pgid=1'
_coh && no "signal=9 with signalled=0 was accepted" || ok "a signal without signalled is incoherent"
_mk 'protocol_version=1' 'run_id=X' 'child_rc=0' 'child_signalled=0' 'child_signal=0' 'process_group_state=empty' 'pgid=1'
_coh && ok "coherent signal fields are accepted (positive control)" || no "coherent fields rejected"

echo "=== only a PROVEN-empty group may permit publication ==="
# A boolean collapsed these: permission_denied means the group EXISTS but is not ours to signal, and
# error:<n> means the question was never answered. Neither is absence.
# THE DECISION IS EXERCISED, NOT RESTATED.
#
# These controls used to write a state into a file and then assert that the string read back was not
# "empty" — a tautology about sed, satisfied with the supervisor's entire refusal block deleted. They
# now call the predicate the supervisor itself calls, so removing that decision turns them red.
group_state_permits_publication empty \
  && ok "a proven-empty group permits publication (positive control)" \
  || no "a proven-empty group was refused"
for st in nonempty permission_denied error:13 "" garbage; do
  group_state_permits_publication "$st" \
    && no "'${st:-<empty>}' was allowed to permit publication" \
    || ok "'${st:-<empty>}' is not absence, so it cannot permit publication"
done

echo ""
# THE POPULATION IS PINNED, NOT JUST THE FAILURE COUNT.
#
# Exiting on FAIL==0 alone means DELETING a control is indistinguishable from passing it: the gate
# reports fewer assertions and still exits green. This harness has already been bitten by exactly
# that — 240 probes were silently lost from another gate the same way — and this round's review
# found controls here that had been inert for rounds without anyone noticing. Changing the count is
# a deliberate act, recorded in the same commit as the control that changed it.
EXPECTED_CONTROLS=40
_total=$((PASS + FAIL))
if [ "$_total" -ne "$EXPECTED_CONTROLS" ]; then
  echo "  FAIL this gate ran $_total controls, expected $EXPECTED_CONTROLS — one was added or removed"
  echo "       without updating EXPECTED_CONTROLS in the same commit."
  FAIL=$((FAIL + 1))
fi

echo "CHILD-PROCESS-GROUP: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
