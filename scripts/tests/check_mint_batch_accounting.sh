#!/usr/bin/env bash
# THE MINT-BATCH ACCOUNTING CONTROLS, ACTUALLY RUN.
#
# `check_dependency_edges.sh` batches its receipt-minting probes — EXPECTED_MINT_PROBES of them,
# pinned in that gate rather than counted again here — into one kernel replay,
# which is sound only because the batch reconciles what it ASKED FOR against what it was ANSWERED:
# a missing result, a duplicate result, a foreign result, a failed replay, a broken probe or a
# grouping failure must each turn the gate red rather than shrink a green total.
#
# Those controls existed as environment-gated branches inside that gate and NOTHING INVOKED THEM.
# An unreachable control is documentation: it can rot, or be quietly disabled, and no run notices.
# Worse, the normal 315-assertion run exercises none of them, so the accounting they protect was
# asserted only by having been tested once, by hand, months from whenever this is read.
#
# This gate runs each one and requires it to FAIL for its own reason. Every case names the marker it
# expects, so a control that starts passing for an unrelated reason is not mistaken for coverage.
#
# It is deliberately a separate gate: it runs the dependency-edge gate repeatedly, which is minutes
# per case, and burying that inside the gate it tests would make the common path pay for it.
set -uEo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
GATE="$ROOT_DIR/scripts/tests/check_dependency_edges.sh"
[ -x "$GATE" ] || [ -f "$GATE" ] || { echo "error: $GATE missing" >&2; exit 2; }

PASS=0; FAIL=0
# COUNTED SEPARATELY FROM PASS. Using PASS to detect "no case selected" conflates it with "the
# selected case FAILED" — which is precisely the state a mutation puts this gate in, so the guard
# misfired exactly when the gate was doing its job.
CASES_RUN=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# SCOPED RUNS ARE LABELLED AS SUCH. A mutation family covering one refusal would otherwise pay for
# all eight cases on four legs — over three hours for one family. MBA_ONLY runs a single case, and
# the final line SAYS so, because a subset that printed the full-coverage line would be a smaller
# green total wearing the same name. CI runs the full set.
: "${MBA_ONLY:=}"

# case <label> <env-assignment> <expected-marker> [shape-predicate]
# The gate is run with ONE self-test flag set. Its verdict must be red AND must name the reason.
#
# THE CASES RUN CONCURRENTLY, THE VERDICTS ARE JUDGED IN ORDER (R-0546). Each case is a full,
# independent run of check_dependency_edges.sh (45-85 s, its own mktemp sandbox), and nine of them in
# series made this the longest gate of a parallel pass (396 s at JOBS=64). case_run only REGISTERS a
# case; run_cases launches every registered case (at most MBA_JOBS at once, default all) and then
# judges each one, in registration order, exactly as before. A case's exit status is recorded by the
# case itself, so a lost result reads as a missing status, never as a pass.
CASE_LABEL=(); CASE_ENV=(); CASE_MARK=(); CASE_PRED=()
case_run() {
  local env_kv="$2"
  if [ -n "$MBA_ONLY" ] && [ "${env_kv%%=*}" != "$MBA_ONLY" ]; then return 0; fi
  CASE_LABEL+=("$1"); CASE_ENV+=("$env_kv"); CASE_MARK+=("$3"); CASE_PRED+=("${4:-}")
}
case_log() { printf '%s/%s.log' "$TMP" "$(echo "$1" | tr '=/ ' '___')"; }
run_cases() {
  local i n="${#CASE_ENV[@]}" max="${MBA_JOBS:-${#CASE_ENV[@]}}"
  [ "$max" -ge 1 ] 2>/dev/null || max=1
  for (( i=0; i<n; i++ )); do
    while [ "$(jobs -rp | wc -l)" -ge "$max" ]; do wait -n 2>/dev/null || true; done
    # GATE_DONE=1 is exported on purpose: an inherited value once disarmed the failure trap for a whole
    # run, so every case here also proves that no longer matters.
    ( local log rc=0; log="$(case_log "${CASE_ENV[$i]}")"
      env GATE_DONE=1 "${CASE_ENV[$i]}" bash "$GATE" > "$log" 2>&1 || rc=$?
      echo "$rc" > "$log.rc" ) &
  done
  wait
  for (( i=0; i<n; i++ )); do
    case_judge "${CASE_LABEL[$i]}" "${CASE_ENV[$i]}" "${CASE_MARK[$i]}" "${CASE_PRED[$i]}"
  done
}
case_judge() {
  local label="$1" env_kv="$2" marker="$3"
  CASES_RUN=$((CASES_RUN + 1))
  local log rc
  log="$(case_log "$env_kv")"
  rc="$(cat "$log.rc" 2>/dev/null)"
  case "$rc" in
    ''|*[!0-9]*) no "$label — the run under $env_kv left no exit status (it died before recording one)"; return ;;
  esac
  if [ "$rc" -eq 0 ]; then
    no "$label — the gate exited 0 under $env_kv, so this control is inert"
    return
  fi
  # THE MARKER MUST APPEAR ON A FAILURE LINE. Searching the whole log accepts a marker printed on an
  # `ok` line — which is exactly what a mutation that neutralises the REFUSAL while preserving its
  # MESSAGE produces. The control would then pass while the accounting it guards was disabled.
  if grep -E '^(  FAIL|FATAL)' "$log" | grep -qF -- "$marker"; then
    # A MARKER IS NECESSARY, NOT SUFFICIENT. Several of these cases assert something about the SHAPE
    # of the failure — that it is confined to one group, or that it names every affected member —
    # and a single matching failure line is equally consistent with the whole gate collapsing. The
    # optional predicate is that extra claim, evaluated against the log with $LOG bound to it.
    if [ -n "${4:-}" ] && ! LOG="$log" bash -c "$4"; then
      no "$label — /$marker/ appeared, but the failure has the wrong shape: $4"
      return
    fi
    ok "$label"
  elif grep -qF -- "$marker" "$log"; then
    no "$label — /$marker/ appears, but NOT on a failure line: the message survived while the refusal did not"
  else
    no "$label — red, but never named /$marker/ (failed for an unrelated reason): $(grep -m1 -E '^  FAIL|FATAL' "$log" | cut -c1-120)"
  fi
}

echo "=== the batch must account for every probe it registered ==="
# EVERY MEMBER IS NAMED, NOT SILENTLY DROPPED — and DISTINCT members, not nineteen lines.
# A count is satisfied by one member repeated nineteen times while another is omitted
# entirely, so result IDs and labels are deduplicated before being counted.
#
# The marker alone was satisfied by a single MISSING line even if every other per-member report had
# vanished with it. The first predicate written here demanded exactly ONE missing probe, which
# MEASUREMENT refuted: the probes share one replay file, so a broken body stops that file compiling
# and all EXPECTED_MINT_PROBES members go unresolved together. That is inherent to batching and is
# not the defect. The defect would be losing them quietly — so what must hold is that every declared
# member is individually reported missing, which is exactly what "accounting" means here.
case_run "a broken probe body breaks its shared group, and every member is still named" \
         "MINT_SELFTEST_BREAK=0" "MISSING from group" \
         'm=$(grep -o "no <<P[0-9][0-9][0-9]>> result" "$LOG" | sort -u | grep -c .); want=$(sed -n "s/^EXPECTED_MINT_PROBES=//p" scripts/tests/check_dependency_edges.sh); [ "$m" = "$want" ]' 
case_run "a result nobody declared is rejected by id" \
         "MINT_SELFTEST_FOREIGN=1" "UNEXPECTED result id"
# EVERY MEMBER, COUNTED. The marker alone is printed once by the group header, so this passed even
# if the per-member reporting loop beneath it were deleted — the exact regression the case is named
# for. The count of `unproven (replay failed)` lines must equal the pinned probe population.
case_run "a failed shared replay names every affected member instead of emptying the batch" \
         "MINT_SELFTEST_REPLAY_FAIL=1" "the shared kernel replay FAILED" \
         'n=$(grep "unproven (replay failed)" "$LOG" | sed "s/.*unproven (replay failed)://" | sort -u | grep -c .); want=$(sed -n "s/^EXPECTED_MINT_PROBES=//p" scripts/tests/check_dependency_edges.sh); [ "$n" = "$want" ]'

# ALONE MEANS ALONE. A second group failing to replay must not disturb the real probes, but the
# marker appears just as readily when EVERY probe failed too. The real assertions must still be
# reported ok, so the passing count must be the gate's full pinned total.
case_run "a second group that cannot replay fails ALONE, leaving the real probes accounted" \
         "MINT_SELFTEST_SECOND_GROUP=1" "unproven (replay failed): SELFTEST" \
         'n=$(grep -c "^  ok " "$LOG"); want=$(sed -n "s/^EXPECTED_TOTAL_ASSERTIONS=//p" scripts/tests/check_dependency_edges.sh); [ "$n" = "$want" ]'

# The batch refuses duplicate result ids, but nothing had ever produced one.
case_run "a duplicated result id is refused, not read as two independent confirmations" \
         "MINT_SELFTEST_DUPLICATE=1" "DUPLICATE result ids"

echo "=== a failure inside the batch cannot be swallowed ==="
case_run "a shell failure inside the batch reaches the trap (errtrace is load-bearing)" \
         "MINT_SELFTEST_SHELL_FAIL=1" "FATAL: unexpected shell failure"
case_run "a failure inside a command substitution is caught by its status check, not the trap" \
         "MINT_SELFTEST_SUBSHELL_FAIL=1" "replay-group count could not be computed"
case_run "no groups means no driver ran, and that is a shortfall — not a smaller green total" \
         "MINT_SELFTEST_EMPTY_GROUPS=1" "verdicts for"

echo "=== the ordinary probe path checks process success, not just output ==="
case_run "a process that prints the wanted string and exits nonzero still fails" \
         "PROBE_SELFTEST_FAKE_EXIT=1" "probe exited 9"

echo "=== verdicts (cases ran concurrently; judged in the order registered above) ==="
run_cases

echo ""
# SELECTION IS CHECKED BEFORE THE SUMMARY IS PRINTED. Emitting a refusal AFTER the final line breaks
# the end-of-run shape every consumer authenticates — the campaign read this gate as never having
# reached its end, and reported INVALID instead of the kill it had actually produced.
if [ -n "$MBA_ONLY" ]; then
  [ "$CASES_RUN" -ge 1 ] || { echo "  FAIL MBA_ONLY=$MBA_ONLY selected no case"; FAIL=$((FAIL+1)); }
else
  # THE FULL SET IS PINNED. A case silently deleted would otherwise shrink a green total.
  EXPECTED_CASES=9
  [ "$CASES_RUN" = "$EXPECTED_CASES" ] \
    || { echo "  FAIL ran $CASES_RUN cases, expected $EXPECTED_CASES"; FAIL=$((FAIL+1)); }
fi
if [ -n "$MBA_ONLY" ]; then
  echo "MINT-BATCH-ACCOUNTING(SUBSET=$MBA_ONLY): PASS=$PASS FAIL=$FAIL"
else
  echo "MINT-BATCH-ACCOUNTING: PASS=$PASS FAIL=$FAIL"
fi
[ "$FAIL" -eq 0 ]
