#!/usr/bin/env bash
# MACHINE-WIDE HEAVY-RUN SEMAPHORE (R-0543): two holders, a third waits, a dead holder is reclaimed.
#
# Every probe runs against a SANDBOX slot directory (CONCRETE_HEAVY_DIR), never the machine's real
# one: a gate that filled the real slots would block every other agent on the host.
#
# Controls:
#   C1  two holders acquire at once with CONCRETE_HEAVY_SLOTS=2
#   C2  a third with --no-wait fails fast with exit 75 and names both holders
#   C3  a third without --no-wait WAITS (still waiting after several polls) and prints the holders
#   C4  SIGKILL one holder (no release runs): the waiter reclaims its slot and proceeds
#   C5  a nested run inheriting a live holder's token takes no slot and does not wait
#   C6  a stale token naming a dead holder is ignored, not trusted
#   C7  an explicit release frees the slot for the next acquirer
#   C8  a reused pid is not mistaken for the holder (start time recorded and compared)
#   C9  light modes take no slot: run_tests.sh --manifest and run_ci_gates_local.sh --plan return
#       while every slot is full; run_tests.sh --full --no-wait exits 75 before running anything
#   C10 the runners are wired: the heavy paths call heavy_slot_acquire, the full runner before its rebuild
#   C11 JOBS scale with the slot count: with N>1 the default is at most (cores-2)/N, no fixed 8
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
TMP="$(mktemp -d)" || { echo "FATAL: could not create a sandbox directory" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox directory is not usable" >&2; exit 2; }
PIDS=()
cleanup() {
  local p
  for p in "${PIDS[@]:-}"; do [ -n "$p" ] && { pkill -9 -P "$p" 2>/dev/null; kill -9 "$p" 2>/dev/null; }; done
  rm -rf "$TMP"
}
trap cleanup EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

LIB="$ROOT_DIR/scripts/tests/lib/heavy_slots.sh"
[ -f "$LIB" ] || { echo "FATAL: $LIB missing" >&2; exit 2; }
export CONCRETE_HEAVY_DIR="$TMP/slots" CONCRETE_HEAVY_SLOTS=2 CONCRETE_HEAVY_POLL=0.2
unset CONCRETE_HEAVY_SLOT CONCRETE_HEAVY_NO_WAIT   # this gate may itself run under a heavy runner

# holder <name>: acquire, record, then sleep until killed. Its pid is the slot owner.
holder() {
  bash -c 'source "$1"; heavy_slot_acquire "$2" || exit $?; echo "$$" > "$3"; sleep 300' _ \
    "$LIB" "$1" "$TMP/$1.acq" 2>"$TMP/$1.err" &
  PIDS+=("$!")
  disown "$!" 2>/dev/null || true   # no "Killed: 9" job notice when a control kills it
}
wait_for() { # file timeout-seconds
  local i=0 lim=$(( $2 * 10 ))
  while [ ! -s "$1" ] && [ "$i" -lt "$lim" ]; do sleep 0.1; i=$((i+1)); done
  [ -s "$1" ]
}
live_slots() { local n=0 d; for d in "$CONCRETE_HEAVY_DIR"/slot-*; do [ -d "$d" ] && n=$((n+1)); done; echo "$n"; }

echo "=== C1: two simultaneous holders ==="
holder A; PA="${PIDS[${#PIDS[@]}-1]}"
holder B; PB="${PIDS[${#PIDS[@]}-1]}"
if wait_for "$TMP/A.acq" 10 && wait_for "$TMP/B.acq" 10 && [ "$(live_slots)" = 2 ]; then
  ok "two holders acquired concurrently (2 slot directories)"
else
  no "two holders did not both acquire (A=$(cat "$TMP/A.acq" 2>/dev/null) B=$(cat "$TMP/B.acq" 2>/dev/null) slots=$(live_slots))"
fi

echo "=== C2: a third with --no-wait fails fast ==="
t0=$(date +%s)
bash -c 'source "$1"; heavy_slot_acquire third --no-wait' _ "$LIB" 2>"$TMP/nw.err"; rc=$?
t1=$(date +%s)
[ "$rc" = 75 ] && [ $((t1 - t0)) -lt 10 ] \
  && ok "--no-wait exits 75 at once while both slots are held" \
  || no "--no-wait returned $rc after $((t1-t0))s (want 75, fast)"
grep -q "pid $PA alive" "$TMP/nw.err" && grep -q "pid $PB alive" "$TMP/nw.err" \
  && ok "...and names both live holders" \
  || no "...but did not name the holders: $(tr '\n' ' ' < "$TMP/nw.err")"
CONCRETE_HEAVY_NO_WAIT=1 bash -c 'source "$1"; heavy_slot_acquire third' _ "$LIB" 2>/dev/null; rc=$?
[ "$rc" = 75 ] && ok "CONCRETE_HEAVY_NO_WAIT=1 is the same as --no-wait" || no "CONCRETE_HEAVY_NO_WAIT=1 returned $rc"

echo "=== C3: a third without --no-wait waits ==="
holder C; PC="${PIDS[${#PIDS[@]}-1]}"
sleep 2
if [ ! -s "$TMP/C.acq" ] && kill -0 "$PC" 2>/dev/null && [ "$(live_slots)" = 2 ]; then
  ok "the third run is still waiting after 2s of polling (no third slot created)"
else
  no "the third run did not wait (acq=$(cat "$TMP/C.acq" 2>/dev/null) slots=$(live_slots))"
fi
grep -q "waiting for one of 2 heavy slots" "$TMP/C.err" && grep -q "pid $PA alive" "$TMP/C.err" \
  && ok "...and prints who holds the slots while it waits" \
  || no "...but printed no holder list: $(tr '\n' ' ' < "$TMP/C.err")"

echo "=== C4: a dead holder's slot is reclaimed ==="
pkill -9 -P "$PA" 2>/dev/null; kill -9 "$PA" 2>/dev/null; wait "$PA" 2>/dev/null
if wait_for "$TMP/C.acq" 15; then
  ok "after SIGKILL of holder A (no release ran), the waiter acquired its slot"
else
  no "the waiter never acquired after holder A died"
fi
grep -q "reclaiming slot-.* from a dead holder" "$TMP/C.err" \
  && ok "...and said it reclaimed a dead holder's slot" || no "...without reporting the reclaim"
[ "$(live_slots)" = 2 ] && ok "...and the slot count stays at the limit (2)" || no "slot count is $(live_slots), want 2"

echo "=== C5: re-entrancy under a live holder ==="
SLOT_B="$(grep -l "^pid=$PB$" "$CONCRETE_HEAVY_DIR"/slot-*/owner 2>/dev/null | head -1)"; SLOT_B="${SLOT_B%/owner}"
t0=$(date +%s)
CONCRETE_HEAVY_SLOT="$PB:$SLOT_B" bash -c 'source "$1"; heavy_slot_acquire nested --no-wait' _ "$LIB" 2>/dev/null; rc=$?
[ "$rc" = 0 ] && [ "$(live_slots)" = 2 ] \
  && ok "a nested run inheriting holder B's token proceeds without a slot (no deadlock)" \
  || no "nested run returned $rc with $(live_slots) slots (want 0, 2)"

echo "=== C6: a stale token is not trusted ==="
CONCRETE_HEAVY_SLOT="999999:$CONCRETE_HEAVY_DIR/slot-9" bash -c 'source "$1"; heavy_slot_acquire forged --no-wait' _ "$LIB" 2>/dev/null; rc=$?
[ "$rc" = 75 ] && ok "a token naming no live holder is ignored; the run had to queue (75)" \
  || no "a stale token was honoured (rc=$rc)"

echo "=== C7: explicit release ==="
pkill -9 -P "$PB" 2>/dev/null; kill -9 "$PB" 2>/dev/null; pkill -9 -P "$PC" 2>/dev/null; kill -9 "$PC" 2>/dev/null
wait 2>/dev/null
rm -rf "$CONCRETE_HEAVY_DIR"; mkdir -p "$CONCRETE_HEAVY_DIR"
out="$(bash -c 'source "$1"; heavy_slot_acquire r1 || exit 9; ls "$CONCRETE_HEAVY_DIR" | grep -c slot-; heavy_slot_release; ls "$CONCRETE_HEAVY_DIR" | grep -c slot-' _ "$LIB" 2>/dev/null | tr '\n' ' ')"
[ "$out" = "1 0 " ] && ok "release removes exactly the slot this run created" || no "acquire/release slot counts were '$out', want '1 0 '"

echo "=== C8: a reused pid is not the holder ==="
mkdir -p "$CONCRETE_HEAVY_DIR/slot-0" "$CONCRETE_HEAVY_DIR/slot-1"
# Both name THIS shell's pid, which is alive, but with a start time it never had.
printf 'pid=%s\nstart=Thu Jan  1 00:00:00 1970\ncmd=ghost\nsince=0\n' "$$" > "$CONCRETE_HEAVY_DIR/slot-0/owner"
printf 'pid=%s\nstart=Thu Jan  1 00:00:00 1970\ncmd=ghost\nsince=0\n' "$$" > "$CONCRETE_HEAVY_DIR/slot-1/owner"
bash -c 'source "$1"; heavy_slot_acquire reuse --no-wait' _ "$LIB" 2>/dev/null; rc=$?
[ "$rc" = 0 ] && ok "slots whose pid is alive but whose start time differs are reclaimed (pid reuse)" \
  || no "a reused pid kept its slots (rc=$rc)"
rm -rf "$CONCRETE_HEAVY_DIR"; mkdir -p "$CONCRETE_HEAVY_DIR"

echo "=== C9: light modes do not wait; heavy modes queue ==="
holder D; holder E
wait_for "$TMP/D.acq" 10; wait_for "$TMP/E.acq" 10
# --manifest exits through run_tests.sh's on_exit (97, no summary by design), so the OUTPUT is judged.
m_out="$(TEST_SUMMARY_FILE="$TMP/m.summary" bash scripts/tests/run_tests.sh --manifest 2>/dev/null | head -1 || true)"
case "$m_out" in
  "# Concrete test manifest"*) ok "run_tests.sh --manifest (a light mode) runs while every slot is full" ;;
  *) no "run_tests.sh --manifest did not run: '$m_out'" ;;
esac
bash scripts/tests/run_ci_gates_local.sh --plan >/dev/null 2>&1 \
  && ok "run_ci_gates_local.sh --plan runs while every slot is full" \
  || no "run_ci_gates_local.sh --plan failed while slots were full"
TEST_SUMMARY_FILE="$TMP/summary" bash scripts/tests/run_tests.sh --full --no-wait >"$TMP/full.out" 2>&1; rc=$?
[ "$rc" = 75 ] && [ ! -e "$TMP/summary" ] && grep -q "no heavy slot" "$TMP/full.out" \
  && ok "run_tests.sh --full --no-wait exits 75 and runs nothing while the slots are full" \
  || no "run_tests.sh --full --no-wait returned $rc: $(tail -3 "$TMP/full.out" | tr '\n' ' ')"

echo "=== C10: the heavy entry points are wired ==="
strip() { sed -E 's/(^|[[:space:]])#.*$/\1/' "$1"; }
for f in scripts/tests/run_ci_gates_local.sh scripts/tests/run_tests.sh \
         scripts/tests/test_mutation.sh scripts/tests/check_gate_mutation_coverage.sh; do
  case "$(strip "$f")" in
    *heavy_slots.sh*heavy_slot_acquire*) ok "$f acquires a heavy slot" ;;
    *) no "$f does not acquire a heavy slot" ;;
  esac
done
_rc="$(strip scripts/tests/run_ci_gates_local.sh)"
_acq="$(printf '%s\n' "$_rc" | grep -n 'heavy_slot_acquire' | head -1 | cut -d: -f1)"
_bld="$(printf '%s\n' "$_rc" | grep -n 'require_fresh_binary "' | head -1 | cut -d: -f1)"
{ [ -n "$_acq" ] && [ -n "$_bld" ] && [ "$_acq" -lt "$_bld" ]; } \
  && ok "run_ci_gates_local.sh takes its slot BEFORE the rebuild (a fresh worktree's build is heavy)" \
  || no "run_ci_gates_local.sh acquires at line ${_acq:-none}, after the rebuild at ${_bld:-none}"

echo "=== C11: JOBS scale with the slot count ==="
cores="$( (sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null) || echo 4)"
j1="$(CONCRETE_HEAVY_SLOTS=1 bash -c 'source "$1"; heavy_jobs_default' _ "$LIB")"
j2="$(CONCRETE_HEAVY_SLOTS=2 bash -c 'source "$1"; heavy_jobs_default' _ "$LIB")"
j4="$(CONCRETE_HEAVY_SLOTS=4 bash -c 'source "$1"; heavy_jobs_default' _ "$LIB")"
fair2=$(( (cores - 2) / 2 )); [ "$fair2" -lt 1 ] && fair2=1
fair4=$(( (cores - 2) / 4 )); [ "$fair4" -lt 1 ] && fair4=1
[ "$j1" -ge 1 ] && [ "$j1" -le 8 ] && ok "one slot keeps the old default (cores-2, at most 8): $j1" || no "one-slot default $j1"
[ "$j2" -ge 1 ] && [ "$j2" -le "$fair2" ] && ok "two slots: at most the fair share (cores-2)/2=$fair2: $j2" || no "two-slot default $j2 > $fair2"
[ "$j4" -ge 1 ] && [ "$j4" -le "$fair4" ] && ok "four slots: at most (cores-2)/4=$fair4: $j4" || no "four-slot default $j4 > $fair4"

echo
echo "HEAVY-SLOTS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
