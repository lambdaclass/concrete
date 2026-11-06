#!/usr/bin/env bash
# THE SHARDED CAMPAIGN'S AGGREGATOR, ATTACKED DIRECTLY.
#
# The full mutation campaign does not fit one GitHub job, so CI runs it as N shards and
# lib/aggregate_mutation_shards.py turns their records into the single campaign verdict. That
# verdict is reachable for real only by a multi-hour sharded dispatch — so, like the supervisor's
# decision (check_campaign_supervisor.sh), it is attacked here with constructed inputs: a shard that
# is missing, cancelled, from another commit, mislabelled; a family in two shards or in none; a
# summary that disagrees with its records. Every one must deny qualification, and most must deny
# completion.
#
# THE PLAN IS THE REAL ONE. The fixture is built from `check_gate_mutation_coverage.sh --shard-plan`
# output for this tree, and the shard summaries from the published schema
# (CAMPAIGN_SCHEMA_PUBLISHED), so a drift in either format fails here instead of on a dispatched run.
#
# EVERY REFUSAL HAS A POSITIVE CONTROL: the honest set must qualify, or a reader that refuses
# everything would pass this gate.
set -uEo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
. "$ROOT_DIR/scripts/tests/lib/campaign_supervise.sh" || { echo "cannot load the decision library" >&2; exit 2; }
DRIVER="$ROOT_DIR/scripts/tests/check_gate_mutation_coverage.sh"
AGG="$ROOT_DIR/scripts/tests/lib/aggregate_mutation_shards.py"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
NSH=4

echo "=== the driver's plan partitions the inventory, by name ==="
bash "$DRIVER" --shard-plan "$NSH" > "$TMP/plan" 2>"$TMP/plan.err" \
  || { echo "error: --shard-plan failed:"; cat "$TMP/plan.err"; exit 2; }
HEAD="$(sed -n 's/^head=//p' "$TMP/plan")"
_rows="$(grep -c '^family=' "$TMP/plan" || true)"
_disc="$(sed -n 's/^discovered=//p' "$TMP/plan")"
[ -n "$_rows" ] && [ "$_rows" = "$_disc" ] && [ "$_rows" -gt 0 ] \
  && ok "plan rows ($_rows) equal its declared population" || no "plan rows $_rows vs discovered=$_disc"
_uniq="$(sed -n 's/^family=\([^ ]*\) .*/\1/p' "$TMP/plan" | sort -u | grep -c .)"
[ "$_uniq" = "$_rows" ] && ok "every family appears exactly once" || no "plan names $_uniq distinct of $_rows rows"
_bad="$(sed -n 's/^family=[^ ]* shard=\([^ ]*\) .*/\1/p' "$TMP/plan" | awk -v n="$NSH" '$1<1||$1>n' | grep -c . || true)"
[ "$_bad" = "0" ] && ok "every shard is in 1..$NSH" || no "$_bad rows outside 1..$NSH"
bash "$DRIVER" --shard-plan "$NSH" > "$TMP/plan2" 2>/dev/null
cmp -s "$TMP/plan" "$TMP/plan2" && ok "the plan is deterministic" || no "two plans of one tree differ"
for _badn in 0 x 257 ""; do
  if bash "$DRIVER" --shard-plan $_badn >/dev/null 2>&1; then no "--shard-plan '$_badn' accepted"
  else ok "--shard-plan '$_badn' refused"; fi
done
# A MALFORMED SHARD WRITES NOTHING: it is refused before the lock, so no artifact can be touched.
for _bads in 0/4 5/4 1/0 x 1/4/4 01/4; do
  _rc=0; SHARD="$_bads" bash "$DRIVER" >/dev/null 2>&1 || _rc=$?
  [ "$_rc" = "2" ] && ok "SHARD=$_bads refused" || no "SHARD=$_bads exit $_rc, expected 2"
done
_rc=0; SHARD=1/4 FAMILY=1 bash "$DRIVER" >/dev/null 2>&1 || _rc=$?
[ "$_rc" = "2" ] && ok "SHARD with FAMILY refused" || no "SHARD with FAMILY exit $_rc"

echo "=== the supervisor recognises a shard and never lets it qualify ==="
_cand() { # mode selected discovered qualified
  local f="$TMP/cand"; : > "$f"
  printf 'mode=%s\nselected=%s\ndiscovered=%s\nexecuted=0\nreported=0\nqualified=%s\n' "$@" > "$f"; echo "$f"; }
[ -z "$(candidate_incoherent "$(_cand shard 30 95 0)")" ] && ok "shard selecting 30 of 95 is coherent" \
  || no "shard selecting 30 of 95 refused: $(candidate_incoherent "$(_cand shard 30 95 0)")"
[ -z "$(candidate_incoherent "$(_cand shard 0 95 0)")" ] && ok "an empty shard is coherent" \
  || no "empty shard refused"
case "$(candidate_incoherent "$(_cand shard 96 95 0)")" in *shard_mode_selected_exceeds*) ok "shard selecting more than discovered refused" ;; *) no "shard 96/95 accepted" ;; esac
case "$(candidate_incoherent "$(_cand shard 30 95 1)")" in *qualified_in_shard_mode*) ok "a shard claiming qualification refused" ;; *) no "qualified shard accepted" ;; esac

# ---------------------------------------------------------------------------------------------
# AN HONEST SHARD SET, built from the real plan. Each summary is the published schema with the
# semantic fields set; each record carries the fields the aggregator attributes by.
build_honest() { # dir
  local out="$1" k fams cnt f run
  rm -rf "$out"; mkdir -p "$out"
  for (( k=1; k<=NSH; k++ )); do
    local d="$out/mutation-shard-$k"; mkdir -p "$d/evidence"
    run="run-$k"
    fams="$(sed -n "s/^family=\([^ ]*\) shard=$k .*/\1/p" "$TMP/plan")"
    cnt="$(printf '%s\n' "$fams" | grep -c . || true)"
    for f in $fams; do
      mkdir -p "$d/evidence/$f"
      printf 'family=%s\ndisposition=killed\nexpected_route=gate\nhead=%s\nrun_id=%s\n' "$f" "$HEAD" "$run" \
        > "$d/evidence/$f/verdict.txt"
    done
    mkdir -p "$d/evidence/_baseline"
    : > "$d/summary.txt"
    for _k in $CAMPAIGN_SCHEMA_NUMERIC;  do echo "$_k=0" >> "$d/summary.txt"; done
    for _k in $CAMPAIGN_SCHEMA_FREEFORM $CAMPAIGN_SCHEMA_SUPERVISOR; do echo "$_k=fixture" >> "$d/summary.txt"; done
    _sset "$d/summary.txt" completed 1; _sset "$d/summary.txt" mode shard
    _sset "$d/summary.txt" integrity_ok 1
    _sset "$d/summary.txt" discovered "$_disc"
    for _k in selected executed reported killed killed_by_gate; do _sset "$d/summary.txt" "$_k" "$cnt"; done
    local ng; ng="$(_ngates "$k")"
    _sset "$d/summary.txt" baseline_gates_green "$ng/$ng"
    _sset "$d/summary.txt" run_id "$run"; _sset "$d/summary.txt" head "$HEAD"
    for _k in inventory_sha repo_driver_sha families_digest; do
      _sset "$d/summary.txt" "$_k" "$(sed -n "s/^$_k=//p" "$TMP/plan")"
    done
    _sset "$d/summary.txt" executed_driver_sha "execsha"
    _sset "$d/summary.txt" refusals " shard_selected($k/$NSH)"
    _sset "$d/summary.txt" supervisor_refusals none
    _sset "$d/summary.txt" supervisor_child_exit 0
    _sset "$d/summary.txt" candidate_incoherent none
    printf 'shard=%s/%s\njob_status=success\n' "$k" "$NSH" > "$d/status.txt"
  done
}
_ngates() { # shard -> distinct gates its planned families name (the shard's baseline population)
  sed -n "s/^family=[^ ]* shard=$1 gate=\([^ ]*\) .*/\1/p" "$TMP/plan" | sort -u | grep -c . || true
}
_sset() { # file key value — the key must already exist, so the fixture cannot invent a field
  grep -q "^$2=" "$1" || { echo "FIXTURE BUG: '$2' is not a declared schema key" >&2; exit 2; }
  local tmp; tmp="$(mktemp)"; awk -v k="$2" -v v="$3" 'index($0, k"=")==1 {print k"="v; next} {print}' "$1" > "$tmp" && mv "$tmp" "$1"
}
_get() { sed -n "s/^$2=//p" "$1" | head -1; }
_bump() { _sset "$1" "$2" "$(( $(_get "$1" "$2") + $3 ))"; }

# run_agg <dir> -> sets AGG_RC and AGG_OUT
run_agg() {
  AGG_OUT="$TMP/agg.out"; rm -f "$AGG_OUT"; AGG_RC=0
  python3 -I "$AGG" --plan "${PLAN_OVERRIDE:-$TMP/plan}" --shards-dir "$1" --expect-head "$HEAD" \
    --out "$AGG_OUT" > "$TMP/agg.log" 2>&1 || AGG_RC=$?
}
# expect <label> <completed> <integrity_ok> <qualified> [refusal-substring]
expect() {
  local label="$1" c="$2" i="$3" q="$4" sub="${5:-}" got
  got="$(_get "$AGG_OUT" completed)/$(_get "$AGG_OUT" integrity_ok)/$(_get "$AGG_OUT" qualified)"
  local want_rc=1; [ "$q" = "1" ] && want_rc=0
  if [ "$got" != "$c/$i/$q" ]; then no "$label: completed/integrity/qualified=$got, expected $c/$i/$q ($(_get "$AGG_OUT" refusals))"
  elif [ "$AGG_RC" != "$want_rc" ]; then no "$label: exit $AGG_RC, expected $want_rc"
  elif [ -n "$sub" ] && ! grep -q "^refusals=.*$sub" "$AGG_OUT"; then no "$label: refusal '$sub' not named ($(_get "$AGG_OUT" refusals))"
  else ok "$label"; fi
}
S="$TMP/shards"
# One family of shard 1 and one of shard 2, for the cases that move a record.
F1="$(sed -n 's/^family=\([^ ]*\) shard=1 .*/\1/p' "$TMP/plan" | head -1)"
F2="$(sed -n 's/^family=\([^ ]*\) shard=2 .*/\1/p' "$TMP/plan" | head -1)"
[ -n "$F1" ] && [ -n "$F2" ] || { echo "error: the real plan left shard 1 or 2 empty; pick another NSH" >&2; exit 2; }

echo "=== positive control: the honest set qualifies ==="
build_honest "$S"; run_agg "$S"; expect "honest shard set" 1 1 1
[ "$(_get "$AGG_OUT" killed)" = "$_disc" ] && ok "killed = discovered = $_disc" || no "killed=$(_get "$AGG_OUT" killed)"

echo "=== an EMPTY shard is a report, not a gap ==="
# Rebuild with a plan in which shard NSH owns nothing: every row of shard NSH is moved to shard 1.
sed "s/^\(family=[^ ]*\) shard=$NSH /\1 shard=1 /" "$TMP/plan" > "$TMP/plan.empty"
cp "$TMP/plan" "$TMP/plan.real"; cp "$TMP/plan.empty" "$TMP/plan"
build_honest "$S"; run_agg "$S"; expect "a shard that owns nothing still lets the set qualify" 1 1 1
rm -rf "$S/mutation-shard-$NSH"; run_agg "$S"; expect "...but its missing artifact is refused" 0 0 0 "shard_missing($NSH)"
cp "$TMP/plan.real" "$TMP/plan"

echo "=== missing, cancelled and timed-out shards deny completion ==="
build_honest "$S"; rm -rf "$S/mutation-shard-2"; run_agg "$S"
expect "missing shard" 0 0 0 "shard_missing(2)"
build_honest "$S"; sed -i.b 's/^job_status=.*/job_status=cancelled/' "$S/mutation-shard-3/status.txt"; run_agg "$S"
expect "cancelled shard (complete-looking summary)" 0 0 0 "shard_job_cancelled(3)"
build_honest "$S"; rm -f "$S/mutation-shard-3/status.txt"; run_agg "$S"
expect "shard with no status" 0 0 0 "shard_status_missing"
# What a job killed at the time limit actually leaves: the driver's startup invalidation record.
build_honest "$S"; printf 'completed=0\nrefusals= run_started_and_did_not_finish\n' > "$S/mutation-shard-1/summary.txt"
sed -i.b 's/^job_status=.*/job_status=cancelled/' "$S/mutation-shard-1/status.txt"; run_agg "$S"
expect "timed-out shard (startup invalidation record)" 0 0 0 "shard_summary_keys(1"
build_honest "$S"; _sset "$S/mutation-shard-2/summary.txt" completed 0; run_agg "$S"
expect "shard reporting completed=0" 0 0 0 "shard_incomplete(2)"
build_honest "$S"; rm -f "$S/mutation-shard-4/summary.txt"; run_agg "$S"
expect "shard with no summary" 0 0 0 "shard_summary_missing(4)"

echo "=== the partition must be exact ==="
# A consistent liar: the record is copied AND the summary counts are bumped to agree with it.
build_honest "$S"; cp -R "$S/mutation-shard-1/evidence/$F1" "$S/mutation-shard-2/evidence/$F1"
sed -i.b 's/^run_id=.*/run_id=run-2/' "$S/mutation-shard-2/evidence/$F1/verdict.txt"
for _k in selected executed reported killed killed_by_gate; do _bump "$S/mutation-shard-2/summary.txt" "$_k" 1; done
run_agg "$S"; expect "family in two shards" 0 0 0 "duplicate_family($F1"
build_honest "$S"; rm -rf "$S/mutation-shard-2/evidence/$F2"
for _k in selected executed reported killed killed_by_gate; do _bump "$S/mutation-shard-2/summary.txt" "$_k" -1; done
run_agg "$S"; expect "family in no shard" 0 0 0 "families_in_no_shard($F2"
build_honest "$S"; mkdir -p "$S/mutation-shard-1/evidence/not-a-family"
printf 'family=not-a-family\ndisposition=killed\nhead=%s\nrun_id=run-1\n' "$HEAD" > "$S/mutation-shard-1/evidence/not-a-family/verdict.txt"
run_agg "$S"; expect "family the plan does not declare" 1 0 0 "shard_unexpected_families(1:not-a-family)"
build_honest "$S"; mkdir -p "$S/mutation-shard-9"; run_agg "$S"
expect "artifact for a shard nobody planned" 1 0 0 "unexpected_shard_artifact(mutation-shard-9)"

echo "=== identity: one commit, one inventory, one driver ==="
build_honest "$S"; _sset "$S/mutation-shard-3/summary.txt" head 0000000000000000000000000000000000000000; run_agg "$S"
expect "shard from another commit" 0 0 0 "shard_foreign_head(3"
build_honest "$S"; _sset "$S/mutation-shard-3/summary.txt" inventory_sha deadbeef; run_agg "$S"
expect "shard from another inventory" 0 0 0 "shard_foreign_inventory_sha(3"
build_honest "$S"; _sset "$S/mutation-shard-1/summary.txt" families_digest deadbeef; run_agg "$S"
expect "shard over another family set" 0 0 0 "shard_foreign_families_digest(1"
build_honest "$S"; _sset "$S/mutation-shard-4/summary.txt" executed_driver_sha othersha; run_agg "$S"
expect "shards that executed different driver bytes" 0 0 0 "shard_driver_identity_differs"
build_honest "$S"; sed -i.b 's/^head=.*/head=0000000000000000000000000000000000000000/' "$S/mutation-shard-2/evidence/$F2/verdict.txt"
run_agg "$S"; expect "record from another commit" 0 0 0 "record_foreign_head(2:$F2)"
sed "s/^head=.*/head=0000000000000000000000000000000000000000/" "$TMP/plan" > "$TMP/plan.foreign"
build_honest "$S"; PLAN_OVERRIDE="$TMP/plan.foreign" run_agg "$S"
expect "plan from another commit" 0 0 0 "plan_foreign_head"
grep -v "^family=$F1 " "$TMP/plan" > "$TMP/plan.short"
build_honest "$S"; PLAN_OVERRIDE="$TMP/plan.short" run_agg "$S"
expect "plan whose rows disagree with its population" 0 0 0 "plan_population"

echo "=== labels, modes and records ==="
build_honest "$S"; cp "$S/mutation-shard-3/summary.txt" "$S/mutation-shard-2/summary.txt"; run_agg "$S"
expect "shard 3's summary in shard 2's slot" 0 0 0 "shard_label_mismatch(2"
build_honest "$S"; _sset "$S/mutation-shard-1/summary.txt" mode single; run_agg "$S"
expect "a single-family record posing as a shard" 0 0 0 "shard_mode(1:single)"
build_honest "$S"; echo "killed=0" >> "$S/mutation-shard-1/summary.txt"; run_agg "$S"
expect "summary with a duplicated key" 0 0 0 "shard_summary_keys(1:killed)"
build_honest "$S"; sed -i.b 's/^disposition=.*/disposition=invalid/' "$S/mutation-shard-2/evidence/$F2/verdict.txt"
run_agg "$S"; expect "record says invalid, summary says killed" 1 0 0 "shard_record_disagreement(2:killed"
build_honest "$S"; _sset "$S/mutation-shard-1/summary.txt" integrity_ok 0; run_agg "$S"
expect "shard with failed integrity" 1 0 0 "shard_integrity(1)"
build_honest "$S"; _sset "$S/mutation-shard-1/summary.txt" supervisor_refusals " family_set_mismatch(x)"; run_agg "$S"
expect "unexplained supervisor refusal" 1 0 0 "shard_supervisor_refused(1"

echo "=== complete but not qualified: honest bad news is a complete report ==="
build_honest "$S"; sed -i.b 's/^disposition=.*/disposition=survived/' "$S/mutation-shard-1/evidence/$F1/verdict.txt"
_bump "$S/mutation-shard-1/summary.txt" killed -1; _bump "$S/mutation-shard-1/summary.txt" killed_by_gate -1
_bump "$S/mutation-shard-1/summary.txt" survived 1
_sset "$S/mutation-shard-1/summary.txt" supervisor_refusals " child_exit(1)"
_sset "$S/mutation-shard-1/summary.txt" supervisor_child_exit 1
sed -i.b 's/^job_status=.*/job_status=failure/' "$S/mutation-shard-1/status.txt"
run_agg "$S"; expect "a survivor: completed, integrity intact, not qualified" 1 1 0
build_honest "$S"; _sset "$S/mutation-shard-2/summary.txt" baseline_gates_green "$(( $(_ngates 2) - 1 ))/$(_ngates 2)"; run_agg "$S"
expect "a red baseline gate in one shard" 1 1 0
build_honest "$S"; _sset "$S/mutation-shard-2/summary.txt" supervisor_child_exit 1; run_agg "$S"
expect "nonzero exit with every family killed" 1 0 0 "shard_unexplained_exit(2"

echo "=== PENDING: declared expected-red gates are accounted, never killed ==="
# The plan carries the driver's declarations; every pending family must be on one of those gates.
ERG="$(sed -n 's/^expected_red_gate=//p' "$TMP/plan")"
[ -n "$ERG" ] && ok "the plan carries the declared expected-red gate(s): $(echo $ERG)" \
  || no "the plan declares no expected-red gate"
# make_pending <dir> — every family on a declared gate becomes pending, with its shard's counts moved
# from killed to pending and one baseline gate moved from green to expected-red.
make_pending() {
  local k d f g n
  for (( k=1; k<=NSH; k++ )); do
    d="$1/mutation-shard-$k"; n=0
    for f in $(sed -n "s/^family=\([^ ]*\) shard=$k gate=\([^ ]*\) .*/\1 \2/p" "$TMP/plan" | awk -v e=" $(echo $ERG) " 'index(e, " "$2" ") {print $1}'); do
      g="$(sed -n "s/^family=$f shard=[0-9]* gate=\([^ ]*\) .*/\1/p" "$TMP/plan")"
      sed -i.b 's/^disposition=.*/disposition=pending/' "$d/evidence/$f/verdict.txt"
      echo "gate=$g" >> "$d/evidence/$f/verdict.txt"; n=$((n+1))
      # The baseline transcript the shard publishes for that gate: red on exactly the plan's legs,
      # ending in a verdict counting them.
      { echo "  ok   a passing leg"
        sed -n "s/^expected_red_leg=$g //p" "$TMP/plan" | sed 's/^/  FAIL /'
        echo "GATE: PASS=1 FAIL=$(grep -c "^expected_red_leg=$g " "$TMP/plan")"; } > "$d/evidence/_baseline/$g.log"
    done
    [ "$n" -gt 0 ] || continue
    _bump "$d/summary.txt" killed "-$n"; _bump "$d/summary.txt" killed_by_gate "-$n"
    _bump "$d/summary.txt" pending "$n"
    _sset "$d/summary.txt" baseline_gates_expected_red 1
    local c; c="$(_ngates "$k")"
    _sset "$d/summary.txt" baseline_gates_green "$((c-1))/$c"
  done
}
_npend="$(sed -n 's/^family=\([^ ]*\) shard=[0-9]* gate=\([^ ]*\) .*/\1 \2/p' "$TMP/plan" | awk -v e=" $(echo $ERG) " 'index(e, " "$2" ")' | grep -c . || true)"
build_honest "$S"; make_pending "$S"; run_agg "$S"
expect "pending families on a declared gate let the set qualify" 1 1 1
[ "$(_get "$AGG_OUT" pending)" = "$_npend" ] && [ "$(( $(_get "$AGG_OUT" killed) + _npend ))" = "$_disc" ] \
  && ok "killed + pending = discovered ($(_get "$AGG_OUT" killed) + $_npend = $_disc)" \
  || no "pending=$(_get "$AGG_OUT" pending) killed=$(_get "$AGG_OUT" killed) of $_disc"
case "$(_get "$AGG_OUT" pending_families)" in none|'') no "pending families not named" ;;
  *) ok "the aggregate names every pending family: $(_get "$AGG_OUT" pending_families)" ;; esac
# A pending record on a gate nobody declared is a family that went unjudged under an excuse.
build_honest "$S"; make_pending "$S"
_G1="$(sed -n "s/^family=$F1 shard=1 gate=\([^ ]*\) .*/\1/p" "$TMP/plan")"
case " $(echo $ERG) " in *" $_G1 "*) no "fixture: F1's gate is itself declared; pick another family" ;; esac
sed -i.b 's/^disposition=.*/disposition=pending/' "$S/mutation-shard-1/evidence/$F1/verdict.txt"
_bump "$S/mutation-shard-1/summary.txt" killed -1; _bump "$S/mutation-shard-1/summary.txt" killed_by_gate -1
_bump "$S/mutation-shard-1/summary.txt" pending 1
_sset "$S/mutation-shard-1/summary.txt" baseline_gates_expected_red 1
run_agg "$S"; expect "pending on an undeclared gate is refused" 1 0 0 "pending_without_declaration(1:$F1"
# Pending families with no expected-red baseline gate in their shard describe a run that cannot happen.
build_honest "$S"; make_pending "$S"
for (( _k=1; _k<=NSH; _k++ )); do
  _d="$S/mutation-shard-$_k/summary.txt"
  [ "$(_get "$_d" pending)" = "0" ] && continue
  _sset "$_d" baseline_gates_expected_red 0; _c="$(_ngates "$_k")"; _sset "$_d" baseline_gates_green "$_c/$_c"
  _PK="$_k"; break
done
run_agg "$S"; expect "pending without an expected-red baseline gate is refused" 1 0 0 "shard_pending_without_expected_red_baseline($_PK"
# An expected-red gate does not excuse a RED one: green + expected-red must still be the whole baseline.
build_honest "$S"; make_pending "$S"; _c="$(_ngates "$_PK")"
_sset "$S/mutation-shard-$_PK/summary.txt" baseline_gates_green "$((_c-2))/$_c"; run_agg "$S"
expect "an expected-red gate beside a red one does not qualify" 1 1 0
# THE TRANSCRIPT IS CHECKED, NOT ASSUMED: a missing, green, wrong-leg or truncated baseline for the
# expected-red gate, or a record naming another gate, leaves the pending families unaccounted.
_XG1="$(echo $ERG | awk '{print $1}')"
_xr_case() { # label refusal-substring — after the caller corrupted shard $_PK
  run_agg "$S"; expect "$1" 1 0 0 "$2"
}
build_honest "$S"; make_pending "$S"; rm -f "$S/mutation-shard-$_PK/evidence/_baseline/$_XG1.log"
_xr_case "pending with no baseline transcript is refused" "pending_without_matching_baseline($_PK"
build_honest "$S"; make_pending "$S"; printf '  ok   all\nGATE: PASS=2 FAIL=0\n' > "$S/mutation-shard-$_PK/evidence/_baseline/$_XG1.log"
_xr_case "pending over a green baseline transcript is refused" "pending_without_matching_baseline($_PK"
build_honest "$S"; make_pending "$S"; printf '  FAIL another leg\nGATE: PASS=1 FAIL=1\n' > "$S/mutation-shard-$_PK/evidence/_baseline/$_XG1.log"
_xr_case "pending over a baseline red on another leg is refused" "pending_without_matching_baseline($_PK"
build_honest "$S"; make_pending "$S"; sed -i.b '$d' "$S/mutation-shard-$_PK/evidence/_baseline/$_XG1.log"
_xr_case "pending over a truncated baseline (no verdict line) is refused" "pending_without_matching_baseline($_PK"
build_honest "$S"; make_pending "$S"
_PF="$(grep -l '^disposition=pending' "$S/mutation-shard-$_PK"/evidence/*/verdict.txt | head -1)"
sed -i.b 's/^gate=.*/gate=check_something_else.sh/' "$_PF"
_xr_case "a pending record naming another gate is refused" "pending_record_gate_mismatch($_PK"
# THE EXPECTED-RED COUNT IS RECONCILED: inflating it while shrinking the green count hides a red gate.
build_honest "$S"; make_pending "$S"; _c="$(_ngates "$_PK")"
_sset "$S/mutation-shard-$_PK/summary.txt" baseline_gates_expected_red 2
_sset "$S/mutation-shard-$_PK/summary.txt" baseline_gates_green "$((_c-2))/$_c"
_xr_case "an expected-red count larger than the backed gates is refused" "shard_expected_red_unbacked($_PK"
# ...and every family the shard ran on a backed expected-red gate must be pending. The plan is
# rewritten so a KILLED family of that shard is planned on the expected-red gate too.
_KF="$(sed -n "s/^family=\([^ ]*\) shard=$_PK gate=\([^ ]*\) .*/\1 \2/p" "$TMP/plan" | awk -v g="$_XG1" '$2 != g {print $1; exit}')"
sed "s/^\(family=$_KF shard=$_PK gate=\)[^ ]*/\1$_XG1/" "$TMP/plan" > "$TMP/plan.xrfam"
build_honest "$S"; make_pending "$S"; PLAN_OVERRIDE="$TMP/plan.xrfam" run_agg "$S"
expect "a family on a backed expected-red gate that is not pending is refused" 1 0 0 "shard_expected_red_gate_family_not_pending($_PK:$_KF:killed)"
# THE DENOMINATOR IS THE PLAN'S: dropping an ordinary gate from the baseline, with valid pending
# evidence and green + expected-red still adding up, is refused.
build_honest "$S"; make_pending "$S"; _c="$(_ngates "$_PK")"
_sset "$S/mutation-shard-$_PK/summary.txt" baseline_gates_green "$((_c-2))/$((_c-1))"
_xr_case "a shard baseline over fewer gates than its plan names is refused" "shard_baseline_population($_PK"
build_honest "$S"; _sset "$S/mutation-shard-2/summary.txt" baseline_gates_green "1/1"; run_agg "$S"
expect "...and so is one without pending, reporting 1/1 over a multi-gate shard" 1 0 0 "shard_baseline_population(2"
# A declaration naming no planned gate is a malformed plan.
{ cat "$TMP/plan"; echo "expected_red_gate=check_no_such_gate.sh"; } > "$TMP/plan.badxr"
build_honest "$S"; PLAN_OVERRIDE="$TMP/plan.badxr" run_agg "$S"
expect "a plan declaring an expected-red gate no family names" 0 0 0 "plan_expected_red_malformed(check_no_such_gate.sh)"

echo
echo "MUTATION-SHARD-AGGREGATE: passed=$PASS failed=$FAIL"
[ "$FAIL" -eq 0 ]
