#!/usr/bin/env bash
# THE SUPERVISOR'S DECISION, ATTACKED DIRECTLY.
#
# The mutation campaign publishes qualification from a SUPERVISOR that outlives the child running the
# campaign, because a process cannot safely publish a verdict about its own exit: reconciliation and
# artifact installation used to happen inside the run, before the EXIT trap's final dirty-target
# check, so a target changing after the last reconciliation let the run print and durably store
# `qualified=1` and then exit nonzero. A failed process left a passing artifact behind.
#
# That decision was reachable only by running a full campaign — hours — which means in practice it
# was never attacked. It now lives in lib/campaign_supervise.sh as a pure function, and this gate
# feeds it hostile inputs that are otherwise hard to produce: a child that exits zero after
# corrupting the tree, a truncated candidate, a candidate that claims qualification it did not earn.
#
# EVERY REFUSAL HAS A POSITIVE CONTROL. A gate that only checks refusals passes when the function
# refuses everything, which would be just as broken and far easier to ship.
set -uEo pipefail
# HERMETIC: run inside a campaign, this gate inherited the driver's CONCRETE_MUT_SNAPSHOT/ROOT/SNAPDIR
# and lock token, and every sandboxed driver copy below then refused to start — red on the clean tree
# in each campaign, green everywhere else. The wiring subshells set the CONCRETE_MUT_* values they need.
for _v in $(compgen -e); do case "$_v" in CONCRETE_MUT_*|CAMPAIGN_HELD_LOCK) unset "$_v" ;; esac; done
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
. "$ROOT_DIR/scripts/tests/lib/campaign_supervise.sh" || { echo "cannot load the decision library" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# A well-formed candidate claiming a fully qualified campaign. Every hostile case below is this file
# with exactly one thing wrong, so a refusal is attributable to that one thing.
GOOD="$TMP/good"
# THE FIXTURE IS BUILT FROM THE SCHEMA, NOT ALONGSIDE IT.
#
# It used to be a hand-written list of twelve fields. When the decoder began requiring the full
# declared key set, every case below started failing on `missing_key(...)` for thirty other fields
# instead of on the one thing it had deliberately broken — so a suite that reported 41/24 was not
# testing what its labels said, and the 65/0 it had reported earlier described a decoder that no
# longer existed. A fixture derived from CAMPAIGN_SCHEMA cannot fall behind it: a new declared key
# appears here automatically, and a key removed from the schema disappears from here too.
#
# Numeric fields default to 0 and freeform ones to a placeholder; the semantic values that the cases
# actually reason about are then set explicitly, so the fixture reads as "a fully qualified campaign"
# and every hostile variant below is this file with exactly one thing wrong.
: > "$GOOD"
for _k in $CAMPAIGN_SCHEMA_NUMERIC;  do echo "$_k=0" >> "$GOOD"; done
for _k in $CAMPAIGN_SCHEMA_FREEFORM; do echo "$_k=fixture" >> "$GOOD"; done
_set() { # key value — replace in place, refusing to invent a key the schema does not declare
  grep -q "^$1=" "$GOOD" || { echo "FIXTURE BUG: '$1' is not a declared schema key" >&2; exit 2; }
  sed -i.bak "s|^$1=.*|$1=$2|" "$GOOD" && rm -f "$GOOD.bak"
}
_set completed 1
_set mode campaign
for _k in discovered selected executed reported killed families_declared families_run evidence_written; do
  _set "$_k" 85
done
_set killed_by_gate 85
_set integrity_ok 1
_set qualified 1
_set refusals ""
# The producer writes exactly `.mutation-evidence/<run_id>`; the fixture must look like the artifact.
_set evidence_dir ".mutation-evidence/fixture"
# PRODUCTION SHAPES, taken from what the publisher actually writes: both are `<n>/<m>`.
# The fixture previously said `yes` and `all`, which no producer emits — a fixture that does not
# look like the artifact cannot test the checks that read the artifact.
_set baseline_gates_green 36/36
_set gates_proven 85/85
# The record must decode cleanly before any case can attribute a refusal to its own mutation.
# THE PINNED POPULATION IS PART OF THE CALL. `candidate_incoherent` takes the family count a
# qualifying campaign must have discharged, and every call here omitted it — so the positive control
# was refused with `qualified_without_a_pinned_population` and the suite could not have been green.
POP=85
# The declared GATE population, distinct from the family population, and READ FROM THE
# DRIVER rather than assumed: the check below compares this to `gate_count_from_driver`.
# It moved 36 -> 37 when bug 071's two mutation families introduced
# `check_cross_package_caps.sh` as a target no earlier family named. POP above is the
# FIXTURE population and is deliberately not the live family count.
GATES=36
# The LIVE driver's gate count, checked against the inventory further down. Kept separate
# from GATES above for the same reason POP is separate from the live family count: GATES
# is the population baked into this gate's synthetic candidate records, which are
# self-consistent at 36, while the real inventory grows whenever a family targets a gate
# no earlier family named. Collapsing the two made every fixture incoherent the moment
# bug 071 added `check_cross_package_caps.sh` as a new target. It moved again 37 -> 38 when
# R-0484's two admission/maintenance families (2026-09-26) introduced
# `check_effect_opacity.sh`; that commit left this pin behind and CI stayed red for three days.
DRIVER_GATES=38
_fixture_refusals="$(decode_candidate "$GOOD")"
if [ -n "$_fixture_refusals" ]; then
  echo "FIXTURE BUG: the positive control does not decode:$_fixture_refusals" >&2
  exit 2
fi

H="headsha"; T="trackedsha"; U="untrackedsha"
sed 's/^qualified=1$/qualified=0/' "$GOOD" > "$TMP/unqual"

# expect <label> <expected-substring-or-EMPTY> <args...>
expect() {
  local label="$1" want="$2"; shift 2
  local got; got="$(supervisor_refusals "$@")"
  if [ "$want" = "EMPTY" ]; then
    [ -z "$got" ] && ok "$label" || no "$label — expected no refusal, got '$got'"
  else
    case "$got" in *"$want"*) ok "$label" ;; *) no "$label — expected /$want/, got '${got:-<none>}'" ;; esac
  fi
}

echo "=== positive control: a clean child and an unchanged tree must NOT refuse ==="
expect "a clean run publishes (no refusal)" EMPTY 0 "$GOOD" "$H" "$H" "$T" "$T" "$U" "$U"
# The population argument is required here too — see POP above.
[ "$(supervisor_qualification "$GOOD" "" "$POP")" = "qualified=1" ] \
  && ok "...and its qualified=1 is carried forward" \
  || no "a clean run's qualification was dropped: $(supervisor_qualification "$GOOD" "" "$POP")"

# AND THE POPULATION MAY NOT BE OMITTED. Every call above passes it, so nothing above would notice
# if the requirement were dropped and an unpinned campaign began qualifying itself.
case "$(candidate_incoherent "$GOOD")" in
  *qualified_without_a_pinned_population*) ok "omitting the pinned population is itself a refusal" ;;
  *) no "a candidate qualified with no population pinned: '$(candidate_incoherent "$GOOD")'" ;;
esac
# A population that disagrees with the record is a refusal, not a rounding difference.
case "$(candidate_incoherent "$GOOD" 84)" in
  ?*) ok "a record discharging 85 families does not qualify against a pinned 84" ;;
  *)  no "population mismatch accepted" ;;
esac

echo "=== the child's exit is evidence ==="
expect "a child that exits nonzero refuses"            "child_exit(1)"  1 "$GOOD" "$H" "$H" "$T" "$T" "$U" "$U"
expect "a child killed by a signal refuses"            "child_exit(143)" 143 "$GOOD" "$H" "$H" "$T" "$T" "$U" "$U"

echo "=== the candidate must exist and be a record ==="
expect "a missing candidate refuses" "candidate_missing" 0 "$TMP/nope" "$H" "$H" "$T" "$T" "$U" "$U"
: > "$TMP/empty"
expect "an empty candidate refuses"  "candidate_missing" 0 "$TMP/empty" "$H" "$H" "$T" "$T" "$U" "$U"
# TRUNCATION IS THE REALISTIC CORRUPTION: a partial write parses as a valid record with fields
# missing, and each missing field is named so the diagnosis does not require guessing.
for k in $CAMPAIGN_CANDIDATE_KEYS; do
  grep -v "^$k=" "$GOOD" > "$TMP/miss.$k"
  expect "a candidate missing '$k' refuses by name" "candidate_schema(missing_key($k))" 0 "$TMP/miss.$k" "$H" "$H" "$T" "$T" "$U" "$U"
done

echo "=== the supervisor's own reconciliation ==="
expect "HEAD moving under the run refuses"       "supervisor_head_changed"      0 "$GOOD" "$H" "other" "$T" "$T" "$U" "$U"
expect "a tracked change under the run refuses"  "supervisor_tracked_changed"   0 "$GOOD" "$H" "$H" "$T" "other" "$U" "$U"
expect "an untracked change under the run refuses" "supervisor_untracked_changed" 0 "$GOOD" "$H" "$H" "$T" "$T" "$U" "other"

echo "=== an unreadable tree state is not an unchanged tree ==="
# Two unavailable values compare EQUAL, so a naive reconciliation agrees with itself and publishes.
expect "TREESTATE-UNAVAILABLE refuses even when both sides match" "supervisor_tree_state_unavailable" \
  0 "$GOOD" "TREESTATE-UNAVAILABLE:head" "TREESTATE-UNAVAILABLE:head" "$T" "$T" "$U" "$U"
expect "an empty tree state refuses even when both sides match" "supervisor_tree_state_empty" \
  0 "$GOOD" "" "" "$T" "$T" "$U" "$U"

echo "=== qualification is the supervisor's to grant ==="
[ "$(supervisor_qualification "$GOOD" "supervisor_tracked_changed")" = "qualified=0" ] \
  && ok "a refused run cannot publish qualified=1" \
  || no "a refused run kept its qualification"
[ "$(supervisor_qualification "$TMP/nope" "")" = "qualified=0" ] \
  && ok "a missing candidate cannot publish qualified=1" \
  || no "a missing candidate produced a qualification"
[ "$(supervisor_qualification "$TMP/unqual" "")" = "qualified=0" ] \
  && ok "a candidate that did not claim qualification is not granted one" \
  || no "qualification was invented for a candidate that did not claim it"

echo "=== a candidate must JUSTIFY its own qualification, not merely declare it ==="
# Key presence was the only check, so a record that contradicts itself qualified. Each case below is
# the good candidate with exactly one field made incoherent, so the refusal is attributable.
inc() { # label field value expected-substring
  local f="$TMP/inc.$2"; sed "s/^$2=.*/$2=$3/" "$GOOD" > "$f"
  local got; got="$(candidate_incoherent "$f" "$POP" "$GATES")"
  case "$got" in *"$4"*) ok "$1" ;; *) no "$1 — expected /$4/, got '${got:-<none>}'" ;; esac
  # THE POPULATION IS PASSED HERE TOO. Omitting it made every one of these five second assertions
  # pass for the same irrelevant reason — a missing pinned population already forces qualified=0 —
  # so each would have stayed green with its named field-specific check deleted. A control that
  # cannot distinguish "the field is wrong" from "I forgot an argument" is not a control.
  [ "$(supervisor_qualification "$f" "" "$POP")" = "qualified=0" ] \
    && ok "...and it cannot publish qualified=1" || no "$1 — it published qualification anyway"
}
inc "qualified=1 with completed=0 is incoherent"       completed 0     qualified_without_completed
inc "qualified=1 with integrity_ok=0 is incoherent"    integrity_ok 0  qualified_without_integrity
inc "qualified=1 in single-family mode is incoherent"  mode single     qualified_in_single_mode
inc "qualified=1 with counts that do not reconcile"    reported 81     qualified_with_counts
inc "qualified=1 with unkilled families"               killed 80       qualified_with_unkilled

for v in survived invalid could_not_apply; do
  # SUBSTITUTED, not appended: appending leaves TWO values for the key and the reader takes the
  # first, so the contradiction reads as the benign value. That is itself a defect, covered below.
  f="$TMP/inc.$v"; sed "s/^$v=.*/$v=1/" "$GOOD" > "$f"
  got="$(candidate_incoherent "$f" "$POP" "$GATES")"
  case "$got" in *"qualified_with_$v"*) ok "qualified=1 with $v=1 is incoherent" ;;
    *) no "qualified=1 with $v=1 — expected refusal, got '${got:-<none>}'" ;; esac
done

echo "=== a key with two values is not a record ==="
for v in survived qualified killed; do
  f="$TMP/dup.$v"; { cat "$GOOD"; printf '%s=1\n' "$v"; } > "$f"
  expect "a duplicated '$v' is refused as a duplicate, not read as its first value" \
    "candidate_schema(duplicate_key($v))" 0 "$f" "$H" "$H" "$T" "$T" "$U" "$U"
done

# THE POSITIVE CONTROL FOR COHERENCE ITSELF: the good candidate must remain coherent, or every case
# above would pass for the wrong reason.
[ -z "$(candidate_incoherent "$GOOD" "$POP" "$GATES")" ] \
  && ok "a coherent candidate is not refused by the coherence check" \
  || no "the coherence check refuses a well-formed candidate: $(candidate_incoherent "$GOOD" "$POP" "$GATES")"
# ...and a candidate that never claimed qualification is not judged on coherence at all.
[ -z "$(candidate_incoherent "$TMP/unqual" "$POP" "$GATES")" ] \
  && ok "a candidate not claiming qualification is not held to it" \
  || no "an unqualified candidate was judged on qualification coherence"

echo "=== counts must be numbers that reconcile, not strings that match ==="
# String equality accepted five identical NON-NUMERIC values, so a record of five "x" qualified.
for fld in discovered selected executed reported killed; do
  sed "s/^$fld=.*/$fld=x/" "$GOOD" > "$TMP/nn.$fld"
  got="$(candidate_incoherent "$TMP/nn.$fld" "$POP")"
  case "$got" in *nonnumeric*) ok "a non-numeric $fld is refused" ;;
    *) no "a non-numeric $fld was accepted (got '${got:-<none>}')" ;; esac
done
sed 's/=8[0-9]*$/=x/' "$GOOD" > "$TMP/allx"
got="$(candidate_incoherent "$TMP/allx" "$POP")"
[ -n "$got" ] && ok "five identical non-numeric counts do not reconcile" \
  || no "five identical non-numeric counts qualified"
for fld in discovered selected executed reported killed; do sed -i "s/^$fld=.*/$fld=0/" "$TMP/allx"; done
sed -i 's/^qualified=.*/qualified=1/' "$TMP/allx"
got="$(candidate_incoherent "$TMP/allx" "$POP")"
case "$got" in *zero_families*) ok "a campaign that discharged ZERO families does not qualify" ;;
  *) no "zero families qualified (got '${got:-<none>}')" ;; esac

echo "=== a missing disposition is absent evidence, not a zero ==="
for v in invalid survived could_not_apply; do
  grep -v "^$v=" "$GOOD" > "$TMP/nodisp.$v"
  got="$(candidate_incoherent "$TMP/nodisp.$v" "$POP")"
  case "$got" in *"qualified_without_$v"*) ok "a candidate with no '$v' field does not qualify" ;;
    *) no "a missing '$v' was treated as zero (got '${got:-<none>}')" ;; esac
  # ...and it is a mandatory key, so the record is malformed as well as incoherent.
  expect "a candidate missing '$v' is malformed" "candidate_schema(missing_key($v))" \
    0 "$TMP/nodisp.$v" "$H" "$H" "$T" "$T" "$U" "$U"
done

echo "=== the four dispositions must account for the reported families ==="
# Each disposition being zero and killed==reported still leaves the ledger unbalanced if a family is
# reported under NO disposition. This is the identity that makes the four numbers one population.
sed -e 's/^killed=.*/killed=84/' -e 's/^invalid=.*/invalid=0/' "$GOOD" > "$TMP/unbal"
got="$(candidate_incoherent "$TMP/unbal" "$POP")"
case "$got" in *unbalanced_ledger*|*unkilled*) ok "reported families unaccounted by the dispositions is refused" ;;
  *) no "an unbalanced ledger qualified (got '${got:-<none>}')" ;; esac
# ...and a BALANCED ledger with real dispositions must not qualify either, since they are nonzero —
# but it must be refused for the disposition, not for the ledger.
sed -e 's/^killed=.*/killed=84/' -e 's/^survived=.*/survived=1/' "$GOOD" > "$TMP/bal"
got="$(candidate_incoherent "$TMP/bal" "$POP")"
case "$got" in *qualified_with_survived*) ok "a balanced ledger with a survivor is refused for the survivor" ;;
  *) no "a balanced ledger with a survivor was misdiagnosed (got '${got:-<none>}')" ;; esac

echo "=== negative, noncanonical and implausible numbers are not numbers ==="
for spec in "discovered=-1:nonnumeric" "killed=007:noncanonical" "reported=1234567:implausible"; do
  fld="${spec%%=*}"; rest="${spec#*=}"; val="${rest%%:*}"; want="${rest##*:}"
  sed "s/^$fld=.*/$fld=$val/" "$GOOD" > "$TMP/num.$fld"
  got="$(candidate_incoherent "$TMP/num.$fld" "$POP")"
  case "$got" in *"$want"*) ok "$fld=$val is refused ($want)" ;;
    *) no "$fld=$val was accepted (got '${got:-<none>}')" ;; esac
done

echo "=== duplicate keys are refused whether they contradict or agree ==="
# An IDENTICAL duplicate is still not a record: it is evidence that two writers produced this file.
for v in qualified killed invalid; do
  val="$(sed -n "s/^$v=//p" "$GOOD" | head -1)"
  { cat "$GOOD"; printf '%s=%s\n' "$v" "$val"; } > "$TMP/dupeq.$v"
  expect "an identical duplicate '$v' is still refused" "candidate_schema(duplicate_key($v))" \
    0 "$TMP/dupeq.$v" "$H" "$H" "$T" "$T" "$U" "$U"
done

echo ""
echo "=== qualification reads every field it publishes, not just the headline counts ==="
# Each case is GOOD with exactly one field falsified, so the refusal is attributable to that field.
# Without these the new checks would be unexercised branches — indistinguishable from deleted ones.
_falsify() { # key value expected-substring label
  sed "s|^$1=.*|$1=$2|" "$GOOD" > "$TMP/fal"
  case "$(candidate_incoherent "$TMP/fal" "$POP" "$GATES")" in
    *"$3"*) ok "$4" ;;
    *) no "$4 — expected /$3/, got '$(candidate_incoherent "$TMP/fal" "$POP" "$GATES")'" ;;
  esac
}
_falsify families_run 0 qualified_with_families_run \
  "a campaign claiming 85 kills while running 0 families is refused"
_falsify evidence_written 0 qualified_with_evidence_written \
  "a campaign that reported 85 families but wrote no evidence is refused"
_falsify killed_by_gate 84 qualified_with_kill_split \
  "kills that do not split into gate and build routes are refused"
_falsify killed_by_gate x qualified_with_nonnumeric_kill_split \
  "a non-numeric kill split is refused rather than coerced"
_falsify baseline_gates_green "" qualified_without_baseline_gates_green \
  "qualification with no baseline-gates result is refused"
_falsify gates_proven "" qualified_without_gates_proven \
  "qualification with no gates-proven result is refused"
# NONEMPTY IS NOT A VALUE. These are the production shapes that are perfectly well-formed and mean
# the opposite of what qualification claims.
_falsify baseline_gates_green 0/36 qualified_with_baseline_gates_red \
  "a campaign qualifying on 0 of 36 green baseline gates is refused"
# SELF-AGREEMENT IS NOT A POPULATION. `1/1` is correct for a single-family run and meaningless for a
# campaign; the denominator must be the number of gates the inventory actually declares.
_falsify baseline_gates_green 1/1 qualified_with_baseline_gate_population \
  "a campaign qualifying on 1 of 1 baseline gates is refused"
_falsify baseline_gates_green 999/999 qualified_with_baseline_gate_population \
  "a self-agreeing ratio over an invented population is refused"
# ...and the count is read from the driver, not restated here.
[ "$(gate_count_from_driver "$ROOT_DIR/scripts/tests/check_gate_mutation_coverage.sh")" = "$DRIVER_GATES" ] \
  && ok "the pinned gate population matches what the driver declares" \
  || no "DRIVER_GATES=$DRIVER_GATES but the driver declares $(gate_count_from_driver "$ROOT_DIR/scripts/tests/check_gate_mutation_coverage.sh")"
_falsify gates_proven 0/85 qualified_with_gates_proven_disagreeing \
  "a campaign qualifying with 0 of 85 gates proven is refused"
_falsify gates_proven 84/85 qualified_with_gates_proven_disagreeing \
  "a gates-proven numerator that disagrees with killed_by_gate is refused"
_falsify gates_proven 85/84 qualified_with_gates_proven_population \
  "a gates-proven denominator that is not the pinned population is refused"
_falsify baseline_gates_green 0/0 qualified_with_no_baseline_gates \
  "a campaign qualifying on zero baseline gates is refused"
_falsify baseline_gates_green x/x qualified_with_unparsable_baseline_gates \
  "a ratio whose halves are equal but are not numbers is refused"
_falsify gates_proven 85/junk/85 qualified_with_unparsable_gates_proven \
  "a three-part value is not a ratio, however its ends compare"

# THE SHAPE A CORRECT CAMPAIGN ACTUALLY PUBLISHES MUST QUALIFY.
#
# Three families are killed by the BUILD rather than by their gate, so a complete campaign publishes
# gates_proven=82/85 — this driver's own header gives 78/81 as the same case. An earlier version of
# this check demanded numerator == denominator and would have REFUSED the qualifying result the whole
# programme exists to produce. A gate that rejects the outcome it is meant to certify is worse than
# no gate, so the honest build-kill shape is a positive control.
sed -e 's/^killed_by_gate=.*/killed_by_gate=82/' -e 's/^killed_by_build=.*/killed_by_build=3/' \
    -e 's|^gates_proven=.*|gates_proven=82/85|' "$GOOD" > "$TMP/buildkills"
[ -z "$(candidate_incoherent "$TMP/buildkills" "$POP" "$GATES")" ] \
  && ok "a campaign with three build-route kills and gates_proven=82/85 qualifies" \
  || no "the honest build-kill shape was refused: $(candidate_incoherent "$TMP/buildkills" "$POP" "$GATES")"
_falsify gates_proven all qualified_with_unparsable_gates_proven \
  "a gates-proven value that is not <n>/<m> is refused rather than accepted as nonempty"
_falsify families_declared 84 qualified_with_families_declared \
  "a record declaring a different population than the pinned one is refused"
_falsify failed 1 qualified_with_failures \
  "a campaign with a failure does not qualify"
# THE FIELDS NOTHING EVER READ. Both are mandatory in the schema and were checked for presence only.
_falsify refusals " fatal_integrity_failure" qualified_with_refusals \
  "a record publishing its own integrity refusals does not qualify"
_falsify evidence_dir ".mutation-evidence/some-other-run" qualified_with_foreign_evidence_dir \
  "a record pointing at another run's evidence does not qualify"
# A SUFFIX TEST ACCEPTED BOTH OF THESE. They end with the run id and name a different tree.
_falsify evidence_dir "foreign-prefix/fixture" qualified_with_foreign_evidence_dir \
  "an evidence path merely ENDING in the run id does not qualify"
_falsify evidence_dir "evilfixture" qualified_with_foreign_evidence_dir \
  "an evidence path whose last component only ends in the run id does not qualify"
_falsify evidence_dir "" qualified_without_evidence_dir \
  "a record naming no evidence directory does not qualify"
# ...and the unfalsified record still qualifies, or the six checks above are just refusing everything.
[ -z "$(candidate_incoherent "$GOOD" "$POP" "$GATES")" ] \
  && ok "the unfalsified record still qualifies (positive control for the six checks above)" \
  || no "the new field checks refuse a well-formed record: $(candidate_incoherent "$GOOD" "$POP" "$GATES")"

echo "=== the evidence directories must BE the declared family set ==="
# Naming the right digest is not the same as HOLDING the right evidence: a candidate could publish
# the correct family digest beside arbitrarily named killed directories and every total would still
# self-agree. This is the comparison the supervisor now makes on the names found on disk.
_declared="$(printf 'famA\nfamB\nfamC\n')"
_dig_declared="$(family_set_digest "$_declared")"
[ "$(family_set_digest "$(printf 'famC\nfamA\nfamB\n')")" = "$_dig_declared" ] \
  && ok "the family-set digest is order-independent (positive control)" \
  || no "the family-set digest depends on order"
[ "$(family_set_digest "$(printf 'famA\nfamB\nfamX\n')")" != "$_dig_declared" ] \
  && ok "a one-for-one family substitution changes the set digest" \
  || no "substituting a family left the set digest unchanged"
[ "$(family_set_digest "$(printf 'famA\nfamB\n')")" != "$_dig_declared" ] \
  && ok "a missing family changes the set digest" \
  || no "dropping a family left the set digest unchanged"
[ "$(family_set_digest "")" != "$_dig_declared" ] \
  && ok "an empty evidence tree does not digest as the declared set" \
  || no "the empty set digests as the declared set"

echo "=== what was tested is verified, not taken on the child's word ==="
# Nine fields described WHAT a qualification is about and nothing checked any of them. The three
# kinds of check are asserted separately, because they are not equally strong and the code says so.
_PV="$TMP/prov"
cat > "$_PV" <<'PVEOF'
executed_driver_sha=aaaa1111
preamble_driver_sha=bbbb2222
repo_driver_sha=cccc3333
inventory_sha=dddd4444
head=HEADSHA
workspace_head=HEADSHA
tracked_sha=TRACKED
workspace_tracked_sha=TRACKED
untracked_sha=UNTRACKED
workspace_untracked_sha=UNTRACKED
baseline_compiler_sha=0123456789abcdef0123456789abcdef
compilers_tested=per-family-rebuilds
PVEOF
_pv() { candidate_provenance "$_PV" aaaa1111 bbbb2222 cccc3333 dddd4444; }
[ -z "$(_pv)" ] \
  && ok "a coherent record passes provenance (positive control)" \
  || no "a coherent record was refused: $(_pv)"

# OBSERVED: the supervisor minted these before exec'ing the snapshot, so a mismatch is exact.
# The diagnostic names the FIELD exactly, so a reader is not left mapping a refusal back to a key.
for _pair in "executed_driver_sha:executed_driver_sha_mismatch" \
             "preamble_driver_sha:preamble_driver_sha_mismatch" \
             "repo_driver_sha:repo_driver_sha_mismatch" \
             "inventory_sha:inventory_sha_mismatch"; do
  _k="${_pair%%:*}"; _want="${_pair##*:}"
  _sav="$(sed -n "s/^$_k=//p" "$_PV")"
  sed -i.bak "s|^$_k=.*|$_k=FORGED|" "$_PV" && rm -f "$_PV.bak"
  case "$(_pv)" in
    *"$_want"*) ok "a forged $_k is refused by comparison with the supervisor's own value" ;;
    *) no "a forged $_k was accepted: $(_pv)" ;;
  esac
  sed -i.bak "s|^$_k=.*|$_k=$_sav|" "$_PV" && rm -f "$_PV.bak"
done

# CROSS-FIELD: the workspace is a copy of this repository at this commit, and no longer exists.
for _pair in "workspace_head:workspace_head_differs" \
             "workspace_tracked_sha:workspace_tracked_differs" \
             "workspace_untracked_sha:workspace_untracked_differs"; do
  _k="${_pair%%:*}"; _want="${_pair##*:}"
  _sav="$(sed -n "s/^$_k=//p" "$_PV")"
  sed -i.bak "s|^$_k=.*|$_k=DRIFTED|" "$_PV" && rm -f "$_PV.bak"
  case "$(_pv)" in
    *"$_want"*) ok "a workspace record that disagrees with the repository is refused ($_k)" ;;
    *) no "$_k drift was accepted: $(_pv)" ;;
  esac
  sed -i.bak "s|^$_k=.*|$_k=$_sav|" "$_PV" && rm -f "$_PV.bak"
done

# SHAPE: unrecoverable once the workspace is gone, so only well-formedness is claimed.
for _bad in unknown absent "" 0123456789abcdef 0123456789abcdef0123456789abcdeZ; do
  sed -i.bak "s|^baseline_compiler_sha=.*|baseline_compiler_sha=$_bad|" "$_PV" && rm -f "$_PV.bak"
  case "$(_pv)" in
    *baseline_compiler_malformed*|*baseline_compiler_not_hex*)
      ok "baseline_compiler_sha='${_bad:-<empty>}' is refused as malformed" ;;
    *) no "baseline_compiler_sha='${_bad:-<empty>}' was accepted as a compiler digest" ;;
  esac
done
sed -i.bak "s|^baseline_compiler_sha=.*|baseline_compiler_sha=0123456789abcdef0123456789abcdef|" "$_PV" && rm -f "$_PV.bak"
sed -i.bak "s|^compilers_tested=.*|compilers_tested=something-new|" "$_PV" && rm -f "$_PV.bak"
case "$(_pv)" in
  *compilers_tested_unrecognised*) ok "an undeclared compilers_tested mode is refused, not recorded silently" ;;
  *) no "an undeclared compilers_tested mode was accepted" ;;
esac
sed -i.bak "s|^compilers_tested=.*|compilers_tested=per-family-rebuilds|" "$_PV" && rm -f "$_PV.bak"
[ -z "$(_pv)" ] \
  && ok "...and the record still passes once restored (the checks above are not refusing everything)" \
  || no "the restored record is still refused: $(_pv)"

# AN OBSERVATION THAT COULD NOT BE MADE IS NOT AGREEMENT.
#
# The first version SKIPPED a comparison whose expected value was empty, so a hashing failure
# silently disabled the check: with `ts_inventory_digest` gone, the child's start and end snapshots
# are both empty, agree with each other, satisfy the freeform schema, and the supervisor compares
# nothing. That is the fail-open this tier exists to remove, introduced by the change removing it.
case "$(candidate_provenance "$_PV" aaaa1111 bbbb2222 cccc3333 "")" in
  *inventory_sha_unobservable*) ok "an EMPTY expected digest is refused, not skipped" ;;
  *) no "an empty expected digest silently skipped the comparison: $(candidate_provenance "$_PV" aaaa1111 bbbb2222 cccc3333 "")" ;;
esac
case "$(candidate_provenance "$_PV" aaaa1111 bbbb2222 cccc3333 'TREESTATE-UNAVAILABLE:no-hasher')" in
  *inventory_sha_unobservable*) ok "an UNAVAILABLE expected digest is refused, not compared as a string" ;;
  *) no "the unavailable marker was compared as if it were a digest" ;;
esac
_pv_sav="$(sed -n 's/^inventory_sha=//p' "$_PV")"
sed -i.bak 's|^inventory_sha=.*|inventory_sha=|' "$_PV" && rm -f "$_PV.bak"
case "$(_pv)" in
  *inventory_sha_unpublished*) ok "a candidate that PUBLISHED nothing for an observed field is refused" ;;
  *) no "an empty published digest was accepted: $(_pv)" ;;
esac
sed -i.bak "s|^inventory_sha=.*|inventory_sha=$_pv_sav|" "$_PV" && rm -f "$_PV.bak"

# THE PRODUCERS THEMSELVES MUST BE PRESENT, or the supervisor computes an empty expectation and the
# refusal above fires for the wrong reason — correct, but pointing at the candidate instead of the
# broken library.
( . "$ROOT_DIR/scripts/tests/lib/treestate.sh" 2>/dev/null
  unset -f ts_inventory_digest 2>/dev/null
  ts_require >/dev/null 2>&1 ) \
  && no "ts_require accepts a treestate library missing ts_inventory_digest" \
  || ok "ts_require refuses a treestate library missing a digest producer"

echo "=== the candidate must be THIS run's candidate ==="
sed 's|^run_id=.*|run_id=THIS-RUN|' "$GOOD" | sed 's|^head=.*|head=THIS-HEAD|' > "$TMP/bound"
[ -z "$(candidate_run_binding "$TMP/bound" THIS-RUN THIS-HEAD)" ] \
  && ok "a candidate from this run against this head binds (positive control)" \
  || no "a correctly bound candidate was refused: $(candidate_run_binding "$TMP/bound" THIS-RUN THIS-HEAD)"
case "$(candidate_run_binding "$TMP/bound" OTHER-RUN THIS-HEAD)" in
  *candidate_from_other_run*) ok "a candidate left by an EARLIER run cannot answer for this one" ;;
  *) no "a stale candidate was accepted" ;;
esac
case "$(candidate_run_binding "$TMP/bound" THIS-RUN OTHER-HEAD)" in
  *candidate_head_mismatch*) ok "a candidate describing another commit is refused" ;;
  *) no "a candidate for a different head was accepted" ;;
esac
case "$(candidate_run_binding "$TMP/bound" THIS-RUN "")" in
  *supervisor_head_unreadable*) ok "an unreadable supervisor head is not a matching head" ;;
  *) no "an empty observed head was treated as agreement" ;;
esac
case "$(candidate_run_binding "$TMP/nonexistent" THIS-RUN THIS-HEAD)" in
  *candidate_missing*) ok "an absent candidate is refused, not silently bound" ;;
  *) no "an absent candidate passed the binding check" ;;
esac

echo "=== a lock is released only when the group is PROVEN empty ==="
supervisor_must_hold_lock empty \
  && no "a proven-empty group held the lock" \
  || ok "a proven-empty group releases the lock"
supervisor_must_hold_lock no_child_launched \
  && no "an early failure before any child stranded the lock" \
  || ok "an early failure with no child launched releases the lock"
for _st in nonempty permission_denied error:13 unexpected_value ""; do
  supervisor_must_hold_lock "$_st" \
    && ok "state '${_st:-<empty>}' holds the lock rather than admitting the next run" \
    || no "state '${_st:-<empty>}' released the lock while work may survive"
done

echo "=== the evidence root binds records to the family that produced them ==="
# A root that digests every line and then sorts globally attests that some evidence exists
# somewhere. What must be attested is that THIS family was killed by THIS transcript, so the
# strongest attack is the one that preserves the multiset of lines: swap two families' contents.
_EV="$TMP/ev"; mkdir -p "$_EV/famA" "$_EV/famB"
printf 'family=famA\ndisposition=killed\n' > "$_EV/famA/record"
printf 'family=famB\ndisposition=killed\n' > "$_EV/famB/record"
_r_before="$(evidence_root_digest "$_EV")"
mv "$_EV/famA/record" "$_EV/.swap" && mv "$_EV/famB/record" "$_EV/famA/record" && mv "$_EV/.swap" "$_EV/famB/record"
_r_after="$(evidence_root_digest "$_EV")"
[ -n "$_r_before" ] && [ -n "$_r_after" ] || no "the evidence-root producer failed; the swap control proves nothing"
[ "$_r_before" != "$_r_after" ] \
  && ok "swapping two families' complete contents moves the evidence root" \
  || no "the root is blind to which family produced which record"
# A SYMLINK IS NOT EVIDENCE. `find -type f` did not match one, but every consumer of this evidence
# follows it — so a verdict could be replaced by a link to content outside the tree, be read as
# authoritative, and never appear in the digest meant to detect exactly that.
mkdir -p "$_EV/famC"
ln -s /etc/hostname "$_EV/famC/record" 2>/dev/null || ln -s /dev/null "$_EV/famC/record"
_sym_out="$(evidence_root_digest "$_EV" 2>/dev/null)"; _sym_rc=$?
{ [ "$_sym_rc" != "0" ] && case "$_sym_out" in *symlink*) true ;; *) false ;; esac; } \
  && ok "a symlink in the evidence tree is refused with its own status, not silently omitted" \
  || no "a symlinked record was accepted (rc=$_sym_rc out='$_sym_out')"
rm -rf "$_EV/famC"
# THE FAMILY DIRECTORY AND THE RUN ROOT ARE LINKS TOO. `-d`, the `*/` glob and `cd` all follow one,
# so `find .` starts inside the target and never sees what it walked through. Neither refusal had a
# control; the only symlink test put a link INSIDE a real directory.
mkdir -p "$TMP/outside"; printf 'family=famD\n' > "$TMP/outside/record"
ln -s "$TMP/outside" "$_EV/famD"
_symd_out="$(evidence_root_digest "$_EV" 2>&1)"; _symd_rc=$?
{ [ "$_symd_rc" != "0" ] && case "$_symd_out" in *family-is-symlink*) true ;; *) false ;; esac; } \
  && ok "a family DIRECTORY that is a symlink is refused" \
  || no "a linked family directory was accepted (rc=$_symd_rc out='$_symd_out')"
rm -f "$_EV/famD"
ln -s "$_EV" "$TMP/linkedroot"
_symr_out="$(evidence_root_digest "$TMP/linkedroot" 2>&1)"; _symr_rc=$?
{ [ "$_symr_rc" != "0" ] && case "$_symr_out" in *root-is-symlink*) true ;; *) false ;; esac; } \
  && ok "an evidence ROOT that is a symlink is refused" \
  || no "a linked evidence root was accepted (rc=$_symr_rc out='$_symr_out')"
rm -f "$TMP/linkedroot"

# ...and the same bytes in the same places still agree with themselves, or the digest is just noise.
mv "$_EV/famA/record" "$_EV/.swap" && mv "$_EV/famB/record" "$_EV/famA/record" && mv "$_EV/.swap" "$_EV/famB/record"
[ "$(evidence_root_digest "$_EV")" = "$_r_before" ] \
  && ok "restoring the contents restores the root" \
  || no "the root is not a function of the evidence"

echo "=== a record that contradicts itself is refused whether or not it claims qualification ==="
# This exact shape was published once: a single-family run wearing a full-campaign label. It carries
# qualified=0, so a coherence check that returns early for unqualified records accepts it.
sed -e 's/^selected=85$/selected=1/' -e 's/^executed=85$/executed=1/' -e 's/^reported=85$/reported=1/' \
    -e 's/^qualified=1$/qualified=0/' "$GOOD" > "$TMP/subset"
case "$(candidate_incoherent "$TMP/subset" "$POP" "$GATES")" in
  *campaign_mode_selected_subset*) ok "mode=campaign with selected<discovered is refused at qualified=0" ;;
  *) no "a single-family run may still describe itself as a campaign: '$(candidate_incoherent "$TMP/subset" "$POP" "$GATES")'" ;;
esac
sed -e 's/^mode=campaign$/mode=single/' -e 's/^selected=85$/selected=85/' "$GOOD" > "$TMP/badsingle"
case "$(candidate_incoherent "$TMP/badsingle" "$POP" "$GATES")" in
  *single_mode_selected*) ok "mode=single claiming 85 selected is refused" ;;
  *) no "single mode accepted an 85-family selection" ;;
esac
sed -e 's/^qualified=1$/qualified=0/' -e 's/^reported=85$/reported=86/' "$GOOD" > "$TMP/overreport"
case "$(candidate_incoherent "$TMP/overreport" "$POP" "$GATES")" in
  *reported_exceeds_executed*) ok "reporting more families than were executed is refused" ;;
  *) no "an invented report was accepted" ;;
esac
# The honest unqualified record must still pass, or the check above is just refusing everything.
[ -z "$(candidate_incoherent "$TMP/unqual" "$POP" "$GATES")" ] \
  && ok "an honest unqualified campaign is not refused by the coherence check" \
  || no "unconditional coherence refuses a well-formed unqualified record"

echo "=== family identity: the label and the experiment are separable ==="
# The driver is the producer of both, so it is asked rather than reimplemented here. Copies live in
# a scratch tree whose ROOT_DIR still resolves to this repository, so no gate writes the real tree.
_DRV="$ROOT_DIR/scripts/tests/check_gate_mutation_coverage.sh"
# The copy derives its own ROOT_DIR from its location, so the sandbox needs the same shape: a
# scripts/tests holding the driver and a lib/ it can source. Symlinking the real lib keeps the
# controls exercising the production decision library rather than a stale duplicate of it.
_SB="$TMP/sb/scripts/tests"; mkdir -p "$_SB"
ln -s "$ROOT_DIR/scripts/tests/lib" "$_SB/lib" 2>/dev/null || { echo "cannot build the identity sandbox" >&2; exit 2; }
_spec() { local out rc; out="$(bash "$1" --spec "$2" 2>/dev/null)"; rc=$?
          { [ "$rc" = "0" ] && [ -n "$out" ]; } || return 1; printf '%s' "$out"; }
cp "$_DRV" "$_SB/base.sh"
sed 's/^add "corecheck-unsafe-op"/add "corecheck-unsafe-op-RENAMED"/' "$_DRV" > "$_SB/renamed.sh"
sed '0,\|^add "corecheck-unsafe-op" "Concrete/Check/CoreCheck.lean"|s||add "corecheck-unsafe-op" "Concrete/Check/Elsewhere.lean"|' "$_DRV" > "$_SB/mutated.sh"
sed '0,/^add "copy-predicate"/s//add "corecheck-unsafe-op"/' "$_DRV" > "$_SB/dupe.sh"
_base="$(_spec "$_SB/base.sh" corecheck-unsafe-op)" || _base=""
_ren="$(_spec "$_SB/renamed.sh" corecheck-unsafe-op-RENAMED)" || _ren=""
_mut="$(_spec "$_SB/mutated.sh" corecheck-unsafe-op)" || _mut=""
if [ -z "$_base" ] || [ -z "$_ren" ] || [ -z "$_mut" ]; then
  no "the --spec producer failed; the identity controls below would prove nothing (base='$_base' ren='$_ren' mut='$_mut')"
else
  [ "$_base" = "$_ren" ] && ok "a renamed family keeps its mutation spec" \
                         || no "renaming changed the spec: $_base -> $_ren"
  [ "$_base" != "$_mut" ] && ok "a family that changed what it mutates gets a new spec" \
                          || no "the spec is blind to the mutation it names"
fi
_spec "$_SB/renamed.sh" corecheck-unsafe-op >/dev/null 2>&1 \
  && no "the pre-rename name still resolves" \
  || ok "the pre-rename name no longer names a family"
# The underscore prefix is reserved for run-level evidence, and the supervisor's evidence check
# SKIPS those directories — so a family able to take that prefix would be silently exempt from
# evidence reconciliation. The reservation is enforced, and here it is attacked.
sed '0,/^add "copy-predicate"/s//add "_sneaky"/' "$_DRV" > "$_SB/reserved.sh"
_res_out="$(bash "$_SB/reserved.sh" --spec corecheck-unsafe-op 2>&1)"; _res_rc=$?
{ [ "$_res_rc" = "2" ] && printf '%s' "$_res_out" | grep -q "may not begin with '_'"; } \
  && ok "a family claiming the reserved underscore prefix is refused" \
  || no "a family named '_sneaky' was accepted (rc=$_res_rc)"

_dupe_out="$(bash "$_SB/dupe.sh" --spec corecheck-unsafe-op 2>&1)"; _dupe_rc=$?
{ [ "$_dupe_rc" = "2" ] && printf '%s' "$_dupe_out" | grep -q 'duplicate names'; } \
  && ok "a duplicated family name is refused, not silently resolved to the first match" \
  || no "duplicate names accepted (rc=$_dupe_rc): $_dupe_out"

echo "=== qualification has ONE authority, not two ==="
# supervisor_qualification calls candidate_incoherent a second time to decide the published
# `qualified=` line. It was omitting the gate count, so a `1/1` baseline the first call refused could
# still be written as qualified=1 by the second. Production was safe only because the first call's
# refusal short-circuited this one — an ordering coincidence, not a property.
sed 's|^baseline_gates_green=.*|baseline_gates_green=1/1|' "$GOOD" > "$TMP/onegate"
[ "$(supervisor_qualification "$TMP/onegate" "" "$POP" "$GATES")" = "qualified=0" ] \
  && ok "a 1/1 baseline cannot be published as qualified even with no prior refusal" \
  || no "the qualification call qualified a 1/1 baseline: $(supervisor_qualification "$TMP/onegate" "" "$POP" "$GATES")"
[ "$(supervisor_qualification "$GOOD" "" "$POP" "$GATES")" = "qualified=1" ] \
  && ok "...and the honest record still qualifies through the same call (positive control)" \
  || no "the qualification call refused a well-formed record"
# The two authorities must agree on every record, not just this one.
for _f in "$GOOD" "$TMP/onegate" "$TMP/buildkills"; do
  _inc="$(candidate_incoherent "$_f" "$POP" "$GATES")"
  _qual="$(supervisor_qualification "$_f" "" "$POP" "$GATES")"
  case "$_inc:$_qual" in
    ":qualified=1"|?*":qualified=0") ;;
    *) no "the two qualification authorities disagree on $(basename "$_f"): incoherent='$_inc' published='$_qual'"; continue ;;
  esac
  ok "both authorities agree on $(basename "$_f")"
done

echo "=== an early failure releases the repository lock ==="
# The driver takes the lock, snapshots itself and re-execs — and `exec` CLEARS TRAPS. Every failure
# between the re-exec and the supervisor's own trap therefore exited holding the lock, and the next
# run refused to start against a repository where nothing was running. This is registered because I
# fixed it once already and confirmed the fix with a control that looked for the wrong lock filename.
# THE SANDBOX'S LOCK, NOT THE REPOSITORY'S.
#
# The copy derives ROOT_DIR from its own location, so it locks $TMP/sb — checking the repository's
# lock here observed a file this control never creates, and passed identically with the fix reverted.
# It is also the safe path: a control must not compete for the real repository lock.
_LOCK="$TMP/sb/.gate.lock"
if [ -e "$_LOCK" ]; then
  no "a lock is already present before this control runs; skipping rather than deleting it"
else
  cp "$_DRV" "$_SB/lockfail.sh"
  # The sandbox copy resolves ROOT_DIR to this repository, so it takes the REAL lock — which is
  # exactly what must be released. Injected failure: the decision library cannot be loaded.
  sed -i 's|scripts/tests/lib/campaign_supervise.sh" 2>/dev/null|scripts/tests/lib/NO_SUCH_LIBRARY.sh" 2>/dev/null|' "$_SB/lockfail.sh"
  _lf_out="$(bash "$_SB/lockfail.sh" 2>&1)"; _lf_rc=$?
  # THE STATUS IS ASSERTED, NOT PRINTED. This checked the message and the lock and never that the
  # run FAILED, so a driver that printed the expected fatal line, released its lock and exited zero
  # satisfied both assertions — a control about refusal that never checked for one.
  case "$_lf_rc:$_lf_out" in
    0:*) no "the injected failure exited 0; a control about refusing must require a refusal" ;;
    *"cannot load the campaign decision library"*)
      ok "a driver that cannot load its decision library refuses (rc=$_lf_rc)" ;;
    *) no "the injected failure did not occur, so the lock control proves nothing: $_lf_out" ;;
  esac
  if [ -e "$_LOCK" ]; then
    no "the failed run STRANDED its lock: $(cat "$_LOCK/owner" 2>/dev/null | tr '\n' ' ')"
    rm -rf "$_LOCK"
  else
    ok "...and it released the lock rather than stranding it"
  fi
fi

# THE WIRING, NOT ONLY THE DECISIONS.
#
# Everything above attacks a decision function directly. None of it proves the supervisor ASKS. The
# reconciliation body used to live inline in the driver where no gate could enter it, so deleting a
# refusal's CALL SITE left every control here green — well-tested decisions nothing was obliged to
# consult. These controls run the REAL supervisor_reconcile_and_publish against a sandbox: a tamper
# that the body must refuse, asserted through its published artifact and its exit status.
. "$ROOT_DIR/scripts/tests/lib/treestate.sh" 2>/dev/null || true
WFAM=mint-missing-result-refusal

# A SANDBOX THAT IS A REAL REPOSITORY. The body takes its own tree observations with the ts_*
# producers; outside a repository every one of them is UNAVAILABLE and every control below would
# pass for the wrong reason. It carries production's ignores too — without them the artifact the
# body writes is itself untracked content, the tree "moves" between reconciliation and publication,
# and the run refuses over the sandbox rather than over the code under test.
_wire_sandbox() {
  local w="$TMP/wire.$1"
  mkdir -p "$w/scripts/tests" || return 1
  cp "$ROOT_DIR/scripts/tests/check_gate_mutation_coverage.sh" "$w/scripts/tests/" || return 1
  printf '%s\n' .mutation-campaign-summary .mutation-campaign-summary.partial \
    .mutation-campaign-summary.candidate '.mutation-campaign-summary.??????' \
    '.mutation-campaign-summary.partial.*' .mutation-evidence/ .gate.lock/ \
    launch out err 'mutcand.*' '*.b' > "$w/.gitignore" || return 1
  # '*.b' because the tampers below use `sed -i.b`, and an unignored backup file moves
  # ts_untracked — every case would then also carry a spurious untracked mismatch, which is
  # noise in exactly the place these controls read their answer.
  ( cd "$w" && git init -q && git add -A \
    && git -c user.email=h@h -c user.name=h commit -qm base ) >/dev/null 2>&1 || return 1
  printf '%s' "$w"
}

# A CANDIDATE WHOSE EVERY FIELD AGREES WITH WHAT THE SUPERVISOR WILL OBSERVE, built with the same
# producers the body uses so it cannot go stale. A committed fixture would carry a head that is
# wrong the moment anything is committed.
# THE DECLARED COUNT FOLLOWS THE INVENTORY. Writing 85 here made these controls fail the moment H-5
# added families — a denominator restated in a second place is a denominator that goes stale.
_wire_famcount() { family_set_from_driver "$1/scripts/tests/check_gate_mutation_coverage.sh" | grep -c .; }

_wire_populate() {
  local w="$1" run="$2" h t u ed root n
  n="$(_wire_famcount "$w")"
  h="$(ts_head "$w")"; t="$(ts_tracked "$w")"; u="$(ts_untracked "$w")"
  ed="$w/.mutation-evidence/$run/$WFAM"; mkdir -p "$ed"
  cat > "$ed/verdict.txt" <<EOF
family=$WFAM
selector=FAMILY=85
array_index=84
file=scripts/tests/check_dependency_edges.sh
gate=check_mint_missing_result.sh
disposition=killed
killed=1
invalid=0
build_required=no
expected_route=gate
head=$h
run_id=$run
verdict=(killed by check_mint_missing_result.sh; reproduced red/green/red)
EOF
  cat > "$w/launch" <<EOF
protocol_version=1
run_id=$run
child_rc=0
child_signalled=0
child_signal=0
process_group_state=empty
pgid=0
EOF
  root="$(evidence_root_digest "$w/.mutation-evidence/$run")"
  cat > "$w/.mutation-campaign-summary.candidate" <<EOF
completed=1
mode=single
discovered=$n
selected=1
executed=1
reported=1
killed=1
invalid=0
survived=0
could_not_apply=0
integrity_ok=1
qualified=0
families_declared=$n
families_run=1
killed_by_gate=1
killed_by_build=0
failed=0
evidence_written=1
baseline_gates_green=1
gates_proven=1/1
refusals=
secs_total=1
secs_copy=1
secs_build=1
secs_gate=1
secs_other=1
run_id=$run
head=$h
tracked_sha=$t
untracked_sha=$u
workspace_head=$h
workspace_tracked_sha=$t
workspace_untracked_sha=$u
executed_driver_sha=$(ts_driver_digest "$w")
preamble_driver_sha=$(ts_driver_digest "$w")
repo_driver_sha=$(ts_driver_digest "$w")
inventory_sha=$(ts_inventory_digest "$w")
families_digest=$(family_set_digest "$(family_set_from_driver "$w/scripts/tests/check_gate_mutation_coverage.sh")")
evidence_dir=.mutation-evidence/$run
evidence_root=$root
baseline_compiler_sha=0123456789abcdef0123456789abcdef
compilers_tested=per-family-rebuilds
EOF
}

# THE PRODUCTION ENTRY POINT, in a subshell because the body exits rather than returning — it is the
# supervisor's last act.
_wire_run() {
  local w="$1" run="$2"
  ( ROOT_DIR="$w"; RUN_ID="$run"; EXPECTED_FAMILIES="$(_wire_famcount "$w")"; FAMILY="$WFAM"
    REFUSALS=""; SCOPE_NOTES=""
    _launch_report="$w/launch"; _launch_rc=0; _group_state="launched_state_unknown"
    _sup_head0="$(ts_head "$w")"; _sup_tracked0="$(ts_tracked "$w")"
    _sup_untracked0="$(ts_untracked "$w")"
    CAMPAIGN_DRIVER="$w/scripts/tests/check_gate_mutation_coverage.sh"
    export CONCRETE_MUT_DRIVER_SHA="$(ts_driver_digest "$w")"
    export CONCRETE_MUT_PREAMBLE_SHA="$(ts_driver_digest "$w")"
    export CONCRETE_MUT_PARTIAL=1
    supervisor_reconcile_and_publish ) >"$w/out" 2>"$w/err"
}

_wire_refusals() { sed -n 's/^supervisor_refusals=//p' "$1/.mutation-campaign-summary.partial" 2>/dev/null | head -1; }

# A CASE IS A TAMPER PLUS THE REFUSAL IT MUST PRODUCE. `_want` empty means "must publish cleanly".
_wire_case() {
  local tag="$1" want="$2" tamper="$3" w run rc refs
  w="$(_wire_sandbox "$tag")" || { no "wiring[$tag]: sandbox could not be built"; return; }
  run="$(ts_head "$w" | cut -c1-12)-20260101T000000-1234-AAAAAA"
  _wire_populate "$w" "$run"
  [ -z "$tamper" ] || eval "$tamper"
  _wire_run "$w" "$run"; rc=$?
  refs="$(_wire_refusals "$w")"
  if [ -z "$want" ]; then
    if [ "$rc" = "0" ] && [ "$refs" = "none" ]; then
      ok "wiring[$tag]: an honest run publishes cleanly through the production body"
    else
      no "wiring[$tag]: an honest run was refused (rc=$rc): $refs $(head -2 "$w/err" | tr '\n' ' ')"
    fi
    return
  fi
  case "$refs" in
    *"$want"*) [ "$rc" = "0" ] \
      && no "wiring[$tag]: refused with '$want' yet exited 0 — the refusal did not reach the status" \
      || ok "wiring[$tag]: $want" ;;
    *) no "wiring[$tag]: expected '$want', got rc=$rc refusals='$refs' $(head -2 "$w/err" | tr '\n' ' ')" ;;
  esac
}

# THE POSITIVE CONTROL FIRST. Without it every refusal below is satisfied by a body that refuses
# everything, which is the failure mode these controls are least able to notice on their own.
_wire_case clean "" ""

# Each of these is a call site inside the body. Neuter the call and the matching control goes red.
_wire_case provenance   inventory_sha_mismatch \
  'sed -i.b "s|^inventory_sha=.*|inventory_sha=deadbeefdeadbeefdeadbeefdeadbeef|" "$w/.mutation-campaign-summary.candidate"'
_wire_case runbinding   candidate_from_other_run \
  'sed -i.b "s|^run_id=.*|run_id=ffffffffffff-20200101T000000-1-ZZZZZZ|" "$w/.mutation-campaign-summary.candidate"'
_wire_case incoherence  candidate_incoherent \
  'sed -i.b "s|^reported=1$|reported=86|" "$w/.mutation-campaign-summary.candidate"'
_wire_case childexit    child_exit \
  'sed -i.b "s|^child_rc=0$|child_rc=1|" "$w/launch"'
# A REPORT NAMING ANOTHER RUN IS FATAL, NOT A REFUSAL RECORD. Measured: the body cannot describe a
# run it cannot identify, so it writes nothing and exits 2. The first version of this control expected
# a published refusal and failed — the control was wrong, not the body.
_wls="$(_wire_sandbox launchstale)"
if [ -n "$_wls" ]; then
  _wlsrun="$(ts_head "$_wls" | cut -c1-12)-20260101T000000-1234-AAAAAA"
  _wire_populate "$_wls" "$_wlsrun"
  sed -i.b 's|^run_id=.*|run_id=ffffffffffff-20200101T000000-1-ZZZZZZ|' "$_wls/launch"
  _wire_run "$_wls" "$_wlsrun"; _wlsrc=$?
  if [ "$_wlsrc" = "2" ] && grep -q 'stale_run_id' "$_wls/err" \
     && [ ! -e "$_wls/.mutation-campaign-summary.partial" ]; then
    ok "wiring[launchstale]: a report naming another run is fatal and publishes nothing"
  else
    no "wiring[launchstale]: rc=$_wlsrc artifact=$([ -e "$_wls/.mutation-campaign-summary.partial" ] && echo present || echo absent): $(head -1 "$_wls/err")"
  fi
else
  no "wiring[launchstale]: sandbox could not be built"
fi
_wire_case evidence     evidence_changed_after_census \
  'echo tampered >> "$w/.mutation-evidence/$run/$WFAM/verdict.txt"'
_wire_case familyset    family_set_mismatch \
  'sed -i.b "s|^families_digest=.*|families_digest=00000000000000000000000000000000000000000000000000000000000000ff|" "$w/.mutation-campaign-summary.candidate"'
_wire_case treedigest   workspace_head_differs \
  'sed -i.b "s|^workspace_head=.*|workspace_head=0000000000000000000000000000000000000000|" "$w/.mutation-campaign-summary.candidate"'

# A NON-EMPTY PROCESS GROUP FORBIDS PUBLICATION OUTRIGHT — this one is checked by absence of the
# artifact, not by a refusal string, because the body must not install a record at all.
_wgrp="$(_wire_sandbox group)"
if [ -n "$_wgrp" ]; then
  _wgrun="$(ts_head "$_wgrp" | cut -c1-12)-20260101T000000-1234-AAAAAA"
  _wire_populate "$_wgrp" "$_wgrun"
  sed -i.b 's|^process_group_state=empty$|process_group_state=nonempty|' "$_wgrp/launch"
  _wire_run "$_wgrp" "$_wgrun"; _wgrc=$?
  # A FAILURE RECORD IS STILL A RECORD: the body writes one naming the refusal rather than leaving a
  # reader with nothing to read. What it must never do is let that record claim qualification.
  _wgref="$(_wire_refusals "$_wgrp")"
  case "$_wgref" in
    *campaign_group_not_empty*) ok "wiring[group]: a surviving process group refuses qualification by name" ;;
    *) no "wiring[group]: the surviving group produced no named refusal: '$_wgref'" ;;
  esac
  case "$(sed -n 's/^qualified=//p' "$_wgrp/.mutation-campaign-summary.partial" 2>/dev/null | head -1)" in
    0) ok "wiring[group]: ...and the record it writes does not claim qualification" ;;
    *) no "wiring[group]: a record was published claiming qualification with a surviving group" ;;
  esac
  [ "$_wgrc" = "0" ] \
    && no "wiring[group]: refused publication yet exited 0" \
    || ok "wiring[group]: ...and the refusal reaches the exit status"
else
  no "wiring[group]: sandbox could not be built"
fi

# THE INVENTORY IS NAMED, NOT INFERRED. Unset CAMPAIGN_DRIVER must refuse rather than fall back to
# $0 — the fallback read this gate's own path and reported an inventory of zero families.
_wnam="$(_wire_sandbox noname)"
if [ -n "$_wnam" ]; then
  _wnrun="$(ts_head "$_wnam" | cut -c1-12)-20260101T000000-1234-AAAAAA"
  _wire_populate "$_wnam" "$_wnrun"
  ( ROOT_DIR="$_wnam"; RUN_ID="$_wnrun"; EXPECTED_FAMILIES="$(_wire_famcount "$_wnam")"; FAMILY="$WFAM"
    REFUSALS=""; SCOPE_NOTES=""
    _launch_report="$_wnam/launch"; _launch_rc=0; _group_state="launched_state_unknown"
    _sup_head0="$(ts_head "$_wnam")"; _sup_tracked0="$(ts_tracked "$_wnam")"
    _sup_untracked0="$(ts_untracked "$_wnam")"
    unset CAMPAIGN_DRIVER
    export CONCRETE_MUT_PARTIAL=1
    supervisor_reconcile_and_publish ) >/dev/null 2>"$_wnam/err2"
  _wnrc=$?
  if [ "$_wnrc" = "2" ] && grep -q CAMPAIGN_DRIVER "$_wnam/err2"; then
    ok "wiring[noname]: an unnamed inventory is a refusal, not a fallback to \$0"
  else
    no "wiring[noname]: expected rc=2 naming CAMPAIGN_DRIVER, got rc=$_wnrc: $(head -1 "$_wnam/err2")"
  fi
else
  no "wiring[noname]: sandbox could not be built"
fi

# EVERY PUBLISHED FIELD HAS A DECLARED AUTHORITY STATUS, AND TWO OF THEM ESTABLISH NOTHING.
#
# A reader who sees `baseline_compiler_sha=<32 hex>` in a qualified record will take it to mean the
# supervisor knows which compiler ran. It does not. That compiler existed only inside a disposable
# workspace the supervisor never entered, and the workspace is deleted before reconciliation, so the
# field is child-reported and unobservable — well-formedness is the ONLY property that can be
# checked. The library says so; these controls make the saying enforceable.
_cls_all="$CAMPAIGN_FIELDS_OBSERVED $CAMPAIGN_FIELDS_CROSSFIELD $CAMPAIGN_FIELDS_NON_AUTHORITATIVE $CAMPAIGN_FIELDS_SEMANTIC"
_cls_missing="$(comm -23 <(printf '%s\n' $CAMPAIGN_SCHEMA_PUBLISHED | sort -u) <(printf '%s\n' $_cls_all | sort -u) | tr '\n' ' ')"
_cls_extra="$(comm -13 <(printf '%s\n' $CAMPAIGN_SCHEMA_PUBLISHED | sort -u) <(printf '%s\n' $_cls_all | sort -u) | tr '\n' ' ')"
_cls_dup="$(printf '%s\n' $_cls_all | sort | uniq -d | tr '\n' ' ')"
[ -z "$_cls_missing" ] \
  && ok "every published field has a declared authority status" \
  || no "published fields with NO declared authority status: $_cls_missing"
[ -z "$_cls_extra" ] \
  && ok "...and nothing is classified that the schema does not publish" \
  || no "classified but unpublished: $_cls_extra"
[ -z "$_cls_dup" ] \
  && ok "...and no field is claimed by two authority tiers at once" \
  || no "a field is in two tiers, so its status is ambiguous: $_cls_dup"

# THE NON-AUTHORITATIVE SET IS EXACTLY THE TWO UNOBSERVABLE COMPILER FIELDS. Pinning it means
# demoting a third field to shape-only is a visible edit here, not a quiet loss of authority.
case "$(printf '%s\n' $CAMPAIGN_FIELDS_NON_AUTHORITATIVE | sort | tr '\n' ' ')" in
  "baseline_compiler_sha compilers_tested ") ok "the non-authoritative set is exactly the two unobservable compiler fields" ;;
  *) no "the non-authoritative set changed: $CAMPAIGN_FIELDS_NON_AUTHORITATIVE" ;;
esac

# AND THE CLAIM THEY CANNOT SUPPORT: qualification must not move when they do.
#
# This is the control that gives the words above their meaning. If a future change made
# candidate_incoherent consult either field, a well-formed lie would start buying qualification and
# this control would go red — which is the whole reason the fields are labelled rather than trusted.
# Run on $GOOD, the QUALIFIED fixture: on an already-refused record every check returns the same
# refusals whatever the compiler fields say, and the qualification-specific branch never executes —
# the comparison would be true and meaningless. This one has to travel the qualified=1 path.
grep -q '^qualified=1$' "$GOOD" \
  && ok "the invariance fixture is qualified=1, so the qualification-specific checks actually run" \
  || no "the fixture is not qualified, so the invariance comparison below would be vacuous"
_NA="$TMP/nonauth"; cp "$GOOD" "$_NA"
_na_before="$(candidate_incoherent "$_NA" 85 "$GATES")"; _na_rc=$?
sed -i.bak 's|^baseline_compiler_sha=.*|baseline_compiler_sha=ffffffffffffffffffffffffffffffff|' "$_NA" && rm -f "$_NA.bak"
sed -i.bak 's|^compilers_tested=.*|compilers_tested=single-baseline|' "$_NA" && rm -f "$_NA.bak"
_na_after="$(candidate_incoherent "$_NA" 85 "$GATES")"; _na_rc2=$?
if [ "$_na_rc" = "$_na_rc2" ] && [ "$_na_before" = "$_na_after" ]; then
  ok "qualification does not change when the non-authoritative compiler fields do"
else
  no "qualification CONSULTED a non-authoritative field: rc $_na_rc->$_na_rc2, '$_na_before' -> '$_na_after'"
fi

# NON-AUTHORITATIVE IS NOT UNCHECKED. Both are still refused when malformed, so the label describes
# what the value means, not a hole where a check used to be. (A control above already proves the
# malformed cases fire; this one proves the well-formed substitution just made is still accepted by
# provenance, so the pair above compared two ACCEPTED records rather than two refused ones.)
sed -i.bak 's|^baseline_compiler_sha=.*|baseline_compiler_sha=ffffffffffffffffffffffffffffffff|' "$_PV" && rm -f "$_PV.bak"
sed -i.bak 's|^compilers_tested=.*|compilers_tested=single-baseline|' "$_PV" && rm -f "$_PV.bak"
[ -z "$(_pv)" ] \
  && ok "...and both substituted values are well-formed, so that comparison was between accepted records" \
  || no "the substitution was itself refused, making the comparison above vacuous: $(_pv)"

# THE POPULATION IS PINNED, NOT JUST THE FAILURE COUNT.
#
# Exiting on FAIL==0 alone means DELETING a control is indistinguishable from passing it: the gate
# reports fewer assertions and still exits green. This harness has already been bitten by exactly
# that — 240 probes were silently lost from another gate the same way — and this round's review
# found controls here that had been inert for rounds without anyone noticing. Changing the count is
# a deliberate act, recorded in the same commit as the control that changed it.
EXPECTED_CONTROLS=169
_total=$((PASS + FAIL))
if [ "$_total" -ne "$EXPECTED_CONTROLS" ]; then
  echo "  FAIL this gate ran $_total controls, expected $EXPECTED_CONTROLS — one was added or removed"
  echo "       without updating EXPECTED_CONTROLS in the same commit."
  FAIL=$((FAIL + 1))
fi

echo "CAMPAIGN-SUPERVISOR: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
