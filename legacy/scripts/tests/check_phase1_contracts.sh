#!/usr/bin/env bash
# Phase 1 source-contract VALIDATION ARTIFACT (ROADMAP Phase 1 #9).
#
# The single gate for the whole source-contract surface. It:
#   1. runs the two contract sub-gates (negatives+positive+hmac anchor, stability);
#   2. asserts one GOLDEN REPORT SNAPSHOT per contract failure class is
#      byte-identical (drift is real signal — a wording/behaviour change must be
#      reflected deliberately);
#   3. checks the README explains every fixture.
#
# Update snapshots deliberately:
#   UPDATE_PHASE1_SNAPSHOTS=1 bash scripts/tests/check_phase1_contracts.sh
#
# See examples/contract_negatives/README.md for the per-class explanation and
# Concrete/Proof/ProofSoundness.lean (R-22..R-28) for the soundness justification.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
SNAP_DIR="scripts/tests/phase1_snapshots"
README="examples/contract_negatives/README.md"
UPDATE="${UPDATE_PHASE1_SNAPSHOTS:-0}"
mkdir -p "$SNAP_DIR"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0; UPD=0

# one row per failure class: <class> <report-kind> <fixture-source>
CN="examples/contract_negatives"
CLASSES=(
  "precondition_callsite|contracts|$CN/precondition_callsite/src/main.con"
  "missing_postcondition|contracts|$CN/missing_postcondition/src/main.con"
  "weakened_postcondition|contracts|$CN/weakened_postcondition/src/main.con"
  "invalid_attribute|contracts|$CN/invalid_attribute/src/main.con"
  "invalid_invariant|contracts|$CN/invalid_invariant/src/main.con"
  "invalid_contract_expression|contracts|$CN/invalid_contract_expression/src/main.con"
  "spec_ghost_totality|contracts|$CN/spec_ghost_totality/src/main.con"
  "vacuous_contract|contracts|$CN/vacuous_contract/src/main.con"
  "assert_obligation|contracts|$CN/assert_obligation/src/main.con"
  "assume_taint|contracts|$CN/assume_taint/src/main.con"
  "duplicate_links|contracts|$CN/duplicate_links/src/main.con"
  "fabricated_proof|proof-status|$CN/fabricated_proof/src/main.con"
  "valid_complex_contract_scope|contracts|examples/contract_positive/valid_complex_contract_scope/src/main.con"
)

# Keep only the report entries belonging to modules the fixture DEFINES.
#
# A fixture with a Concrete.toml builds as a project, so `--report contracts` includes the
# standard library's contracts and the golden file ends up depending on every contract in std/.
# The attribution message below explains such a drift after it happens; this removes the
# dependency so it does not happen. Entries for other modules are dropped from the COMPARISON
# only — the report itself still shows them, so no coverage is lost from what a user sees.
#
# The report is entry-structured: `===` section headers, then qualified `module.fn` entries at
# column 0, each followed by indented detail. Keep a header always; keep an entry block iff its
# module is one of ours.
scope_to_own_modules(){ local ownmods="$1"
  awk -v own="$ownmods" '
    BEGIN { n = split(own, a, " "); for (i = 1; i <= n; i++) keepmod[a[i]] = 1; keep = 1 }
    /^===/                       { keep = 1; print; next }
    /^[A-Za-z_][A-Za-z0-9_]*\./ { split($0, p, "."); keep = (p[1] in keepmod) }
    keep { print }
  '; }

snap_one(){ local class="$1" kind="$2" src="$3"
  local golden="$SNAP_DIR/$class.$kind.txt" actual="$TMP/$class.$kind.txt"
  # Scope is in the FILENAME, not only in a comment. A golden named `.contracts.txt` reads as a
  # snapshot of the whole report; this one covers the fixture's own modules only, and someone
  # will eventually diff it against a full report and conclude the report lost content.
  local ownmods
  ownmods="$(grep -oE '^[[:space:]]*(pub )?mod [a-zA-Z0-9_]+' "$src" 2>/dev/null | awk '{print $NF}' | sort -u | paste -sd' ')"
  local isproject=0
  [ -f "$(dirname "$(dirname "$src")")/Concrete.toml" ] && isproject=1
  if [ "$kind" = "contracts" ] && [ "$isproject" = "1" ] && [ -n "$ownmods" ]; then
    golden="$SNAP_DIR/$class.$kind.own-modules.txt"
    "$COMPILER" "$src" --report "$kind" 2>&1 | scope_to_own_modules "$ownmods" > "$actual"
    # DEPENDENCY VISIBILITY. Scoping the comparison must not become scoping the REPORT: an
    # undischarged obligation inside stdlib code still affects this program, so it has to stay
    # visible even though it is not compared. Without this, the two could drift into agreement by
    # the report quietly losing dependency content — local stability masking a real regression.
    local fullrep depcount
    fullrep="$("$COMPILER" "$src" --report "$kind" 2>&1)"
    depcount="$(printf '%s\n' "$fullrep" | grep -cE "^[A-Za-z_][A-Za-z0-9_]*\." || true)"
    local owncount
    owncount="$(printf '%s\n' "$fullrep" | grep -cE "^($(printf '%s' "$ownmods" | tr ' ' '|'))\." || true)"
    if [ "$depcount" -gt "$owncount" ]; then
      echo "  ok   $class full report still shows dependency obligations ($((depcount - owncount)) entries beyond this fixture)"; PASS=$((PASS+1))
    else
      echo "  FAIL $class full report shows NO dependency obligations — scoping the comparison"
      echo "       has become scoping the report; stdlib obligations affecting this program are"
      echo "       now invisible. The golden is meant to be narrow, the REPORT is not."
      FAIL=$((FAIL+1))
    fi
  else
    "$COMPILER" "$src" --report "$kind" > "$actual" 2>&1
  fi
  if [ "$UPDATE" = "1" ]; then cp "$actual" "$golden"; echo "  UPD  $class ($kind)"; UPD=$((UPD+1)); return; fi
  if [ ! -f "$golden" ]; then echo "  FAIL $class — no golden snapshot (run UPDATE_PHASE1_SNAPSHOTS=1)"; FAIL=$((FAIL+1)); return; fi
  if cmp -s "$golden" "$actual"; then echo "  ok   $class ($kind) snapshot"; PASS=$((PASS+1));
  else
    # ATTRIBUTE THE DRIFT BEFORE PRINTING IT. A fixture with a Concrete.toml compiles as a
    # PROJECT, so `--report contracts` includes the standard library's contracts, and the golden
    # file transitively depends on every contract in the stdlib. Any stdlib contract edit
    # invalidates it — and the failure then points at the fixture, which is not where anything
    # changed. That mis-attribution cost real time once (H26): the drift was traced to the wrong
    # commit because the message said "assume_taint" and the cause was a change to
    # std/src/sha256.con three commits earlier.
    #
    # So: split the differing lines by whether they belong to a module the fixture DEFINES.
    local difflines fixturelines
    difflines="$(diff "$golden" "$actual" | grep -E '^[<>]' || true)"
    fixturelines=0
    if [ -n "$ownmods" ]; then
      # A report entry is `module.function`; count differing lines naming a module we define.
      fixturelines="$(printf '%s\n' "$difflines" | grep -cE "\b($(printf '%s' "$ownmods" | tr ' ' '|'))\." || true)"
    fi
    echo "  FAIL $class — snapshot drift"
    if [ "$isproject" = "1" ] && [ "$fixturelines" = "0" ]; then
      echo "    LIKELY NOT A FIXTURE REGRESSION: this fixture has a Concrete.toml, so it builds as"
      echo "    a project and its snapshot includes STDLIB contracts. No differing line names a"
      echo "    module this fixture defines ($ownmods), so look for a"
      echo "    recent change under std/ before looking at the fixture. See H26 / R-0475."
    fi
    diff -u "$golden" "$actual" | head -20 | sed 's/^/    /'; FAIL=$((FAIL+1)); fi; }

# THE INVARIANT, asserted directly rather than simulated. `scope_to_own_modules` exists so a
# project-mode fixture's golden cannot depend on the standard library. Checking that property on
# every run is stronger than a registered mutation: a mutation proves the gate reacts to one
# specific cause, while this proves the golden contains nothing it should not, whatever the cause.
#
# (The mutation was also run by hand once, and is what H26 was: adding a third `#[requires]` to
# `rotr` in std/src/sha256.con — no rebuild needed, std is `.con` source — moved the old 153-line
# golden and leaves the scoped 32-line one untouched.)
echo "=== snapshot scoping (a fixture golden must not depend on the stdlib) ==="
for row in "${CLASSES[@]}"; do
  IFS='|' read -r class kind src <<< "$row"
  [ "$kind" = "contracts" ] || continue
  eval "src=\"$src\""
  [ -f "$(dirname "$(dirname "$src")")/Concrete.toml" ] || continue
  own="$(grep -oE '^[[:space:]]*(pub )?mod [a-zA-Z0-9_]+' "$src" 2>/dev/null | awk '{print $NF}' | sort -u | tr '\n' '|' | sed 's/|$//')"
  foreign="$(grep -E '^[A-Za-z_][A-Za-z0-9_]*\.' "$SNAP_DIR/$class.$kind.own-modules.txt" 2>/dev/null | grep -vE "^($own)\." | sort -u || true)"
  if [ -n "$foreign" ]; then
    echo "  FAIL $class golden contains entries from modules it does not define:"
    printf '%s\n' "$foreign" | head -5 | sed 's/^/      /'
    echo "      A stdlib change will silently drift this golden (H26). See scope_to_own_modules."
    FAIL=$((FAIL+1))
  else
    echo "  ok   $class ($kind) golden contains only its own modules — stdlib-independent"
    PASS=$((PASS+1))
  fi
done

echo "=== per-class report snapshots ==="
for row in "${CLASSES[@]}"; do IFS='|' read -r class kind src <<< "$row"; snap_one "$class" "$kind" "$src"; done

# stability class: the artifact is a `diff`, not a --report. Capture the diff text.
echo "=== contract_stability (diff) snapshot ==="
"$COMPILER" snapshot examples/contract_stability/v1.con -o "$TMP/v1.json" >/dev/null 2>&1
"$COMPILER" snapshot examples/contract_stability/v2.con -o "$TMP/v2.json" >/dev/null 2>&1
sdiff_golden="$SNAP_DIR/contract_stability.diff.txt"; sdiff_actual="$TMP/contract_stability.diff.txt"
"$COMPILER" diff "$TMP/v1.json" "$TMP/v2.json" > "$sdiff_actual" 2>&1
if [ "$UPDATE" = "1" ]; then cp "$sdiff_actual" "$sdiff_golden"; echo "  UPD  contract_stability (diff)"; UPD=$((UPD+1));
elif [ ! -f "$sdiff_golden" ]; then echo "  FAIL contract_stability — no golden (run UPDATE_PHASE1_SNAPSHOTS=1)"; FAIL=$((FAIL+1));
elif cmp -s "$sdiff_golden" "$sdiff_actual"; then echo "  ok   contract_stability (diff) snapshot"; PASS=$((PASS+1));
else echo "  FAIL contract_stability — diff drift"; diff -u "$sdiff_golden" "$sdiff_actual" | head -20 | sed 's/^/    /'; FAIL=$((FAIL+1)); fi

if [ "$UPDATE" = "1" ]; then echo ""; echo "PHASE1-CONTRACTS: UPDATED=$UPD snapshots"; exit 0; fi

echo "=== README covers every fixture ==="
[ -f "$README" ] || { echo "  FAIL README missing: $README"; FAIL=$((FAIL+1)); }
if [ -f "$README" ]; then
  for row in "${CLASSES[@]}"; do IFS='|' read -r class _ _ <<< "$row"
    if grep -qF "$class" "$README"; then PASS=$((PASS+1)); else echo "  FAIL README does not mention '$class'"; FAIL=$((FAIL+1)); fi
  done
  grep -qF "contract_stability" "$README" && PASS=$((PASS+1)) || { echo "  FAIL README omits contract_stability"; FAIL=$((FAIL+1)); }
  echo "  ok   README present and covers all classes"
fi

echo "=== sub-gates (each runs once, as its own CI step) ==="
# DELEGATED, NOT RE-RUN (R-0546, scripts/tests/lib/delegated_gates.sh): check_contract_negatives.sh and check_contract_stability.sh are each its own CI
# step and its own command in the local full set, so re-running it here repeated every one of its
# assertions in the same run. This gate now asserts only that the step exists and is unconditional.
p1_ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
p1_no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
source scripts/tests/lib/delegated_gates.sh
delegated_gates_control p1_ok p1_no
require_delegated p1_ok p1_no check_contract_negatives.sh check_contract_stability.sh

echo ""
echo "PHASE1-CONTRACTS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
