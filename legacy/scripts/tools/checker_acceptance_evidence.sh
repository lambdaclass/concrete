#!/usr/bin/env bash
# CHECKER ACCEPTANCE EVIDENCE (R-0208): what a measured checker build must show before its digest may be
# added to the register (Concrete/Proof/CheckerPolicy.lean). SELF-CONTAINED: the artifact itself must
# demonstrate every requirement on THIS build, not lean on step order in some job. It records:
#   - the exact checker identity under test (the candidate digest);
#   - the hostile controls RUN HERE, with their logs: check_checker_identity, check_kernel_recheck,
#     check_checker_policy — and the identity the policy gate reports having tested must be the
#     identity captured above, or the controls describe some other build;
#   - the axiom inventory on this build (check_axiom_inventory.sh: native trust classified, nothing
#     escaping the allow-list);
#   - a fresh replay of the frozen claim inventory, compared ROW BY ROW with the accepted reference
#     (check_claim_inventory.sh).
# These five gates run ONLY here (R-0546): this step is their CI step, and the local full set runs it.
# It never registers anything. The register is edited by hand, citing this evidence; an unregistered
# build keeps reading `unknown`.
#
# Usage: scripts/tools/checker_acceptance_evidence.sh <out-dir>   (exit 0 = evidence complete and clean)
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../tests/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
OUT="${1:?usage: checker_acceptance_evidence.sh <out-dir>}"
mkdir -p "$OUT"
REF="docs/verification/claim-inventory-v4.34.1.tsv"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
rc=0

# ONE LOCK FOR THE WHOLE RUN, taken BEFORE the identity is captured: three of the gates below take the
# repository lock (require_fresh_binary), a second concurrent acquisition is refused, and the lock taken
# here is inherited by every child (re-entrant). Verifying freshness first also means the identity
# captured next is the identity of the binary the gates will run.
# shellcheck source=scripts/tests/lib/fresh.sh
source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
require_fresh_binary || { echo "FAIL: could not take the repository lock / verify the compiler"; exit 1; }

ID="$("$BIN" examples/elf_header/src/main.con --report check-proofs --json 2>/dev/null \
      | python3 -c 'import json,sys; print(json.load(sys.stdin).get("checker","UNIDENTIFIED"))' 2>/dev/null)"
echo "${ID:-UNIDENTIFIED}" > "$OUT/checker.txt"
case "$ID" in lean\ *\ build\ *) ;; *) echo "FAIL: no checker identity"; rc=1 ;; esac
BUILD="$(grep -oE 'build [0-9a-f]{64}$' <<<"$ID" | awk '{print $2}')"
if [ -n "$BUILD" ] && grep -q "build := \"$BUILD\"" Concrete/Proof/CheckerPolicy.lean; then
  echo "registered" > "$OUT/register-status.txt"
else
  echo "UNREGISTERED candidate" > "$OUT/register-status.txt"
fi

# THE ONLY RUN OF THESE GATES (R-0546). They used to run as their own CI steps AND again here, so every
# assertion they make ran twice per CI run (about 10 of this step's 11 minutes). They now run only
# here — this step is their CI step and their command in the local full set (run_ci_gates_local.sh no
# longer excludes it) — which keeps the artifact self-contained: the logs below are the run. They are
# independent of each other, so they run concurrently; each records its own exit status, and a
# missing status is a failure.
EVIDENCE_GATES=("check_checker_identity.sh" "check_kernel_recheck.sh" "check_checker_policy.sh"
                "check_axiom_inventory.sh" "check_claim_inventory.sh")
# NO STALE STATUS. A directory reused from an earlier run (or the runner's sequential re-run) must not
# lend a dead worker its predecessor's exit status or inventory.
for g in "${EVIDENCE_GATES[@]}"; do rm -f "$OUT/${g%.sh}.rc" "$OUT/${g%.sh}.log"; done
rm -rf "$OUT/claim-inventory" "$OUT/claim-inventory.tsv" "$OUT/claim-inventory.diff" "$OUT/axiom-inventory.log"
mkdir -p "$OUT/claim-inventory"
for g in "${EVIDENCE_GATES[@]}"; do
  ( grc=0
    CLAIM_INVENTORY_KEEP_DIR="$OUT/claim-inventory" bash "scripts/tests/$g" > "$OUT/${g%.sh}.log" 2>&1 || grc=$?
    echo "$grc" > "$OUT/${g%.sh}.rc" ) &
done
wait
gate_rc() { cat "$OUT/${1%.sh}.rc" 2>/dev/null || echo missing; }

CONTROLS=""
for g in check_checker_identity check_kernel_recheck check_checker_policy; do
  if [ "$(gate_rc "$g")" = "0" ]; then
    CONTROLS="$CONTROLS $g:$(grep -oE 'PASS=[0-9]+ FAIL=[0-9]+' "$OUT/$g.log" | tail -1)"
  else
    CONTROLS="$CONTROLS $g:FAILED"; echo "FAIL: control $g (exit $(gate_rc "$g"); log $OUT/$g.log)"; rc=1
  fi
done
TESTED="$(sed -n 's/^  info checker under test: //p' "$OUT/check_checker_policy.log" | head -1)"
# AN ACCEPTED BUILD MUST SHOW THE CONSUMER RE-CHECK RESTORING ACCEPTANCE ON ITSELF: a registered build is
# only evidenced if, on this very build, the forged store is accepted 0 of 5 without the consumer's
# re-check and 5 of 5 with it. An unregistered candidate shows the 0-of-5 side only.
if grep -q '^registered$' "$OUT/register-status.txt"; then
  RECHECK_LEG="$(grep -E 'ok .*accepted 5 of 5 once the consumer re-checks' "$OUT/check_checker_policy.log" | head -1)"
  [ -n "$RECHECK_LEG" ] || { echo "FAIL: registered build without the accepted-build consumer re-check leg"; rc=1; }
else
  RECHECK_LEG="$(grep -E 'ok .*0 of 5 without the consumer' "$OUT/check_checker_policy.log" | head -1)"
fi
if [ "$TESTED" != "$ID" ]; then
  echo "FAIL: the controls tested '$TESTED', not the captured identity"; rc=1
fi

cp "$OUT/check_axiom_inventory.log" "$OUT/axiom-inventory.log" 2>/dev/null
[ "$(gate_rc check_axiom_inventory)" = "0" ] || { echo "FAIL: axiom inventory (exit $(gate_rc check_axiom_inventory))"; rc=1; }

# The claim inventory is judged by check_claim_inventory.sh (row-by-row replay, env_failure, header
# counts and its own negative controls); it leaves the replayed rows and their diff here.
cp "$OUT/claim-inventory/inv.tsv" "$OUT/claim-inventory.tsv" 2>/dev/null || : > "$OUT/claim-inventory.tsv"
cp "$OUT/claim-inventory/diff" "$OUT/claim-inventory.diff" 2>/dev/null || : > "$OUT/claim-inventory.diff"
if [ "$(gate_rc check_claim_inventory)" = "0" ] && [ ! -s "$OUT/claim-inventory.diff" ] && [ -s "$OUT/claim-inventory.tsv" ]; then
  INV=IDENTICAL
else
  INV=DIFFERS; rc=1; echo "FAIL: claim inventory (exit $(gate_rc check_claim_inventory); log $OUT/check_claim_inventory.log)"
fi

{
  echo "checker:   $ID"
  echo "register:  $(cat "$OUT/register-status.txt")"
  echo "controls: $CONTROLS"
  echo "tested:    ${TESTED:-NONE} ($( [ "$TESTED" = "$ID" ] && echo 'matches the identity under test' || echo 'DOES NOT MATCH'))"
  echo "re-check:  ${RECHECK_LEG:-MISSING}"
  echo "axioms:    $(tail -1 "$OUT/axiom-inventory.log")"
  echo "inventory: $INV against $REF ($(grep -c . "$OUT/claim-inventory.diff") diff lines)"
} | tee "$OUT/SUMMARY.txt"
exit $rc
