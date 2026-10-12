#!/usr/bin/env bash
# CHECKER ACCEPTANCE EVIDENCE (R-0208): what a measured checker build must show before its digest may be
# added to the register (Concrete/Proof/CheckerPolicy.lean). Run as the LAST step of the CI job that
# already ran the hostile controls (check_checker_identity, check_kernel_recheck, check_checker_policy),
# so reaching it means those passed ON THIS BUILD. It adds what they do not:
#   - the exact checker identity under test (the candidate digest);
#   - the axiom inventory on this build (native trust classified, nothing escaping the allow-list);
#   - a fresh replay of the frozen claim inventory, compared ROW BY ROW with the accepted reference.
# It never registers anything. The register is edited by hand, citing this evidence; an unregistered
# build keeps reading `unknown`.
#
# Usage: scripts/tools/checker_acceptance_evidence.sh <out-dir>   (exit 0 = evidence complete and clean)
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
OUT="${1:?usage: checker_acceptance_evidence.sh <out-dir>}"
mkdir -p "$OUT"
REF="docs/verification/claim-inventory-v4.34.1.tsv"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
rc=0

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

bash scripts/tests/check_axiom_inventory.sh > "$OUT/axiom-inventory.log" 2>&1 || { echo "FAIL: axiom inventory"; rc=1; }

bash scripts/tools/claim_inventory.sh > "$OUT/claim-inventory.tsv" 2>"$OUT/claim-inventory.err" || true
if diff <(grep -v '^#' "$REF") <(grep -v '^#' "$OUT/claim-inventory.tsv") > "$OUT/claim-inventory.diff"; then
  INV=IDENTICAL
else
  INV=DIFFERS; rc=1
fi

{
  echo "checker:   $ID"
  echo "register:  $(cat "$OUT/register-status.txt")"
  echo "controls:  check_checker_identity, check_kernel_recheck, check_checker_policy passed earlier in this job"
  echo "axioms:    $(tail -1 "$OUT/axiom-inventory.log")"
  echo "inventory: $INV against $REF ($(grep -c . "$OUT/claim-inventory.diff") diff lines)"
} | tee "$OUT/SUMMARY.txt"
exit $rc
