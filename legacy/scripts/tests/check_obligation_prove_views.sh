#!/usr/bin/env bash
# `concrete prove` as a ledger view (ROADMAP Phase 3 #16).
#
# `concrete prove` is a proof-authoring surface (lemma hints, theorem shapes,
# workspace scaffolding) — that tooling is legitimately prove-specific. But the
# OBLIGATION facts it reports must not be an independent truth source: they derive
# from the same ProofCore + VC discharge the unified ledger does. This gate proves
# `prove`'s obligation view is a faithful VIEW of the ledger:
#   - prove's function proof status == the ledger's `#prooflink` status;
#   - prove --show-obligation <id> reports the SAME stable id and status the
#     ledger carries for that obligation;
#   - the status vocabulary matches;
#   - the prove CLI and snapshot gates stay green (output parity).

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
# Per-run scratch (R-0546): a fixed /tmp path collides with a concurrent run of this gate.
GATE_TMP="$(mktemp -d)"; trap 'rm -rf "$GATE_TMP"' EXIT
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TD="examples/thesis_demo/src/main.con"
LOOP="examples/contract_negatives/loop_scope_adversarial/src/main.con"
SP="examples/evidence_classes/stale_proof/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# prove status ↔ ledger #prooflink status mapping.
pro_status(){ "$COMPILER" prove "$1" "$2" --json 2>/dev/null | python3 -c "import json,sys;print(json.load(sys.stdin)['status'])"; }
link_status(){ "$COMPILER" "$1" --report obligation-ledger --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(next((o['status'] for o in d['obligations'] if o['id']=='$2#prooflink'),'?'))"; }
map(){ case "$1" in proved) echo proved_by_lean;; stale) echo stale;; missing) echo missing;; blocked) echo unproven;; ineligible) echo ineligible;; trusted) echo trusted;; *) echo "$1";; esac; }

echo "=== prove function status == ledger #prooflink status ==="
for fn in main.parse_byte main.validate main.report; do
  p="$(pro_status "$TD" "$fn")"; l="$(link_status "$TD" "$fn")"
  [ "$(map "$p")" = "$l" ] && ok "$fn: prove=$p ↔ ledger=$l" || no "$fn: prove=$p maps to $(map "$p") ≠ ledger=$l"
done
sp_fn="$("$COMPILER" "$SP" --report obligation-ledger --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(next((o['function'] for o in d['obligations'] if o['kind']=='spec_drift'),''))")"
sp_p="$(pro_status "$SP" "$sp_fn" 2>/dev/null)"
[ -n "$sp_fn" ] && [ "$sp_p" = "stale" ] && ok "stale proof ($sp_fn): prove status = stale (↔ ledger spec_drift)" || no "stale proof: fn=$sp_fn prove status=$sp_p (want stale)"

echo "=== prove --show-obligation reports the ledger's stable id + status ==="
showid="$("$COMPILER" prove "$LOOP" adv.with_requires --show-obligation O1 --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(d['id']+'|'+d['status'])")"
ledid="$("$COMPILER" "$LOOP" --report obligation-ledger --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);o=next(o for o in d['obligations'] if o['id']=='adv.with_requires@16#O1');print(o['id']+'|'+o['status'])")"
[ -n "$showid" ] && [ "$showid" = "$ledid" ] && ok "show-obligation O1 == ledger ($showid)" || no "show-obligation drifted: prove=$showid ledger=$ledid"

echo "=== status vocabulary ==="
"$COMPILER" prove "$LOOP" adv.with_requires --show-obligation O1 --json 2>/dev/null | python3 -c "
import sys,json
ok={'proved_by_lean','proved_by_kernel_decision','proved_by_lean_replay','arithmetic_proved','solver_trusted','tested_by_oracle','runtime_checked','enforced','assumed','trusted','partial','stale','vacuous','missing','unproven','planned','counterexample','unknown','timeout','solver_error','ineligible'}
sys.exit(0 if json.load(sys.stdin)['status'] in ok else 1)" && ok "show-obligation status ∈ canonical vocabulary" || no "status outside vocabulary"

echo "=== prove CLI + snapshots stay green (output parity) ==="
# DELEGATED, NOT RE-RUN (R-0546, scripts/tests/lib/delegated_gates.sh): test_prove_cli.sh and check_snapshots.sh are each its own CI
# step and its own command in the local full set, so re-running it here repeated every one of its
# assertions in the same run. This gate now asserts only that the step exists and is unconditional.
source scripts/tests/lib/delegated_gates.sh
require_delegated ok no test_prove_cli.sh check_snapshots.sh

echo ""
echo "OBLIGATION-PROVE-VIEWS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
