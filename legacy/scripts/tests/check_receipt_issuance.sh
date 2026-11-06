#!/usr/bin/env bash
# PRODUCTION RECEIPT ISSUANCE (R-0004 package 3).
#
# Until 2026-08-16 `ProofEvidenceReceipt` was, honestly described, a well-tested helper type: the
# envelope was closed, the minting authority was closed, and NOTHING IN THE PIPELINE EVER MINTED ONE.
# `--report receipts` is the path from "the kernel accepted this" to a receipt, and this gate holds
# it to the two things that make issuance evidence rather than output:
#
#   * IT DERIVES NOTHING ITSELF. Each of the four inputs — the kernel's acceptance, the theorem's
#     classification, the subject digest, the dependency root — comes from the single producer that
#     already answers that question elsewhere, and each refuses on its own terms.
#   * EVERY WITHHOLDING NAMES ITS CAUSE. "No receipt" and "receipt withheld because X" are different
#     facts, and only the second is actionable.
#
# THE DRIFT FIXTURE IS THE LOAD-BEARING CONTROL, because issuance got this wrong on the day it
# landed: it minted four receipts for `elf_header_drifted`, two of them for `stale` claims whose
# bodies had changed since their proofs were linked. The token says the kernel accepted a THEOREM; it
# says nothing about whether that theorem still proves THIS body.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
# PRIVATE CHECKOUT (R-0546, lib/private_checkout.sh): this gate creates a relocation probe directory inside the checkout,
# which a full pass shares with every other gate; it runs in a private copy instead.
source "$ROOT_DIR/scripts/tests/lib/private_checkout.sh"
reexec_in_private_checkout "${BASH_SOURCE[0]}" "$@"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

receipts_of() { "$BIN" "$1" --report receipts 2>/dev/null || true; }

echo "=== a clean package issues, and the issuance is not vacuous ==="

ELF="$(receipts_of examples/elf_header/src/main.con)"
if grep -q 'Summary: 5 issued, 0 withheld' <<<"$ELF"; then
  ok "elf_header issues 5 receipts and withholds none"
else
  no "elf_header issuance moved: $(grep -o 'Summary:.*' <<<"$ELF" | head -1)"
fi
# EVERY RECEIPT NAMES WHAT IT BINDS. A receipt whose artifact, subject or root were absent would
# still be counted above, so the count alone proves nothing about content.
BOUND="$(grep -cE '^\s+\+ main\.[a-z_]+ — Examples\.ElfHeader\.Proofs\.[a-z_]+' <<<"$ELF" || true)"
DETAIL="$(grep -cE 'subject=[0-9a-f]{8}.* root=[0-9a-f]{8}.* edge=(body|contract)' <<<"$ELF" || true)"
if [ "$BOUND" = "5" ] && [ "$DETAIL" = "5" ]; then
  ok "all 5 receipts name a theorem artifact, a subject digest, a root and an edge kind"
else
  no "receipt content incomplete: $BOUND artifact lines, $DETAIL binding lines (expected 5/5)"
fi

echo "=== the composed authority verdict gates issuance ==="

# THE REGRESSION CONTROL. `elf_header_drifted` is a DIFFERENT program sharing every declaration name
# with `elf_header`. Two of its functions drifted from what their proofs were linked against, and one
# depends on them. Before the status gate, all three received receipts.
DRIFT="$(receipts_of examples/elf_header_drifted/src/main.con)"
STALE_HELD="$(grep -cE '^\s+- main\.(check_magic|check_version) \[claim_not_proved\].*stale' <<<"$DRIFT" || true)"
if [ "$STALE_HELD" = "2" ]; then
  ok "both drifted (stale) claims are WITHHELD, naming staleness as the cause"
else
  no "expected 2 stale withholdings in the drift fixture, got $STALE_HELD"
fi
if grep -qE '^\s+- main\.validate_header \[claim_not_proved\].*deps_not_current' <<<"$DRIFT"; then
  ok "the claim depending on the drifted pair is withheld as deps_not_current"
else
  no "validate_header no longer withheld — a claim resting on drifted callees would receive a receipt"
fi
# ...and issuance is not blanket-refusing the fixture, which would satisfy the two legs above without
# discriminating anything. The two leaf functions that did NOT drift still issue, which is the pinned
# corpus expectation: they have no outgoing edges and their bodies match their fingerprints.
if grep -q 'Summary: 2 issued, 3 withheld' <<<"$DRIFT"; then
  ok "the two undrifted leaves still issue (2 issued, 3 withheld) — the refusal is targeted"
else
  no "drift fixture issuance moved: $(grep -o 'Summary:.*' <<<"$DRIFT" | head -1)"
fi

# AN UNBOUND CLAIM CANNOT MINT. The kernel accepts these theorems; the claims have no stored subject
# digest, so freshness would compare a body with itself. `hmac_sha256` is the fixture where this once
# read "11 verified".
HMAC="$(receipts_of examples/hmac_sha256/src/main.con)"
if grep -q 'Summary: 0 issued, 11 withheld' <<<"$HMAC"; then
  ok "hmac_sha256 issues nothing: 11 unbound claims, all withheld"
else
  no "hmac_sha256 issuance moved: $(grep -o 'Summary:.*' <<<"$HMAC" | head -1)"
fi

echo "=== a trusted closure keeps its named boundary ==="

# Trust is an ASSUMPTION and it has to travel with the claim. A receipt that recorded a
# kernel-checked claim without saying part of it rests on an unproven boundary is exactly the
# laundering the qualification exists to prevent.
TRUST="$(receipts_of examples/proof_patterns/composition_trusted_helper/src/main.con)"
if grep -qE '^\s+\+ calls\.combine — .* ASSUMING calls\.dbl' <<<"$TRUST"; then
  ok "the receipt over a trusted boundary names it (combine ASSUMING calls.dbl)"
else
  no "the trusted boundary is missing from the receipt — trust was laundered into an unqualified claim"
fi
# TARGETED, not blanket: the sibling claim that assumes nothing must NOT acquire a qualification.
if grep -qE '^\s+\+ calls\.inc — [^ ]+$' <<<"$TRUST"; then
  ok "the unqualified claim stays unqualified (inc carries no ASSUMING)"
else
  no "calls.inc acquired a trust qualification it does not have"
fi

echo "=== issuance is a property of the program, not of where it sits ==="

# Receipts bind environment identities, and an identity that embedded a path would make every receipt
# un-replayable anywhere else — the opposite of what binding the environment is for. Relocated INSIDE
# the workspace, because a copy outside it cannot resolve the proof library from its own input and is
# refused for that separate reason, asserted below.
REL="$ROOT_DIR/.receipt-reloc-probe"
rm -rf "$REL"; mkdir -p "$REL"
cp -r examples/elf_header "$REL/elf_under_another_name"
HERE="$(receipts_of examples/elf_header/src/main.con | grep -E 'subject=|^\s+\+ ' || true)"
THERE="$(receipts_of "$REL/elf_under_another_name/src/main.con" | grep -E 'subject=|^\s+\+ ' || true)"
rm -rf "$REL"
if [ -z "$HERE" ]; then
  no "no receipt material to compare — the check below would be vacuous"
elif [ "$HERE" = "$THERE" ]; then
  ok "the same program issues byte-identical receipts from a different path and directory name"
else
  no "receipts DIFFER when the program moves — a path is reaching a receipt"
  diff <(printf '%s' "$HERE") <(printf '%s' "$THERE") | head -6 | sed 's/^/      /'
fi

# A PROGRAM OUTSIDE THE PROOF WORKSPACE MAY REPLAY BUT MAY NOT MINT, and says which. The workspace
# would come from wherever the caller stood, so the verdict is not a property of the program alone.
#
# THIS IS A MITIGATION, NOT A BINDING, and the difference is recorded because it bounds what these
# receipts are worth: nothing in a receipt identifies WHICH PROOF LIBRARY was replayed.
# `classificationSurfaceDigest` is compiled into the binary, so replaying against a workspace whose
# theorems differ would move no field of the receipt. Refusing a caller-resolved workspace is what
# currently stands in for that missing binding.
mkdir -p "$TMP/outside" && cp -r examples/elf_header "$TMP/outside/elf"
OUT_OF_WS="$(receipts_of "$TMP/outside/elf/src/main.con")"
if grep -q 'Summary: 0 issued' <<<"$OUT_OF_WS" \
   && grep -q 'resolved from the caller.s directory rather than from the input' <<<"$OUT_OF_WS"; then
  ok "a program outside the proof workspace issues nothing, naming the caller-resolved workspace"
else
  no "an out-of-workspace program minted, or did not name why not: $(grep -o 'Summary:.*' <<<"$OUT_OF_WS" | head -1)"
fi

echo "=== the report does not overclaim ==="

# ISSUANCE IS NOT STATUS, and the report must say which. Without a disclosure a reader takes
# "5 issued" for "5 claims are now receipt-backed". Both spellings are asserted, because the honest
# sentence differs by whether the receipts went anywhere: unstored receipts vanish with the process,
# and stored ones are bytes that get re-checked rather than believed.
if grep -q 'not stored. Pass --out' <<<"$ELF"; then
  ok "an unstored run discloses that nothing was written"
else
  no "the unstored disclosure is gone — 'issued' now reads as evidence in use"
fi
STORED="$("$BIN" examples/elf_header/src/main.con --report receipts --out "$TMP/store.txt" 2>/dev/null || true)"
if grep -q 'Stored 5 receipt(s)' <<<"$STORED"    && grep -q 'not evidence on its own' <<<"$STORED"; then
  ok "a stored run names the file AND states that a stored receipt is re-checked, not believed"
else
  no "the storage disclosure is missing or the store was not written"
fi

GATE_DONE=1
echo "RECEIPT-ISSUANCE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
