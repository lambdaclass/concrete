#!/usr/bin/env bash
# CHECKER ADVISORY POLICY (R-0208 decisions 3 and 4).
#
# Whether the checker that established a receipt is still TRUSTED is decided by a register compiled
# into the consumer (Concrete/Proof/CheckerPolicy.lean), at read time, and reported BESIDE receipt
# currency — never written into the receipt. Transitions are exercised with SYNTHETIC identities and
# registers, as decided: "fresh replay under an accepted kernel restores acceptance" is reserved for
# the real upgraded-kernel integration test, not simulated here.
#
#   - an advisory moves exactly the receipts whose checker it names; withdrawing it moves them back;
#     unrelated receipts keep their decision (re-evaluation, not invalidation);
#   - revoked beats affected beats an explicit acceptance; a checker neither accepted nor named, or an
#     unparseable identity, is `unknown` — never accepted;
#   - every decision names the register (version + identity), and the identity moves when ANY field
#     of the register does;
#   - rollback: the compiled policyVersion may not fall below the separately owned minimum in
#     scripts/tests/fixtures/checker_policy_minimum (LIMITED: reverting both together is not refused);
#   - the real report shows the Lean #14576 decision on every current receipt and keeps currency
#     unchanged.
set -uo pipefail
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"
SRC="examples/elf_header/src/main.con"
MIN_FILE="scripts/tests/fixtures/checker_policy_minimum"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== policy transitions on synthetic identities ==="

cat > "$TMP/policy.lean" <<'LEAN'
import Concrete
open Concrete.Proof
def ck (v : String) : String := s!"lean {v} commit c-{v} platform synthetic build 00ff"
def adv (id : String) (vs : List String) (st : AdvisoryStatus) : CheckerAdvisory :=
  { id, checker := "lean", versions := vs, status := st, affects := "kernel acceptance"
  , replacement := [], replay := "fresh re-check" }
def report (tag : String) (r : CheckerRegister) (c : String) : IO Unit := do
  let d := r.decide c
  IO.println s!"{tag} {d.status.canonical} [{",".intercalate d.advisories}] v{d.policyVersion} {d.policyIdentity}"
#eval show IO Unit from do
  let reg0 : CheckerRegister := { policyVersion := 1, accepted := [("lean", "4.29.0")], advisories := [] }
  let withAdv : CheckerRegister := { reg0 with policyVersion := 2, advisories := [adv "X-1" ["4.28.0"] .affected] }
  let revoked : CheckerRegister := { reg0 with policyVersion := 3, advisories := [adv "X-1" ["4.28.0"] .affected, adv "X-2" ["4.28.0"] .revoked] }
  let acceptedButNamed : CheckerRegister := { policyVersion := 4, accepted := [("lean", "4.28.0")], advisories := [adv "X-1" ["4.28.0"] .affected] }
  report "NONE-428" reg0 (ck "4.28.0")
  report "NONE-429" reg0 (ck "4.29.0")
  report "ADV-428" withAdv (ck "4.28.0")
  report "ADV-429" withAdv (ck "4.29.0")
  report "ADV-430" withAdv (ck "4.30.0")
  report "REV-428" revoked (ck "4.28.0")
  report "ACCNAMED-428" acceptedButNamed (ck "4.28.0")
  report "GARBAGE" withAdv "not a checker identity"
  report "EMPTY" withAdv ""
  -- identity moves with ANY field
  let ids := [reg0.policyIdentity, withAdv.policyIdentity, { withAdv with policyVersion := 9 }.policyIdentity,
              { withAdv with advisories := [adv "X-1" ["4.28.0"] .revoked] }.policyIdentity,
              { withAdv with advisories := [adv "X-1" ["4.28.0", "4.27.0"] .affected] }.policyIdentity,
              { withAdv with accepted := [] }.policyIdentity]
  IO.println s!"IDS-DISTINCT {ids.eraseDups.length == ids.length}"
  IO.println s!"REAL v{checkerRegister.policyVersion} {checkerRegister.policyIdentity}"
LEAN
P="$(lake env lean "$TMP/policy.lean" 2>&1 || true)"
line() { grep -E "^$1 " <<<"$P" | head -1; }
expect() { # tag status advisories label
  if grep -qE "^$1 $2 \[$3\] " <<<"$P"; then ok "$4"; else no "$4 — got: $(line "$1")"; fi
}
expect NONE-428 unknown "" "with no advisory, a build that is not explicitly accepted is unknown (never accepted)"
expect NONE-429 accepted "" "an explicitly accepted build is accepted (the positive control)"
expect ADV-428 affected "X-1" "an advisory moves the build it names to affected and names itself"
expect ADV-429 accepted "" "an advisory for 4.28.0 leaves the accepted 4.29.0 receipt's decision unchanged (re-evaluation, not invalidation)"
expect ADV-430 unknown "" "a build neither accepted nor named stays unknown"
expect REV-428 revoked "X-1,X-2" "revoked beats affected"
expect ACCNAMED-428 affected "X-1" "an accepted build named by an advisory is not accepted"
expect GARBAGE unknown "" "an unparseable checker identity is unknown"
expect EMPTY unknown "" "an empty checker identity is unknown"
if grep -q '^IDS-DISTINCT true' <<<"$P"; then
  ok "the register identity moves when its version, an advisory's status or versions, or the accepted list changes"
else
  no "two different registers share an identity — a decision could not say which register made it"
fi

echo "=== rollback: the compiled register may not fall below the separately owned minimum ==="

REAL_V="$(grep -oE '^REAL v[0-9]+' <<<"$P" | grep -oE '[0-9]+$' || true)"
MIN_V="$(tr -dc '0-9' < "$MIN_FILE" 2>/dev/null || true)"
below() { [ -n "$1" ] && [ -n "$2" ] && [ "$1" -lt "$2" ]; }
if [ -z "$REAL_V" ] || [ -z "$MIN_V" ]; then
  no "could not read the compiled policyVersion ('$REAL_V') or the minimum ('$MIN_V') — the rollback check would be vacuous"
elif below "$REAL_V" "$MIN_V"; then
  no "ROLLBACK: the compiled register is policyVersion $REAL_V, below the minimum $MIN_V"
else
  ok "the compiled register (v$REAL_V) is at or above the separately owned minimum (v$MIN_V)"
fi
# The comparison itself must be able to refuse: a minimum one above the real version is a rollback.
if below "$REAL_V" "$((REAL_V + 1))"; then
  ok "the rollback comparison refuses a register one version below its minimum (self-test)"
else
  no "the rollback comparison cannot refuse — its passing above means nothing"
fi

echo "=== the real report: the decision beside currency ==="

"$BIN" "$SRC" --report receipts --out "$TMP/r.txt" >/dev/null 2>&1 || true
OUT="$("$BIN" "$SRC" --report proof-status --receipts "$TMP/r.txt" 2>/dev/null | sed -n '/=== Replay Receipts/,$p' || true)"
REAL_ID="$(grep -oE '^REAL v[0-9]+ [0-9a-f]+' <<<"$P" | awk '{print $3}')"
if grep -q '5 current, 0 not current, 0 unreadable' <<<"$OUT" \
   && [ "$(grep -c 'checker policy: affected (lean4#14576)' <<<"$OUT")" = "5" ]; then
  ok "every current receipt reads checker policy: affected (lean4#14576), and currency is unchanged"
else
  no "the real decision or currency moved: $(grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$OUT" | head -1)"
fi
if [ -n "$REAL_ID" ] && grep -q "Checker policy v$REAL_V ($REAL_ID): 0 accepted, 5 affected, 0 revoked, 0 unknown" <<<"$OUT"; then
  ok "the report names the register that decided (v$REAL_V, $REAL_ID)"
else
  no "the report does not name the deciding register: $(grep 'Checker policy' <<<"$OUT")"
fi

GATE_DONE=1
echo "CHECKER-POLICY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
