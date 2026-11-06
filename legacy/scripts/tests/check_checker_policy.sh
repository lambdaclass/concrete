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
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
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
def ck (v : String) (b : String := "00ff") (c : String := s!"c-{v}") (p : String := "synthetic") : String :=
  s!"lean {v} commit {c} platform {p} build {b}"
def acc (v : String) : AcceptedBuild :=
  { checker := "lean", version := v, commit := s!"c-{v}", platform := "synthetic", build := "00ff", evidence := "test" }
def adv (id : String) (vs : List String) (st : AdvisoryStatus) : CheckerAdvisory :=
  { id, checker := "lean", versions := vs, status := st, affects := "kernel acceptance"
  , replacement := [], replay := "fresh re-check" }
def report (tag : String) (r : CheckerRegister) (c : String) : IO Unit := do
  let d := r.decide c
  IO.println s!"{tag} {d.status.canonical} [{",".intercalate d.advisories}] v{d.policyVersion} {d.policyIdentity}"
#eval show IO Unit from do
  let reg0 : CheckerRegister := { policyVersion := 1, accepted := [acc "4.29.0"], advisories := [] }
  let withAdv : CheckerRegister := { reg0 with policyVersion := 2, advisories := [adv "X-1" ["4.28.0"] .affected] }
  let revoked : CheckerRegister := { reg0 with policyVersion := 3, advisories := [adv "X-1" ["4.28.0"] .affected, adv "X-2" ["4.28.0"] .revoked] }
  let acceptedButNamed : CheckerRegister := { policyVersion := 4, accepted := [acc "4.28.0"], advisories := [adv "X-1" ["4.28.0"] .affected] }
  report "NONE-428" reg0 (ck "4.28.0")
  report "NONE-429" reg0 (ck "4.29.0")
  -- A MEASURED BUILD, not a version: the same version and commit with another build digest is a
  -- rebuild, a new candidate; another commit or platform under the same digest is not that build.
  report "REBUILD-429" reg0 (ck "4.29.0" (b := "11ee"))
  report "OTHERCOMMIT-429" reg0 (ck "4.29.0" (c := "c-other"))
  report "OTHERPLATFORM-429" reg0 (ck "4.29.0" (p := "elsewhere"))
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
              { withAdv with accepted := [] }.policyIdentity,
              { withAdv with accepted := [{ acc "4.29.0" with build := "11ee" }] }.policyIdentity]
  IO.println s!"IDS-DISTINCT {ids.eraseDups.length == ids.length}"
  -- ACCEPTED NOW is the conjunction, tested here on every platform.
  let acc : PolicyDecision := { status := .accepted, advisories := [], policyVersion := 1, policyIdentity := "x" }
  let unk : PolicyDecision := { acc with status := .unknown }
  IO.println s!"ACCNOW {acceptedNow acc .rechecked} {acceptedNow acc .notRun} {acceptedNow acc .native} {acceptedNow acc (.failed "x")} {acceptedNow unk .rechecked}"
  IO.println s!"REAL v{checkerRegister.policyVersion} {checkerRegister.policyIdentity}"
LEAN
P="$(lake env lean "$TMP/policy.lean" 2>&1 || true)"
line() { grep -E "^$1 " <<<"$P" | head -1; }
expect() { # tag status advisories label
  if grep -qE "^$1 $2 \[$3\] " <<<"$P"; then ok "$4"; else no "$4 — got: $(line "$1")"; fi
}
expect NONE-428 unknown "" "with no advisory, a build that is not explicitly accepted is unknown (never accepted)"
expect NONE-429 accepted "" "an explicitly accepted build is accepted (the positive control)"
expect REBUILD-429 unknown "" "a rebuild (same version and commit, different build digest) is a new candidate: unknown"
expect OTHERCOMMIT-429 unknown "" "a build reporting another commit is not the accepted build: unknown"
expect OTHERPLATFORM-429 unknown "" "the accepted digest on another platform is not the accepted build: unknown"
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

if grep -q '^ACCNOW true false false false false' <<<"$P"; then
  ok "accepted now requires BOTH an accepted checker and this run's re-check (only accepted+rechecked counts)"
else
  no "the accepted-now rule moved: $(grep '^ACCNOW' <<<"$P")"
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
# PLATFORM-AWARE, NOT CIRCULAR. Whether THIS build is accepted is read from the register SOURCE (its
# digest listed under `accepted`), not from `decide` — the function under test. A build the register
# does not list must read unknown; it is never accepted by commit. The identity is printed so CI's
# Linux digest can be captured, validated and then registered.
NOW="$(sed -n 's/^  Checker now: //p' <<<"$OUT" | head -1)"
NOW_BUILD="$(grep -oE 'build [0-9a-f]{64}$' <<<"$NOW" | awk '{print $2}')"
echo "  info checker under test: $NOW"
if [ -n "$NOW_BUILD" ] && grep -q "build := \"$NOW_BUILD\"" Concrete/Proof/CheckerPolicy.lean; then
  WANT=accepted
else
  WANT=unknown
fi
if grep -q '5 current, 0 not current, 0 unreadable' <<<"$OUT" \
   && [ "$(grep -c "checker policy: $WANT\$" <<<"$OUT")" = "5" ]; then
  ok "every current receipt reads checker policy: $WANT (this build's digest is $( [ $WANT = accepted ] && echo listed || echo NOT listed ) in the register), and currency is unchanged"
else
  no "the real decision or currency moved (wanted $WANT): $(grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$OUT" | head -1) $(grep -m1 'checker policy:' <<<"$OUT")"
fi
if [ -n "$REAL_ID" ] && grep -qE "Checker policy v$REAL_V \($REAL_ID\): " <<<"$OUT"; then
  ok "the report names the register that decided (v$REAL_V, $REAL_ID)"
else
  no "the report does not name the deciding register: $(grep 'Checker policy' <<<"$OUT")"
fi

echo "=== metadata edits cannot restore acceptance; only a fresh re-check under an accepted build can ==="

# Receipts are unauthenticated bytes. These legs forge them in every way short of a fresh re-check and
# require that acceptance moves only when the CONSUMER's own kernel re-check, under an accepted build,
# says so. `toolchainIdOf` is unkeyed, so a forger can recompute it; the probe below does exactly that.
accepted_now() { grep -oE 'ACCEPTED NOW \(current, re-checked in this run, accepted checker build\): [0-9]+ of [0-9]+' <<<"$1" | grep -oE '[0-9]+ of [0-9]+'; }
consume_store() { "$BIN" "$SRC" --report proof-status --receipts "$1" 2>/dev/null | sed -n '/=== Replay Receipts/,$p' || true; }
COMP="$(sed -n 's/^compiler //p' "$TMP/r.txt" | head -1)"
NOW_ID="$(sed -n 's/^checker //p' "$TMP/r.txt" | head -1)"
OLD_ID="lean 4.28.0 commit v4.28.0 platform arm64-apple-darwin build ba9da303f51c10ee6bab7ecb9374d3a8338b638162f9e16d462cea65e66ef832"
cat > "$TMP/tid.lean" <<LEAN
import Concrete
#eval IO.println (Concrete.Proof.toolchainIdOf "$COMP" "$OLD_ID")
#eval IO.println (Concrete.Proof.toolchainIdOf "$COMP" "$NOW_ID")
LEAN
TIDS="$(lake env lean "$TMP/tid.lean" 2>/dev/null)"
OLD_TID="$(sed -n 1p <<<"$TIDS")"; NOW_TID="$(sed -n 2p <<<"$TIDS")"
forge() { # $1 out, $2 checker, $3 toolchain id
  python3 - "$TMP/r.txt" "$1" "$2" "$3" <<'PY2'
import sys
src, out, ck, tid = sys.argv[1:]
lines = []
for l in open(src).read().split("\n"):
    if l.startswith("checker "): l = "checker " + ck
    elif l.startswith("toolchain ") and tid: l = "toolchain " + tid
    lines.append(l)
open(out, "w").write("\n".join(lines))
PY2
}
if [ -z "$OLD_TID" ] || [ -z "$NOW_TID" ] || [ -z "$NOW_ID" ]; then
  no "could not compute forged toolchain ids — every leg below would be vacuous"
else
  forge "$TMP/old.txt" "$OLD_ID" "$OLD_TID"
  O="$(consume_store "$TMP/old.txt")"
  if grep -q '0 current, 5 not current, 0 unreadable' <<<"$O"; then
    ok "a consistent receipt bound to the old 4.28.0 checker is not current, so nothing about it is accepted"
  else
    no "an old-checker receipt was not refused: $(grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$O" | head -1)"
  fi
  # THE DOWNGRADE IS ONE DIMENSION. The old checker's logical validity is what the advisory removes; the
  # subject, correspondence and dependency facts were established independently and must stay visible.
  if [ "$(grep -c 'differs in: toolchain; still matching: subject, edge, tables, root, artifact, trust, scope, context, imports' <<<"$O")" = "5" ]; then
    ok "each old-checker receipt differs ONLY in toolchain; subject, edge, tables, root, artifact, trust, scope, context and imports stay visibly matched"
  else
    no "the downgrade is not shown as toolchain-only: $(grep -m1 'differs in:' <<<"$O")"
  fi
  forge "$TMP/edit_ck.txt" "$NOW_ID" "$OLD_TID"
  O="$(consume_store "$TMP/edit_ck.txt")"
  if [ "$(grep -c 'unreadable \[identity_not_bound\]' <<<"$O")" = "5" ]; then
    ok "editing an old receipt's checker line to the accepted build is refused (identity_not_bound)"
  else
    no "an edited checker line was not refused: $(grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$O" | head -1)"
  fi
  forge "$TMP/edit_ver.txt" "${OLD_ID/4.28.0/4.34.1}" "$OLD_TID"
  O="$(consume_store "$TMP/edit_ver.txt")"
  if [ "$(grep -c 'unreadable \[identity_not_bound\]' <<<"$O")" = "5" ]; then
    ok "editing only the version string in an old receipt is refused (identity_not_bound)"
  else
    no "an edited version string was not refused: $(grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$O" | head -1)"
  fi
  # A FULLY CONSISTENT FORGERY naming the accepted build is byte-for-byte what issuance would write; its
  # acceptance must come from the consumer's re-check, so suppressing that re-check must accept nothing.
  forge "$TMP/forged.txt" "$NOW_ID" "$NOW_TID"
  mkdir -p "$TMP/norecheck"
  cat > "$TMP/norecheck/lake" <<EOF
#!/usr/bin/env bash
set -o pipefail
"$(command -v lake)" "\$@" 2>&1 | sed -E '/^CONCRETE-RECHECK /d'
EOF
  chmod +x "$TMP/norecheck/lake"
  O="$(PATH="$TMP/norecheck:$PATH" consume_store "$TMP/forged.txt")"
  if grep -q '5 current, 0 not current, 0 unreadable' <<<"$O" && [ "$(accepted_now "$O")" = "0 of 5" ]; then
    ok "a fully consistent forgery reads current but is accepted 0 of 5 without the consumer's own re-check"
  else
    no "forged metadata produced acceptance without a re-check: $(accepted_now "$O")"
  fi
  O="$(consume_store "$TMP/forged.txt")"
  if [ "$WANT" = accepted ] && [ "$(accepted_now "$O")" = "5 of 5" ]; then
    ok "...and the same store is accepted 5 of 5 once the consumer re-checks under the accepted build (the only route)"
  elif [ "$WANT" = unknown ] && [ "$(accepted_now "$O")" = "0 of 5" ]; then
    ok "...and on this unregistered build even the consumer's re-check accepts nothing (0 of 5): acceptance needs a registered build"
  else
    no "acceptance after a fresh re-check moved: $(accepted_now "$O") (build $WANT)"
  fi
fi

GATE_DONE=1
echo "CHECKER-POLICY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
