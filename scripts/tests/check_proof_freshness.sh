#!/usr/bin/env bash
# R-0004 slice 1 — executable witnesses for the proof-freshness defect class.
#
# Bugs 058, 059, 060 and 062 had numbered documents with replay transcripts, but
# a transcript in prose is not a reproducer: it cannot fail. This gate makes each
# one executable, so the defect is observable on every run and the eventual fix
# is observable too.
#
# It is a TRIPWIRE, and that needs saying plainly. Several legs below assert the
# CURRENT, WRONG verdict — a `proved` that should not be `proved`. They pass
# today because the bug is present. When a later slice fixes it, those legs FAIL,
# and that failure is the signal to move the leg from "gap open" to "gap closed",
# not a regression. This is the same shape as the `boundary.lean` leg in
# check_operational_vc_auto_discharge.sh.
#
# Every gap leg is paired with a CONTROL: a nearby edit that IS detected. Without
# the control, "reports proved" could just mean the harness never re-read the
# file, and the gate would be measuring nothing.
#
# Fixture policy: these drive the real `examples/loop_invariant` and
# `examples/crypto_verify` projects, copied to a temp dir and edited there. Real
# proof links with real stored fingerprints are the point — a synthetic fixture
# with a hand-written fingerprint would prove only that string comparison works.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

LI="$TMP/loop_invariant"
CV="$TMP/crypto_verify"
cp -r examples/loop_invariant "$LI"
cp -r examples/crypto_verify  "$CV"
cp "$LI/src/main.con" "$TMP/li.base"
cp "$CV/src/main.con" "$TMP/cv.base"

# verdict <project> [fn-line-hint] — the first status headline, e.g. "proved".
verdict() {
  "$COMPILER" "$1/src/main.con" --report proof-status 2>&1 \
    | grep -oE '^-- [a-z ]+(\[[a-z_]+\])?' | head -1 | sed 's/^-- //; s/ *$//'
}
# status_of <project> <qualified-name> — that function's status, looked up BY
# NAME via --report proof-deps / obligations rather than by source line.
# An earlier version indexed by line number and silently read `check_nonce`
# where it meant `verify_message`: the leg passed while asserting the wrong
# function. Line numbers are not identity (PRINCIPLES 12); the name is.
status_of() {
  "$COMPILER" "$1/src/main.con" --report obligations 2>&1 \
    | awk -v fn="  $2" '$0 == fn {found=1; next} found && /status:/ {print $2; exit}'
}
# edit <file> <old> <new> — records a FAIL if the anchor is gone. These fixtures
# are the REAL examples, so they can drift; a silent no-op would leave the gate
# asserting verdicts about an unmodified file and reporting all-green. Counting
# the miss here means drift names itself rather than surfacing as a confusing
# downstream verdict.
edit() {
  if python3 "$ROOT_DIR/scripts/tests/lib/replace_once.py" "$1" "$2" "$3"; then
    return 0
  else
    no "fixture drift: anchor not found in $(basename "$1") -- $(printf '%.55s' "$2")"
    return 1
  fi
}
restore() { cp "$TMP/$1.base" "$TMP/$2/src/main.con"; }

echo "=== CONTROLS: the freshness mechanism works at all ==="
# If these fail, every "gap still open" leg below is meaningless — a fingerprint
# that never matches, or never mismatches, would produce the same output.
v="$(verdict "$LI")"
[ "$v" = "proved [invariant]" ] && ok "baseline loop_invariant is proved ($v)" \
                                || no "baseline loop_invariant is '$v', expected 'proved [invariant]'"

edit "$LI/src/main.con" 'acc = acc + i;' 'acc = acc + i + 1000;'
v="$(verdict "$LI")"
[ "$v" = "proof stale" ] && ok "a STATEMENT edit stales the proof ($v)" \
                         || no "a statement edit gave '$v', expected 'proof stale' — the mechanism is not working"
restore li loop_invariant

echo
echo "=== bug 058 — CONTAINED (slice 2); this leg guards the containment ==="
# `#[proof_by]` with no `#[proof_fingerprint]` compared the current body with
# itself and stayed proved forever. It is now `unbound`: not proved, and
# deliberately not `stale` either, because nothing has been shown to change.
edit "$LI/src/main.con" '    #[proof_fingerprint("40b964856119044ac9bbec490d2e86ff")]
' ''
v="$(verdict "$LI")"
case "$v" in
  *unbound*) ok "a proof link with no stored digest is '$v', not proved" ;;
  *proved*)  no "058 REGRESSED: a proof link with no stored digest reports '$v'" ;;
  *)         no "058: unexpected verdict '$v' (expected unbound)" ;;
esac
OUT="$("$COMPILER" "$LI/src/main.con" --report proof-status 2>&1)"
grep -q "proof link unbound: no stored proof-subject digest" <<<"$OUT" \
  && ok "058 reports the exact unbound wording" \
  || no "058 lost its specified message 'proof link unbound: no stored proof-subject digest'"
restore li loop_invariant

echo
echo "=== bug 059 — GAP OPEN: the digest omits declared types ==="
# Return type, accumulator and loop counter all change i32 -> u32. Every
# STATEMENT is textually identical, so a body-only hash sees nothing — but the
# theorem was proved about the i32 version, where the arithmetic has different
# overflow behaviour and a different value domain.
edit "$LI/src/main.con" 'fn count_up() -> i32 {'                        'fn count_up() -> u32 {'
edit "$LI/src/main.con" 'let mut acc: i32 = 0;'                         'let mut acc: u32 = 0;'
edit "$LI/src/main.con" 'for (let mut i: i32 = 0; i < 8; i = i + 1) {'  'for (let mut i: u32 = 0; i < 8; i = i + 1) {'
v="$(verdict "$LI")"
case "$v" in
  *proved*)
    ok "TRIPWIRE(059): a whole-signature type change still reports '$v' — gap open, as recorded" ;;
  *stale*|*unbound*)
    no "TRIPWIRE(059) FIRED: type change now reports '$v'. Bug 059 is FIXED — move this leg to a positive assertion and update docs/bugs/059" ;;
  *)  no "059: unexpected verdict '$v'" ;;
esac
restore li loop_invariant

echo
echo "=== bug 060 — GAP OPEN: contracts are outside the digest ==="
# Body and types untouched; only the postcondition changes. A TRUE and a FALSE
# contract must not be indistinguishable — `result == 999` is false (the loop
# sums 0..7 = 28), and reporting it proved is a claim the compiler cannot back.
edit "$LI/src/main.con" '    #[proof_coverage(invariant)]
' '    #[proof_coverage(invariant)]
    #[ensures(result == 999)]
'
FALSE_V="$(verdict "$LI")"
restore li loop_invariant
edit "$LI/src/main.con" '    #[proof_coverage(invariant)]
' '    #[proof_coverage(invariant)]
    #[ensures(result == 28)]
'
TRUE_V="$(verdict "$LI")"
restore li loop_invariant

if [ "$FALSE_V" = "$TRUE_V" ]; then
  case "$FALSE_V" in
    *proved*) ok "TRIPWIRE(060): a TRUE and a FALSE #[ensures] are indistinguishable (both '$FALSE_V') — gap open, as recorded" ;;
    *)        no "060: true and false contracts agree on '$FALSE_V', which is not the recorded 'proved'" ;;
  esac
else
  no "TRIPWIRE(060) FIRED: false='$FALSE_V' vs true='$TRUE_V'. Bug 060 is FIXED — move this leg to a positive assertion and update docs/bugs/060"
fi

echo
echo "=== bug 062 — CLOSED by slice 3: containment propagates over the closure ==="
# crypto_verify is a real chain: verify_message -> verify_tag -> compute_tag.
# Stale ONLY the leaf; the two dependents are untouched and correctly bound.
edit "$CV/src/main.con" '    return key * message + nonce;' '    return key * message + nonce + 1;' \


LEAF="$(status_of "$CV" main.compute_tag)"      # edited
MID="$(status_of "$CV" main.verify_tag)"       # DIRECT dependent
TOP="$(status_of "$CV" main.verify_message)"   # TWO HOPS up
SIBLING="$(status_of "$CV" main.check_nonce)"  # unrelated to the edit
DEPS="$("$COMPILER" "$CV/src/main.con" --report proof-deps 2>&1)"

case "$LEAF" in
  *stale*) ok "the edited leaf itself is '$LEAF' — the chain's premise holds" ;;
  *)       no "the edited leaf is '$LEAF', expected stale; the 062 witness is not set up" ;;
esac
# The dependency EDGE is recorded — so this is not "the graph cannot see it".
grep -q "main.compute_tag (stale)" <<<"$DEPS" \
  && ok "the stale edge IS recorded in --report proof-deps" \
  || no "the stale edge is not even recorded, which contradicts bug 062's transcript"

# CLOSED by R-0004 slice 3. Both halves are now positive assertions: a stale
# dependency downgrades its dependent at one hop AND at two.
[ "$MID" = "deps_not_current" ] \
  && ok "the DIRECT dependent is contained ($MID)" \
  || no "the direct dependent is '$MID', expected deps_not_current — 062's direct half REGRESSED"
[ "$TOP" = "deps_not_current" ] \
  && ok "the TWO-HOP dependent is contained ($TOP) — containment is transitive" \
  || no "the two-hop dependent is '$TOP', expected deps_not_current — 062's transitive half REGRESSED"
# Containment must be targeted, not blanket: a function that reaches nothing
# stale keeps its proof. Without this, "everything is deps_not_current" would
# pass both legs above.
[ "$SIBLING" = "proved" ] \
  && ok "an unrelated function in the same module is still proved ($SIBLING)" \
  || no "an unrelated function became '$SIBLING' — containment is over-firing" 
# The transitive half is specifically that `top` shows NOTHING about the stale
# leaf — worse than showing it and ignoring it, because a reader of top's line
# sees an all-proved chain.
# The transitive half was specifically that `verify_message` showed NOTHING
# about the stale leaf — worse than showing it and ignoring it, because a reader
# of that line saw an all-proved chain. It must now name it.
if awk '/main\.verify_message \[/{f=1;next} f&&/^$/{exit} f' <<<"$DEPS" | grep -q "compute_tag (stale)"; then
  ok "the two-hop dependency block now names the stale leaf two hops down"
else
  no "the two-hop block still hides the stale leaf:
$(awk '/main\.verify_message \[/{f=1;next} f&&/^$/{exit} f' <<<"$DEPS")"
fi
restore cv crypto_verify

echo
echo "=== CONTROL: an untouched chain reports no stale dependencies ==="
# Proves the 062 legs above respond to the edit rather than always reporting it.
DEPS0="$("$COMPILER" "$CV/src/main.con" --report proof-deps 2>&1)"
grep -q "(stale)" <<<"$DEPS0" \
  && no "the UNEDITED chain already reports a stale edge — the 062 witness proves nothing" \
  || ok "the unedited chain has no stale edge"

echo
echo "=== R-0004 slice 4: the replay verdict does not depend on the caller's cwd ==="
# `lake` finds its workspace by walking up from where it is invoked, so kernel
# replay with no `cwd` answered according to where the user happened to stand:
# the same file by absolute path gave "3 verified, 0 failed" from the repo root
# and "0 verified, 3 failed" from /tmp — and blamed each theorem with
# `theorem_lookup`, sending the reader after the wrong thing entirely.
CDIR="$ROOT_DIR/examples/proof_patterns/composition/src/main.con"
ABS_COMPILER="$(cd "$(dirname "$COMPILER")" && pwd)/$(basename "$COMPILER")"
from_root="$("$ABS_COMPILER" "$CDIR" --report check-proofs 2>&1 | grep -oE '[0-9]+ verified, [0-9]+ failed' | tail -1)"
from_tmp="$(cd "$TMP" && "$ABS_COMPILER" "$CDIR" --report check-proofs 2>&1 | grep -oE '[0-9]+ verified, [0-9]+ failed' | tail -1)"
if [ -n "$from_root" ] && [ "$from_root" = "$from_tmp" ]; then
  ok "same verdict from the repo root and from elsewhere ($from_root)"
else
  no "the replay verdict moved with the working directory: root='$from_root' elsewhere='$from_tmp'"
fi

# An input with no workspace above it must SAY SO and fail closed, not report a
# pile of missing theorems that are not missing.
NOWS="$TMP/nows"; mkdir -p "$NOWS"; cp "$CDIR" "$NOWS/main.con"
nows_out="$(cd "$NOWS" && "$ABS_COMPILER" "$NOWS/main.con" --report check-proofs 2>&1)"
nows_rc=0; (cd "$NOWS" && "$ABS_COMPILER" "$NOWS/main.con" --report check-proofs >/dev/null 2>&1) || nows_rc=$?
if grep -q "cannot locate a Lake workspace" <<<"$nows_out"; then
  ok "a missing workspace is reported as a missing workspace"
else
  no "a missing workspace is not named; got: $(printf '%s' "$nows_out" | tr '\n' ' ' | head -c 200)"
fi
if grep -qi "theorem_lookup" <<<"$nows_out"; then
  no "a missing workspace is still blamed on the theorems (theorem_lookup)"
else
  ok "no theorem is blamed for a workspace that was never found"
fi
[ "$nows_rc" -ne 0 ] && ok "unreplayable input fails closed (rc=$nows_rc)" \
                     || no "unreplayable input exited 0 — replay must fail closed"

echo
echo "PROOF-FRESHNESS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
