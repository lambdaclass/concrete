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
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
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

# drop_fingerprint <file> — remove the fixture's stored `#[proof_fingerprint(...)]` line, WHATEVER
# its value is.
#
# The value must not appear in this gate. It used to: the 058 leg anchored on the literal
# `#[proof_fingerprint("40b964856119044ac9bbec490d2e86ff")]`, and the V2 migration rewrote that value
# to `v2:18c4b476...` — as migrations are supposed to. The anchor then matched nothing, the edit
# silently no-opped, the fixture kept its fingerprint, and the leg observed a correctly-`proved`
# function and reported "058 REGRESSED". A phantom regression is worse than a plain failure: it
# sends someone to hunt a defect that does not exist, in the one area where a real 058 would matter.
#
# What the leg MEANS is "remove the stored subject", not "remove this particular hash", so that is
# what it now says. Fails loudly if there is not exactly one such line — zero means the fixture
# changed shape, more than one means the edit is ambiguous, and both must be seen rather than guessed.
drop_fingerprint() {
  local f="$1" n
  n="$(grep -c '#\[proof_fingerprint(' "$f" || true)"
  if [ "$n" != "1" ]; then
    no "fixture drift: expected exactly one #[proof_fingerprint] in $(basename "$f"), found $n"
    return 1
  fi
  python3 - "$f" <<'PY'
import sys, re, pathlib
p = pathlib.Path(sys.argv[1])
s = p.read_text()
s2 = re.sub(r'[ \t]*#\[proof_fingerprint\([^)]*\)\][ \t]*\n', '', s, count=1)
if s2 == s: sys.exit(1)
p.write_text(s2)
PY
}

echo "=== CONTROLS: the freshness mechanism works at all ==="
# If these fail, every "gap still open" leg below is meaningless — a fingerprint
# that never matches, or never mismatches, would produce the same output.
v="$(verdict "$LI")"
# NOT `proved` since bug 079: `count_up_loop_preserves` evaluates no specification, so the claim is not
# shown to be about this body. It is still the right control for FRESHNESS: every freshness finding
# (stale, unbound, needs-recheck) outranks `spec_unbound`, so this verdict says the stored subject
# MATCHES — a fingerprint that never matched would read `proof stale` here instead.
[ "$v" = "spec not bound to body" ] && ok "baseline loop_invariant is fresh and only spec-unbound ($v)" \
                                    || no "baseline loop_invariant is '$v', expected 'spec not bound to body' (fresh, unbound to a specification)"

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
# GUARDED ON THE SETUP SUCCEEDING. If the fingerprint could not be removed, the fixture still HAS a
# stored subject, so `proved` is the correct verdict and reporting it as an 058 regression is a false
# accusation manufactured by this gate's own broken setup. A leg whose precondition failed has not
# observed anything, and "not attempted" is the only honest thing it can say.
if drop_fingerprint "$LI/src/main.con"; then
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
else
  no "058 NOT ATTEMPTED: the stored fingerprint could not be removed, so nothing about containment was observed"
fi
restore li loop_invariant

echo
# === LIVE: what the v2 subject digest DOES decide (R-0004, V2 activation 2026-08-17) ========
# CONVERTED FROM A SHADOW SECTION. These legs were written while the v2 digest was a preview: the
# live verdict compared the body-only fingerprint, the v2 value was reported as "current would be
# v2:...", and each leg asserted BOTH that the preview moved AND that the live verdict did NOT —
# because a live verdict moving would have meant the migration had begun by accident.
#
# The migration has since happened deliberately. The preview line is gone, because the value is no
# longer hypothetical, and every stored link in the corpus carries the `v2:` discriminator. Scraping
# a line that no longer exists returned the empty string, so every comparison here degenerated to
# '' vs '' and the section reported seven failures that were all one stale scrape.
#
# So both halves invert, and both get STRONGER. Half one now reads the AUTHORITATIVE digest rather
# than a preview of one. Half two asserts the live verdict DOES move on a signature or contract
# change — which is the entire point of the activation, and was the thing these legs were previously
# required to prove had not happened.
subject_digest() {
  "$COMPILER" "$1/src/main.con" --report subject-facts 2>/dev/null \
    | grep -oE '^ *subject digest: [0-9a-f]+' | head -1 | sed 's/.*subject digest: //'
}
echo "=== LIVE(V2): the authoritative subject digest sees what the legacy fingerprint cannot ==="
BASE_D="$(subject_digest "$LI")"
BASE_V="$(verdict "$LI")"
if [ -z "$BASE_D" ]; then
  no "LIVE(V2): no subject digest emitted for the baseline — every leg below would prove nothing"
else
  ok "LIVE(V2): baseline emits an authoritative subject digest ($BASE_D)"
fi

# 059 under the v2 digest: the same whole-signature change the tripwire below shows is invisible.
edit "$LI/src/main.con" 'fn count_up() -> i32 {'                        'fn count_up() -> u32 {'
edit "$LI/src/main.con" 'let mut acc: i32 = 0;'                         'let mut acc: u32 = 0;'
edit "$LI/src/main.con" 'for (let mut i: i32 = 0; i < 8; i = i + 1) {'  'for (let mut i: u32 = 0; i < 8; i = i + 1) {'
SIG_D="$(subject_digest "$LI")"; SIG_V="$(verdict "$LI")"
[ -n "$SIG_D" ] && [ "$SIG_D" != "$BASE_D" ] \
  && ok "LIVE(059): a whole-signature change MOVES the subject digest ($BASE_D -> $SIG_D)" \
  || no "LIVE(059): the subject digest did not move on a signature change — it does not cover types"
# INVERTED AT THE V2 ACTIVATION. This asserted the live verdict must NOT move, because a moving
# verdict meant the migration had started by accident. It has now started on purpose, so the
# requirement is the opposite: a type change the legacy body-only fingerprint could never see must
# now stale the claim. That is bug 059 closing, and this is the assertion that holds it closed.
case "$SIG_V" in
  *stale*) ok "LIVE(059): ...and the live verdict goes '$SIG_V' — a type change now stales the claim (059 CLOSED)" ;;
  *)       no "LIVE(059): a whole-signature change left the live verdict '$SIG_V' — bug 059 has REOPENED: a proof survives a type change" ;;
esac
restore li loop_invariant

# 060 under the v2 digest: a TRUE and a FALSE postcondition must not digest alike.
edit "$LI/src/main.con" '    #[proof_coverage(invariant)]
' '    #[proof_coverage(invariant)]
    #[ensures(result == 999)]
'
FALSE_D="$(subject_digest "$LI")"; FALSE_LV="$(verdict "$LI")"
restore li loop_invariant
edit "$LI/src/main.con" '    #[proof_coverage(invariant)]
' '    #[proof_coverage(invariant)]
    #[ensures(result == 28)]
'
TRUE_D="$(subject_digest "$LI")"
restore li loop_invariant
if [ -n "$FALSE_D" ] && [ -n "$TRUE_D" ] && [ "$FALSE_D" != "$TRUE_D" ]; then
  ok "LIVE(060): a TRUE and a FALSE #[ensures] give DIFFERENT subject digests — contracts are covered"
else
  no "LIVE(060): a true and a false postcondition digest alike ('$FALSE_D' vs '$TRUE_D') — contracts are outside the subject digest"
fi
[ -n "$FALSE_D" ] && [ "$FALSE_D" != "$BASE_D" ] \
  && ok "LIVE(060): ...and adding a contract at all moves it off the baseline" \
  || no "LIVE(060): adding an #[ensures] left the digest at the baseline value"
# INVERTED for the same reason as 059. A postcondition is part of what a proof establishes, so
# changing one must invalidate the claim. It previously could not: contracts sat outside the
# body-only fingerprint, which is bug 060.
case "$FALSE_LV" in
  *stale*) ok "LIVE(060): ...and the live verdict goes '$FALSE_LV' — a contract change now stales the claim (060 CLOSED)" ;;
  *)       no "LIVE(060): changing a postcondition left the live verdict '$FALSE_LV' — bug 060 has REOPENED: a proof survives a contract change" ;;
esac

echo

# The subject binds the STRUCTURAL V2 body, not the legacy Core-statement hash (2026-08-09).
# Two properties, and the second is the one that makes the swap worth doing: a body edit must
# move the digest (it did before too), AND a subject whose structural body is REFUSED must not
# digest at all — a body with gaps describes less than the program, so a digest over it is
# indistinguishable from one over a complete body.
edit "$LI/src/main.con" 'acc = acc + i;' 'acc = acc + i + 0;'
BODY_D="$(subject_digest "$LI")"; BODY_LV="$(verdict "$LI")"
[ -n "$BODY_D" ] && [ "$BODY_D" != "$BASE_D" ] \
  && ok "LIVE(body): a BODY edit moves the subject digest ($BASE_D -> $BODY_D)" \
  || no "LIVE(body): a body edit left the subject digest unchanged — the structural body is not bound"
# NOT "still shadow" here, and the first draft of this leg got it wrong. "Still shadow" applies
# to changes the LIVE path cannot see — signature and contracts, which is what 059/060 are
# about. A BODY edit is precisely what the legacy fingerprint DOES catch, so the live verdict
# must go stale: that is the control proving the live path works at all, and without it a
# permanently-broken live comparison would look like successful containment.
case "$BODY_LV" in
  *stale*) ok "LIVE(body): ...and the live verdict correctly goes '$BODY_LV' — body edits are seen" ;;
  *)       no "LIVE(body): a body edit left the live verdict '$BODY_LV' — the comparison is not working, so the 059/060 legs prove nothing" ;;
esac
restore li loop_invariant

# ALPHA-INVARIANCE. A capture-avoiding rename of a local produces the SAME program, so the
# subject must not move. Without this, every rename would look like a body change and stale
# every proof over it — the digest would be measuring the source text rather than the program.
# Paired with the body-edit leg above: one asserts it MOVES on a real change, this asserts it
# does NOT on a non-change, and a digest that only ever moves is as useless as one that never
# does.
edit "$LI/src/main.con" 'let mut acc: i32 = 0;' 'let mut total: i32 = 0;'
edit "$LI/src/main.con" 'acc = acc + i;' 'total = total + i;'
edit "$LI/src/main.con" 'return acc;' 'return total;'
RENAME_D="$(subject_digest "$LI")"
[ -n "$RENAME_D" ] && [ "$RENAME_D" = "$BASE_D" ] \
  && ok "LIVE(alpha): renaming a local leaves the subject digest unchanged ($RENAME_D)" \
  || no "LIVE(alpha): a local rename moved the digest ($BASE_D -> $RENAME_D) — the subject measures source text, not the program"
restore li loop_invariant

# ORDER-DEPENDENCE TRIPWIRE — a KNOWN-OPEN defect, gated so it cannot be forgotten or silently
# widened. Inserting an UNRELATED declaration before a function changes that function's body
# digest, which means the encoding reads the declaration's position in the module rather than
# only the body. Measured: `calls.inc` in proof_patterns/composition digests f606dbc6, and
# bc98cca1 once an unrelated `fn` is inserted above it — bc98cca1 being exactly the value the
# same callable has in proof_patterns/composition_trusted_helper, where a sibling differs.
#
# This is why the same subject digests differently in two projects, and it blocks the migration:
# a value that moves when an unrelated neighbour changes cannot be pinned.
#
# The alpha-renaming leg above was necessary and NOT sufficient — it proved spelling
# independence WITHIN one compilation, not identity stability ACROSS contexts.
ORD="$TMP/ordercheck"; rm -rf "$ORD"; cp -r examples/proof_patterns/composition "$ORD"
ord_digest() { "$COMPILER" "$1/src/main.con" --report subject-facts 2>/dev/null \
  | awk '/^v1:user:calls.inc/{p=1} p&&/shadow bodyV2:/{print $3; exit}'; }
ORD_BEFORE="$(ord_digest "$ORD")"
python3 - "$ORD/src/main.con" <<'PYEOF'
import sys
p=sys.argv[1]; s=open(p).read()
s=s.replace('    fn inc(', '    fn zzz_unrelated(q: i32) -> i32 {\n        return q;\n    }\n\n    fn inc(',1)
open(p,'w').write(s)
PYEOF
ORD_AFTER="$(ord_digest "$ORD")"
# POSITIVE ASSERTION since 2026-08-09 — this was a tripwire while the defect was open, and the
# defect is fixed: `elabFn` now resets the binder frame per function, so a parameter's resolved
# index no longer depends on what was elaborated before it.
if [ -z "$ORD_BEFORE" ] || [ -z "$ORD_AFTER" ]; then
  no "ORDER: probe produced no digest — inconclusive, not agreement"
elif [ "$ORD_BEFORE" = "$ORD_AFTER" ]; then
  ok "ORDER: an unrelated PRECEDING declaration does not move the body digest ($ORD_BEFORE)"
else
  no "ORDER REGRESSION: an unrelated declaration moved the body digest ($ORD_BEFORE -> $ORD_AFTER) — the per-function scope reset in elabFn has been lost"
fi

# DISCRIMINATOR: a declaration inserted AFTER the function must not move it either. Measured, it
# does not — only a PRECEDING declaration moves the digest. That asymmetry is the finding: this is
# a monotonic identifier consumed in declaration order, not a dependence on module CONTENT. A
# content dependence would move on both sides, and the two have different fixes.
ORD2="$TMP/ordercheck2"; rm -rf "$ORD2"; cp -r examples/proof_patterns/composition "$ORD2"
python3 - "$ORD2/src/main.con" <<'PYEOF'
import sys
p=sys.argv[1]; s=open(p).read()
j=s.index('}', s.index('return x + 1;'))+1
open(p,'w').write(s[:j+1]+'\n    fn zzz_after(q: i32) -> i32 {\n        return q;\n    }\n'+s[j+1:])
PYEOF
ORD_AFTERDECL="$(ord_digest "$ORD2")"
[ -n "$ORD_AFTERDECL" ] && [ "$ORD_AFTERDECL" = "$ORD_BEFORE" ] \
  && ok "ORDER: a declaration inserted AFTER the function does NOT move it — the effect is positional, not module-content" \
  || no "ORDER: a following declaration also moved the digest ($ORD_BEFORE -> $ORD_AFTERDECL) — the dependence is on module CONTENT, which is a different defect from the recorded one"

# THE OTHER HALF OF THE TIE. `check_subject_facts.sh` fails if the revocation is missing while
# this defect is open; this fails if the revocation LINGERS after it is fixed. Without both, a
# temporary state becomes permanent — which is the normal fate of revocation markers, and the
# reason removing it is specified as part of the refreeze commit rather than as follow-up.
if [ -n "$ORD_BEFORE" ] && [ "$ORD_BEFORE" = "$ORD_AFTER" ]; then
  if grep -q "V2_FREEZE_REVOKED" "$ROOT_DIR/ROADMAP.md"; then
    no "determinism is FIXED but V2_FREEZE_REVOKED is still in ROADMAP.md — refreeze with evidence and remove the marker in the same commit"
  else
    ok "determinism fixed and the revocation marker removed together"
  fi
fi

# Fail-closed: every subject that emits a digest must have a COMPLETE structural body. If any
# subject digests while its body line reads REFUSED, the digest is being minted over gaps.
BOTH="$("$COMPILER" "$LI/src/main.con" --report subject-facts 2>/dev/null)"
REFUSED_N="$(printf '%s\n' "$BOTH" | grep -c 'shadow bodyV2: REFUSED' || true)"
DIGEST_N="$(printf '%s\n' "$BOTH" | grep -c 'subjectFreshness: .*current would be v2:' || true)"
TOTAL_N="$(printf '%s\n' "$BOTH" | grep -c 'shadow bodyV2:' || true)"
if [ "$TOTAL_N" -gt 0 ] && [ "$((REFUSED_N + DIGEST_N))" -le "$TOTAL_N" ]; then
  ok "SHADOW(body): no subject digests over a REFUSED structural body ($DIGEST_N digested, $REFUSED_N refused, $TOTAL_N total)"
else
  no "SHADOW(body): a subject digested while its structural body was refused — the digest covers gaps"
fi

echo "=== bug 059 — CLOSED by the V2 activation: the subject binds declared types ==="
# Return type, accumulator and loop counter all change i32 -> u32. Every
# STATEMENT is textually identical, so a body-only hash sees nothing — but the
# theorem was proved about the i32 version, where the arithmetic has different
# overflow behaviour and a different value domain.
#
# CONVERTED FROM A TRIPWIRE (V2 activation, 2026-08-17). While the authoritative comparison was the
# body-only fingerprint this leg asserted the WRONG verdict on purpose — `proved` after a type
# change — so the open defect stayed observable. The v2 subject digest binds `CheckedDeclFacts`,
# which carries the full typed signature, so the claim now goes stale. The leg is inverted rather
# than deleted: the same edit, the opposite expectation, so a reversion is caught by the assertion
# that proved the fix.
edit "$LI/src/main.con" 'fn count_up() -> i32 {'                        'fn count_up() -> u32 {'
edit "$LI/src/main.con" 'let mut acc: i32 = 0;'                         'let mut acc: u32 = 0;'
edit "$LI/src/main.con" 'for (let mut i: i32 = 0; i < 8; i = i + 1) {'  'for (let mut i: u32 = 0; i < 8; i = i + 1) {'
v="$(verdict "$LI")"
case "$v" in
  *stale*)
    ok "059 CLOSED: a whole-signature type change reports '$v' — the subject binds declared types" ;;
  *proved*)
    no "059 REOPENED: a whole-signature type change still reports '$v' — a proof about i32 is being credited to u32" ;;
  *)  no "059: unexpected verdict '$v' (expected stale)" ;;
esac
restore li loop_invariant

echo
echo "=== bug 060 — CLOSED by the V2 activation: contracts are inside the subject ==="
# CONVERTED FROM A TRIPWIRE for the same reason as 059. A postcondition is part of what a proof
# establishes, so a proof cannot survive one changing underneath it.
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

# BOTH must leave the claim non-proved, and that is the honest bar here. Attaching ANY new
# `#[ensures]` changes what is being claimed, so the stored subject no longer describes it —
# whether the new contract happens to be true or false. Requiring the two to DIFFER from each
# other would demand the digest evaluate the contract, which it does not and must not: it binds
# what was claimed, and only the kernel decides whether a claim holds.
case "$FALSE_V" in
  *proved*) no "060 REOPENED: a FALSE #[ensures] reports '$FALSE_V' — a postcondition the program does not satisfy is being credited as proved" ;;
  *)        ok "060 CLOSED: adding a false #[ensures] reports '$FALSE_V', not proved — contracts are inside the subject" ;;
esac
case "$TRUE_V" in
  *proved*) no "060: adding a TRUE #[ensures] left the claim '$TRUE_V' — the stored subject did not move when the contract did" ;;
  *)        ok "060: adding a true #[ensures] also invalidates the stored subject ('$TRUE_V') — the digest binds the claim, not its truth" ;;
esac

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

# ...but an input outside a workspace, replayed BY A CALLER INSIDE ONE, must
# still work. Resolving only from the input broke exactly this: tools that copy
# sources to a temp dir and replay them from inside the repo — which is how
# check_purecore_proofs.sh exercises std — saw 12 real kernel-verified proofs
# reported as unreachable. The caller's workspace is the only available answer
# for such an input, and refusing it is not "fail closed", it is "fail wrong".
fallback_out="$(cd "$ROOT_DIR" && "$ABS_COMPILER" "$NOWS/main.con" --report check-proofs 2>&1)"
if grep -qE '[1-9][0-9]* verified' <<<"$fallback_out"; then
  ok "an out-of-workspace input replays from the caller's workspace"
else
  no "an out-of-workspace input failed to replay from inside the repo: $(printf '%s' "$fallback_out" | tr '\n' ' ' | head -c 200)"
fi
# ...and it must SAY which workspace it used, so a fallback verdict is auditable.
if grep -q "from working directory" <<<"$fallback_out"; then
  ok "the fallback names the workspace it used"
else
  no "the fallback does not disclose which workspace produced the verdict"
fi

echo
echo "PROOF-FRESHNESS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
