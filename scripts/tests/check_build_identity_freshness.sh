#!/usr/bin/env bash
# IS THE EMBEDDED BUILD IDENTITY STILL WHAT THE SOURCES SAY? (R-0004 package 3.)
#
# `Concrete.buildIdentity` is a constant compiled into the binary, and every receipt binds it to name
# the compiler that produced the evidence. A STALE constant is the failure that matters: the binary
# would keep claiming the identity of an older build while running different code, which is precisely
# the false-provenance the value exists to prevent.
#
# So it is re-derived here and compared. This is the same discipline `ClassificationTable` is held to,
# and for the same reason: nothing else in R-0004 rests on a value nobody re-derives.
#
# BEHAVIOURAL, NOT SHAPE-CHECKING. An earlier control for compiler identity asserted only that three
# strings were still present in `Main.lean` — it never made identification fail and never observed
# whether issuance refused, so the production branch could have become unreachable while the strings
# remained. This compares VALUES: what the binary reports against what the sources produce.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"

# THE GENERATOR RUNS IN A PRIVATE COPY OF ITS INPUTS, never in this checkout (R-0546). It rewrites
# Concrete/BuildIdentity.lean in place (`cat >`, not atomic), and the probes below append to
# Concrete/Proof/Replay.lean and add Concrete/__InventoryProbe.lean. In a parallel pass every other
# gate's require_fresh_binary then rebuilt the shared compiler from those transient sources
# ("object file .../Concrete/Proof/Replay.olean does not exist"). The copy holds exactly the
# generator's inventory roots (Concrete/, Main.lean, lakefile.toml, lean-toolchain) and the generator.
SRC_COPY="$TMP/src"
mkdir -p "$SRC_COPY/scripts/gen"
cp -R Concrete "$SRC_COPY/Concrete" && cp Main.lean lakefile.toml lean-toolchain "$SRC_COPY/" \
  && cp scripts/gen/build_identity.sh "$SRC_COPY/scripts/gen/" \
  || { echo "FATAL: could not make a private copy of the identity inputs" >&2; exit 2; }
gen_identity() { bash "$SRC_COPY/scripts/gen/build_identity.sh" >/dev/null 2>&1; }
CBI="$SRC_COPY/Concrete/BuildIdentity.lean"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== the binary reports the identity its sources produce ==="

# What the COMPILER says, read off the production surface rather than out of the source file.
REPORTED="$("$BIN" examples/elf_header/src/main.con --report receipts 2>/dev/null \
            | grep -oE 'build:[0-9a-f]+' | head -1 | cut -d: -f2)"
# What the SOURCES say, re-derived now.
cp "Concrete/BuildIdentity.lean" "$TMP/committed.lean"
gen_identity
FRESH="$(grep -oE 'buildIdentity : String := "[0-9a-f]+"' "$CBI" \
         | grep -oE '[0-9a-f]{32}' | head -1)"
COMMITTED="$(grep -oE 'buildIdentity : String := "[0-9a-f]+"' "$TMP/committed.lean" \
             | grep -oE '[0-9a-f]{32}' | head -1)"
cp "$TMP/committed.lean" "$CBI"

if [ -n "$REPORTED" ] && [ -n "$FRESH" ] && [ -n "$COMMITTED" ]; then
  ok "all three values were readable (reported, committed, freshly derived)"
else
  no "a value was missing — reported='$REPORTED' committed='$COMMITTED' fresh='$FRESH'; every comparison below would be vacuous"
  GATE_DONE=1; echo "BUILD-IDENTITY-FRESHNESS: PASS=$PASS FAIL=$FAIL"; exit 1
fi

# 128 BITS, the project standard. An earlier compiler identity kept 64, and rehashing a truncated
# digest downstream cannot recover the entropy it discarded.
if [ "${#COMMITTED}" = "32" ]; then
  ok "the identity is 32 hex characters (128 bits), not a truncated digest"
else
  no "the identity is ${#COMMITTED} hex characters — an evidence identity must not be truncated"
fi
if grep -qE '^[0-9a-f]{32}$' <<<"$COMMITTED"; then
  ok "the identity is well-formed lowercase hex"
else
  no "the identity is not canonical hex: '$COMMITTED'"
fi

if [ "$COMMITTED" = "$FRESH" ]; then
  ok "the committed build identity matches a fresh derivation from the sources"
else
  no "STALE build identity: committed $COMMITTED, sources produce $FRESH — regenerate with scripts/gen/build_identity.sh. A receipt would name a compiler build that is not the one running."
fi
if [ "$REPORTED" = "$COMMITTED" ]; then
  ok "the running binary reports the committed identity (the constant reached the executable)"
else
  no "the binary reports $REPORTED but the source constant is $COMMITTED — the binary is older than the constant"
fi

echo "=== the inventory is complete, and its completeness is checked separately ==="

# FRESHNESS IS NOT COMPLETENESS, and conflating them is how a digest quietly narrows. Freshness asks
# "does the committed value match a fresh derivation" — and it keeps saying yes while the inventory
# shrinks, because both sides shrink together. These legs ask the other question: is the inventory
# still the whole compiler?
INV_COUNT_SRC="$(grep -oE 'buildIdentitySourceCount : Nat := [0-9]+' Concrete/BuildIdentity.lean | grep -oE '[0-9]+$' || true)"
INV_DIG_SRC="$(grep -oE 'buildIdentityInventoryDigest : String := "[0-9a-f]+"' Concrete/BuildIdentity.lean | grep -oE '[0-9a-f]{32}' || true)"

# Every `*.lean` under the compiler roots is either digested or NAMED as an exclusion. A file that is
# neither is a compiler source the identity says nothing about.
ALL_LEAN="$(find Concrete Main.lean -name '*.lean' -type f 2>/dev/null | LC_ALL=C sort -u | grep -c . || true)"
# The one declared exclusion is the generator's own output.
EXPECTED_LEAN=$(( ALL_LEAN - 1 ))
# Plus the two non-Lean roots that define the build: lakefile.toml and lean-toolchain.
EXPECTED_TOTAL=$(( EXPECTED_LEAN + 2 ))
if [ -n "$INV_COUNT_SRC" ] && [ "$INV_COUNT_SRC" = "$EXPECTED_TOTAL" ]; then
  ok "the inventory covers every compiler source: $INV_COUNT_SRC = $ALL_LEAN Lean files - 1 named exclusion + 2 build files"
else
  no "inventory incompleteness: the constant says $INV_COUNT_SRC sources but the roots hold $EXPECTED_TOTAL (${ALL_LEAN} Lean files, 1 named exclusion, 2 build files). A source outside the inventory is one the identity is silent about."
fi

# The generator's own output must be excluded BY NAME, and the exclusion must still be reachable. If
# the path moved, an inline filter would silently stop excluding and the value would never converge.
if grep -q '\["Concrete/BuildIdentity.lean"\]=' scripts/gen/build_identity.sh; then
  ok "the generator's output is excluded by a NAMED rule, not by an incidental filter"
else
  no "the named self-exclusion is gone from the generator — if it moved to a filter, a path change will silently stop excluding it and the digest will never reach a fixed point"
fi

# NON-VACUITY for the inventory digest: it must move when the FILE LIST moves, and it must NOT move
# for an ordinary edit. Without both halves, a constant would satisfy either one alone.
INV_PROBE="$TMP/inv_probe.lean"
cp "$CBI" "$TMP/inv.committed"
printf -- '-- transient inventory probe\n' > "$SRC_COPY/Concrete/__InventoryProbe.lean"
gen_identity
INV_DIG_MOVED="$(grep -oE 'buildIdentityInventoryDigest : String := "[0-9a-f]+"' "$CBI" | grep -oE '[0-9a-f]{32}' || true)"
rm -f "$SRC_COPY/Concrete/__InventoryProbe.lean"
cp "$TMP/inv.committed" "$CBI"
if [ -n "$INV_DIG_MOVED" ] && [ "$INV_DIG_MOVED" != "$INV_DIG_SRC" ]; then
  ok "adding a compiler source moves the inventory digest ($INV_DIG_SRC -> $INV_DIG_MOVED)"
else
  no "adding a compiler source did NOT move the inventory digest — the inventory is not being measured"
fi

echo "=== the identity moves when the compiler's sources move ==="

# NON-VACUITY. Without this, a generator that emitted a fixed string would satisfy every assertion
# above. A file the digest covers is perturbed, the value re-derived, and the file restored.
PROBE="$SRC_COPY/Concrete/Proof/Replay.lean"
cp "$PROBE" "$TMP/probe.orig"
printf -- '-- transient probe for check_build_identity_freshness.sh\n' >> "$PROBE"
gen_identity
MOVED="$(grep -oE '[0-9a-f]{32}' "$CBI" | head -1)"
cp "$TMP/probe.orig" "$PROBE"
cp "$TMP/committed.lean" "$CBI"
if [ "$MOVED" != "$COMMITTED" ]; then
  ok "changing a compiler source changes the build identity ($MOVED)"
else
  no "the build identity did not move when a compiler source changed — it is not content-derived"
fi
# ...and restoring the source restores the value, so the digest is a function of content and not of
# how many times the generator has run.
gen_identity
RESTORED="$(grep -oE '[0-9a-f]{32}' "$CBI" | head -1)"
cp "$TMP/committed.lean" "$CBI"
if [ "$RESTORED" = "$COMMITTED" ]; then
  ok "restoring the source restores the identity — it is a function of content alone"
else
  no "the identity did not return to $COMMITTED after restoring the source (got $RESTORED)"
fi

echo "=== nothing at run time depends on a non-portable tool ==="

# THE PORTABILITY REGRESSION THIS REPLACED. An earlier identity shelled out to `sha256sum` on
# `/proc/self/exe`; macOS has neither, and it is an active CI platform, so receipt issuance refused
# there outright.
#
# ASSERTED AGAINST CONCRETE'S OWN SOURCES, NOT AGAINST THE LINKED BINARY.
#
# The previous form ran `strings` over the compiled artifact. That measures the LINKAGE MODE of
# third-party code, not anything about Concrete: MEASURED on this machine, `libuv` — which Lean links
# — contains `/proc/self/exe`, and it is a shared object here, so the string is absent from the
# binary and the check passed. In CI the same dependency is linked statically, the string appears,
# and the check failed while reporting that "receipt issuance would refuse on macOS" — a conclusion
# about Concrete drawn from a fact about libuv. It was red on every CI run for that reason alone.
#
# The property that actually matters is that CONCRETE does not inspect its own executable at run
# time, and it is established by two checks that do not depend on how a dependency is linked: no
# non-comment use in Concrete's own sources, and — asserted above — that the reported identity is the
# committed constant, i.e. a value compiled in rather than derived by reading the binary.
# THE SCANNER MUST EXIST, or its silence is not evidence. Without this the loop below would strip
# nothing, find nothing, and report the property as established — a fail-open in the check whose
# whole job is to refuse.
_bi_strip='s{/-.*?-/}{}gs; s{--[^\n]*}{}g'
if ! command -v perl >/dev/null 2>&1; then
  no "perl is unavailable, so Lean block comments cannot be stripped and this scan cannot be trusted"
  _bi_src_hits="SCANNER-UNAVAILABLE"
else
_bi_src_hits=""
for _f in $(git ls-files 'Concrete/**/*.lean' 'Main.lean' 2>/dev/null); do
  # NO PIPELINE INTO `grep -q`. grep exits at the first match, perl dies of SIGPIPE and reports 141,
  # and `pipefail` then turns a SUCCESSFUL match into a failed command — which this `if` would read
  # as "no use found". That is the exact false negative recorded as a follow-up elsewhere in this
  # session; it would have made this check fail open. The text is captured, then matched.
  _bi_text="$(perl -0777 -pe "$_bi_strip" "$_f" 2>/dev/null)" || _bi_text=""
  case "$_bi_text" in
    */proc/self/exe*|*sha256sum*) _bi_src_hits="$_bi_src_hits $_f" ;;
  esac
done
fi
if [ -z "$_bi_src_hits" ]; then
  ok "no Concrete source uses /proc/self/exe or sha256sum outside a comment"
else
  no "Concrete sources use /proc/self/exe or sha256sum:$_bi_src_hits — receipt issuance would refuse on macOS"
fi
# NON-VACUITY: the check must actually fire on a real use, or it is a green line that proves nothing.
_bi_probe="$(mktemp)"
printf 'def f : String := "sha256sum /proc/self/exe"\n' > "$_bi_probe"
_bi_t="$(perl -0777 -pe "$_bi_strip" "$_bi_probe" 2>/dev/null)" || _bi_t=""
case "$_bi_t" in
  */proc/self/exe*) ok "CONTROL: the source scan detects a real use (it is not matching nothing)" ;;
  *) no "CONTROL FAILED: the source scan cannot see a use even when one is present" ;;
esac
printf '/-- sha256sum /proc/self/exe -/\ndef g : Nat := 0\n' > "$_bi_probe"
_bi_t="$(perl -0777 -pe "$_bi_strip" "$_bi_probe" 2>/dev/null)" || _bi_t=""
case "$_bi_t" in
  */proc/self/exe*) no "CONTROL FAILED: the scan counts a docstring mention as a use — the old false alarm returns" ;;
  *) ok "CONTROL: a mention inside a docstring is not counted as a use" ;;
esac
rm -f "$_bi_probe"

GATE_DONE=1
echo "BUILD-IDENTITY-FRESHNESS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
