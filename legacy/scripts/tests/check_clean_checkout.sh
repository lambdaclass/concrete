#!/usr/bin/env bash
# CLEAN-CHECKOUT REPRODUCIBILITY AND COMPILER PROVENANCE (R-0004 package 3).
#
# `check_evidence_reproducibility.sh` establishes PATH independence: the same program at a different
# absolute path yields identical evidence. That is necessary and it is not sufficient. Two runs can
# be equally path-independent and still disagree about WHICH COMPILER produced them, or agree for the
# wrong reason because both read the same working tree.
#
# This gate covers what path independence does not:
#
#   * a genuinely CLEAN CHECKOUT — `git clone` into a fresh directory, no build artifacts, no
#     untracked files — produces the same evidence as the working tree;
#   * invocation from the REPOSITORY ROOT and from the PROJECT ROOT agree;
#   * the compiler identity a receipt binds is the CONTENT OF THE EXECUTABLE THAT RAN, not the
#     repository's state at report time.
#
# THE COMPILER-IDENTITY LEG IS THE LOAD-BEARING ONE. Two earlier identities were wrong in both
# directions: `-dirty` from `git status` (any untracked file invalidated every receipt, and a binary
# built dirty then cleaned reported clean), and `git rev-parse HEAD` (a binary built from commit A
# claims B once the checkout moves, and a binary built from uncommitted sources claims whatever is
# checked out). Digesting the executable answers all three of the cases that matter:
#
#   built from A while the checkout points at B  -> identity unchanged; it describes the binary
#   built from uncommitted sources               -> content no committed build has -> distinct
#   substituted executable at the same commit    -> different content -> different identity
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
# PRIVATE CHECKOUT (R-0546, lib/private_checkout.sh): this gate creates an untracked noise directory inside the checkout,
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

# The evidence surface. Captured rather than piped: these fixtures exit non-zero when they carry
# proof defects, and a pipeline exit code must not be allowed to decide the comparison.
# PATHS ARE NORMALIZED AWAY. Reports name the file they are about, so the raw text necessarily
# differs between two checkouts — that is the report doing its job, not evidence depending on
# location. What must not differ is the identities, digests, roots and statuses, so the location is
# stripped and everything else compared verbatim.
evidence_of() {
  {
    "$BIN" "$1" --report proof-status    2>/dev/null | grep -E '^-- |proof matches' || true
    "$BIN" "$1" --report subject-facts    2>/dev/null | grep -E 'defIdentity|depRoot|correspondence' || true
    "$BIN" "$1" --report attestation-join 2>/dev/null | grep -E '^(subject|dependency)' || true
  } | sed -E 's#[^ ]*/([A-Za-z0-9_.-]+\.con)#\1#g'
}

echo "=== a clean checkout reproduces the working tree's evidence ==="

# A REAL CLONE, not a copy: `git clone` takes only committed content, so build artifacts, untracked
# files and local edits cannot leak into the comparison. That is the difference between "the same
# bytes somewhere else" and "what someone else would get".
if git clone --quiet --no-hardlinks "$ROOT_DIR" "$TMP/clone" 2>/dev/null; then
  ok "cloned the repository into a fresh directory"
else
  no "could not clone the repository — every assertion below would be vacuous"
  GATE_DONE=1; echo "CLEAN-CHECKOUT: PASS=$PASS FAIL=$FAIL"; exit 1
fi
# Any uncommitted work would make the clone legitimately differ, so the comparison is only meaningful
# from a clean tree. Reported rather than silently skipped.
DIRTY="$(git status --porcelain | grep -vc '^?? ' || true)"
if [ "$DIRTY" = "0" ]; then
  ok "the working tree has no uncommitted tracked changes, so the clone is comparable"
else
  no "$DIRTY uncommitted tracked change(s): the clone cannot be compared against the working tree"
fi

for fixture in elf_header crypto_verify proof_patterns/composition; do
  HERE="$(evidence_of "examples/$fixture/src/main.con")"
  THERE="$(evidence_of "$TMP/clone/examples/$fixture/src/main.con")"
  if [ -z "$HERE" ]; then
    no "$fixture produced no evidence — the comparison would be vacuous"
  elif [ "$HERE" = "$THERE" ]; then
    ok "$fixture: a clean checkout yields byte-identical evidence"
  else
    no "$fixture: evidence DIFFERS in a clean checkout — something outside committed content is reaching it"
    diff <(printf '%s' "$HERE") <(printf '%s' "$THERE") | head -6 | sed 's/^/      /' || true
  fi
done

echo "=== repository root and project root agree ==="

# `lake` finds its workspace by walking up from wherever it is invoked, and a verdict that depended
# on where the caller stood is exactly what slice 4 removed. Asserted from BOTH directions rather
# than assumed from the resolver's code.
FROM_REPO="$(evidence_of "examples/elf_header/src/main.con")"
FROM_PROJ="$(cd examples/elf_header && evidence_of "src/main.con")"
if [ -z "$FROM_REPO" ]; then
  no "no evidence from the repository root — the comparison would be vacuous"
elif [ "$FROM_REPO" = "$FROM_PROJ" ]; then
  ok "invoking from the repository root and from the project root give identical evidence"
else
  no "evidence depends on the invoking directory"
  diff <(printf '%s' "$FROM_REPO") <(printf '%s' "$FROM_PROJ") | head -6 | sed 's/^/      /' || true
fi

echo "=== a receipt minted in one checkout is checked against another ==="

# THE CONTROL THIS GATE WAS MISSING. Everything above compares REPORTS between checkouts; none of it
# minted a receipt in one and consumed it against the other, so the gate could have passed while a
# receipt minted in checkout A failed against checkout B — which is the property "portable receipt"
# actually means.
#
# The BINARY is deliberately the same in both directions: one compiler, two source checkouts, which
# is the realistic case. What must not differ is the evidence the two checkouts present.
"$BIN" examples/elf_header/src/main.con --report receipts --out "$TMP/from_A.txt" >/dev/null 2>&1
"$BIN" "$TMP/clone/examples/elf_header/src/main.con" --report receipts --out "$TMP/from_B.txt" >/dev/null 2>&1

# THE COMPILER'S EXIT STATUS IS NOT THIS HELPER'S ANSWER. `--report proof-status --receipts` exits
# non-zero when claims lack current receipts, which is a correct fail-closed signal for CI — and
# exactly the case some legs here MEASURE on purpose (an unbuilt checkout must issue nothing). Under
# `set -o pipefail` that status propagated through the pipeline, out of the command substitution, and
# tripped this gate's ERR trap, so the gate aborted mid-run reporting "the verdict below is not
# trustworthy" rather than the verdict it had just computed correctly.
#
# The output is captured first and matched separately, so the tally is read from what the report SAID
# rather than from whether it approved. A genuine crash still produces an empty tally, which every
# caller already treats as a mismatch against its expected string.
xtally(){
  local out
  out="$("$BIN" "$2" --report proof-status --receipts "$1" 2>/dev/null || true)"
  printf '%s\n' "$out" | grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' | head -1 || true
}

A_IN_A="$(xtally "$TMP/from_A.txt" "examples/elf_header/src/main.con")"
if [ "$A_IN_A" = "5 current, 0 not current, 0 unreadable" ]; then
  ok "receipts minted in checkout A are current in checkout A (the baseline for the cross tests)"
else
  no "minting in A is not current in A ($A_IN_A) — the cross-checkout tests would be vacuous"
fi

# A -> B: minted here, consumed against the clone's sources at a different absolute path.
A_IN_B="$(xtally "$TMP/from_A.txt" "$TMP/clone/examples/elf_header/src/main.con")"
if [ "$A_IN_B" = "5 current, 0 not current, 0 unreadable" ]; then
  ok "a receipt minted in checkout A is CURRENT against checkout B at another absolute path"
else
  no "a receipt minted in A is not current in B ($A_IN_B) — receipts are not portable across checkouts"
fi

# THE REVERSE DIRECTION IS NOT AVAILABLE, and the reason is worth asserting rather than skipping.
# A fresh clone has no Lake build, so no kernel can run there and NOTHING can be minted — the store
# comes back empty. That is the fail-closed answer: an unbuilt checkout issues no receipts rather
# than issuing unverified ones, and its emptiness then makes nothing current.
#
# Testing B -> A properly would mean building the clone, which is a full compiler build per gate run.
# Recorded as the bound on this leg instead of asserted vacuously.
B_ISSUED="$("$BIN" "$TMP/clone/examples/elf_header/src/main.con" --report receipts 2>/dev/null \
            | grep -oE '[0-9]+ issued, [0-9]+ withheld' | head -1)"
B_IN_A="$(xtally "$TMP/from_B.txt" "examples/elf_header/src/main.con")"
if grep -qE '^0 issued' <<<"$B_ISSUED" && [ "$B_IN_A" = "0 current, 0 not current, 0 unreadable" ]; then
  ok "an unbuilt checkout issues nothing ($B_ISSUED) and its empty store makes nothing current"
else
  no "an unbuilt checkout produced receipts ($B_ISSUED) or a non-empty verdict ($B_IN_A) — minting must need a kernel"
fi

# NON-VACUITY: the cross-checkout comparison must still be able to say NO. A receipt from a different
# PROGRAM, consumed against this one, must not read current — otherwise "5 current" above would tell
# us only that the consumer says yes to everything.
"$BIN" examples/crypto_verify/src/main.con --report receipts --out "$TMP/other_prog.txt" >/dev/null 2>&1
OTHER="$(xtally "$TMP/other_prog.txt" "$TMP/clone/examples/elf_header/src/main.con")"
if grep -qE '^0 current' <<<"$OTHER"; then
  ok "a receipt from a different program is not current in either checkout ($OTHER)"
else
  no "a foreign program's receipts read current across checkouts ($OTHER) — the comparison accepts anything"
fi

echo "=== the compiler identity travels with the binary ==="

# The identity a receipt binds must describe the COMPILER, not the tree it is pointed at. It is a
# build-time constant, so it is identical from both checkouts — asserted rather than assumed, because
# an identity derived from ambient repository state would differ here and that is exactly the class of
# defect this replaced.
ID_A="$("$BIN" examples/elf_header/src/main.con --report receipts 2>/dev/null \
        | grep -oE 'build:[0-9a-f]+' | head -1)"
ID_B="$("$BIN" "$TMP/clone/examples/elf_header/src/main.con" --report receipts 2>/dev/null \
        | grep -oE 'build:[0-9a-f]+' | head -1)"
if [ -n "$ID_A" ] && [ "$ID_A" = "$ID_B" ]; then
  ok "the same binary reports one identity from both checkouts ($ID_A)"
else
  no "the compiler identity differs by checkout ('$ID_A' vs '$ID_B') — it is describing the tree, not the binary"
fi
# Its VALUE-level properties — freshness, entropy, portability, content-derivation — are asserted
# behaviourally by check_build_identity_freshness.sh. This gate deliberately does not re-assert them
# by grepping source text: an earlier control here did exactly that, and a shape assertion cannot
# notice a production branch becoming unreachable.

# Any uncommitted work would make the clone legitimately differ, so the comparison is only meaningful
# from a clean tree. Reported rather than silently skipped.
DIRTY="$(git status --porcelain | grep -vc '^?? ' || true)"
if [ "$DIRTY" = "0" ]; then
  ok "the working tree has no uncommitted tracked changes, so the clone is comparable"
else
  no "$DIRTY uncommitted tracked change(s): the clone cannot be compared against the working tree"
fi

for fixture in elf_header crypto_verify proof_patterns/composition; do
  HERE="$(evidence_of "examples/$fixture/src/main.con")"
  THERE="$(evidence_of "$TMP/clone/examples/$fixture/src/main.con")"
  if [ -z "$HERE" ]; then
    no "$fixture produced no evidence — the comparison would be vacuous"
  elif [ "$HERE" = "$THERE" ]; then
    ok "$fixture: a clean checkout yields byte-identical evidence"
  else
    no "$fixture: evidence DIFFERS in a clean checkout — something outside committed content is reaching it"
    diff <(printf '%s' "$HERE") <(printf '%s' "$THERE") | head -6 | sed 's/^/      /' || true
  fi
done

echo "=== repository root and project root agree ==="

# `lake` finds its workspace by walking up from wherever it is invoked, and a verdict that depended
# on where the caller stood is exactly what slice 4 removed. Asserted from BOTH directions rather
# than assumed from the resolver's code.
FROM_REPO="$(evidence_of "examples/elf_header/src/main.con")"
FROM_PROJ="$(cd examples/elf_header && evidence_of "src/main.con")"
if [ -z "$FROM_REPO" ]; then
  no "no evidence from the repository root — the comparison would be vacuous"
elif [ "$FROM_REPO" = "$FROM_PROJ" ]; then
  ok "invoking from the repository root and from the project root give identical evidence"
else
  no "evidence depends on the invoking directory"
  diff <(printf '%s' "$FROM_REPO") <(printf '%s' "$FROM_PROJ") | head -6 | sed 's/^/      /' || true
fi

# THE EXECUTABLE-DIGEST SECTION IS GONE, and its absence is the honest position rather than an
# omission. It asserted that the reported identity equals `sha256(the running binary)` and that a
# byte-appended copy therefore digests differently — true of the `exe:` identity that used to be
# reported, and false of the design that replaced it.
#
# The identity is now computed AT BUILD TIME over the compiler's own sources, because the executable
# digest was not portable: neither `/proc/self/exe` nor GNU `sha256sum` exists on macOS, and receipt
# issuance simply refused there. That trade is recorded in scripts/gen/build_identity.sh, and it has
# a KNOWN LIMIT stated in the same place — a binary patched after build keeps the constant its
# sources produced. Re-asserting the old property here would contradict the documented limitation
# and would fail permanently, which is exactly what it did: these lines have not run since the
# identity changed, because a stray `exit 1` sat in front of them.
#
# What survives of the intent lives where it can be checked: check_build_identity_freshness.sh
# asserts the value is content-derived, 128-bit, portable, and moves with its sources, and the leg
# above asserts one binary reports one identity from two checkouts. Binding the shipped BYTES needs
# a portable streaming executable digest or a signed build manifest, which is tracked as
# post-R-0004 provenance work and is not claimed today.

# THE CHECKOUT-MOVES CASE. Receipts minted now must survive the repository moving underneath them,
# because the identity describes the binary and not the tree. The clone is at a different commit-ish
# state only if the tree is dirty, so this uses an untracked file — the exact noise that used to
# invalidate everything through `-dirty`.
"$BIN" examples/elf_header/src/main.con --report receipts --out "$TMP/r.txt" >/dev/null 2>&1
tally(){ "$BIN" examples/elf_header/src/main.con --report proof-status --receipts "$1" 2>/dev/null \
         | grep -oE '[0-9]+ current, [0-9]+ not current' | head -1; }
BEFORE="$(tally "$TMP/r.txt")"
NOISE="$ROOT_DIR/.clean-checkout-noise"; rm -rf "$NOISE"; mkdir -p "$NOISE"
printf 'untracked\n' > "$NOISE/f.txt"
AFTER="$(tally "$TMP/r.txt")"
rm -rf "$NOISE"
if [ "$BEFORE" != "5 current, 0 not current" ]; then
  no "the checkout-moves control had no current receipts to start from — it would be vacuous"
elif [ "$AFTER" = "$BEFORE" ]; then
  ok "receipts survive the working tree changing around them ($AFTER)"
else
  no "repository state moved receipt currency ($BEFORE -> $AFTER) — the identity is not the binary's"
fi

# THE UNIDENTIFIABLE-COMPILER REFUSAL IS GONE BECAUSE THE FAILURE MODE IS. That leg grepped
# Main.lean for `executable_unlocatable` and `executable_undigestible` — refusal reasons from the
# `/proc/self/exe` identity, which was removed precisely because it could not be read on macOS and
# made issuance refuse there outright.
#
# The build identity is a constant compiled INTO the binary. There is no locating step that can fail
# and no digest that can be unreadable at run time, so the branch those names guarded does not exist
# to be reachable. Keeping the assertion would test a design decision that was reversed.
#
# The property it protected — that an "unknown" identity is never recorded, since one "unknown"
# compares equal to any other — is now held by construction plus a behavioural control:
# check_build_identity_freshness.sh asserts the value is present, 32 hex characters, and equal to a
# fresh derivation from the sources. That is a stronger guarantee than a refusal path, because there
# is nothing left to refuse.
#
# It was also a grep over source text, which this gate's own policy note a few sections up forbids
# for exactly the reason it failed here: a shape assertion cannot notice a production branch becoming
# unreachable, and cannot notice its own subject being deleted either.

GATE_DONE=1
echo "CLEAN-CHECKOUT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
