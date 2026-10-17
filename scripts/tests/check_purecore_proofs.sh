#!/usr/bin/env bash
# Pure-core proof arc, slice 1 (docs/verification/PURE_CORE_PROOF_ARC.md): the stdlib's
# first kernel-backed proof link, held to the arc's Definition of Done —
# registered + fingerprint-fresh + kernel-verified + MUTATION-SENSITIVE.
set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C=".lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# std IS A PACKAGE AND IS NOW COMPILED AS ONE. This gate used to copy `std/src` to a temp directory
# and compile the loose tree, because `loadProject` refused a package whose entry is `src/lib.con`.
# That workaround DESTROYED the package identity, which is the thing scoped evidence depends on — so
# after the Package 2 authority transition every std subject carried a source name only and its
# correspondence and dependency root refused. The 11-proved figure this gate asserted was obtainable
# only by bypassing scoped evidence.
#
# The underlying bug (a package injected as its own dependency) was fixed 2026-08-16, so the copy is
# gone and std is compiled in place.
st=$("$C" std/src/lib.con --report proof-status 2>&1)

# WHAT THESE LINKS ARE WORTH NOW, re-pinned 2026-08-18 to the measured truth.
#
# Nine of the eleven report `needs_recheck`, and the reported reason CHANGED at the V2 activation
# without the underlying situation improving. They used to report `dependency closure unjustified`:
# their theorems rest on `pureCoreFns`, which carries no manifest row, describes no definitions and
# justifies nothing. That is still true. It is simply no longer what gets reported, because the
# deriver settles comparability BEFORE justification and these nine are no longer comparable at all.
#
# Their v2 subject is INCOMPLETE, so no digest is minted and the stored v1 value has nothing to be
# measured against. Verified, not assumed: `--report subject-facts` gives
# `numeric_Port_try_new` `shadow bodyV2: REFUSED (1 gap(s): intrinsic cast: target TypeId not
# minted here)` and `subject digest: INCOMPLETE (no digest minted)`. The structural body producer
# does not yet cover intrinsic casts, and four of the nine are additionally generic (`/1`, `/2`,
# `/3`). That is a REMAINING GAP IN THE V2 PRODUCER, recorded here rather than papered over.
#
# Both dispositions are non-proved, so nothing false is claimed either way — but the newer one is
# LESS informative: `needs_recheck` reads as "re-record the fingerprint", and re-recording would not
# help, because the closure would still be unjustified underneath. Asserting these nine as `proved`
# would require re-adopting the de-packaged compilation that made the refusal invisible.
#
# The two base64 links DO prove, and they are the live positive control: without them "nine are
# unjustified" would also be satisfied by std carrying no evidence at all.
# R-0484 WOULD SPLIT THESE NINE, and does not yet. Three — `Option::map`, `Result::map`,
# `Result::map_err` — are HIGHER-ORDER: they call the function they are handed, and a
# capability-free fn type can still hide effects behind a `trusted` body, so their
# effect-freedom cannot be shown. The intended rule refuses them and reports `ineligible`
# with that reason instead of `needs_recheck`.
#
# It is not enabled, because its reach is wider than the admission question. 188 of std's
# 890 functions are higher-order, and moving these three drops them from drift coverage
# (11 -> 8) and out of `replayTargetsOf` entirely — and whether an INADMISSIBLE claim
# should still be replayed is a question about evidence semantics, which this file argues
# one way ("replay must keep asking about pending claims, or the migration cannot clear
# them") and which a change about effect-freedom should not settle by accident. See
# `EligibilityEntry.admissible`, deliberately inert, and R-0484.
#
# No proof rides on the answer: both dispositions are non-proved, and the two proved std
# links are base64 and unaffected either way.
for fn in std.option.option_Option_unwrap_or \
          std.numeric.numeric_NonZeroU32_try_new \
          std.numeric.numeric_NonZeroU32_try_from_u64 \
          std.numeric.numeric_NonZeroU64_try_new \
          std.numeric.numeric_Port_try_new \
          std.numeric.numeric_Port_try_from_u32; do
  grep -q "\`$fn\` has a stored proof-subject digest from an EARLIER SCHEMA" <<<"$st" \
    && ok "$fn: needs_recheck — its v2 subject is INCOMPLETE, so the stored v1 value is not comparable" \
    || no "$fn: no longer reports needs_recheck — if its v2 subject became computable, re-pin to what it now honestly is"
done
for fn in std.base64.base64_char_of std.base64.base64_val_of; do
  grep -q "✓ \`$fn\` — proof matches current body" <<<"$st" \
    && ok "$fn: registered + fingerprint-fresh (the control the nine above are measured against)" \
    || no "$fn: proof link missing or stale — the unjustified assertions above are now vacuous"
done

# THE TRIPWIRE ON THE DEFERRAL. The nine are unjustified only while `pureCoreFns` is unconverted.
# The day it gains a manifest row this must go red, so the std arc is re-pinned rather than quietly
# staying at two.
if "$C" examples/elf_header/src/main.con --report subject-facts >/dev/null 2>&1; then
  PCF="$(cat > "$TMP/pcf.lean" <<'LEAN'
import Concrete
open Concrete.Proof
#eval (scopedEntryEvidence pureCoreFns).toOption == some []
LEAN
  lake env lean "$TMP/pcf.lean" 2>&1 || true)"
  if grep -q 'true' <<<"$PCF"; then
    ok "TRIPWIRE: pureCoreFns still has empty scoped membership, so the nine cannot be justified yet"
  else
    no "pureCoreFns now carries scoped membership — the nine std links must be re-pinned to proved"
  fi
fi

# 1c. every std link is SUBJECT-BOUND (bug 079): its body and parameters ARE the specification
#     value its theorem evaluates. This replaced a drift check keyed by the function's source name
#     in a side table, which skipped any link whose name had no entry; the binding has no uncovered
#     set — a link either binds or says why it cannot.
bound_ok=$(grep -c "spec: bound to " <<<"$st")
bound_no=$(grep -c "spec: NOT bound" <<<"$st")
# 11 BOUND, 0 NOT BOUND. The bound count is pinned exactly so that a DROP is caught: fewer bound links
# would mean claims quietly left the evidence-bearing set. A NOT-bound std link is the defect itself.
[ "$bound_ok" -eq 11 ] && [ "$bound_no" -eq 0 ] \
  && ok "spec binding: all 11 std links are bound to their theorem's specification, none unbound" \
  || no "spec binding: expected 11 bound / 0 not bound, got $bound_ok/$bound_no"

# 2. the Lean kernel verifies the referenced theorems (import-reachable)
#
# RE-PINNED 2026-08-18, and the new form is stronger than the one it replaces. These nine used to be
# absent from the replay set, and the legs asserted that absence. `replayTargetsOf` was widened in
# 2c921d6d to include `correspondenceUnjustified` and `needsRecheck`, deliberately: replay answers
# "does this theorem typecheck", which is independent of whether a claim's dependencies are current
# or its closure is justified — those are facts about OTHER declarations. Excluding them stranded
# links in the V1->V2 migration with `not_replayed`, unable to ask about the very claims the
# migration exists to clear.
#
# So asserting "not replayed" now asserts a policy that was intentionally abandoned. What must hold
# instead is the boundary that actually matters, and it is the one this whole effort is about:
# KERNEL ACCEPTANCE MUST NOT BECOME `proved`. The kernel says a theorem typechecks; it says nothing
# about whether this function's closure is justified or its stored subject is comparable. Each leg
# below therefore asserts BOTH halves — the theorem verifies, AND the claim is still not proved —
# which is a real conjunction that a regression in either direction breaks. The previous form could
# be satisfied by the replay set going empty for any reason at all.
cp=$("$C" std/src/lib.con --report check-proofs 2>&1)
# not_proved <fn> — the claim is absent from the proved set in --report proof-status.
not_proved() { ! grep -q "✓ \`$1\` — proof matches current body" <<<"$st"; }
if grep -q '✓ std.option.option_Option_unwrap_or — Examples.PureCore.Proofs.option_unwrap_or_correct' <<<"$cp"; then
  if not_proved std.option.option_Option_unwrap_or; then
    ok "std.option.option_Option_unwrap_or: theorem verifies, and the claim is still NOT proved — kernel acceptance did not become evidence"
  else
    no "std.option.option_Option_unwrap_or: kernel acceptance was upgraded into `proved` while its closure is unjustified and its subject incomparable"
  fi
elif grep -q 'Examples.PureCore.Proofs.option_unwrap_or_correct' <<<"$cp"; then
  no "std.option.option_Option_unwrap_or entered the replay set but did not verify — that is a real kernel failure"
else
  no "std.option.option_Option_unwrap_or is no longer replayed at all — replayTargetsOf must keep asking about needs_recheck claims, or the migration cannot clear them"
fi
if grep -q '✓ std.option.option_Option_map — Examples.PureCore.Proofs.option_map_correct' <<<"$cp"; then
  if not_proved std.option.option_Option_map; then
    ok "std.option.option_Option_map: theorem verifies, and the claim is still NOT proved — kernel acceptance did not become evidence"
  else
    no "std.option.option_Option_map: kernel acceptance was upgraded into `proved` while its closure is unjustified and its subject incomparable"
  fi
elif grep -q 'Examples.PureCore.Proofs.option_map_correct' <<<"$cp"; then
  no "std.option.option_Option_map entered the replay set but did not verify — that is a real kernel failure"
else
  no "std.option.option_Option_map is no longer replayed at all — replayTargetsOf must keep asking about needs_recheck claims, or the migration cannot clear them"
fi
if grep -q '✓ std.result.result_Result_map — Examples.PureCore.Proofs.result_map_correct' <<<"$cp"; then
  if not_proved std.result.result_Result_map; then
    ok "std.result.result_Result_map: theorem verifies, and the claim is still NOT proved — kernel acceptance did not become evidence"
  else
    no "std.result.result_Result_map: kernel acceptance was upgraded into `proved` while its closure is unjustified and its subject incomparable"
  fi
elif grep -q 'Examples.PureCore.Proofs.result_map_correct' <<<"$cp"; then
  no "std.result.result_Result_map entered the replay set but did not verify — that is a real kernel failure"
else
  no "std.result.result_Result_map is no longer replayed at all — replayTargetsOf must keep asking about needs_recheck claims, or the migration cannot clear them"
fi
if grep -q '✓ std.result.result_Result_map_err — Examples.PureCore.Proofs.result_map_err_correct' <<<"$cp"; then
  if not_proved std.result.result_Result_map_err; then
    ok "std.result.result_Result_map_err: theorem verifies, and the claim is still NOT proved — kernel acceptance did not become evidence"
  else
    no "std.result.result_Result_map_err: kernel acceptance was upgraded into `proved` while its closure is unjustified and its subject incomparable"
  fi
elif grep -q 'Examples.PureCore.Proofs.result_map_err_correct' <<<"$cp"; then
  no "std.result.result_Result_map_err entered the replay set but did not verify — that is a real kernel failure"
else
  no "std.result.result_Result_map_err is no longer replayed at all — replayTargetsOf must keep asking about needs_recheck claims, or the migration cannot clear them"
fi
for pair in "numeric_NonZeroU32_try_new:numeric_try_new_correct" \
            "numeric_NonZeroU32_try_from_u64:nonzero_u32_try_from_u64_correct" \
            "numeric_NonZeroU64_try_new:numeric_try_new_correct" \
            "numeric_Port_try_new:numeric_try_new_correct" \
            "numeric_Port_try_from_u32:port_try_from_u32_correct"; do
  fn=${pair%%:*}; thm=${pair##*:}
  if grep -q "✓ std.numeric.$fn — Examples.PureCore.Proofs.$thm" <<<"$cp"; then
    if not_proved "std.numeric.$fn"; then
      ok "$fn: theorem verifies, and the claim is still NOT proved — kernel acceptance did not become evidence"
    else
      no "$fn: kernel acceptance was upgraded into `proved` while its closure is unjustified and its subject incomparable"
    fi
  elif grep -q "Examples.PureCore.Proofs.$thm" <<<"$cp"; then
    no "$fn entered the replay set but did not verify — that is a real kernel failure"
  else
    no "$fn is no longer replayed at all — replayTargetsOf must keep asking about needs_recheck claims"
  fi
done
grep -q '✓ std.base64.base64_char_of — Examples.PureCore.Proofs.base64_char_of_correct' <<<"$cp" \
  && ok "base64.char_of: kernel-verified" \
  || no "base64.char_of: kernel check failed or theorem unreachable"
grep -q '✓ std.base64.base64_val_of — Examples.PureCore.Proofs.base64_val_of_correct' <<<"$cp" \
  && ok "base64.val_of: kernel-verified" \
  || no "base64.val_of: kernel check failed or theorem unreachable"
# 2 -> 11 verified, and ZERO failures. This count went 11 -> 2 when std began compiling as a package
# (the nine stopped being replay targets), and back to 11 when `replayTargetsOf` was widened to keep
# asking about `needs_recheck` and `correspondenceUnjustified` claims — without that, the migration
# could not ask about the very claims it exists to clear.
#
# VERIFIED IS NOT PROVED, and the distinction is the whole reason this number is allowed to rise.
# Eleven theorems typecheck; two claims are proved. The legs above assert exactly that conjunction
# per function, so a rise here cannot quietly become evidence. Zero failures still matters
# independently: a link that entered the set and was refused is a real kernel failure, distinct from
# one that was never asked about.
grep -q 'Summary: 11 verified, 0 failed' <<<"$cp" \
  && ok "check-proofs: exactly 11 verified, zero failures (2 of them proved — see the per-function legs)" \
  || no "check-proofs count drifted or reports failures: $(grep -o 'Summary:.*' <<<"$cp" | head -1)"

# 3. MUTATION: a body change must go STALE (evidence is load-bearing)
# The mutation needs a WRITABLE copy, and it must be a copy of the PACKAGE — `Concrete.toml` and
# all — because a loose source tree has no package identity and would reproduce the very bypass
# this gate stopped relying on.
#
# THE MUTATION MOVED FROM option_Option_unwrap_or TO base64_char_of, and the reason is the finding
# rather than a convenience. `unwrap_or` is one of the nine whose v2 subject is INCOMPLETE, so its
# stored value is not comparable to anything: `storedFreshness` returns `notComparable`, the claim
# reports `needs_recheck` no matter what the body does, and editing the body changes NOTHING in the
# report. Asserting "a body edit is flagged stale" there could never pass again, and — worse — an
# incomparable subject makes any freshness control run against it permanently vacuous.
#
# So the control runs on a subject the machinery can actually see. `base64_char_of` has a real v2
# subject and a migrated v2 stored value, and the edit below moves it, which is what proves the
# fingerprint path is live rather than decorative.
cp -r std "$TMP/stdpkg"
perl -0pi -e 's/if v == 62 \{ return 43; \}/if v == 62 { return 43; } if v == 63 { return 47; }/' "$TMP/stdpkg/src/base64.con"
mst=$("$C" "$TMP/stdpkg/src/lib.con" --report proof-status 2>&1)
grep -q "stale fingerprint for .std.base64.base64_char_of" <<<"$mst" \
  && ok "mutation: body change flagged stale (fingerprint machinery live)" \
  || no "mutation: body change NOT flagged — evidence is decorative"
# AND THE LIMITATION, asserted rather than left as a comment. A body edit to one of the nine is
# INVISIBLE, because there is no comparable subject to measure it against. That is sound only because
# those claims are not proved either — an invisible body change under a `proved` claim would be the
# defect this whole effort exists to prevent. Both halves are checked, so if the nine ever become
# proved while still incomparable, this fails.
perl -0pi -e 's/Option::None => \{ return default; \},/Option::None => { let d2: T = default; return d2; },/' "$TMP/stdpkg/src/option.con"
mst2=$("$C" "$TMP/stdpkg/src/lib.con" --report proof-status 2>&1)
if grep -q "✓ \`std.option.option_Option_unwrap_or\` — proof matches current body" <<<"$mst2"; then
  no "an INCOMPARABLE subject reports proved after a body edit — an unseen body change is backing a proved claim"
else
  ok "a body edit to an incomparable subject leaves it unproved (invisible, but never credited as proved)"
fi

# 4. trusted fns stay excluded from proof links (the honesty boundary):
#    bytes.view carries a kernel-checked MODEL theorem but no registry link.
grep -q '#\[proof_by' std/src/bytes.con \
  && no "bytes.view carries proof_by attributes (trusted fns must not link)" \
  || ok "bytes.view: model-refined comment only (trusted boundary visible)"
grep -q "bytes_view_guard_correct" proofs/Examples/PureCore/Proofs.lean \
  && ok "bytes.view model theorem present (kernel-checked in Examples lib)" \
  || no "bytes.view model theorem missing"

# 5. the H1 radix guard-step fact: same pattern (step lemma is not a
#    whole-function spec, so no registry link — comment + kernel theorem).
grep -qE '^\s*#\[proof_by' std/src/parse.con \
  && no "parse.con carries proof_by attributes (step lemma must not claim a whole-fn link)" \
  || ok "parse_hex: guard-step comment only (no whole-loop overclaim)"
grep -q "hex_guard_step_preserves_u64" proofs/Examples/PureCore/Proofs.lean \
  && ok "H1 guard-step lemma present (kernel-checked, by omega)" \
  || no "H1 guard-step lemma missing"
grep -q "hex_guard_step_preserves_u64" std/src/parse.con \
  && ok "parse_hex source comment references the lemma" \
  || no "parse_hex source comment missing lemma reference"

# 6. PureCore slice 3 — Bytes::index_of / Bytes::slice (workloads 5/7/8):
#    model-refinement class like bytes.view — kernel theorems + source
#    comments, NEVER registry links (both are trusted raw-pointer impls).
for thm in bytes_slice_guard_correct bytes_index_of_step_correct \
           index_of_scan_step_preserves_bounds index_of_hit_in_range \
           slice_copy_step_in_bounds; do
  grep -q "$thm" proofs/Examples/PureCore/Proofs.lean \
    && ok "slice-3 theorem present: $thm" \
    || no "slice-3 theorem missing: $thm"
done
grep -q "bytes_index_of_step_correct" std/src/bytes.con \
  && ok "index_of source comment references its model theorem" \
  || no "index_of source comment missing"
grep -q "bytes_slice_guard_correct" std/src/bytes.con \
  && ok "slice source comment references its model theorem" \
  || no "slice source comment missing"

# 6. the alphabet ROUNDTRIP corollary (encode-then-decode identity) is
#    kernel-present and referenced from the base64 source.
grep -q "base64_alphabet_roundtrip" proofs/Examples/PureCore/Proofs.lean \
  && ok "base64 roundtrip theorem present (kernel-checked)" \
  || no "base64 roundtrip theorem missing"
grep -q "base64_alphabet_roundtrip" std/src/base64.con \
  && ok "base64 source comment references the roundtrip" \
  || no "base64 source comment missing roundtrip reference"

echo
echo "PURECORE-PROOFS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
