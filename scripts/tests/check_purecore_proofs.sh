#!/usr/bin/env bash
# Pure-core proof arc, slice 1 (docs/PURE_CORE_PROOF_ARC.md): the stdlib's
# first kernel-backed proof link, held to the arc's Definition of Done —
# registered + fingerprint-fresh + kernel-verified + MUTATION-SENSITIVE.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C=".lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# std is a package; reports need a de-packaged copy of its source tree.
cp -r std/src "$TMP/std"

# 1. the link is registered and fingerprint-fresh
st=$("$C" "$TMP/std/lib.con" --report proof-status 2>&1)
grep -q '✓ `std.option.option_Option_unwrap_or` — proof matches current body' <<<"$st" \
  && ok "unwrap_or: registered + fingerprint-fresh" \
  || no "unwrap_or: proof link missing or stale"

# 1b. slice 1+2 surface: Option/Result laws + numeric checked helpers,
#     each linked and fresh
for fn in std.option.option_Option_map std.result.result_Result_map \
          std.result.result_Result_map_err \
          std.numeric.numeric_NonZeroU32_try_new \
          std.numeric.numeric_NonZeroU32_try_from_u64 \
          std.numeric.numeric_NonZeroU64_try_new \
          std.numeric.numeric_Port_try_new \
          std.numeric.numeric_Port_try_from_u32 \
          std.base64.base64_char_of \
          std.base64.base64_val_of; do
  grep -q "✓ \`$fn\` — proof matches current body" <<<"$st" \
    && ok "$fn: registered + fingerprint-fresh" \
    || no "$fn: proof link missing or stale"
done

# 1c. every std proved link is SPEC-DRIFT-COVERED — the specs table is keyed
#     by qualified name and proof-status witnesses the lookup (a mis-keyed
#     spec silently skips the drift check; slice 2 made that state visible).
drift_ok=$(grep -c "spec: drift-checked" <<<"$st")
drift_no=$(grep -c "spec: NOT drift-covered" <<<"$st")
[ "$drift_ok" -eq 11 ] && [ "$drift_no" -eq 0 ] \
  && ok "drift coverage: all 11 std links drift-checked, none uncovered" \
  || no "drift coverage: expected 11 drift-checked / 0 uncovered, got $drift_ok/$drift_no"

# 2. the Lean kernel verifies the referenced theorems (import-reachable)
cp=$("$C" "$TMP/std/lib.con" --report check-proofs 2>&1)
grep -q '✓ std.option.option_Option_unwrap_or — Examples.PureCore.Proofs.option_unwrap_or_correct' <<<"$cp" \
  && ok "unwrap_or: kernel-verified via Examples.PureCore.Proofs" \
  || no "unwrap_or: kernel check failed or theorem unreachable"
grep -q '✓ std.option.option_Option_map — Examples.PureCore.Proofs.option_map_correct' <<<"$cp" \
  && ok "option.map: kernel-verified" \
  || no "option.map: kernel check failed or theorem unreachable"
grep -q '✓ std.result.result_Result_map — Examples.PureCore.Proofs.result_map_correct' <<<"$cp" \
  && ok "result.map: kernel-verified" \
  || no "result.map: kernel check failed or theorem unreachable"
grep -q '✓ std.result.result_Result_map_err — Examples.PureCore.Proofs.result_map_err_correct' <<<"$cp" \
  && ok "result.map_err: kernel-verified" \
  || no "result.map_err: kernel check failed or theorem unreachable"
for pair in "numeric_NonZeroU32_try_new:numeric_try_new_correct" \
            "numeric_NonZeroU32_try_from_u64:nonzero_u32_try_from_u64_correct" \
            "numeric_NonZeroU64_try_new:numeric_try_new_correct" \
            "numeric_Port_try_new:numeric_try_new_correct" \
            "numeric_Port_try_from_u32:port_try_from_u32_correct"; do
  fn=${pair%%:*}; thm=${pair##*:}
  grep -q "✓ std.numeric.$fn — Examples.PureCore.Proofs.$thm" <<<"$cp" \
    && ok "$fn: kernel-verified" \
    || no "$fn: kernel check failed or theorem unreachable"
done
grep -q '✓ std.base64.base64_char_of — Examples.PureCore.Proofs.base64_char_of_correct' <<<"$cp" \
  && ok "base64.char_of: kernel-verified" \
  || no "base64.char_of: kernel check failed or theorem unreachable"
grep -q '✓ std.base64.base64_val_of — Examples.PureCore.Proofs.base64_val_of_correct' <<<"$cp" \
  && ok "base64.val_of: kernel-verified" \
  || no "base64.val_of: kernel check failed or theorem unreachable"
grep -q 'Summary: 11 verified, 0 failed' <<<"$cp" \
  && ok "check-proofs: exactly 11 verified, zero failures" \
  || no "check-proofs count drifted or reports failures"

# 3. MUTATION: a body change must go STALE (evidence is load-bearing)
perl -0pi -e 's/Option::None => \{ return default; \},/Option::None => { let d2: T = default; return d2; },/' "$TMP/std/option.con"
mst=$("$C" "$TMP/std/lib.con" --report proof-status 2>&1)
grep -q 'stale fingerprint for .std.option.option_Option_unwrap_or' <<<"$mst" \
  && ok "mutation: body change flagged stale (fingerprint machinery live)" \
  || no "mutation: body change NOT flagged — evidence is decorative"

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
