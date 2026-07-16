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

# 1b. slice-1 remainder: the Option/Result map laws are linked and fresh
for fn in std.option.option_Option_map std.result.result_Result_map std.result.result_Result_map_err; do
  grep -q "✓ \`$fn\` — proof matches current body" <<<"$st" \
    && ok "$fn: registered + fingerprint-fresh" \
    || no "$fn: proof link missing or stale"
done

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
grep -q 'Summary: 4 verified, 0 failed' <<<"$cp" \
  && ok "check-proofs: exactly 4 verified, zero failures" \
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

echo
echo "PURECORE-PROOFS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
