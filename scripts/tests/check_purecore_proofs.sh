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

# 2. the Lean kernel verifies the referenced theorem (import-reachable)
cp=$("$C" "$TMP/std/lib.con" --report check-proofs 2>&1)
grep -q '✓ std.option.option_Option_unwrap_or — Examples.PureCore.Proofs.option_unwrap_or_correct' <<<"$cp" \
  && ok "unwrap_or: kernel-verified via Examples.PureCore.Proofs" \
  || no "unwrap_or: kernel check failed or theorem unreachable"
grep -qE 'Summary: [0-9]+ verified, 0 failed' <<<"$cp" \
  && ok "check-proofs: zero failures" \
  || no "check-proofs reports failures"

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

echo
echo "PURECORE-PROOFS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
