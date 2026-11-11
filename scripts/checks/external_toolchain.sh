#!/usr/bin/env bash
# Exercise the external builder with a local source pin and a planted mismatched pin.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
source "$ROOT/scripts/checks/lib/git_sandbox.sh"
git_sandbox
source "$ROOT/scripts/proofs/lake_tool.sh"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/source" "$TMP/bin" "$TMP/tools"
git -C "$TMP/source" init -q
printf '%s\n' leanprover/lean4:v4.35.0 > "$TMP/source/lean-toolchain"
git -C "$TMP/source" add lean-toolchain
git -C "$TMP/source" -c user.name=Control -c user.email=control@example.invalid commit -qm fixture
REV=$(git -C "$TMP/source" rev-parse HEAD)
cat > "$TMP/bin/elan" <<'CONTROL'
#!/usr/bin/env bash
set -eu
[ "$*" = 'run --install leanprover/lean4:v4.35.0 lake build tool' ]
[ "$(cat lean-toolchain)" = leanprover/lean4:v4.35.0 ]
echo dispatched >> "$DISPATCH_LOG"
CONTROL
chmod +x "$TMP/bin/elan"
export PATH="$TMP/bin:$PATH" DISPATCH_LOG="$TMP/dispatched"
TOOLS="$TMP/tools"
publish() {
  local dir=$1 work="$1.partial.$$"; shift
  (cd "$work" && "$@") && touch "$work/.built" && mv "$work" "$dir"
}
build_lake_tool tool "$TMP/source" "$REV" leanprover/lean4:v4.35.0 tool >/dev/null
build_lake_tool tool "$TMP/source" "$REV" leanprover/lean4:v4.35.0 tool >/dev/null
[ "$(wc -l < "$DISPATCH_LOG")" -eq 1 ]
if build_lake_tool tool "$TMP/source" "$REV" leanprover/lean4:v4.34.1 tool > "$TMP/out" 2>&1; then
  echo 'FAIL: mismatched pin accepted'; exit 1
fi
grep -q 'toolchain mismatch' "$TMP/out"
[ "$(wc -l < "$DISPATCH_LOG")" -eq 1 ]
echo 'external toolchain: explicit dispatch and cache reuse pass; mismatched pin refused before build'
