#!/usr/bin/env bash
# Differential witness: the original helper must fail every product/invocation
# obligation. Kept out of source-copy builds (which deliberately have no .git).
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
source "$root/scripts/checks/lib/bounded.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
# Frozen byte-for-byte source keeps this witness available in shallow CI clones.
historical="$root/scripts/checks/lib/fixtures/slice0_native_688e10a86.txt"
[ "$(sha256sum "$historical" | cut -d' ' -f1)" = "7460e8394edc60c76de4b9429d9236f12f9e9fe3bdc69d8aa6212eb79054ba70" ] || {
  echo 'FAIL historical helper source digest' >&2; exit 1;
}
cp "$historical" "$work/historical.sh"
rc=0
CONCRETE_LINK_CONTROL_NATIVE_HELPER="$work/historical.sh" bounded_run -i /dev/null 120 -- bash "$root/scripts/checks/lib/link_controls.sh" >"$work/products.log" 2>&1 || rc=$?
cat "$work/products.log"
[ "$rc" = 1 ] && [ "$(grep -c '^FAIL link controls: .* executable purity$' "$work/products.log")" = 4 ] || {
  echo "FAIL historical product witness: exit=$rc (expected 1 and four failed obligations)" >&2; exit 1;
}
rc=0
bounded_run -i /dev/null 120 -- bash "$root/scripts/checks/lib/link_invocation_controls.sh" "$work/historical.sh" >"$work/invocations.log" 2>&1 || rc=$?
cat "$work/invocations.log"
[ "$rc" = 1 ] && grep -qx 'invocation controls: total=8 failed=8' "$work/invocations.log" || {
  echo "FAIL historical invocation witness: exit=$rc (expected 1 and eight failed obligations)" >&2; exit 1;
}
echo 'PASS historical link witness: 688e10a86 product=4/4 failed invocation=8/8 failed'
