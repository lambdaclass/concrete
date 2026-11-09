#!/usr/bin/env bash
# Weekly scheduled leg for gate 9 (R-0676 / R-0509): content verification and
# independent rebuild of the flake-derived pipeline roots from instantiated flake
# derivations (never nix-store --query --deriver on substituted paths).
set -euo pipefail
export LC_ALL=C
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
source scripts/checks/lib/slice0_environment.sh
slice0_trusted_store_main() {
root=$PWD
work=$(mktemp -d)
trap 'rc=$?; bounded_summary || true
  if [ "$rc" = 0 ] && grep -Eqs "^(incomplete|runner-failed)" "${BOUNDED_LOG:-/nonexistent}"; then
    echo "INCOMPLETE: a bounded run timed out twice; not a pass"; rc=3
  fi
  rm -rf "${work:?}"; exit "$rc"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "slice0_trusted_store: python3 is required for bounded runs" >&2; exit 2; }
bounded() {
  local secs=$1 rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "slice0_trusted_store: INCOMPLETE (timed out twice): $*" >&2
  fi
  return "$rc"
}
system=$(bounded 30 nix eval --impure --raw --expr builtins.currentSystem)
# Instantiate the flake's devShell derivation graph into the local store so every
# pipeline root's drvPath is present even when its output was substituted from a cache.
bounded 120 nix path-info --derivation ".#devShells.$system.default" >/dev/null
bounded 120 nix eval --json ".#devShells.$system.default.trustedIdentity.derivations" >"$work/derivations.json"
python3 -I - "$work/derivations.json" >"$work/roots.tsv" <<'PY'
import json
from pathlib import Path
import sys

drvs = json.loads(Path(sys.argv[1]).read_text())
if not drvs:
    sys.exit("FAIL store: empty trustedIdentity.derivations")
for out_path, drv_path in sorted({(item["path"], item["drv"]) for item in drvs}):
    if not out_path.startswith("/nix/store/") or not drv_path.startswith("/nix/store/") or not drv_path.endswith(".drv"):
        sys.exit(f"FAIL store: invalid derivation mapping {out_path} -> {drv_path}")
    if not Path(drv_path).exists():
        sys.exit(f"FAIL store: instantiated derivation missing from store: {drv_path}")
    print(f"{out_path}\t{drv_path}")
PY
while IFS=$'\t' read -r out_path drv_path; do
  bounded 1800 nix store verify --recursive --no-trust "$out_path"
  bounded 14400 nix build --rebuild --no-link "$drv_path^*"
done <"$work/roots.tsv"
echo "PASS slice0_trusted_store"

}
slice0_gate_environment slice0_trusted_store_main "$@"
