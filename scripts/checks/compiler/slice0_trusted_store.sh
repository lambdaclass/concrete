#!/usr/bin/env bash
# Weekly: content verification and independent rebuild of the lock-derived roots.
set -euo pipefail
source scripts/checks/lib/bounded.sh
work=$(mktemp -d)
trap 'rm -rf "${work:?}"' EXIT
system=$(bounded_run 30 -- nix eval --impure --raw --expr builtins.currentSystem)
bounded_run 120 -- nix eval --json ".#devShells.$system.default.trustedPipelineRoots" >"$work/roots.json"
python3 -I -c 'import json,sys; print("\n".join(sorted(set(json.load(open(sys.argv[1]))))))' "$work/roots.json" >"$work/roots"
while IFS= read -r root; do
  bounded_run 1800 -- nix store verify --recursive --no-trust "$root"
  drv=$(bounded_run 30 -- nix-store --query --deriver "$root")
  [ "$drv" != unknown-deriver ] || { echo "FAIL store: no derivation for $root"; exit 1; }
  bounded_run 14400 -- nix build --rebuild --no-link "$drv^*"
done <"$work/roots"
