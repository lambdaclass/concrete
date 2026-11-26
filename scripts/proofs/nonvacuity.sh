#!/usr/bin/env bash
# B0b closure gate. Run from the repository root inside nix develop.
set -euo pipefail
source scripts/checks/lib/gate_env.sh

if (( $# > 1 )); then
  echo "usage: $0 [--controls]" >&2
  exit 2
fi

case ${1:-} in
  "") export NONVACUITY_MODE=audit ;;
  --controls) export NONVACUITY_MODE=controls ;;
  *) echo "usage: $0 [--controls]" >&2; exit 2 ;;
esac
bash scripts/checks/lib/bounded.sh 600 lake build ConcreteSpec
bash scripts/checks/lib/bounded.sh 180 lake env lean scripts/proofs/nonvacuity.lean
