#!/usr/bin/env bash
# B0b closure gate. Run inside nix develop.
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/checks/lib/gate_env.sh"
cd "$ROOT_DIR"
command -v lake >/dev/null || { echo "error: nonvacuity needs Lean's lake on PATH" >&2; exit 2; }

if (( $# > 1 )); then
  echo "usage: $0 [--controls]" >&2
  exit 2
fi

case ${1:-} in
  "") export NONVACUITY_MODE=audit ;;
  --controls) export NONVACUITY_MODE=controls ;;
  *) echo "usage: $0 [--controls]" >&2; exit 2 ;;
esac
bash scripts/checks/lib/bounded.sh 600 lake build ConcreteSpec ConcreteSpecEvidence
bash scripts/checks/lib/bounded.sh 180 lake env lean scripts/proofs/nonvacuity.lean
