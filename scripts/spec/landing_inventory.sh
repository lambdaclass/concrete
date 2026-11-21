#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
cd "$(dirname "$0")/../.."
python3 -I scripts/spec/landing_inventory.py "$@"
