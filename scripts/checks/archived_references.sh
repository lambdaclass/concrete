#!/usr/bin/env bash
# Refuse stale archived endpoints in maintained text.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh"
git_sandbox
python3 -I "$ROOT_DIR/scripts/checks/lib/archived_references.py" "$ROOT_DIR"
