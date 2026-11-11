#!/usr/bin/env bash
# Live tools and guidance use the sole repository; frozen records retain their original evidence.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh"
git_sandbox
python3 -I "$ROOT_DIR/scripts/checks/lib/repository_identity.py" "$ROOT_DIR"
# The hook must refuse an old configured destination before its skip-gates path.
old_owner=unbalancedparentheses
if out="$(CONCRETE_SKIP_GATES=1 bash "$ROOT_DIR/.githooks/pre-push" origin "https://github.com/$old_owner/concrete2.git" </dev/null 2>&1)"; then
  echo "repository_identity: hook accepted an archived destination" >&2; exit 1
fi
[[ "$out" == *"archived repository destination refused"* ]] || { echo "$out" >&2; exit 1; }
