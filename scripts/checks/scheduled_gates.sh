#!/usr/bin/env bash
# Cheap protocol/controller controls; full campaigns retain their manifest tiers.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [ "${1:-}" != --scheduled-context ]; then
  exec python3 -I "$ROOT/scripts/checks/scheduled/suite_budget.py" "${BASH_SOURCE[0]}" "$@"
fi
shift
deadline="$1"
shift
source "$ROOT/scripts/checks/lib/gate_env.sh" || exit 2
source "$ROOT/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox || exit 2
source "$ROOT/scripts/checks/lib/scheduled_process.sh" || exit 2
exec python3 -I "$ROOT/scripts/checks/lib/scheduled_process.py" --cwd "$PWD" --deadline "$deadline" --forward-deadline -- python3 -I "$ROOT/scripts/checks/scheduled/hermetic_suite.py"
