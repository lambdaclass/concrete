#!/usr/bin/env bash
# Cheap protocol/controller controls; full campaigns retain their manifest tiers.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [ "${1:-}" != --scheduled-context ]; then
  exec python3 -I "$ROOT/scripts/checks/lib/scheduled_process.py" --cwd "$PWD" --timeout 110 -- bash "${BASH_SOURCE[0]}" --scheduled-context "$@"
fi
shift
source "$ROOT/scripts/checks/lib/gate_env.sh" || exit 2
source "$ROOT/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox || exit 2
source "$ROOT/scripts/checks/lib/scheduled_process.sh" || exit 2
bounded_run 100 -- python3 -I "$ROOT/scripts/checks/scheduled/hermetic_suite.py"; rc=$?
if bounded_is_incomplete "$rc"; then echo 'INCOMPLETE scheduled controller tests'; exit 1; fi
exit "$rc"
