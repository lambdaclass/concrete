#!/usr/bin/env bash
# The parser and publication controls use disposable inputs. Run the full migration
# census separately via scripts/spec/f31_census.py --output NEW_DIRECTORY.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../lib/gate_env.sh"
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."
source scripts/checks/lib/git_sandbox.sh
git_sandbox
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
BOUNDED_PARENT="$work"
source scripts/checks/lib/bounded.sh
rc=0
bounded_run -i /dev/null 120 -- python3 -B scripts/spec/f31_census_controls.py || rc=$?
bounded_summary
exit "$rc"
