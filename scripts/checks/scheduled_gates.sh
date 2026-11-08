#!/usr/bin/env bash
# Cheap protocol/controller controls; full campaigns retain their manifest tiers.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT/scripts/checks/lib/gate_env.sh" || exit 2
source "$ROOT/scripts/checks/lib/bounded.sh" || exit 2
bounded_run 45 -- python3 -I -m unittest discover -s "$ROOT/scripts/checks/scheduled" -p 'test_*.py'; rc=$?
if bounded_is_incomplete "$rc"; then echo 'INCOMPLETE scheduled controller tests'; exit 1; fi
exit "$rc"
