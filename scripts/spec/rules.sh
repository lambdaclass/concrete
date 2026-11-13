#!/usr/bin/env bash
# Production and controls share the Lean exporter and closed-schema validator.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
cd "$(dirname "$0")/../.."
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
BOUNDED_PARENT="$TMP"; source scripts/checks/lib/bounded.sh
bounded_run -i /dev/null -r 3600 3600 -- lake build ConcreteSpec
bounded_run -i /dev/null 600 -- lake env lean scripts/spec/rules.lean > "$TMP/rules.jsonl"
bounded_run -i /dev/null 120 -- python3 -I scripts/spec/rules.py "$TMP/rules.jsonl" docs/decisions.md ROADMAP.md
bounded_run -i /dev/null 600 -- python3 -I scripts/spec/rules_controls.py "$TMP"
bounded_summary
