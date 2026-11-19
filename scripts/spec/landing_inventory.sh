#!/usr/bin/env bash
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
BOUNDED_PARENT="$scratch"
source scripts/checks/lib/bounded.sh

bounded_run -i /dev/null -r 3600 3600 -- lake build ConcreteSpec
bounded_run -i /dev/null 60 -- lake env lean -DautoImplicit=false -DwarningAsError=true scripts/spec/landing_inventory.lean
bounded_summary
