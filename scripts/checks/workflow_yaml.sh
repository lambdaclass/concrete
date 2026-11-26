#!/usr/bin/env bash
# Workflow-YAML gate.
#
# GitHub Actions workflows that fail to PARSE do not run at all — they report a
# 0-second "completed failure" with no logs, so every gate they were supposed to
# run is silently skipped. This actually happened: a `name:` value that began
# with a backtick (`` ` `` is a reserved YAML indicator and cannot start a plain
# scalar) killed `lean_action_ci.yml` for 40+ consecutive pushes without any
# obvious signal. This gate parses every workflow locally so a malformed file is
# caught before it is pushed, not after CI has been dark for days.
#
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
exec python3 -I scripts/checks/lib/workflow_checks.py
