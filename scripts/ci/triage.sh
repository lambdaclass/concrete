#!/usr/bin/env bash
# CI failure triage for one run. See scripts/ci/triage.py for the rules; the classes are in
# scripts/ci/ci_logs.py. Usage: scripts/ci/triage.sh <run-id> [--repo R] [--no-rerun] [--summary F]
set -euo pipefail
exec python3 -I "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/triage.py" "$@"
