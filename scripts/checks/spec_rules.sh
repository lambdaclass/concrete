#!/usr/bin/env bash
# Keep the diagnostic registry and all its controls in the root/pre-push check set.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
exec bash "$(dirname "${BASH_SOURCE[0]}")/../spec/rules.sh"
