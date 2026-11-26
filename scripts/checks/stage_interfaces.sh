#!/usr/bin/env bash
# Pin the interface round byte for byte, independently of Git index state.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
cd "$(dirname "${BASH_SOURCE[0]}")/../.."

export LC_ALL=C
manifest=scripts/checks/lib/stage_interfaces.sha256
digest() {
  sha256sum ConcreteSpec/StageInterface.lean ConcreteSpec/StageGrammar.lean Concrete/Backend/Observation.lean Concrete/Interfaces/*.lean
}
expected=$(cat "$manifest")
actual=$(digest)
if [[ "$actual" != "$expected" ]]; then
  echo "FAIL stage interfaces changed. Open an interface round before updating $manifest." >&2
  diff -u "$manifest" <(printf '%s\n' "$actual") >&2 || true
  exit 1
fi
echo "PASS frozen stage interfaces"
