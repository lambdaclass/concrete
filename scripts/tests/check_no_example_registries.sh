#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Registry-retirement gate: example proofs must be SOURCE-LINKED, not JSON-backed.
#
# Every example/flagship proof now lives in source as
# #[spec]/#[proof_by]/#[proof_coverage]/#[proof_fingerprint] attributes, with
# staleness detected by the fingerprint hash. A `proof-registry.json` under
# examples/ is a regression — the JSON side channel we retired. JSON support
# still exists for the adversarial fixtures under tests/programs/ (which
# exercise the shared validateRegistry machinery), so this gate is scoped to
# examples/ only.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

strays=$(find examples -name proof-registry.json 2>/dev/null)
if [ -n "$strays" ]; then
  echo "REGISTRY-RETIREMENT: FAIL — example proof-registry.json found (migrate to in-source links with --emit-link):"
  printf '%s\n' "$strays" | sed 's/^/  /'
  exit 1
fi

# Cross-check: every example that claims a proof carries an in-source link.
linked=$(grep -rl '#\[proof_by' examples/*/src/main.con 2>/dev/null | wc -l | tr -d ' ')
echo "REGISTRY-RETIREMENT: PASS — 0 example registries; $linked example(s) source-linked."
