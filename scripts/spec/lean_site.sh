#!/usr/bin/env bash
# Build the browsable owner view of ConcreteSpec into OUT (default _build/spec-site).
# Requires a built ConcreteSpec (`lake build ConcreteSpec`). Fails on a broken link.
set -euo pipefail
cd "$(dirname "$0")/../.."
OUT=${1:-_build/spec-site}
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
lake env lean scripts/spec/lean_site.lean > "$TMP/decls.jsonl"
rm -rf "$OUT"; mkdir -p "$OUT"
python3 scripts/spec/lean_site.py "$TMP/decls.jsonl" "$OUT"
