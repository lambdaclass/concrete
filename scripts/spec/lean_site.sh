#!/usr/bin/env bash
# Build the browsable owner view of ConcreteSpec into OUT (default _build/spec-site).
# Builds the specification and its evidence. Fails on a broken link.
set -euo pipefail
cd "$(dirname "$0")/../.."
OUT=${1:-_build/spec-site}
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
lake build ConcreteSpecEvidence
lake env lean scripts/spec/lean_site.lean > "$TMP/decls.jsonl"
rm -rf "$OUT"; mkdir -p "$OUT"
python3 scripts/spec/lean_site.py "$TMP/decls.jsonl" "$OUT"
