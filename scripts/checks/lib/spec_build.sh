#!/usr/bin/env bash
set -euo pipefail
OUTPUT=$(lake build 2>&1) || { echo "$OUTPUT"; exit 1; }
if printf '%s\n' "$OUTPUT" | grep -i "warning"; then
  echo "::error::Build produced warnings"
  printf '%s\n' "$OUTPUT"
  exit 1
fi
