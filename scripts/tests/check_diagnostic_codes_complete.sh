#!/usr/bin/env bash
# Diagnostic-code registry completeness gate (ROADMAP 13m, first slice).
#
# `--report diagnostic-codes` (Concrete/Report.lean) claims to be the ledger
# of every diagnostic the compiler can emit. It silently drifted 8 codes
# behind Check.lean (E0286–E0293 were shipped but unlisted, found 2026-07-06).
# This gate extracts every `=> "E NNNN"` code from the CheckError.code and
# CoreCheckError-style mappings and requires each to appear as an `entry` in
# Report.lean — a new diagnostic cannot land without its ledger row.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== every emittable E-code has a --report diagnostic-codes entry ==="
missing=0
for f in Concrete/Check.lean Concrete/CoreCheck.lean; do
  codes="$(grep -oE '=> "E[0-9]{4}"' "$f" | grep -oE 'E[0-9]{4}' | sort -u)"
  [ -z "$codes" ] && { no "$f: extracted no codes (pattern drift?)"; continue; }
  while IFS= read -r c; do
    if ! grep -q "entry \"$c\"" Concrete/Report.lean; then
      no "$c (emitted by $f) has no entry in Report.lean diagnostic-codes"
      missing=1
    fi
  done <<< "$codes"
done
[ "$missing" -eq 0 ] && ok "all emittable codes are in the report ledger"

echo ""
echo "DIAGNOSTIC-CODES-COMPLETE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
