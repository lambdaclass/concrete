#!/usr/bin/env bash
# Diagnostic-code registry completeness gate (ROADMAP 13m, first slice).
#
# `--report diagnostic-codes` (Concrete/Report/Report.lean) claims to be the ledger
# of every diagnostic the compiler can emit. It silently drifted 8 codes
# behind Check.lean (E0286–E0293 were shipped but unlisted, found 2026-07-06).
# This gate extracts every `=> "E NNNN"` code from the CheckError.code and
# CoreCheckError-style mappings and requires each to appear as an `entry` in
# Report.lean — a new diagnostic cannot land without its ledger row.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== every emittable E-code has a --report diagnostic-codes entry ==="
missing=0
# CheckError / CoreCheck / Elab map errors to codes (`=> "E…"`); the post-mono verifier builds its
# diagnostics directly (`code := "E…"`), so both spellings are scanned (exit 2i G2: Verify.lean's
# E0236 was outside this gate).
# Bug 206 added the sites that wrote their codes outside these mappings (Check, the module
# loader in Project, FileSummary, SSAVerify).
for f in Concrete/Check/CheckError.lean Concrete/Check/CoreCheck.lean Concrete/Check/Verify.lean Concrete/Resolve/Resolve.lean Concrete/Elab/Elab.lean Concrete/Check/Check.lean Concrete/Resolve/Project.lean Concrete/Resolve/FileSummary.lean Concrete/IR/SSAVerify.lean; do
  codes="$(grep -oE '(=> |code := |loadError )"E[0-9]{4}"' "$f" | grep -oE 'E[0-9]{4}' | sort -u)"
  [ -z "$codes" ] && { no "$f: extracted no codes (pattern drift?)"; continue; }
  while IFS= read -r c; do
    # `entryR` is an entry that also names the exit 2i rules the code enforces (§15.5)
    if ! grep -qE "entryR? \"$c\"" Concrete/Report/Report.lean; then
      no "$c (emitted by $f) has no entry in Report.lean diagnostic-codes"
      missing=1
    fi
  done <<< "$codes"
done
[ "$missing" -eq 0 ] && ok "all emittable codes are in the report ledger"

echo ""
echo "DIAGNOSTIC-CODES-COMPLETE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
