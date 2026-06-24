#!/usr/bin/env bash
# Stdlib handoff gate (ROADMAP Phase 6 #19; docs/STDLIB_HANDOFF.md).
#
# The handoff contract names every language surface the Phase 7 stdlib depends on
# and gives each a status. This gate keeps the contract honest:
#   - every REQUIRED surface appears in the table (none silently dropped),
#   - each status is one of stable_for_stdlib / provisional_with_gate / blocked,
#   - the backing artifact (a CI gate or a design doc) actually exists, so a
#     "stable"/"provisional" claim is tied to something real,
#   - POLICY: Phase 7 may not start while any required surface is `blocked` — a
#     blocked required surface fails this gate.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
DOC="docs/STDLIB_HANDOFF.md"
[ -f "$DOC" ] || { echo "error: $DOC missing" >&2; exit 2; }

# Canonical required surfaces (ROADMAP #19). Dropping one from the doc fails here.
REQUIRED="modules-imports project-model tests diagnostics bytes-text-path \
collections const-generics iteration callable-values capability-callbacks \
build-profiles cli-verbs"

PASS=0; FAIL=0; BLOCKED=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

valid_status(){ case "$1" in stable_for_stdlib|provisional_with_gate|blocked) return 0;; *) return 1;; esac; }

# Extract a table row's status (col 2) and backing (col 3) for a surface (col 1).
row_field(){ # $1=surface $2=field-index(2|3)
  awk -F'|' -v s="$1" -v f="$2" '
    { gsub(/^[ \t]+|[ \t]+$/, "", $2) }
    $2 == s { v=$(f+1); gsub(/^[ \t]+|[ \t]+$/, "", v); print v; exit }' "$DOC"; }

echo "=== stdlib handoff contract: every required surface present, valid, backed ==="
for surface in $REQUIRED; do
  status="$(row_field "$surface" 2)"
  backing="$(row_field "$surface" 3)"
  if [ -z "$status" ]; then no "$surface: missing from $DOC table"; continue; fi
  if ! valid_status "$status"; then no "$surface: invalid status '$status'"; continue; fi
  if [ "$status" = "blocked" ]; then
    no "$surface: BLOCKED — Phase 7 may not start while a required surface is blocked"
    BLOCKED=$((BLOCKED+1)); continue
  fi
  if [ -z "$backing" ] || [ ! -e "$backing" ]; then
    no "$surface ($status): backing artifact missing or absent: '$backing'"; continue
  fi
  ok "$surface: $status (backed by $backing)"
done

echo ""
echo "STDLIB-HANDOFF: PASS=$PASS  FAIL=$FAIL  (blocked required surfaces: $BLOCKED)"
[ "$FAIL" -eq 0 ]
