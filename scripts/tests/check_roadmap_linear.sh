#!/usr/bin/env bash
# Fail-closed structural gate for ROADMAP.md's single stable-ID task sequence.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail=0
active_file="$(mktemp)"
retired_file="$(mktemp)"
all_ids_file="$(mktemp)"
trap 'rm -f "$active_file" "$retired_file" "$all_ids_file"' EXIT

grep -oE '^### Task R-[0-9]{4}$' ROADMAP.md | awk '{print $3}' > "$active_file" || true
grep -oE '^### Completed Task R-[0-9]{4}$' CHANGELOG.md | awk '{print $4}' > "$retired_file" || true
cat "$active_file" "$retired_file" > "$all_ids_file"
count="$(wc -l < "$active_file" | tr -d ' ')"

if [ "$count" -eq 0 ]; then
  echo "FAIL ROADMAP.md has no stable task IDs"
  fail=$((fail + 1))
fi

duplicates="$(sort "$all_ids_file" | uniq -d)"
if [ -n "$duplicates" ]; then
  echo "FAIL task IDs are declared more than once:"
  printf '%s\n' "$duplicates"
  fail=$((fail + 1))
fi

while IFS=: read -r file line ref; do
  if ! grep -qx "$ref" "$all_ids_file"; then
    echo "FAIL $file:$line: dangling task reference $ref"
    fail=$((fail + 1))
  fi
done < <(
  grep -nHoE 'R-[0-9]{4}' ROADMAP.md CHANGELOG.md
)

if grep -nE '^(##|###) (Current Frontier|.*Queue|Phase .* Tasks)$' ROADMAP.md; then
  echo "FAIL ROADMAP.md reintroduced a parallel queue or phase-local task list"
  fail=$((fail + 1))
fi

if grep -nE '^### Task (R-[0-9]{1,3}|[0-9]+)([^0-9]|$)' ROADMAP.md; then
  echo "FAIL malformed or legacy positional task heading"
  fail=$((fail + 1))
fi

if grep -nE '\bTasks? [0-9]+\b' ROADMAP.md; then
  echo "FAIL legacy positional task reference; use stable R-NNNN identity"
  fail=$((fail + 1))
fi

if [ "$fail" -ne 0 ]; then
  echo "ROADMAP-LINEAR: FAIL=$fail ACTIVE_TASKS=$count"
  exit 1
fi

first="$(head -n 1 "$active_file")"
echo "ROADMAP-LINEAR: PASS ACTIVE_TASKS=$count FIRST=$first"
