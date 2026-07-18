#!/usr/bin/env bash
# Fail-closed structural gate for ROADMAP.md's single global task sequence.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail=0
expected=0
while IFS=: read -r line heading; do
  number="${heading#\#\#\# Task }"
  number="${number%% *}"
  expected=$((expected + 1))
  if [ "$number" != "$expected" ]; then
    echo "FAIL ROADMAP.md:$line: expected Task $expected, found Task $number"
    fail=$((fail + 1))
  fi
done < <(grep -nE '^### Task [0-9]+$' ROADMAP.md)

if [ "$expected" -eq 0 ]; then
  echo "FAIL ROADMAP.md has no globally numbered tasks"
  fail=$((fail + 1))
fi

if grep -nE '^(##|###) (Current Frontier|.*Queue|Phase .* Tasks)$' ROADMAP.md; then
  echo "FAIL ROADMAP.md reintroduced a parallel queue or phase-local task list"
  fail=$((fail + 1))
fi

if grep -nE '^### Task [0-9]+[^0-9[:space:]]' ROADMAP.md; then
  echo "FAIL malformed task heading"
  fail=$((fail + 1))
fi

if [ "$fail" -ne 0 ]; then
  echo "ROADMAP-LINEAR: FAIL=$fail TASKS=$expected"
  exit 1
fi

echo "ROADMAP-LINEAR: PASS TASKS=$expected"
