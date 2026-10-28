#!/usr/bin/env bash
# Fail-closed structural gate for ROADMAP.md's single stable-ID task sequence.
# Since the 2026-10-19 backlog split the sequence spans the task register and the backlog
# (docs/project/ROADMAP_TASKS.md, docs/project/ROADMAP_BACKLOG.md; ROADMAP.md itself keeps the control
# plane since 2026-10-28): every task is declared once across them (and CHANGELOG's Completed Tasks), and
# all three files obey the rules below.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail=0
active_file="$(mktemp)"
retired_file="$(mktemp)"
all_ids_file="$(mktemp)"
changelog_file="$(mktemp)"
trap 'rm -f "$active_file" "$retired_file" "$all_ids_file" "$changelog_file"' EXIT
# CHANGELOG.md with the pending changelog.d/ fragments folded in (scripts/gen/changelog.py): a branch
# retires a task with a `### Completed Task R-NNNN` heading in its fragment.
if ! python3 -I scripts/gen/changelog.py --print > "$changelog_file"; then
  echo "FAIL scripts/gen/changelog.py --print failed; the changelog cannot be read"
  exit 1
fi
# The 2026-10-19 reconciliation record (moved out of ROADMAP.md) cites task IDs but declares none.
RECORD="docs/project/ROADMAP_RECONCILIATION.md"
BACKLOG="docs/project/ROADMAP_BACKLOG.md"
if [ ! -f "$BACKLOG" ]; then
  echo "FAIL $BACKLOG is missing; ROADMAP.md links it as the home of unscheduled tasks"
  fail=$((fail + 1))
  BACKLOG=""
elif grep -nE '^#+ Completed Task R-[0-9]{4}' "$BACKLOG"; then
  echo "FAIL $BACKLOG declares completed tasks; those belong to CHANGELOG.md"
  fail=$((fail + 1))
fi
TASKS="docs/project/ROADMAP_TASKS.md"
if [ ! -f "$TASKS" ]; then
  echo "FAIL $TASKS is missing; ROADMAP.md links it as the home of scheduled task bodies"
  fail=$((fail + 1))
  TASKS=""
elif grep -nE '^#+ Completed Task R-[0-9]{4}' "$TASKS"; then
  echo "FAIL $TASKS declares completed tasks; those belong to CHANGELOG.md"
  fail=$((fail + 1))
fi
TASK_FILES="ROADMAP.md${TASKS:+ $TASKS}${BACKLOG:+ $BACKLOG}"
if [ ! -f "$RECORD" ]; then
  echo "FAIL $RECORD is missing; ROADMAP.md's Reconciliation record links it"
  fail=$((fail + 1))
elif grep -nE '^#+ (Completed )?Task R-[0-9]{4}' "$RECORD"; then
  echo "FAIL $RECORD declares task headings; task IDs are declared only in ROADMAP.md and CHANGELOG.md"
  fail=$((fail + 1))
fi

# The roadmap archives (history and retired task bodies) cite task IDs but declare none either: a
# retired body sits under its bare `## R-NNNN` heading, its `### Task` stub stays in ROADMAP.md.
# The roadmap's companion documents (direction, decision log, promotion pipeline, and the archived
# language scope pending absorption into the specification) cite task IDs under the same rule.
ARCHIVES="docs/archive/ROADMAP_HISTORY.md docs/archive/ROADMAP_RETIRED_TASKS.md"
for c in docs/project/ROADMAP_DIRECTION.md docs/project/ROADMAP_DECISIONS.md docs/project/PROMOTION_PIPELINE.md docs/archive/roadmap-language-scope.md; do
  [ -f "$c" ] && ARCHIVES="$ARCHIVES $c"
done
for a in $ARCHIVES; do
  if [ ! -f "$a" ]; then
    echo "FAIL $a is missing; ROADMAP.md links it"
    fail=$((fail + 1))
  elif grep -nE '^#+ (Completed )?Task R-[0-9]{4}' "$a"; then
    echo "FAIL $a declares task headings; task IDs are declared only in ROADMAP.md and CHANGELOG.md"
    fail=$((fail + 1))
  fi
done

# shellcheck disable=SC2086
cat $TASK_FILES | grep -oE '^### Task R-[0-9]{4}$' | awk '{print $3}' > "$active_file" || true
grep -oE '^### Completed Task R-[0-9]{4}$' "$changelog_file" | awk '{print $4}' > "$retired_file" || true
cat "$active_file" "$retired_file" > "$all_ids_file"
count="$(wc -l < "$active_file" | tr -d ' ')"

if [ "$count" -eq 0 ]; then
  echo "FAIL ROADMAP.md, its task register and its backlog have no stable task IDs"
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
  # shellcheck disable=SC2086
  grep -nHoE 'R-[0-9]{4}' $TASK_FILES CHANGELOG.md $(ls changelog.d/[0-9]*.md 2>/dev/null) $([ -f "$RECORD" ] && echo "$RECORD") \
    $(for a in $ARCHIVES; do [ -f "$a" ] && echo "$a"; done)
)

# shellcheck disable=SC2086
if grep -nE '^(##|###) (Current Frontier|.*Queue|Phase .* Tasks)$' $TASK_FILES; then
  echo "FAIL ROADMAP.md or its backlog reintroduced a parallel queue or phase-local task list"
  fail=$((fail + 1))
fi

# shellcheck disable=SC2086
if grep -nE '^### Task (R-[0-9]{1,3}|[0-9]+)([^0-9]|$)' $TASK_FILES; then
  echo "FAIL malformed or legacy positional task heading"
  fail=$((fail + 1))
fi

# shellcheck disable=SC2086
if grep -nE '\bTasks? [0-9]+\b' $TASK_FILES; then
  echo "FAIL legacy positional task reference; use stable R-NNNN identity"
  fail=$((fail + 1))
fi

if [ "$fail" -ne 0 ]; then
  echo "ROADMAP-LINEAR: FAIL=$fail ACTIVE_TASKS=$count"
  exit 1
fi

first="$(head -n 1 "$active_file")"
echo "ROADMAP-LINEAR: PASS ACTIVE_TASKS=$count FIRST=$first"
