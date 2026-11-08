#!/usr/bin/env bash
# Every root check, in order; the one entry point the pre-push hook, CI's "Root checks" job and the
# landing tools (scripts/land/train.sh, ready.sh: LAND_FULL_GATES_CMD) run.
#
# The set is every scripts/checks/*.sh except this file, so a new check cannot be written and then
# forgotten here. REQUIRED names the checks that must exist: deleting one fails the run instead of
# shrinking it. Each check runs to completion; the run fails if any check failed.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
REQUIRED="legacy_frozen root_inventory workflow_yaml doc_links landing_script landing_ready review_tool sandbox_isolation retention_manifest issues oracle_fetch test_manifest import_layering"
missing=""
for c in $REQUIRED; do [ -f "scripts/checks/$c.sh" ] || missing="$missing $c"; done
[ -z "$missing" ] || { echo "run.sh: required check(s) missing:$missing" >&2; exit 1; }
failed="" n=0
for f in scripts/checks/*.sh; do
  [ "$f" = scripts/checks/run.sh ] && continue
  n=$((n+1))
  echo "=== $f ==="
  s="$(date +%s)"
  if bash "$f"; then echo "--- $f: pass ($(( $(date +%s) - s ))s)"
  else echo "--- $f: FAIL ($(( $(date +%s) - s ))s)"; failed="$failed ${f##*/}"; fi
done
echo
if [ -n "$failed" ]; then echo "root checks: FAILED:$failed (of $n)"; exit 1; fi
echo "root checks: all $n pass"
