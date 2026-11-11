#!/usr/bin/env bash
# Workflow-YAML gate.
#
# GitHub Actions workflows that fail to PARSE do not run at all — they report a
# 0-second "completed failure" with no logs, so every gate they were supposed to
# run is silently skipped. This actually happened: a `name:` value that began
# with a backtick (`` ` `` is a reserved YAML indicator and cannot start a plain
# scalar) killed `lean_action_ci.yml` for 40+ consecutive pushes without any
# obvious signal. This gate parses every workflow locally so a malformed file is
# caught before it is pushed, not after CI has been dark for days.
#
# Uses python3+PyYAML if available, otherwise ruby's psych (both ship the parser
# GitHub uses closely enough to catch indicator/indentation errors).

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
# Per-run scratch (R-0546): a fixed /tmp path collides with a concurrent run of this gate.
GATE_TMP="$(mktemp -d)"; trap 'rm -rf "$GATE_TMP"' EXIT
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

WF_DIR=".github/workflows"
[ -d "$WF_DIR" ] || { echo "no $WF_DIR — nothing to check"; exit 0; }

parse_one() {
  local f="$1"
  if python3 -c 'import yaml,sys; yaml.safe_load(open(sys.argv[1]))' "$f" 2>"$GATE_TMP/wfyaml.err"; then
    return 0
  fi
  # python3 missing PyYAML (ModuleNotFoundError) → fall back to ruby
  if grep -q "No module named 'yaml'" "$GATE_TMP/wfyaml.err" 2>/dev/null; then
    ruby -ryaml -e "YAML.load_file(ARGV[0])" "$f" 2>"$GATE_TMP/wfyaml.err"
    return $?
  fi
  return 1
}

PASS=0; FAIL=0
for f in "$WF_DIR"/*.yml "$WF_DIR"/*.yaml; do
  [ -e "$f" ] || continue
  if parse_one "$f"; then
    echo "  ok   $f"; PASS=$((PASS+1))
  else
    echo "  FAIL $f"; sed 's/^/         /' "$GATE_TMP/wfyaml.err" | head -4; FAIL=$((FAIL+1))
  fi
done

# A copied workflow must not execute as another repository's control plane.
if python3 -I - "$WF_DIR" <<'PYGUARD'
import copy
import pathlib
import sys
import yaml

def unguarded(doc):
    bad = []
    for name, job in doc.get('jobs', {}).items():
        cond = str(job.get('if', '')).strip().removeprefix('${{').removesuffix('}}').strip()
        guard = "github.repository == 'lambdaclass/concrete'"
        if not (cond == guard or (cond.startswith(guard + ' && ') and '||' not in cond)):
            bad.append(name)
    return bad

fixture = {'jobs': {'run': {'if': "github.repository == 'lambdaclass/concrete'"}}}
assert not unguarded(fixture)
missing = copy.deepcopy(fixture); missing['jobs']['run'].pop('if')
assert unguarded(missing) == ['run']
wrong = copy.deepcopy(fixture); wrong['jobs']['run']['if'] = "github.repository == 'another/repo'"
assert unguarded(wrong) == ['run']
failed = []
for path in sorted(pathlib.Path(sys.argv[1]).glob('*')):
    if path.suffix not in ('.yml', '.yaml'): continue
    bad = unguarded(yaml.safe_load(path.read_text()))
    if bad: failed.append(f'{path}: unguarded jobs: {bad}')
if failed:
    print('\n'.join(failed)); sys.exit(1)
PYGUARD
then
  echo "  ok   every job runs only in the sole repository; missing/wrong guard controls refused"
  PASS=$((PASS+1))
else
  echo "  FAIL repository execution guards"; FAIL=$((FAIL+1))
fi

echo ""
echo "WORKFLOW-YAML: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
