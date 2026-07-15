#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

WF_DIR=".github/workflows"
[ -d "$WF_DIR" ] || { echo "no $WF_DIR — nothing to check"; exit 0; }

parse_one() {
  local f="$1"
  if python3 -c 'import yaml,sys; yaml.safe_load(open(sys.argv[1]))' "$f" 2>/tmp/wfyaml.err; then
    return 0
  fi
  # python3 missing PyYAML (ModuleNotFoundError) → fall back to ruby
  if grep -q "No module named 'yaml'" /tmp/wfyaml.err 2>/dev/null; then
    ruby -ryaml -e "YAML.load_file(ARGV[0])" "$f" 2>/tmp/wfyaml.err
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
    echo "  FAIL $f"; sed 's/^/         /' /tmp/wfyaml.err | head -4; FAIL=$((FAIL+1))
  fi
done

echo ""
echo "WORKFLOW-YAML: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
