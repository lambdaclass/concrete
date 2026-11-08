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

echo "=== cross-machine reproducibility (ISS-0029) ==="
if python3 -I scripts/checks/lib/repro_guard_check.py "$WF_DIR/lean_action_ci.yml" &&
  python3 -I - "$WF_DIR/lean_action_ci.yml" "$GATE_TMP/compare-linux.sh" <<'PY'
import sys
from pathlib import Path
import yaml

workflow, output = sys.argv[1:]
jobs = yaml.safe_load(open(workflow))["jobs"]
repro = jobs["reproducibility"]
rows = repro["strategy"]["matrix"]["include"]
by_id = {row["id"]: row for row in rows}

def require(condition, message):
    if not condition:
        raise ValueError(message)

require(set(by_id) == {"linux-github", "linux-ci2", "macos"},
        f"reproducibility legs are {sorted(by_id)}")
require(by_id["linux-github"]["runner"] == "ubuntu-24.04",
        "linux-github is not GitHub-hosted ubuntu-24.04")
ci2_runner = by_id["linux-ci2"]["runner"]
require(ci2_runner == '${{ github.event_name != \'pull_request\' && fromJSON(\'["self-hosted","linux","x64","concrete","concrete-ci-2"]\') || \'ubuntu-24.04\' }}',
        f"linux-ci2 does not select only provisioned concrete-ci-2 runners outside pull_request: {ci2_runner}")
require(by_id["linux-ci2"]["machine"] == "concrete-ci-2",
        "linux-ci2 does not record its machine")
require(by_id["macos"]["runner"] == "macos-15", "the macOS leg changed")
require(repro["runs-on"] == "${{ matrix.runner }}", "reproducibility does not use matrix.runner")
nix = [step for step in repro["steps"] if step.get("uses") == "cachix/install-nix-action@v31"]
require(len(nix) == 1 and nix[0].get("if") == "runner.environment != 'self-hosted'",
        "Nix installation is not GitHub-hosted-only")
build = next(step for step in repro["steps"]
             if "slice0_repro.sh" in step.get("run", ""))
require(build["env"]["CONCRETE_REPRO_GITHUB_REPOSITORY"] == "${{ github.repository }}",
        "github.repository is not recorded")
require(build["env"]["CONCRETE_REPRO_RUNNER_ENVIRONMENT"] == "${{ runner.environment }}",
        "runner.environment is not recorded")
require(build["env"]["CONCRETE_REPRO_RUNNER_NAME"] == "${{ runner.name }}",
        "runner.name is not recorded")
require("CONCRETE_REPRO_RUNNER_HOST" not in build["env"],
        "runner_host is declared by the matrix and is not runner attestation")
compare = jobs["compare-linux"]
require(compare["needs"] == "reproducibility", "compare-linux does not need both Linux legs")
download = next(step for step in compare["steps"]
                if step.get("uses") == "actions/download-artifact@v4")
require(download["with"]["pattern"] == "reproducibility-linux-*",
        "compare-linux does not download the Linux manifests")
run = next(step["run"] for step in compare["steps"]
           if "repro_manifest_compare.py" in step.get("run", ""))
for text in ("reproducibility-linux-github/manifest.txt",
             "reproducibility-linux-ci2/manifest.txt"):
    require(text in run, f"compare-linux does not read {text}")
Path(output).write_text(run)
PY
then
  echo "  ok   workflow uses dedicated ci2 labels, records repository and behaviorally refuses substitutes"; PASS=$((PASS+1))
else
  echo "  FAIL workflow reproducibility structure or substitute guard"; FAIL=$((FAIL+1))
fi

guard_mutation_control() { # LABEL KIND EXPECTED_ERROR
  local label=$1 kind=$2 expected=$3
  local mutated="$GATE_TMP/guard-$label.yml" output="$GATE_TMP/guard-$label.out" rc=0
  if ! python3 -I - "$WF_DIR/lean_action_ci.yml" "$mutated" "$kind" <<'PY'
import copy
import sys
from pathlib import Path
import yaml

source, output, kind = sys.argv[1:]
workflow = copy.deepcopy(yaml.safe_load(Path(source).read_text()))
steps = workflow["jobs"]["reproducibility"]["steps"]
guard = next((step for step in steps if step.get("name") == "Refuse a substitute for concrete-ci-2"), None)
if kind == "removed":
    del steps[0]
elif kind == "moved":
    steps.insert(1, steps.pop(0))
elif kind == "exit0":
    guard["run"] = guard["run"].replace("exit 1", "exit 0 # exit 1")
elif kind == "inverted":
    guard["if"] = "!(" + guard["if"] + ")"
else:
    raise ValueError(kind)
Path(output).write_text(yaml.safe_dump(workflow, sort_keys=False))
PY
  then
    echo "  FAIL guard mutation $label could not be generated"; FAIL=$((FAIL+1)); return
  fi
  python3 -I scripts/checks/lib/repro_guard_check.py "$mutated" >"$output" 2>&1 || rc=$?
  if [ "$rc" != 0 ] && has_match -F "$expected" <"$output"; then
    echo "  ok   guard mutation $label is refused"; PASS=$((PASS+1))
  else
    echo "  FAIL guard mutation $label was not refused as '$expected' (rc=$rc)"
    sed 's/^/         /' "$output"; FAIL=$((FAIL+1))
  fi
}

guard_mutation_control body-exit0 exit0 "exited 0"
guard_mutation_control condition-inverted inverted "guard enabled="
guard_mutation_control guard-removed removed "exactly one"
guard_mutation_control guard-after-checkout moved "before checkout"

write_manifest() { # PATH ENVIRONMENT RUNNER_NAME HASH [REPOSITORY]
  local repository=${5:-unbalancedparentheses/concrete2}
  mkdir -p "${1%/*}"
  {
    [ "$repository" = none ] || printf 'github_repository=%s\n' "$repository"
    printf 'runner_environment=%s\n' "$2"
    printf 'runner_name=%s\n' "$3"
    printf 'compiler_commit=0123456789abcdef\n'
    printf 'target=x86_64-linux\n'
    [ "$4" = none ] || printf '%s  example/lcb.exe\n' "$4"
  } > "$1"
}

compare_control() { # LABEL SECOND_ENVIRONMENT SECOND_RUNNER_NAME SECOND_HASH SECOND_KIND EXPECTED_ERROR [CI2_REPOSITORY [GITHUB_REPOSITORY]]
  local label=$1 env=$2 name=$3 hash=$4 kind=$5 expected=$6
  local ci2_repository=${7:-unbalancedparentheses/concrete2}
  local github_repository=${8:-unbalancedparentheses/concrete2}
  local d="$GATE_TMP/repro-$label" rc=0
  mkdir -p "$d/scripts/checks/lib"
  cp scripts/checks/lib/repro_manifest_compare.py scripts/checks/lib/ci2_runners.txt "$d/scripts/checks/lib/"
  [ "$kind" != missing-inventory ] || rm "$d/scripts/checks/lib/ci2_runners.txt"
  write_manifest "$d/manifests/reproducibility-linux-github/manifest.txt" \
    github-hosted github-actions aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \
    "$github_repository"
  if [ "$kind" != missing ]; then
    write_manifest "$d/manifests/reproducibility-linux-ci2/manifest.txt" \
      "$env" "$name" "$hash" "$ci2_repository"
  fi
  (cd "$d" && bash "$GATE_TMP/compare-linux.sh") > "$d/output" 2>&1 || rc=$?
  if [ -z "$expected" ]; then
    if [ "$rc" = 0 ]; then
      echo "  ok   comparison control $label passes"; PASS=$((PASS+1))
    else
      echo "  FAIL comparison control $label refused"; sed 's/^/         /' "$d/output"; FAIL=$((FAIL+1))
    fi
  elif [ "$rc" != 0 ] && has_match -F "$expected" < "$d/output"; then
    echo "  ok   comparison control $label is refused"; PASS=$((PASS+1))
  else
    echo "  FAIL comparison control $label was not refused as '$expected' (rc=$rc)"
    sed 's/^/         /' "$d/output"; FAIL=$((FAIL+1))
  fi
}

if [ -s "$GATE_TMP/compare-linux.sh" ]; then
  a=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  b=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  compare_control identical self-hosted concrete-ci-2-gha1 "$a" normal ""
  compare_control lambdaclass self-hosted concrete-ci-2-gha4 "$a" normal "" \
    lambdaclass/concrete lambdaclass/concrete
  compare_control differing-ci2 self-hosted concrete-ci-2-gha1 "$b" normal "manifests differ"
  compare_control unregistered-pattern-name self-hosted concrete-ci-2-gha999 "$a" normal \
    "runner_name=concrete-ci-2-gha999 is not eligible for unbalancedparentheses/concrete2"
  compare_control wrong-repository-runner self-hosted concrete-ci-2-gha4 "$a" normal \
    "runner_name=concrete-ci-2-gha4 is not eligible for unbalancedparentheses/concrete2"
  compare_control light-pool-runner self-hosted concrete-ci-2-gha7 "$a" normal \
    "runner_name=concrete-ci-2-gha7 is not eligible for lambdaclass/concrete" \
    lambdaclass/concrete lambdaclass/concrete
  compare_control repository-mismatch self-hosted concrete-ci-2-gha4 "$a" normal \
    "github.repository differs" lambdaclass/concrete
  compare_control unknown-repository self-hosted concrete-ci-2-gha1 "$a" normal \
    "no concrete-ci-2 runner inventory for example/unknown" example/unknown example/unknown
  compare_control two-github github-hosted concrete-ci-2-gha1 "$a" normal \
    "expected runner_environment=self-hosted"
  compare_control missing-repository self-hosted concrete-ci-2-gha1 "$a" normal \
    "missing github_repository" none
  compare_control missing-inventory self-hosted concrete-ci-2-gha1 "$a" missing-inventory \
    "cannot read runner inventory"
  compare_control missing-manifest self-hosted concrete-ci-2-gha1 "$a" missing "cannot read manifest"
  compare_control missing-hashes self-hosted concrete-ci-2-gha1 none normal "missing lcb.exe SHA-256"
fi

echo ""
echo "WORKFLOW-YAML: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
