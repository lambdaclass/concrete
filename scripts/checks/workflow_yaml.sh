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

cat > "$GATE_TMP/check_scaling_ci.py" <<'PY'
import ast
import subprocess
import sys
import tempfile
from pathlib import Path
import yaml

GUARD_NAME = "Refuse a substitute for concrete-ci-2"
EXPECTED_RUNS_ON = (
    '${{ github.event_name != \'pull_request\' && '
    'fromJSON(\'["self-hosted","linux","x64","concrete","concrete-ci-2"]\') || \'ubuntu-24.04\' }}'
)
BOUNDED_JOBS = ("determinism", "scaling", "artifacts", "trusted-base", "reproducibility")


def require(condition, message):
    if not condition:
        raise ValueError(message)


def lookup(node, context):
    if isinstance(node, ast.Name):
        require(node.id in context, f"unknown context name {node.id}")
        return context[node.id]
    if isinstance(node, ast.Attribute):
        value = lookup(node.value, context)
        require(isinstance(value, dict) and node.attr in value,
                f"unknown context attribute {node.attr}")
        return value[node.attr]
    raise ValueError("only context names and attributes are allowed")


def evaluate_node(node, context):
    if isinstance(node, ast.BoolOp) and isinstance(node.op, (ast.And, ast.Or)):
        values = (evaluate_node(value, context) for value in node.values)
        return all(values) if isinstance(node.op, ast.And) else any(values)
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.Not):
        return not evaluate_node(node.operand, context)
    if isinstance(node, ast.Compare) and len(node.ops) == 1 and isinstance(node.ops[0], (ast.Eq, ast.NotEq)):
        left = evaluate_node(node.left, context)
        right = evaluate_node(node.comparators[0], context)
        return (left == right) if isinstance(node.ops[0], ast.Eq) else (left != right)
    if isinstance(node, ast.Constant) and isinstance(node.value, (str, bool)):
        return node.value
    if isinstance(node, (ast.Name, ast.Attribute)):
        return lookup(node, context)
    raise ValueError(f"unsupported GitHub expression syntax: {ast.dump(node)}")


def evaluate_condition(condition, context):
    expression = condition.strip()
    if expression.startswith("${{") and expression.endswith("}}"):
        expression = expression[3:-2].strip()
    expression = expression.replace("&&", " and ").replace("||", " or ")
    expression = expression.replace("!=", "\0").replace("!", " not ").replace("\0", "!=")
    tree = ast.parse(expression.strip(), mode="eval")
    result = evaluate_node(tree.body, context)
    require(isinstance(result, bool), f"condition is not boolean: {condition}")
    return result


def execute_guard(body):
    with tempfile.TemporaryDirectory(prefix="concrete-scaling-guard-") as work:
        return subprocess.run(
            ["bash", "--noprofile", "--norc", "-eo", "pipefail", "-c", body],
            cwd=work,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )


def check(workflow_path):
    workflow = yaml.safe_load(Path(workflow_path).read_text(encoding="utf-8"))
    jobs = workflow["jobs"]
    scaling = jobs["scaling"]
    require(scaling.get("runs-on") == EXPECTED_RUNS_ON,
            f"scaling does not route only to concrete-ci-2 outside pull_request: {scaling.get('runs-on')}")
    steps = scaling["steps"]
    guards = [step for step in steps if step.get("name") == GUARD_NAME]
    require(len(guards) == 1, f"scaling must contain exactly one {GUARD_NAME!r} step")
    require(steps[0] is guards[0], "the scaling concrete-ci-2 substitute guard must run before checkout")
    guard = guards[0]
    condition = guard.get("if")
    body = guard.get("run")
    require(isinstance(condition, str) and condition.strip(), "the substitute guard has no if condition")
    require(isinstance(body, str) and body.strip(), "the substitute guard has no run body")
    for label, env_name, should_refuse in (
        ("self-hosted concrete-ci-2", "self-hosted", False),
        ("GitHub-hosted substitute", "github-hosted", True),
    ):
        enabled = evaluate_condition(condition, {"runner": {"environment": env_name}})
        require(enabled == should_refuse, f"{label}: guard enabled={enabled}, expected {should_refuse}")
        if enabled:
            res = execute_guard(body)
            require(res.returncode != 0, f"{label}: substitute guard exited 0")
            output = res.stdout + res.stderr
            require(all(t in output for t in ("concrete-ci-2", "self-hosted", "pull_request")),
                    f"{label}: refusal does not explain concrete-ci-2/self-hosted/pull_request")
    nix = [step for step in steps if step.get("uses") == "cachix/install-nix-action@v31"]
    require(len(nix) == 1 and nix[0].get("if") == "runner.environment != 'self-hosted'",
            "scaling Nix installation is not GitHub-hosted-only")
    run_step = next(step for step in steps if "scaling.sh" in step.get("run", ""))
    env = run_step.get("env") or {}
    require(env.get("CONCRETE_SCALING_GITHUB_REPOSITORY") == "${{ github.repository }}",
            "scaling does not record github.repository")
    require(env.get("CONCRETE_SCALING_RUNNER_ENVIRONMENT") == "${{ runner.environment }}",
            "scaling does not record runner.environment")
    require(env.get("CONCRETE_SCALING_RUNNER_NAME") == "${{ runner.name }}",
            "scaling does not record runner.name")
    for job_name in BOUNDED_JOBS:
        job_steps = jobs[job_name]["steps"]
        for st in job_steps:
            cmd = st.get("run", "")
            require("command timeout " not in cmd,
                    f"{job_name} uses GNU timeout instead of scripts/checks/lib/bounded.sh: {cmd}")
        require(any("scripts/checks/lib/bounded.sh" in st.get("run", "") for st in job_steps),
                f"{job_name} does not invoke scripts/checks/lib/bounded.sh")


if __name__ == "__main__":
    try:
        check(sys.argv[1])
    except Exception as exc:
        sys.exit(f"FAIL workflow scaling/bounded check: {exc}")
PY

if python3 -I "$GATE_TMP/check_scaling_ci.py" "$WF_DIR/lean_action_ci.yml"; then
  echo "  ok   scaling concrete-ci-2 guard and bounded workflow runners"; PASS=$((PASS+1))
else
  echo "  FAIL scaling concrete-ci-2 guard and bounded workflow runners"; FAIL=$((FAIL+1))
fi

mutation_control() { # LABEL KIND EXPECTED
  local label=$1 kind=$2 expected=$3 mutated="$GATE_TMP/mut-$1.yml" out="$GATE_TMP/mut-$1.out" rc=0
  python3 -I - "$WF_DIR/lean_action_ci.yml" "$mutated" "$kind" <<'PY'
import sys
from pathlib import Path
import yaml

src, dst, kind = sys.argv[1:]
wf = yaml.safe_load(Path(src).read_text(encoding="utf-8"))
steps = wf["jobs"]["scaling"]["steps"]
if kind == "exit0":
    steps[0]["run"] = "exit 0\n"
elif kind == "inverted":
    steps[0]["if"] = "runner.environment == 'self-hosted'"
elif kind == "removed":
    wf["jobs"]["scaling"]["steps"] = steps[1:]
elif kind == "moved":
    wf["jobs"]["scaling"]["steps"] = [steps[1], steps[0], *steps[2:]]
elif kind == "gnu-timeout":
    wf["jobs"]["determinism"]["steps"][-1]["run"] = "nix develop --command timeout 3000 bash scripts/checks/compiler/determinism.sh"
Path(dst).write_text(yaml.safe_dump(wf, sort_keys=False), encoding="utf-8")
PY
  python3 -I "$GATE_TMP/check_scaling_ci.py" "$mutated" >"$out" 2>&1 || rc=$?
  if [ "$rc" != 0 ] && grep -qF "$expected" "$out"; then
    echo "  ok   workflow mutation control $label is refused"; PASS=$((PASS+1))
  else
    echo "  FAIL workflow mutation control $label was not refused as '$expected' (rc=$rc)"
    sed 's/^/         /' "$out"; FAIL=$((FAIL+1))
  fi
}

mutation_control body-exit0 exit0 "substitute guard exited 0"
mutation_control condition-inverted inverted "guard enabled="
mutation_control guard-removed removed "exactly one"
mutation_control guard-after-checkout moved "before checkout"
mutation_control gnu-timeout gnu-timeout "uses GNU timeout"

echo ""
echo "WORKFLOW-YAML: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
