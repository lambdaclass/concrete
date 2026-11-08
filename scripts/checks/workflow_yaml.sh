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
import importlib.util
import json
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
EXPECTED_CHECKOUT_REF = "${{ github.event_name != 'pull_request' && github.sha || '' }}"
BOUNDED_JOBS = ("determinism", "scaling", "artifacts", "trusted-base", "reproducibility")


def require(condition, message):
    if not condition:
        raise ValueError(message)


def load_gate_manifest(manifest_module_path):
    spec = importlib.util.spec_from_file_location("gate_manifest", manifest_module_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


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


def check_schedule_workflow(schedule_path, manifest_module, gates_override=None):
    rows = manifest_module.validate_manifest(gates_override)
    daily_rows = manifest_module.scheduled_rows("daily", rows)
    require(len(daily_rows) == 1 and daily_rows[0]["id"] == "scaling",
            "daily tier manifest missing scaling gate")
    schedule_wf = yaml.safe_load(Path(schedule_path).read_text(encoding="utf-8"))
    on_block = schedule_wf.get("on") or schedule_wf.get(True) or {}
    crons = [entry.get("cron") for entry in (on_block.get("schedule") or []) if isinstance(entry, dict)]
    require("17 2 * * *" in crons and "43 3 * * 0" in crons,
            "gate_schedule.yml is missing expected daily or weekly cron schedule")
    jobs = schedule_wf.get("jobs") or {}
    select_job = jobs.get("select") or {}
    require(select_job.get("runs-on") == "ubuntu-24.04",
            "gate_schedule.yml select job must run on ubuntu-24.04")
    select_steps = [st for st in (select_job.get("steps") or []) if st.get("id") == "select"]
    require(len(select_steps) == 1, "gate_schedule.yml select job must have one id=select step")
    select_run = select_steps[0].get("run", "")
    require(
        "tier=daily" in select_run
        and "if [ \"$CRON\" = '43 3 * * 0' ]; then tier=weekly; fi" in select_run
        and "SCHEDULE_SELECTION" in select_run
        and "merge-base --is-ancestor" in select_run,
        "gate_schedule.yml select step must route daily schedule to tier=daily",
    )
    campaign = jobs.get("campaign") or {}
    require(campaign.get("runs-on") == ["self-hosted", "linux", "x64", "concrete", "concrete-ci-2"],
            "gate_schedule.yml campaign job must route only to concrete-ci-2")
    c_steps = campaign.get("steps") or []
    run_steps = [st for st in c_steps if st.get("name") == "Run exact-SHA tier"]
    require(len(run_steps) == 1,
            "gate_schedule.yml campaign step must invoke scripts/checks/scheduled/run.py with SELECTED_SHA and SELECTED_TIER")
    run_step = run_steps[0]
    run_env = run_step.get("env") or {}
    run_body = run_step.get("run", "")
    require(
        run_env.get("SELECTED_SHA") == "${{ needs.select.outputs.sha }}"
        and run_env.get("SELECTED_TIER") == "${{ needs.select.outputs.tier }}"
        and 'python3 scripts/checks/scheduled/run.py "$SELECTED_SHA" "$SELECTED_TIER" result-index' in run_body,
        "gate_schedule.yml campaign step must invoke scripts/checks/scheduled/run.py with SELECTED_SHA and SELECTED_TIER",
    )


def check(workflow_path, schedule_path, manifest_module_path, gates_override=None):
    manifest_module = load_gate_manifest(manifest_module_path)
    check_schedule_workflow(schedule_path, manifest_module, gates_override)
    pr_row = manifest_module.tier_rows("pr", gates_override)[0]
    qual_row = manifest_module.tier_rows("qualification", gates_override)[0]
    workflow = yaml.safe_load(Path(workflow_path).read_text(encoding="utf-8"))
    jobs = workflow["jobs"]
    scaling = jobs["scaling"]
    require(scaling.get("runs-on") == EXPECTED_RUNS_ON,
            f"scaling does not route only to concrete-ci-2 outside pull_request: {scaling.get('runs-on')}")
    require(scaling.get("timeout-minutes") == 60,
            f"scaling timeout-minutes={scaling.get('timeout-minutes')}, expected 60 for cold hosted PR builds")
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
        enabled = evaluate_condition(condition, {"runner": {"environment": env_name}, "github": {"event_name": "workflow_dispatch"}})
        require(enabled == should_refuse, f"{label}: guard enabled={enabled}, expected {should_refuse}")
        if enabled:
            res = execute_guard(body)
            require(res.returncode != 0, f"{label}: substitute guard exited 0")
            output = res.stdout + res.stderr
            require(all(t in output for t in ("concrete-ci-2", "self-hosted", "qualification")),
                    f"{label}: refusal does not explain concrete-ci-2/self-hosted/qualification")
    require(not evaluate_condition(condition, {"runner": {"environment": "github-hosted"},
                "github": {"event_name": "pull_request"}}), "PR coverage is intentionally refused")
    checkouts = [st for st in steps if st.get("uses") == "actions/checkout@v4"]
    require(len(checkouts) == 1, "scaling must contain exactly one checkout step")
    checkout_with = checkouts[0].get("with") or {}
    require(
        checkout_with.get("ref") == EXPECTED_CHECKOUT_REF and checkout_with.get("persist-credentials") is False,
        "scaling checkout must bind ref to github.sha outside pull_request and disable credential persistence",
    )
    pr = [st for st in steps if st.get("if") == "github.event_name == 'pull_request'"]
    require(len(pr) == 1, "missing deterministic PR family coverage")
    pr_run = pr[0].get("run", "")
    pr_expected_cmd = " ".join(pr_row["command"])
    require("scripts/checks/lib/bounded.sh" in pr_run and pr_expected_cmd in pr_run and "lean --run" not in pr_run,
            "PR scaling step must invoke scripts/checks/compiler/scaling.sh with SCALING_TIER=pr (so Concrete is built before lean --run)")
    nix = [step for step in steps if step.get("uses") == "cachix/install-nix-action@v31"]
    require(len(nix) == 1 and nix[0].get("if") == "runner.environment != 'self-hosted'",
            "scaling Nix installation is not GitHub-hosted-only")
    heavy = [step for step in steps if "scripts/checks/compiler/scaling.sh" in step.get("run", "")
             and step.get("if") != "github.event_name == 'pull_request'"]
    require(len(heavy) == 1 and heavy[0].get("if") == "github.event_name != 'pull_request'",
            "heavy scaling executes pull_request code")
    run_step = heavy[0]
    qual_expected_cmd = " ".join(qual_row["command"])
    require(qual_expected_cmd in run_step.get("run", ""),
            "qualification scaling step must invoke scripts/checks/compiler/scaling.sh with SCALING_TIER=qualification")
    env = run_step.get("env") or {}
    require(env.get("CONCRETE_SCALING_EXPECTED_SHA") == "${{ github.sha }}",
            "scaling does not bind CONCRETE_SCALING_EXPECTED_SHA to github.sha")
    require(env.get("CONCRETE_SCALING_EVENT_NAME") == "${{ github.event_name }}",
            "scaling does not record github.event_name")
    require(env.get("CONCRETE_SCALING_REF") == "${{ github.ref }}",
            "scaling does not record github.ref")
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
            require("lean --run" not in cmd,
                    f"{job_name} invokes lean --run directly in workflow YAML without building Concrete first: {cmd}")
        require(any("scripts/checks/lib/bounded.sh" in st.get("run", "") for st in job_steps),
                f"{job_name} does not invoke scripts/checks/lib/bounded.sh")


if __name__ == "__main__":
    try:
        override = json.loads(Path(sys.argv[4]).read_text(encoding="utf-8")) if len(sys.argv) > 4 else None
        check(sys.argv[1], sys.argv[2], sys.argv[3], override)
    except Exception as exc:
        sys.exit(f"FAIL workflow scaling/bounded check: {exc}")
PY

if python3 -I "$GATE_TMP/check_scaling_ci.py" "$WF_DIR/lean_action_ci.yml" "$WF_DIR/gate_schedule.yml" "scripts/checks/lib/gate_manifest.py"; then
  echo "  ok   scaling concrete-ci-2 guard, daily schedule routing and bounded workflow runners"; PASS=$((PASS+1))
else
  echo "  FAIL scaling concrete-ci-2 guard, daily schedule routing and bounded workflow runners"; FAIL=$((FAIL+1))
fi

mutation_control() { # LABEL KIND EXPECTED
  local label=$1 kind=$2 expected=$3 mutated_ci="$GATE_TMP/mut-ci-$1.yml" mutated_sched="$GATE_TMP/mut-sched-$1.yml" mutated_gates="$GATE_TMP/mut-gates-$1.json" out="$GATE_TMP/mut-$1.out" rc=0
  python3 -I - "$WF_DIR/lean_action_ci.yml" "$WF_DIR/gate_schedule.yml" "scripts/checks/lib/gate_manifest.py" "$mutated_ci" "$mutated_sched" "$mutated_gates" "$kind" <<'PY'
import copy
import importlib.util
import json
import sys
from pathlib import Path
import yaml

ci_src, sched_src, manifest_src, ci_dst, sched_dst, gates_dst, kind = sys.argv[1:]
wf = yaml.safe_load(Path(ci_src).read_text(encoding="utf-8"))
sched = yaml.safe_load(Path(sched_src).read_text(encoding="utf-8"))
spec = importlib.util.spec_from_file_location("gate_manifest", manifest_src)
manifest = importlib.util.module_from_spec(spec)
spec.loader.exec_module(manifest)
gates = copy.deepcopy(manifest.GATES)

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
elif kind == "pr-unbuilt-lean":
    for st in steps:
        if st.get("if") == "github.event_name == 'pull_request'":
            st["run"] = "nix develop --command bash scripts/checks/lib/bounded.sh 120 lake env lean --run scripts/checks/compiler/scaling.lean check-small"
elif kind == "missing-expected-sha":
    for st in steps:
        if st.get("if") == "github.event_name != 'pull_request'" and "env" in st:
            st["env"].pop("CONCRETE_SCALING_EXPECTED_SHA", None)
elif kind == "unpinned-checkout":
    for st in steps:
        if st.get("uses") == "actions/checkout@v4":
            st["with"] = {"persist-credentials": True}
elif kind == "daily-removed":
    gates = [row for row in gates if not (row["id"] == "scaling" and row["tier"] == "daily")]
elif kind == "daily-misrouted-tier":
    for row in gates:
        if row["id"] == "scaling" and row["tier"] == "daily":
            row["tier"] = "weekly"
elif kind == "daily-wrong-scaling-tier":
    for row in gates:
        if row["id"] == "scaling" and row["tier"] == "daily":
            row["command"] = [
                "env",
                "SCALING_TIER=qualification",
                "bash",
                "scripts/checks/compiler/scaling.sh",
                "scaling-report",
            ]
elif kind == "schedule-daily-misrouted":
    for st in sched["jobs"]["select"]["steps"]:
        if st.get("id") == "select":
            st["run"] = st["run"].replace("tier=daily", "tier=weekly")
elif kind == "schedule-runner-removed":
    for st in sched["jobs"]["campaign"]["steps"]:
        if st.get("name") == "Run exact-SHA tier":
            st["run"] = "echo skipped\n"

Path(ci_dst).write_text(yaml.safe_dump(wf, sort_keys=False), encoding="utf-8")
Path(sched_dst).write_text(yaml.safe_dump(sched, sort_keys=False), encoding="utf-8")
Path(gates_dst).write_text(json.dumps(gates), encoding="utf-8")
PY
  python3 -I "$GATE_TMP/check_scaling_ci.py" "$mutated_ci" "$mutated_sched" "scripts/checks/lib/gate_manifest.py" "$mutated_gates" >"$out" 2>&1 || rc=$?
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
mutation_control pr-unbuilt-lean pr-unbuilt-lean "PR scaling step must invoke scripts/checks/compiler/scaling.sh with SCALING_TIER=pr"
mutation_control missing-expected-sha missing-expected-sha "scaling does not bind CONCRETE_SCALING_EXPECTED_SHA to github.sha"
mutation_control unpinned-checkout unpinned-checkout "scaling checkout must bind ref to github.sha outside pull_request and disable credential persistence"
mutation_control daily-removed daily-removed "daily tier manifest missing scaling gate"
mutation_control daily-misrouted-tier daily-misrouted-tier "scaling daily gate misrouted to tier 'weekly'"
mutation_control daily-wrong-scaling-tier daily-wrong-scaling-tier "daily scaling gate must run with SCALING_TIER=daily"
mutation_control schedule-daily-misrouted schedule-daily-misrouted "gate_schedule.yml select step must route daily schedule to tier=daily"
mutation_control schedule-runner-removed schedule-runner-removed "gate_schedule.yml campaign step must invoke scripts/checks/scheduled/run.py with SELECTED_SHA and SELECTED_TIER"

echo ""
echo "WORKFLOW-YAML: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
