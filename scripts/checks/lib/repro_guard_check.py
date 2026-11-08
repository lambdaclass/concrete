#!/usr/bin/env python3
"""Evaluate and execute the linux-ci2 substitute-refusal step in representative contexts."""
import ast
import subprocess
import sys
import tempfile
from pathlib import Path

import yaml

GUARD_NAME = "Refuse a substitute for concrete-ci-2"
CASES = (
    ("ci2 self-hosted", "push", "concrete-ci-2", "self-hosted", False),
    ("ci2 GitHub-hosted substitute on push", "push", "concrete-ci-2", "github-hosted", True),
    ("ci2 GitHub-hosted substitute on pull_request", "pull_request", "concrete-ci-2", "github-hosted", True),
    ("GitHub leg", "push", "github", "github-hosted", False),
    ("GitHub leg with unexpected runner environment", "push", "github", "self-hosted", False),
)


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
        if isinstance(node.op, ast.And):
            return all(values)
        return any(values)
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
    with tempfile.TemporaryDirectory(prefix="concrete-repro-guard-") as work:
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
    workflow = yaml.safe_load(Path(workflow_path).read_text())
    steps = workflow["jobs"]["reproducibility"]["steps"]
    guards = [step for step in steps if step.get("name") == GUARD_NAME]
    require(len(guards) == 1, f"workflow must contain exactly one {GUARD_NAME!r} step")
    require(steps[0] is guards[0], "the concrete-ci-2 substitute guard must run before checkout")
    guard = guards[0]
    condition = guard.get("if")
    body = guard.get("run")
    require(isinstance(condition, str) and condition.strip(), "the substitute guard has no if condition")
    require(isinstance(body, str) and body.strip(), "the substitute guard has no run body")

    for label, event, machine, environment, should_refuse in CASES:
        context = {
            "github": {"event_name": event},
            "matrix": {"machine": machine},
            "runner": {"environment": environment},
        }
        enabled = evaluate_condition(condition, context)
        require(enabled == should_refuse,
                f"{label}: guard enabled={enabled}, expected {should_refuse}")
        if not enabled:
            print(f"  ok   guard {label}: skipped (status 0)")
            continue
        result = execute_guard(body)
        require(result.returncode != 0, f"{label}: substitute guard exited 0")
        output = result.stdout + result.stderr
        require(all(text in output for text in ("concrete-ci-2", "self-hosted", "pull_request")),
                f"{label}: refusal does not explain the concrete-ci-2/self-hosted/pull_request requirement")
        print(f"  ok   guard {label}: refused with status {result.returncode}")


def main(argv):
    if len(argv) != 2:
        sys.exit(__doc__)
    try:
        check(argv[1])
    except (KeyError, TypeError, ValueError, OSError, subprocess.TimeoutExpired, yaml.YAMLError) as error:
        sys.exit(f"FAIL concrete-ci-2 substitute guard: {error}")


if __name__ == "__main__":
    main(sys.argv)
