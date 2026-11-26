"""Check the workflow inventory and require successful jobs on a queue candidate."""

import copy
import json
from pathlib import Path
import re
import sys


INVENTORY = Path("required-checks.txt")
WORKFLOW = Path(".github/workflows/lean_action_ci.yml")
AGGREGATE = "landing-gates"
AGGREGATE_NAME = "Landing gates"


def gate_names():
    names = [
        line.strip()
        for line in INVENTORY.read_text().splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    if not names or names[0] != AGGREGATE_NAME:
        raise ValueError("required-checks.txt must list Landing gates first")
    return names[1:]


def check_inventory(workflow, names):
    jobs = workflow["jobs"]
    aggregate = jobs[AGGREGATE]
    gate_ids = set(jobs) - {AGGREGATE}
    if not gate_ids or set(aggregate["needs"]) != gate_ids:
        raise ValueError("Landing gates must depend on every gate job")
    if aggregate["name"] != AGGREGATE_NAME or aggregate["if"] != "${{ always() }}":
        raise ValueError("Landing gates must always run under its required check name")
    if any(key in aggregate for key in ("strategy", "continue-on-error")) or any(
        key in step for step in aggregate["steps"] for key in ("if", "continue-on-error")
    ):
        raise ValueError("Landing gates and its steps must run and report their own verdict")
    if workflow["on"]["merge_group"]["types"] != ["checks_requested"]:
        raise ValueError("CI must run for merge-group candidates")

    expanded = []
    for job_id in sorted(gate_ids):
        job = jobs[job_id]
        matrix = job.get("strategy", {}).get("matrix")
        if matrix is not None and (set(matrix) != {"include"} or not matrix["include"]):
            raise ValueError(f"{job_id}: use a nonempty include-only matrix")
        rows = [{}] if matrix is None else matrix["include"]
        for row in rows:
            name = re.sub(r"\$\{\{\s*matrix\.(\w+)\s*\}\}",
                          lambda match: row[match[1]], job["name"])
            if "${{" in name:
                raise ValueError(f"{job_id}: unsupported expression in check name")
            expanded.append(name)
    for labels in (expanded, names):
        if AGGREGATE_NAME in labels or len(set(labels)) != len(labels):
            raise ValueError("gate names must be unique and distinct from Landing gates")
    unlisted = sorted(set(expanded) - set(names))
    stale = sorted(set(names) - set(expanded))
    if unlisted or stale:
        raise ValueError(f"required-checks.txt mismatch: unlisted={unlisted}, stale={stale}")


def require_success(names, pages):
    conclusions = {}
    for page in pages:
        for job in page["jobs"]:
            name = job["name"]
            if name == AGGREGATE_NAME:
                continue
            if name in conclusions:
                raise ValueError(f"duplicate job result: {name}")
            conclusions[name] = job["conclusion"]

    problems = [
        f"{name}: {conclusions.get(name, 'missing')}"
        for name in names if conclusions.get(name) != "success"
    ]
    problems.extend(f"{name}: unlisted" for name in sorted(set(conclusions) - set(names)))
    if problems:
        raise ValueError("\n".join(problems))


def expect_failure(check, diagnostic, *args):
    try:
        check(*args)
    except ValueError as error:
        if diagnostic in str(error):
            print(f"  ok   control rejects {diagnostic}")
            return
        raise
    raise ValueError(f"control unexpectedly accepted {diagnostic}")


def check_workflows(names):
    import yaml

    for path in sorted(WORKFLOW.parent.iterdir()):
        if path.suffix in {".yml", ".yaml"}:
            # BaseLoader keeps the GitHub trigger key "on" as a string.
            yaml.load(path.read_text(), Loader=yaml.BaseLoader)
            print(f"  ok   {path}")

    workflow = yaml.load(WORKFLOW.read_text(), Loader=yaml.BaseLoader)
    check_inventory(workflow, names)

    extra_leg = copy.deepcopy(workflow)
    extra_leg["jobs"]["inventory-control"] = {
        "name": "Inventory control (${{ matrix.arch }})",
        "strategy": {"matrix": {"include": [{"arch": "listed"}, {"arch": "unlisted"}]}},
    }
    extra_leg["jobs"][AGGREGATE]["needs"].append("inventory-control")
    expect_failure(check_inventory, "Inventory control (unlisted)", extra_leg,
                   [*names, "Inventory control (listed)"])
    expect_failure(check_inventory, "Retired gate", workflow, [*names, "Retired gate"])
    missing_dependency = copy.deepcopy(workflow)
    missing_dependency["jobs"][AGGREGATE]["needs"].pop()
    expect_failure(check_inventory, "must depend on every gate job", missing_dependency, names)
    skipped_aggregate = copy.deepcopy(workflow)
    skipped_aggregate["jobs"][AGGREGATE]["if"] = "${{ success() }}"
    expect_failure(check_inventory, "must always run", skipped_aggregate, names)

    jobs = [{"name": name, "conclusion": "success"} for name in names]
    require_success(names, [{"jobs": jobs[:1]}, {"jobs": jobs[1:]}])
    for name, conclusion in ((names[0], "skipped"), (names[-1], "neutral")):
        changed = [dict(job, conclusion=conclusion) if job["name"] == name else job for job in jobs]
        expect_failure(require_success, f"{name}: {conclusion}", names, [{"jobs": changed}])
    expect_failure(require_success, f"{names[-1]}: missing", names, [{"jobs": jobs[:-1]}])


def main():
    names = gate_names()
    if sys.argv[1:] == ["verdict"]:
        require_success(names, json.load(sys.stdin))
        print(f"All {len(names)} landing gate jobs succeeded.")
    elif not sys.argv[1:]:
        check_workflows(names)
    else:
        raise ValueError("usage: workflow_checks.py [verdict]")


if __name__ == "__main__":
    try:
        main()
    except (OSError, KeyError, TypeError, ValueError) as error:
        sys.exit(f"Workflow checks: {error}")
