"""Check the workflow inventory, cache policy, and landing gate verdict."""

import copy
import json
from pathlib import Path
import re
import sys


INVENTORY = Path("required-checks.txt")
WORKFLOW = Path(".github/workflows/lean_action_ci.yml")
ACTIONS_DIR = Path(".github/actions")
LAKE_PACKAGES_ACTION_USE = "./.github/actions/lake-packages"
LAKE_PACKAGES_ACTION_PATH = ACTIONS_DIR / "lake-packages/action.yml"
FETCH_HELPER = "scripts/checks/lib/fetch_lake_packages.sh"
EXTERNAL_CACHE_PATH = "~/.cache/concrete-ext"
LAKE_PACKAGES_CACHE_PATH = ".lake/packages"
AGGREGATE = "landing-gates"
AGGREGATE_NAME = "Landing gates"

HASHFILES_EXPR = re.compile(
    r"\$\{\{\s*hashFiles\(\s*('[^']+'(?:\s*,\s*'[^']+')*)\s*\)\s*\}\}"
)
QUOTED_LITERAL = re.compile(r"'([^']+)'")
LAKE_CMD_RE = re.compile(r"\blake\b")
DEFAULT_NIX_DEVELOP_RE = re.compile(r"\bnix\s+develop\b(?!\s+\.#(?:gates|extcheck)\b)")
LAKE_SCRIPT_RE = re.compile(r"\bscripts/(?:checks/compiler|proofs|spec)/")


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


def expect_failure(check, diagnostic, *args, label=None, **kwargs):
    try:
        check(*args, **kwargs)
    except ValueError as error:
        if diagnostic in str(error):
            print(f"  ok   control rejects {label or diagnostic}")
            return
        raise
    raise ValueError(f"control unexpectedly accepted {label or diagnostic}")


def hashed_files_in_key(key):
    if not isinstance(key, str):
        return set()
    files = set()
    for match in HASHFILES_EXPR.finditer(key):
        files.update(QUOTED_LITERAL.findall(match.group(1)))
    return files


def step_action(step):
    return step.get("uses", "").partition("@")[0].lower()


def step_uses_lake(step):
    if step_action(step) == "leanprover/lean-action":
        return True
    run_cmd = step.get("run", "")
    if not isinstance(run_cmd, str) or not run_cmd:
        return False
    return bool(
        LAKE_CMD_RE.search(run_cmd)
        or DEFAULT_NIX_DEVELOP_RE.search(run_cmd)
        or LAKE_SCRIPT_RE.search(run_cmd)
    )


def check_step_cache_policy(scope, step, *, allow_lake_packages_cache=False):
    action = step_action(step)
    inputs = step.get("with", {})
    if action == "actions/checkout" and inputs.get("clean", "true") != "true":
        raise ValueError(f"{scope}: checkout must clean the workspace")
    if action == "leanprover/lean-action":
        for option in ("use-github-cache", "use-mathlib-cache"):
            if inputs.get(option) != "false":
                raise ValueError(f"{scope}: lean-action requires {option} disabled")
    if action in {"actions/cache", "actions/cache/restore", "actions/cache/save"}:
        cache_path = inputs.get("path")
        # A path or glob rooted above .lake (or including .lake/build) can restore repo outputs.
        if cache_path not in {EXTERNAL_CACHE_PATH, LAKE_PACKAGES_CACHE_PATH}:
            raise ValueError(
                f"{scope}: cache paths must contain only pinned external tools or "
                f"{LAKE_PACKAGES_CACHE_PATH} (got {cache_path!r})"
            )
        if "restore-keys" in inputs:
            raise ValueError(f"{scope}: cache step must not use restore-keys fallback")
        key = inputs.get("key")
        if not isinstance(key, str) or not key.strip():
            raise ValueError(f"{scope}: cache step requires an exact key")
        hashed = hashed_files_in_key(key)
        if cache_path == LAKE_PACKAGES_CACHE_PATH:
            if action == "actions/cache":
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache must use split restore/save before build"
                )
            if "lake-manifest.json" not in hashed:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash lake-manifest.json"
                )
            if "lean-toolchain" not in hashed:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash lean-toolchain"
                )
            if not allow_lake_packages_cache:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache must live only in {LAKE_PACKAGES_ACTION_USE}"
                )
        elif not hashed:
            raise ValueError(f"{scope}: external tool cache key must use hashFiles(...)")


def check_lake_packages_action(action_doc, scope=str(LAKE_PACKAGES_ACTION_PATH)):
    runs = action_doc.get("runs")
    if not isinstance(runs, dict) or runs.get("using") != "composite":
        raise ValueError(f"{scope}: must be a composite action")
    steps = runs.get("steps")
    if not isinstance(steps, list) or not steps:
        raise ValueError(f"{scope}: composite action must define steps")

    restore_steps = []
    fetch_steps = []
    save_steps = []
    for index, step in enumerate(steps):
        check_step_cache_policy(scope, step, allow_lake_packages_cache=True)
        if step_uses_lake(step):
            raise ValueError(f"{scope}: dependency source action must not run Lake or lean-action")
        action = step_action(step)
        inputs = step.get("with", {})
        if action == "actions/cache/restore" and inputs.get("path") == LAKE_PACKAGES_CACHE_PATH:
            restore_steps.append((index, step))
        elif action == "actions/cache/save" and inputs.get("path") == LAKE_PACKAGES_CACHE_PATH:
            save_steps.append((index, step))
        run_cmd = step.get("run", "")
        if isinstance(run_cmd, str) and FETCH_HELPER in run_cmd:
            fetch_steps.append((index, step))

    if len(restore_steps) != 1 or len(fetch_steps) != 1 or len(save_steps) != 1:
        raise ValueError(
            f"{scope}: must contain exactly one restore, one fetch/verify ({FETCH_HELPER}) and one save step"
        )
    (restore_idx, restore_step), (fetch_idx, fetch_step), (save_idx, save_step) = (
        restore_steps[0], fetch_steps[0], save_steps[0]
    )
    if not (restore_idx < fetch_idx < save_idx):
        raise ValueError(f"{scope}: steps must run in strict restore, fetch, save order")

    restore_id = restore_step.get("id")
    if not isinstance(restore_id, str) or not restore_id.strip():
        raise ValueError(f"{scope}: restore step must declare an id")
    if "if" in fetch_step or "continue-on-error" in fetch_step:
        raise ValueError(f"{scope}: fetch/verify step must run unconditionally and fail closed")
    if restore_step["with"]["key"] != save_step["with"]["key"]:
        raise ValueError(f"{scope}: restore and save steps must use the same key")
    expected_save_if = f"steps.{restore_id}.outputs.cache-hit != 'true'"
    if save_step.get("if") != expected_save_if:
        raise ValueError(f"{scope}: save step must be gated by {expected_save_if!r}")


def check_cache_steps(workflow, *, builds_project=False):
    for job_id, job in workflow["jobs"].items():
        checkout_indices = []
        helper_indices = []
        lake_indices = []
        for index, step in enumerate(job["steps"]):
            if builds_project:
                env = workflow.get("env", {}) | job.get("env", {}) | step.get("env", {})
                if env.get("LAKE_NO_CACHE") != "true" or env.get("LAKE_ARTIFACT_CACHE") != "false":
                    raise ValueError(f"{job_id}: build Lake packages locally with artifact caching disabled")
            check_step_cache_policy(job_id, step, allow_lake_packages_cache=False)

            uses = step.get("uses", "")
            if isinstance(uses, str) and uses.startswith("./"):
                local_dir = Path(uses.removeprefix("./"))
                if not ((local_dir / "action.yml").is_file() or (local_dir / "action.yaml").is_file()):
                    raise ValueError(f"{job_id}: missing local action {uses}")

            action = step_action(step)
            if action == "actions/checkout":
                checkout_indices.append(index)
            if uses == LAKE_PACKAGES_ACTION_USE:
                if "if" in step or "continue-on-error" in step:
                    raise ValueError(f"{job_id}: {LAKE_PACKAGES_ACTION_USE} must run unconditionally")
                helper_indices.append(index)
            run_cmd = step.get("run", "")
            if isinstance(run_cmd, str) and FETCH_HELPER in run_cmd:
                raise ValueError(f"{job_id}: call {LAKE_PACKAGES_ACTION_USE} instead of inlining {FETCH_HELPER}")
            if step_uses_lake(step):
                lake_indices.append(index)

        if builds_project and lake_indices:
            if len(helper_indices) != 1:
                raise ValueError(
                    f"{job_id}: Lake-using job must invoke {LAKE_PACKAGES_ACTION_USE} once"
                )
            helper_idx = helper_indices[0]
            if checkout_indices and helper_idx <= checkout_indices[0]:
                raise ValueError(
                    f"{job_id}: {LAKE_PACKAGES_ACTION_USE} must run after actions/checkout"
                )
            if helper_idx >= lake_indices[0]:
                raise ValueError(
                    f"{job_id}: {LAKE_PACKAGES_ACTION_USE} must run before any Lake step"
                )


def check_workflows(names):
    import yaml

    if not Path(FETCH_HELPER).is_file():
        raise ValueError(f"missing dependency fetch helper {FETCH_HELPER}")
    if not LAKE_PACKAGES_ACTION_PATH.is_file():
        raise ValueError(f"missing dependency cache action {LAKE_PACKAGES_ACTION_PATH}")

    action_paths = []
    if ACTIONS_DIR.is_dir():
        action_paths = sorted(
            p for p in ACTIONS_DIR.rglob("*")
            if p.is_file() and p.name in {"action.yml", "action.yaml"}
        )
    if LAKE_PACKAGES_ACTION_PATH not in action_paths:
        raise ValueError(f"missing {LAKE_PACKAGES_ACTION_PATH}")

    lake_action_doc = None
    for path in action_paths:
        parsed = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
        if path == LAKE_PACKAGES_ACTION_PATH:
            check_lake_packages_action(parsed, str(path))
            lake_action_doc = parsed
        else:
            runs = parsed.get("runs", {})
            for step in runs.get("steps", []):
                check_step_cache_policy(str(path), step, allow_lake_packages_cache=False)
        print(f"  ok   {path}")

    for path in sorted(WORKFLOW.parent.iterdir()):
        if path.suffix in {".yml", ".yaml"}:
            # BaseLoader keeps the GitHub trigger key "on" as a string.
            parsed = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
            check_cache_steps(parsed, builds_project=(path == WORKFLOW))
            print(f"  ok   {path}")

    workflow = yaml.load(WORKFLOW.read_text(), Loader=yaml.BaseLoader)
    check_inventory(workflow, names)

    # Controls on lean-action, checkout cleanliness, repository build paths, and restore-keys.
    retry_cache = copy.deepcopy(workflow)
    retry = next(step for step in retry_cache["jobs"]["spec"]["steps"]
                 if step.get("uses", "").startswith("leanprover/lean-action@") and "if" in step)
    retry["with"] = dict(retry["with"])
    del retry["with"]["use-github-cache"]  # The action defaults to restoring a previous build.
    expect_failure(check_cache_steps, "use-github-cache disabled", retry_cache)

    dirty_checkout = copy.deepcopy(workflow)
    dirty_checkout["jobs"]["tests"]["steps"][0]["with"] = {"clean": "false"}
    expect_failure(
        check_cache_steps,
        "checkout must clean the workspace",
        dirty_checkout,
        builds_project=True,
        label="actions/checkout with clean: false",
    )

    native_cache = copy.deepcopy(workflow)
    native_cache["jobs"]["tests"]["steps"].insert(1, {
        "uses": "actions/cache/restore@v4", "with": {"path": ".lake", "restore-keys": "native-"},
    })
    expect_failure(check_cache_steps, "cache paths", native_cache, label="workflow cache path .lake")

    for bad_path, label in (
        (".lake", "cached path .lake (repo root cache dir)"),
        (".lake/build", "cached path .lake/build (repo build outputs)"),
        (".lake/packages\n.lake/build", "multiline cache path smuggling .lake/build"),
    ):
        bad_build_cache = copy.deepcopy(lake_action_doc)
        bad_build_cache["runs"]["steps"][0]["with"]["path"] = bad_path
        expect_failure(check_lake_packages_action, "cache paths", bad_build_cache, label=label)

    # Controls on exact cache key (manifest hash + toolchain hash, no bypasses).
    for step_idx, key_val, diag, label in (
        (
            0,
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "restore key without lake-manifest.json hash",
        ),
        (
            2,
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "save key without lake-manifest.json hash",
        ),
        (
            0,
            "${{ runner.os }}-lake-manifest.json-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "literal lake-manifest.json outside hashFiles(...)",
        ),
        (
            0,
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('!lake-manifest.json', 'lean-toolchain') }}",
            "must hash lake-manifest.json",
            "negated !lake-manifest.json in hashFiles(...)",
        ),
        (
            0,
            "${{ false && hashFiles('lake-manifest.json', 'lean-toolchain') || 'static' }}",
            "must hash lake-manifest.json",
            "conditional expression bypassing hashFiles(...)",
        ),
        (
            0,
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json') }}",
            "must hash lean-toolchain",
            "cache key without lean-toolchain hash",
        ),
    ):
        mutated = copy.deepcopy(lake_action_doc)
        mutated["runs"]["steps"][step_idx]["with"]["key"] = key_val
        expect_failure(check_lake_packages_action, diag, mutated, label=label)

    # Controls on restore-keys fallback (on both .lake/packages and ~/.cache/concrete-ext).
    restore_keys_action = copy.deepcopy(lake_action_doc)
    restore_keys_action["runs"]["steps"][0]["with"]["restore-keys"] = "${{ runner.os }}-lake-packages-sources-"
    expect_failure(
        check_lake_packages_action,
        "restore-keys fallback",
        restore_keys_action,
        label="restore-keys fallback on .lake/packages cache",
    )

    restore_keys_ext = copy.deepcopy(workflow)
    ext_cache = next(step for step in restore_keys_ext["jobs"]["external-recheck"]["steps"]
                     if step.get("uses", "").startswith("actions/cache@"))
    ext_cache["with"]["restore-keys"] = "${{ runner.os }}-concrete-ext-"
    expect_failure(
        check_cache_steps,
        "restore-keys fallback",
        restore_keys_ext,
        builds_project=True,
        label="restore-keys fallback on ~/.cache/concrete-ext cache",
    )

    # Controls on sources-only save discipline and step ordering inside the action.
    combined_cache_action = copy.deepcopy(lake_action_doc)
    combined_cache_action["runs"]["steps"][0]["uses"] = "actions/cache@v4"
    expect_failure(
        check_lake_packages_action,
        "split restore/save before build",
        combined_cache_action,
        label="combined post-build actions/cache@v4 on .lake/packages",
    )

    mismatched_keys_action = copy.deepcopy(lake_action_doc)
    mismatched_keys_action["runs"]["steps"][2]["with"]["key"] = (
        "other-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}"
    )
    expect_failure(
        check_lake_packages_action,
        "must use the same key",
        mismatched_keys_action,
        label="mismatched restore and save cache keys",
    )

    unconditional_save_action = copy.deepcopy(lake_action_doc)
    del unconditional_save_action["runs"]["steps"][2]["if"]
    expect_failure(
        check_lake_packages_action,
        "save step must be gated",
        unconditional_save_action,
        label="save step without cache-hit guard",
    )

    reordered_action = copy.deepcopy(lake_action_doc)
    reordered_action["runs"]["steps"][1], reordered_action["runs"]["steps"][2] = (
        reordered_action["runs"]["steps"][2], reordered_action["runs"]["steps"][1]
    )
    expect_failure(
        check_lake_packages_action,
        "strict restore, fetch, save order",
        reordered_action,
        label="save step placed before fetch/verify step",
    )

    missing_fetch_action = copy.deepcopy(lake_action_doc)
    del missing_fetch_action["runs"]["steps"][1]
    expect_failure(
        check_lake_packages_action,
        "must contain exactly one restore, one fetch/verify",
        missing_fetch_action,
        label="action omitting fetch_lake_packages.sh",
    )

    soft_fetch_action = copy.deepcopy(lake_action_doc)
    soft_fetch_action["runs"]["steps"][1]["continue-on-error"] = "true"
    expect_failure(
        check_lake_packages_action,
        "fail closed",
        soft_fetch_action,
        label="fetch/verify step with continue-on-error",
    )

    lake_in_action = copy.deepcopy(lake_action_doc)
    lake_in_action["runs"]["steps"].insert(2, {"shell": "bash", "run": "lake build"})
    expect_failure(
        check_lake_packages_action,
        "must not run Lake",
        lake_in_action,
        label="lake build inside dependency source action before save",
    )

    # Controls on workflow job wiring (every Lake job calls the shared helper before Lake).
    missing_helper = copy.deepcopy(workflow)
    missing_helper["jobs"]["tests"]["steps"] = [
        step for step in missing_helper["jobs"]["tests"]["steps"]
        if step.get("uses") != LAKE_PACKAGES_ACTION_USE
    ]
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        missing_helper,
        builds_project=True,
        label="Lake-using job omitting ./.github/actions/lake-packages",
    )

    conditional_helper = copy.deepcopy(workflow)
    cond_step = next(step for step in conditional_helper["jobs"]["tests"]["steps"]
                     if step.get("uses") == LAKE_PACKAGES_ACTION_USE)
    cond_step["if"] = "false"
    expect_failure(
        check_cache_steps,
        "must run unconditionally",
        conditional_helper,
        builds_project=True,
        label="conditional ./.github/actions/lake-packages step",
    )

    late_helper = copy.deepcopy(workflow)
    spec_steps = late_helper["jobs"]["spec"]["steps"]
    helper_step = next(step for step in spec_steps if step.get("uses") == LAKE_PACKAGES_ACTION_USE)
    spec_steps.remove(helper_step)
    spec_steps.append(helper_step)
    expect_failure(
        check_cache_steps,
        "must run before any Lake step",
        late_helper,
        builds_project=True,
        label="./.github/actions/lake-packages placed after lean-action",
    )

    inlined_cache = copy.deepcopy(workflow)
    inlined_cache["jobs"]["tests"]["steps"].insert(1, {
        "uses": "actions/cache/restore@v4",
        "with": {
            "path": ".lake/packages",
            "key": "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}",
        },
    })
    expect_failure(
        check_cache_steps,
        f"must live only in {LAKE_PACKAGES_ACTION_USE}",
        inlined_cache,
        builds_project=True,
        label="inlined .lake/packages cache step in workflow job",
    )

    inlined_fetch = copy.deepcopy(workflow)
    inlined_fetch["jobs"]["grammar"]["steps"].insert(2, {
        "shell": "bash",
        "run": f"bash {FETCH_HELPER}",
    })
    expect_failure(
        check_cache_steps,
        f"instead of inlining {FETCH_HELPER}",
        inlined_fetch,
        builds_project=True,
        label="inlined fetch_lake_packages.sh in workflow job",
    )

    artifact_cache_on = copy.deepcopy(workflow)
    artifact_cache_on["env"]["LAKE_ARTIFACT_CACHE"] = "true"
    expect_failure(
        check_cache_steps,
        "artifact caching disabled",
        artifact_cache_on,
        builds_project=True,
        label="LAKE_ARTIFACT_CACHE enabled",
    )

    # Inventory and verdict controls.
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
