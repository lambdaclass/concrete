"""Check the workflow inventory, cache policy, and landing gate verdict."""

import copy
import json
from pathlib import Path
import re
import sys


INVENTORY = Path("required-checks.txt")
WORKFLOW = Path(".github/workflows/lean_action_ci.yml")
ACTIONS_DIR = Path(".github/actions")
LAKE_PACKAGES_ACTION_DIR = ".github/actions/lake-packages"
LAKE_PACKAGES_ACTION_USE = f"./{LAKE_PACKAGES_ACTION_DIR}"
LAKE_PACKAGES_ACTION_PATH = ACTIONS_DIR / "lake-packages/action.yml"
FETCH_HELPER = "scripts/checks/lib/fetch_lake_packages.sh"
EXPECTED_FETCH_RUN = (
    "nix develop .#gates --command bash --noprofile --norc -eo pipefail "
    f"{FETCH_HELPER}"
)
EXTERNAL_CACHE_PATH = "~/.cache/concrete-ext"
LAKE_PACKAGES_CACHE_PATH = ".lake/packages"
LAKE_PACKAGES_HASH_INPUTS = frozenset({"lake-manifest.json", "lean-toolchain"})
AGGREGATE = "landing-gates"
AGGREGATE_NAME = "Landing gates"

GHA_EXPR_RE = re.compile(r"\$\{\{(.*?)\}\}", re.DOTALL)
PURE_HASHFILES_RE = re.compile(
    r"^\s*hashFiles\(\s*('[^']*'(?:\s*,\s*'[^']*')*)\s*\)\s*$",
    re.IGNORECASE,
)
QUOTED_LITERAL = re.compile(r"'([^']*)'")
LAKE_CMD_RE = re.compile(r"\blake\b")
DEFAULT_NIX_DEVELOP_RE = re.compile(r"\bnix\s+develop\b(?!\s+\.#(?:gates|extcheck)(?:\s|$))")
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


def parse_hashfiles_args(key, scope):
    """Return ordered list of literal arguments from pure ${{ hashFiles(...) }} expressions.

    Rejects any exclusion pattern ('!...') and any empty or whitespace-padded pattern.
    Compound expressions that wrap hashFiles(...) in conditionals or fallbacks are not
    accepted as pure hashFiles(...) expressions.
    """
    if not isinstance(key, str):
        return []
    args = []
    for match in GHA_EXPR_RE.finditer(key):
        expr = match.group(1)
        pure = PURE_HASHFILES_RE.fullmatch(expr)
        if not pure:
            continue
        call_args = QUOTED_LITERAL.findall(pure.group(1))
        for pattern in call_args:
            if not pattern or pattern.strip() != pattern:
                raise ValueError(
                    f"{scope}: hashFiles(...) pattern must be non-empty and trimmed (got {pattern!r})"
                )
            if pattern.startswith("!"):
                raise ValueError(
                    f"{scope}: hashFiles(...) must not contain exclusion pattern {pattern!r}"
                )
            args.append(pattern)
    return args


def normalize_local_action_use(uses, scope):
    if not isinstance(uses, str) or not uses.startswith("./"):
        return None
    if "@" in uses:
        raise ValueError(f"{scope}: local action {uses!r} must not include a ref")
    raw = uses.removeprefix("./")
    parts = []
    for part in raw.split("/"):
        if part in {"", "."}:
            continue
        if part == "..":
            if not parts:
                raise ValueError(f"{scope}: local action {uses!r} escapes repository root")
            parts.pop()
        else:
            parts.append(part)
    if not parts:
        raise ValueError(f"{scope}: local action {uses!r} must name a directory")
    return "/".join(parts)


def step_action(step):
    return step.get("uses", "").partition("@")[0].lower()


def step_uses_lake(step, *, default_shell=""):
    if step_action(step) == "leanprover/lean-action":
        return True
    run_cmd = step.get("run", "")
    if not isinstance(run_cmd, str) or not run_cmd:
        return False
    shell = step.get("shell") or default_shell
    if isinstance(shell, str) and DEFAULT_NIX_DEVELOP_RE.search(shell):
        return True
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
        hashed_args = parse_hashfiles_args(key, scope)
        if cache_path == LAKE_PACKAGES_CACHE_PATH:
            if action == "actions/cache":
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache must use split restore/save before build"
                )
            if "lake-manifest.json" not in hashed_args:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash lake-manifest.json"
                )
            if "lean-toolchain" not in hashed_args:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash lean-toolchain"
                )
            if set(hashed_args) != LAKE_PACKAGES_HASH_INPUTS or len(hashed_args) != len(LAKE_PACKAGES_HASH_INPUTS):
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash only literal "
                    f"lake-manifest.json and lean-toolchain (got {hashed_args!r})"
                )
            if not allow_lake_packages_cache:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache must live only in {LAKE_PACKAGES_ACTION_USE}"
                )
            extra_with = set(inputs) - {"path", "key"}
            if extra_with:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache step sets unexpected inputs {sorted(extra_with)}"
                )
        elif not hashed_args:
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
        if not isinstance(step, dict):
            raise ValueError(f"{scope}: step {index} must be a mapping")
        check_step_cache_policy(scope, step, allow_lake_packages_cache=True)
        if step_uses_lake(step):
            raise ValueError(f"{scope}: dependency source action must not run Lake or lean-action")
        action = step_action(step)
        inputs = step.get("with", {})
        if action == "actions/cache/restore" and inputs.get("path") == LAKE_PACKAGES_CACHE_PATH:
            restore_steps.append((index, step))
        elif action == "actions/cache/save" and inputs.get("path") == LAKE_PACKAGES_CACHE_PATH:
            save_steps.append((index, step))
        elif "run" in step:
            fetch_steps.append((index, step))
        else:
            raise ValueError(f"{scope}: unexpected step {index} in dependency source action")

    if len(restore_steps) != 1 or len(fetch_steps) != 1 or len(save_steps) != 1 or len(steps) != 3:
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
    if "if" in restore_step:
        raise ValueError(f"{scope}: restore step must not be conditional")
    extra_restore = set(restore_step) - {"name", "id", "continue-on-error", "uses", "with"}
    if extra_restore:
        raise ValueError(f"{scope}: restore step sets unexpected keys {sorted(extra_restore)}")

    if "if" in fetch_step or "continue-on-error" in fetch_step:
        raise ValueError(f"{scope}: fetch/verify step must run unconditionally and fail closed")
    extra_fetch = set(fetch_step) - {"name", "shell", "run"}
    if extra_fetch:
        raise ValueError(f"{scope}: fetch/verify step sets unexpected keys {sorted(extra_fetch)}")
    if fetch_step.get("shell") != "bash":
        raise ValueError(
            f"{scope}: fetch/verify step must use shell 'bash' (got {fetch_step.get('shell')!r})"
        )
    run_cmd = fetch_step.get("run")
    if not isinstance(run_cmd, str) or run_cmd.strip() != EXPECTED_FETCH_RUN:
        raise ValueError(
            f"{scope}: fetch/verify step must run exact fail-closed command "
            f"{EXPECTED_FETCH_RUN!r} (got {run_cmd!r})"
        )

    if restore_step["with"]["key"] != save_step["with"]["key"]:
        raise ValueError(f"{scope}: restore and save steps must use the same key")
    expected_save_if = f"steps.{restore_id}.outputs.cache-hit != 'true'"
    if save_step.get("if") != expected_save_if:
        raise ValueError(f"{scope}: save step must be gated by {expected_save_if!r}")
    extra_save = set(save_step) - {"name", "id", "if", "continue-on-error", "uses", "with"}
    if extra_save:
        raise ValueError(f"{scope}: save step sets unexpected keys {sorted(extra_save)}")


def load_local_action(action_dir, scope, local_actions):
    if action_dir in local_actions:
        doc = local_actions[action_dir]
    else:
        import yaml

        base = Path(action_dir)
        action_file = None
        for candidate in (base / "action.yml", base / "action.yaml"):
            if candidate.is_file():
                action_file = candidate
                break
        if action_file is None:
            raise ValueError(f"{scope}: missing local action ./{action_dir}")
        doc = yaml.load(action_file.read_text(), Loader=yaml.BaseLoader)
        local_actions[action_dir] = doc

    if not isinstance(doc, dict):
        raise ValueError(f"{scope}: local action ./{action_dir} must be a mapping")
    runs = doc.get("runs")
    if not isinstance(runs, dict) or runs.get("using") != "composite":
        raise ValueError(f"{scope}: local action ./{action_dir} must be a composite action")
    steps = runs.get("steps")
    if not isinstance(steps, list):
        raise ValueError(f"{scope}: local action ./{action_dir} must define steps")
    return doc


def expand_steps(
    steps,
    *,
    scope,
    base_env,
    default_shell,
    conditional_or_soft,
    local_actions,
    visiting,
    events,
):
    for index, step in enumerate(steps):
        if not isinstance(step, dict):
            raise ValueError(f"{scope}: step {index} must be a mapping")
        step_scope = f"{scope}[{index}]"
        step_env = base_env | (step.get("env") or {})
        step_cond = conditional_or_soft or ("if" in step) or ("continue-on-error" in step)

        check_step_cache_policy(step_scope, step, allow_lake_packages_cache=False)

        run_cmd = step.get("run", "")
        if isinstance(run_cmd, str) and FETCH_HELPER in run_cmd:
            raise ValueError(
                f"{step_scope}: call {LAKE_PACKAGES_ACTION_USE} instead of inlining {FETCH_HELPER}"
            )

        uses = step.get("uses", "")
        local_dir = normalize_local_action_use(uses, step_scope) if isinstance(uses, str) else None
        if local_dir is not None:
            if local_dir == LAKE_PACKAGES_ACTION_DIR:
                if step_cond:
                    raise ValueError(
                        f"{step_scope}: {LAKE_PACKAGES_ACTION_USE} must run unconditionally and fail closed"
                    )
                events.append(("helper", step_scope, step_env))
                continue
            if local_dir in visiting:
                raise ValueError(f"{step_scope}: cyclic local action reference ./{local_dir}")
            action_doc = load_local_action(local_dir, step_scope, local_actions)
            expand_steps(
                action_doc["runs"]["steps"],
                scope=f"{step_scope}->./{local_dir}",
                base_env=step_env,
                default_shell="",
                conditional_or_soft=step_cond,
                local_actions=local_actions,
                visiting=visiting | {local_dir},
                events=events,
            )
            continue

        action = step_action(step)
        if action == "actions/checkout":
            events.append(("checkout", step_scope, step_env))
        if step_uses_lake(step, default_shell=default_shell):
            events.append(("lake", step_scope, step_env))
        else:
            events.append(("other", step_scope, step_env))


def check_cache_steps(
    workflow,
    *,
    local_actions=None,
    require_all_jobs_no_artifact_cache=False,
    workflow_scope="<workflow>",
):
    actions_map = {} if local_actions is None else dict(local_actions)
    wf_env = workflow.get("env") or {}
    wf_shell = (workflow.get("defaults") or {}).get("run", {}).get("shell", "")
    jobs = workflow.get("jobs")
    if not isinstance(jobs, dict) or not jobs:
        raise ValueError(f"{workflow_scope}: workflow must define jobs")

    for job_id, job in jobs.items():
        job_scope = f"{workflow_scope}:{job_id}"
        if not isinstance(job, dict):
            raise ValueError(f"{job_scope}: job must be a mapping")
        if "steps" not in job:
            if "uses" in job:
                raise ValueError(
                    f"{job_scope}: reusable workflow call {job['uses']!r} cannot bypass step checks"
                )
            continue
        job_env = wf_env | (job.get("env") or {})
        job_shell = (job.get("defaults") or {}).get("run", {}).get("shell") or wf_shell
        events = []
        expand_steps(
            job["steps"],
            scope=job_scope,
            base_env=job_env,
            default_shell=job_shell,
            conditional_or_soft=False,
            local_actions=actions_map,
            visiting=frozenset(),
            events=events,
        )

        lake_events = [(i, sc, env) for i, (kind, sc, env) in enumerate(events) if kind == "lake"]
        helper_events = [(i, sc) for i, (kind, sc, _) in enumerate(events) if kind == "helper"]
        checkout_events = [(i, sc) for i, (kind, sc, _) in enumerate(events) if kind == "checkout"]

        if lake_events:
            if len(helper_events) != 1:
                raise ValueError(
                    f"{job_scope}: Lake-using job must invoke {LAKE_PACKAGES_ACTION_USE} once"
                )
            helper_idx = helper_events[0][0]
            first_lake_idx = lake_events[0][0]
            prior_checkouts = [idx for idx, _ in checkout_events if idx < first_lake_idx]
            if prior_checkouts and helper_idx <= prior_checkouts[-1]:
                raise ValueError(
                    f"{job_scope}: {LAKE_PACKAGES_ACTION_USE} must run after actions/checkout"
                )
            if helper_idx >= first_lake_idx:
                raise ValueError(
                    f"{job_scope}: {LAKE_PACKAGES_ACTION_USE} must run before any Lake step"
                )
            for _i, step_scope, env in lake_events:
                if env.get("LAKE_NO_CACHE") != "true" or env.get("LAKE_ARTIFACT_CACHE") != "false":
                    raise ValueError(
                        f"{step_scope}: build Lake packages locally with artifact caching disabled"
                    )

        if require_all_jobs_no_artifact_cache:
            for _kind, step_scope, env in events:
                if env.get("LAKE_NO_CACHE") != "true" or env.get("LAKE_ARTIFACT_CACHE") != "false":
                    raise ValueError(
                        f"{step_scope}: build Lake packages locally with artifact caching disabled"
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

    local_actions = {}
    for path in action_paths:
        parsed = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
        action_dir = path.parent.as_posix()
        local_actions[action_dir] = parsed
        if path == LAKE_PACKAGES_ACTION_PATH:
            check_lake_packages_action(parsed, str(path))
        else:
            runs = parsed.get("runs")
            if not isinstance(runs, dict) or runs.get("using") != "composite":
                raise ValueError(f"{path}: local action must be a composite action")
            steps = runs.get("steps")
            if not isinstance(steps, list):
                raise ValueError(f"{path}: local composite action must define steps")
            for step in steps:
                check_step_cache_policy(str(path), step, allow_lake_packages_cache=False)
                run_cmd = step.get("run", "")
                if isinstance(run_cmd, str) and FETCH_HELPER in run_cmd:
                    raise ValueError(
                        f"{path}: call {LAKE_PACKAGES_ACTION_USE} instead of inlining {FETCH_HELPER}"
                    )
        print(f"  ok   {path}")

    lake_action_doc = local_actions[LAKE_PACKAGES_ACTION_DIR]

    for path in sorted(WORKFLOW.parent.iterdir()):
        if path.suffix in {".yml", ".yaml"}:
            # BaseLoader keeps the GitHub trigger key "on" as a string.
            parsed = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
            check_cache_steps(
                parsed,
                local_actions=local_actions,
                require_all_jobs_no_artifact_cache=(path == WORKFLOW),
                workflow_scope=str(path),
            )
            print(f"  ok   {path}")

    workflow = yaml.load(WORKFLOW.read_text(), Loader=yaml.BaseLoader)
    check_inventory(workflow, names)

    # Controls on lean-action, checkout cleanliness, repository build paths, and restore-keys.
    retry_cache = copy.deepcopy(workflow)
    retry = next(step for step in retry_cache["jobs"]["spec"]["steps"]
                 if step.get("uses", "").startswith("leanprover/lean-action@") and "if" in step)
    retry["with"] = dict(retry["with"])
    del retry["with"]["use-github-cache"]  # The action defaults to restoring a previous build.
    expect_failure(
        check_cache_steps,
        "use-github-cache disabled",
        retry_cache,
        local_actions=local_actions,
    )

    dirty_checkout = copy.deepcopy(workflow)
    dirty_checkout["jobs"]["tests"]["steps"][0]["with"] = {"clean": "false"}
    expect_failure(
        check_cache_steps,
        "checkout must clean the workspace",
        dirty_checkout,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="actions/checkout with clean: false",
    )

    native_cache = copy.deepcopy(workflow)
    native_cache["jobs"]["tests"]["steps"].insert(1, {
        "uses": "actions/cache/restore@v4", "with": {"path": ".lake", "restore-keys": "native-"},
    })
    expect_failure(
        check_cache_steps,
        "cache paths",
        native_cache,
        local_actions=local_actions,
        label="workflow cache path .lake",
    )

    for bad_path, label in (
        (".lake", "cached path .lake (repo root cache dir)"),
        (".lake/build", "cached path .lake/build (repo build outputs)"),
        (".lake/packages\n.lake/build", "multiline cache path smuggling .lake/build"),
    ):
        bad_build_cache = copy.deepcopy(lake_action_doc)
        bad_build_cache["runs"]["steps"][0]["with"]["path"] = bad_path
        expect_failure(check_lake_packages_action, "cache paths", bad_build_cache, label=label)

    # Controls on exact cache key (exact two literal inputs, no exclusion patterns or bypasses).
    for step_indices, key_val, diag, label in (
        (
            (0,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "restore key without lake-manifest.json hash",
        ),
        (
            (2,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "save key without lake-manifest.json hash",
        ),
        (
            (0,),
            "${{ runner.os }}-lake-manifest.json-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "literal lake-manifest.json outside hashFiles(...)",
        ),
        (
            (0,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('!lake-manifest.json', 'lean-toolchain') }}",
            "exclusion pattern",
            "negated !lake-manifest.json in hashFiles(...)",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', '!lake-manifest.json') }}",
            "exclusion pattern",
            "trailing negated !lake-manifest.json excluding manifest from both keys",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', '!*.json') }}",
            "exclusion pattern",
            "trailing wildcard exclusion !*.json in hashFiles(...)",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', '!lean-toolchain') }}",
            "exclusion pattern",
            "trailing negated !lean-toolchain in hashFiles(...)",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', 'README.md') }}",
            "must hash only literal lake-manifest.json and lean-toolchain",
            "extra unapproved input in .lake/packages hashFiles(...)",
        ),
        (
            (0,),
            "${{ false && hashFiles('lake-manifest.json', 'lean-toolchain') || 'static' }}",
            "must hash lake-manifest.json",
            "conditional expression bypassing hashFiles(...)",
        ),
        (
            (0,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json') }}",
            "must hash lean-toolchain",
            "cache key without lean-toolchain hash",
        ),
    ):
        mutated = copy.deepcopy(lake_action_doc)
        for step_idx in step_indices:
            mutated["runs"]["steps"][step_idx]["with"]["key"] = key_val
        expect_failure(check_lake_packages_action, diag, mutated, label=label)

    # Control: exclusion pattern in external tool cache key is also rejected.
    excluded_ext = copy.deepcopy(workflow)
    ext_cache = next(step for step in excluded_ext["jobs"]["external-recheck"]["steps"]
                     if step.get("uses", "").startswith("actions/cache@"))
    ext_cache["with"]["key"] = (
        "${{ runner.os }}-concrete-ext-${{ hashFiles('scripts/proofs/external.pins', '!scripts/proofs/external.pins') }}"
    )
    expect_failure(
        check_cache_steps,
        "exclusion pattern",
        excluded_ext,
        local_actions=local_actions,
        label="exclusion pattern in ~/.cache/concrete-ext hashFiles(...)",
    )

    # Controls on restore-keys fallback (on both .lake/packages and ~/.cache/concrete-ext).
    restore_keys_action = copy.deepcopy(lake_action_doc)
    restore_keys_action["runs"]["steps"][0]["with"]["restore-keys"] = "${{ runner.os }}-lake-packages-sources-"
    expect_failure(
        check_lake_packages_action,
        "restore-keys fallback",
        restore_keys_action,
        label="restore-keys fallback on .lake/packages cache",
    )

    lookup_only_action = copy.deepcopy(lake_action_doc)
    lookup_only_action["runs"]["steps"][0]["with"]["lookup-only"] = "true"
    expect_failure(
        check_lake_packages_action,
        "unexpected inputs",
        lookup_only_action,
        label="lookup-only input on .lake/packages cache restore",
    )

    restore_keys_ext = copy.deepcopy(workflow)
    ext_cache = next(step for step in restore_keys_ext["jobs"]["external-recheck"]["steps"]
                     if step.get("uses", "").startswith("actions/cache@"))
    ext_cache["with"]["restore-keys"] = "${{ runner.os }}-concrete-ext-"
    expect_failure(
        check_cache_steps,
        "restore-keys fallback",
        restore_keys_ext,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="restore-keys fallback on ~/.cache/concrete-ext cache",
    )

    # Controls on sources-only save discipline, exact fail-closed helper invocation, and step ordering.
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

    for bad_run, label in (
        (f"bash -n {FETCH_HELPER}", "bash -n syntax check instead of executing helper"),
        (f"{EXPECTED_FETCH_RUN} || true", "helper command swallowing failure with || true"),
        (f"echo {FETCH_HELPER}", "no-op command merely mentioning helper path"),
        (f"# {FETCH_HELPER}\ntrue", "comment mentioning helper path followed by true"),
    ):
        noop_fetch = copy.deepcopy(lake_action_doc)
        noop_fetch["runs"]["steps"][1]["run"] = bad_run
        expect_failure(
            check_lake_packages_action,
            "exact fail-closed command",
            noop_fetch,
            label=label,
        )

    bad_shell_fetch = copy.deepcopy(lake_action_doc)
    bad_shell_fetch["runs"]["steps"][1]["shell"] = "bash -n {0}"
    expect_failure(
        check_lake_packages_action,
        "must use shell 'bash'",
        bad_shell_fetch,
        label="fetch/verify step overriding shell to syntax-check only",
    )

    bad_workdir_fetch = copy.deepcopy(lake_action_doc)
    bad_workdir_fetch["runs"]["steps"][1]["working-directory"] = "subdir"
    expect_failure(
        check_lake_packages_action,
        "unexpected keys",
        bad_workdir_fetch,
        label="fetch/verify step changing working-directory",
    )

    bad_env_fetch = copy.deepcopy(lake_action_doc)
    bad_env_fetch["runs"]["steps"][1]["env"] = {"PATH": "/tmp/fake"}
    expect_failure(
        check_lake_packages_action,
        "unexpected keys",
        bad_env_fetch,
        label="fetch/verify step injecting env overrides",
    )

    extra_step_action = copy.deepcopy(lake_action_doc)
    extra_step_action["runs"]["steps"].append({"shell": "bash", "run": "echo extra"})
    expect_failure(
        check_lake_packages_action,
        "must contain exactly one restore, one fetch/verify",
        extra_step_action,
        label="extra step smuggled into dependency source action",
    )

    lake_in_action = copy.deepcopy(lake_action_doc)
    lake_in_action["runs"]["steps"].insert(2, {"shell": "bash", "run": "lake build"})
    expect_failure(
        check_lake_packages_action,
        "must not run Lake",
        lake_in_action,
        label="lake build inside dependency source action before save",
    )

    # Controls on workflow job wiring (every Lake job across ALL workflows and local composite actions).
    missing_helper = copy.deepcopy(workflow)
    missing_helper["jobs"]["tests"]["steps"] = [
        step for step in missing_helper["jobs"]["tests"]["steps"]
        if step.get("uses") != LAKE_PACKAGES_ACTION_USE
    ]
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        missing_helper,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
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
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="conditional ./.github/actions/lake-packages step",
    )

    soft_helper = copy.deepcopy(workflow)
    soft_step = next(step for step in soft_helper["jobs"]["tests"]["steps"]
                     if step.get("uses") == LAKE_PACKAGES_ACTION_USE)
    soft_step["continue-on-error"] = "true"
    expect_failure(
        check_cache_steps,
        "must run unconditionally",
        soft_helper,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="continue-on-error ./.github/actions/lake-packages step",
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
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="./.github/actions/lake-packages placed after lean-action",
    )

    # Control: second workflow (not lean_action_ci.yml) with checkout + direct lake build and no helper.
    second_workflow_unwired = {
        "name": "Extra",
        "on": "workflow_dispatch",
        "jobs": {
            "extra-lake": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"run": "lake build"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        second_workflow_unwired,
        local_actions=local_actions,
        workflow_scope=".github/workflows/extra.yml",
        label="second workflow running lake build without ./.github/actions/lake-packages",
    )

    # Control: second workflow with helper but without LAKE_NO_CACHE / LAKE_ARTIFACT_CACHE.
    second_workflow_no_env = {
        "name": "Extra",
        "on": "workflow_dispatch",
        "jobs": {
            "extra-lake": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"uses": LAKE_PACKAGES_ACTION_USE},
                    {"run": "lake build"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "artifact caching disabled",
        second_workflow_no_env,
        local_actions=local_actions,
        workflow_scope=".github/workflows/extra.yml",
        label="second workflow running lake build without disabling Lake artifact cache",
    )

    # Positive control: properly wired second workflow passes.
    second_workflow_valid = copy.deepcopy(second_workflow_no_env)
    second_workflow_valid["env"] = {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"}
    check_cache_steps(
        second_workflow_valid,
        local_actions=local_actions,
        workflow_scope=".github/workflows/extra.yml",
    )
    print("  ok   control accepts properly wired second workflow")

    # Controls: local composite action hiding a Lake build (direct, nested, late helper, conditional wrapper).
    actions_with_lake = dict(local_actions)
    actions_with_lake[".github/actions/run-lake"] = {
        "runs": {
            "using": "composite",
            "steps": [{"shell": "bash", "run": "lake build"}],
        },
    }
    actions_with_lake[".github/actions/outer-lake"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": "./.github/actions/run-lake"}],
        },
    }
    actions_with_lake[".github/actions/wrap-helper"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": LAKE_PACKAGES_ACTION_USE}],
        },
    }
    actions_with_lake[".github/actions/cycle-a"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": "./.github/actions/cycle-b"}],
        },
    }
    actions_with_lake[".github/actions/cycle-b"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": "./.github/actions/cycle-a"}],
        },
    }

    composite_lake_unwired = {
        "name": "CompositeLake",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"uses": "./.github/actions/run-lake"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        composite_lake_unwired,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="Lake build hidden inside local composite action without helper",
    )

    nested_composite_unwired = copy.deepcopy(composite_lake_unwired)
    nested_composite_unwired["jobs"]["build"]["steps"][1] = {"uses": "./.github/actions/outer-lake"}
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        nested_composite_unwired,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="Lake build hidden inside nested local composite action without helper",
    )

    composite_lake_before_helper = copy.deepcopy(composite_lake_unwired)
    composite_lake_before_helper["jobs"]["build"]["steps"].append({"uses": LAKE_PACKAGES_ACTION_USE})
    expect_failure(
        check_cache_steps,
        "must run before any Lake step",
        composite_lake_before_helper,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="local composite action running Lake before ./.github/actions/lake-packages",
    )

    conditional_wrapper_helper = {
        "name": "CondWrap",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"if": "false", "uses": "./.github/actions/wrap-helper"},
                    {"uses": "./.github/actions/run-lake"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "must run unconditionally",
        conditional_wrapper_helper,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="conditional local composite wrapper around ./.github/actions/lake-packages",
    )

    checkout_after_helper = {
        "name": "LateCheckout",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"uses": LAKE_PACKAGES_ACTION_USE},
                    {"uses": "actions/checkout@v4"},
                    {"run": "lake build"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "must run after actions/checkout",
        checkout_after_helper,
        local_actions=local_actions,
        workflow_scope=".github/workflows/late-checkout.yml",
        label="actions/checkout wiping .lake/packages after ./.github/actions/lake-packages",
    )

    cyclic_composite = {
        "name": "Cyclic",
        "on": "workflow_dispatch",
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [{"uses": "./.github/actions/cycle-a"}],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "cyclic local action reference",
        cyclic_composite,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/cyclic.yml",
        label="cyclic local composite action reference",
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
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
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
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="inlined fetch_lake_packages.sh in workflow job",
    )

    artifact_cache_on = copy.deepcopy(workflow)
    artifact_cache_on["env"]["LAKE_ARTIFACT_CACHE"] = "true"
    expect_failure(
        check_cache_steps,
        "artifact caching disabled",
        artifact_cache_on,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
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
