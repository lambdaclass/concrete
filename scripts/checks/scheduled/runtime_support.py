"""Runtime tool paths may be inherited; test identities are always fixture-owned."""
import os

PREFIXES = ("GITHUB_", "SELECTED_", "SCHEDULED_", "RUNNER_", "GIT_")

def runtime_environment():
    return {key: value for key, value in os.environ.items()
            if not key.startswith(PREFIXES) and key not in {"OCCURRENCE", "GH_TOKEN"}}



def copy_policy_runtime(controller):
    """Copy the trusted slot adapter's actual runtime closure into a fixture."""
    import shutil
    from pathlib import Path
    root = Path(__file__).resolve().parents[3]
    for relative in ('scripts/checks/lib/scheduler_policy.py',
                     'scripts/checks/lib/gate_manifest.py',
                     'scripts/checks/scheduled/policy.json',
                     'scripts/checks/scheduled/suite_budget.py',
                     'scripts/checks/scheduled/process_adapter.py',
                     'scripts/checks/gates.json'):
        target = controller / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(root / relative, target)
