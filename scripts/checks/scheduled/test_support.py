"""Runtime tool paths may be inherited; test identities are always fixture-owned."""
import os

PREFIXES = ("GITHUB_", "SELECTED_", "SCHEDULED_", "RUNNER_", "GIT_")

def runtime_environment():
    return {key: value for key, value in os.environ.items()
            if not key.startswith(PREFIXES) and key not in {"OCCURRENCE", "GH_TOKEN"}}
