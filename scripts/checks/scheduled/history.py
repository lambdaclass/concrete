"""Single owner of eligible main ancestry and operational freshness ordering."""
import os
import re
import sys
from protocol import EvidenceError, git, require

MAIN = "refs/remotes/origin/main"


def main_history(environment, command=git):
    tip = command("rev-parse", MAIN, environment=environment)
    # First-parent order is meaningful only for operational freshness coverage.
    first = command("rev-list", "--first-parent", tip, environment=environment).splitlines()
    eligible = command("rev-list", "--topo-order", tip, environment=environment).splitlines()
    require(bool(first) and set(first) <= set(eligible), "invalid main history")
    return first, eligible


def authorize(sha, environment):
    require(bool(re.fullmatch(r"[0-9a-f]{40}", sha)), "invalid full SHA")
    _, eligible = main_history(environment)
    require(sha in eligible, "replay SHA is not eligible main history")
    return sha


if __name__ == "__main__":
    try:
        require(len(sys.argv) == 2, "expected replay SHA")
        print(authorize(sys.argv[1], dict(os.environ)))
    except (EvidenceError, OSError, ValueError) as error:
        sys.exit(str(error))
