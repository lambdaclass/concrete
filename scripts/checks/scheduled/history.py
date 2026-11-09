"""Single owner of eligible main ancestry and operational freshness ordering."""
import os
import re
import sys
from protocol import EvidenceError, git, require

from scheduler_policy import MAIN_HISTORY as MAIN
from dataclasses import dataclass


@dataclass(frozen=True)
class MainHistory:
    first_parent: tuple
    eligible: tuple

    @property
    def presentation(self):
        return list(self.first_parent) + [sha for sha in self.eligible if sha not in set(self.first_parent)]

    def __iter__(self):
        # Compatibility for callers explicitly requesting both order roles.
        return iter((list(self.first_parent), list(self.eligible)))


def main_history(environment, command=git):
    tip = command("rev-parse", MAIN, environment=environment)
    # First-parent order is meaningful only for operational freshness coverage.
    first = command("rev-list", "--first-parent", tip, environment=environment).splitlines()
    eligible = command("rev-list", "--topo-order", tip, environment=environment).splitlines()
    require(bool(first) and set(first) <= set(eligible), "invalid main history")
    return MainHistory(tuple(first), tuple(eligible))


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
