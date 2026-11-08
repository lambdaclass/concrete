"""Read-only independent freshness observation of the latest resolved main SHA."""
import datetime as dt
import sys
from protocol import EvidenceError, WINDOWS, git, inventory, observe, read_note

try:
    sha = git("rev-parse", "refs/remotes/origin/main")
    runners = inventory()
    failed = False
    for tier in WINDOWS:
        try:
            print(observe(read_note(sha, tier), sha, tier,
                          dt.datetime.now(dt.timezone.utc).timestamp(), runners))
        except EvidenceError as error:
            print(error); failed = True
    sys.exit(1 if failed else 0)
except (EvidenceError, OSError) as error:
    sys.exit(str(error))
