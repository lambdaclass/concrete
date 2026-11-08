"""Independent main-history observation; ancestor coverage is not SHA qualification."""
import datetime as dt
import json
import subprocess
import sys
from protocol import EvidenceError, WINDOWS, git, inventory, observe, read_note, timestamp

ERRORS = (EvidenceError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError)

def observe_main(lineage, histories, tier, now, runners):
    annotated = [(sha, histories[sha]) for sha in lineage if histories.get(sha)]
    if not annotated:
        return [f"MISSING {tier} {lineage[0]}: absent run"]
    problems = []
    fresh = []
    for sha, history in annotated:
        try:
            message = observe(history, sha, tier, now, runners)
            fresh.append(message)
        except ERRORS as error:
            # Old non-failing occurrences age into tier staleness, while FAIL never ages out.
            if not str(error).startswith("STALE "):
                text = str(error)
                if not text.startswith(("FAIL ", "INCOMPLETE ", "MISSING ")):
                    text = f"INCOMPLETE {tier} {sha}: {error}"
                problems.append(text)
    if problems:
        # All exact-SHA defects are surfaced, with failures first.
        return sorted(problems, key=lambda s: not s.startswith("FAIL "))
    if fresh:
        return [f"{fresh[0]}: ancestor coverage; tip {lineage[0]} requires its own evidence"]
    newest = max(annotated, key=lambda pair: max(timestamp(r["completed"]) for r in pair[1]))[0]
    return [f"STALE {tier} {newest}: no fresh PASS"]

def main():
    failed = False
    try:
        lineage = git("rev-list", "--first-parent", "refs/remotes/origin/main").splitlines()
        for tier in WINDOWS:
            try:
                ref = "refs/notes/concrete-gates-" + tier
                # No inventory needed to establish genuine absence.
                refs = git("for-each-ref", "--format=%(refname)", ref).splitlines()
                listing = git("notes", "--ref=" + ref, "list") if ref in refs else ""
                shas = {line.split()[-1] for line in listing.splitlines()}
                histories = {}; malformed = []
                for sha in lineage:
                    if sha in shas:
                        try:
                            histories[sha] = read_note(sha, tier, listed=True)
                        except ERRORS as error:
                            malformed.append(f"INCOMPLETE {tier} {sha}: {error}")
                messages = observe_main(lineage, histories, tier,
                                        dt.datetime.now(dt.timezone.utc).timestamp(), inventory() if histories else {}) + malformed
                for message in sorted(messages, key=lambda s: not s.startswith("FAIL ")):
                    print(message)
                    failed |= not message.startswith("PASS ")
            except ERRORS as error:
                print(f"INCOMPLETE {tier}: {error}"); failed = True
    except ERRORS as error:
        print(f"INCOMPLETE observer: {error}"); failed = True
    return int(failed)

if __name__ == "__main__":
    sys.exit(main())
