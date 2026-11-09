"""Independent main-history observation; ancestor coverage is not SHA qualification."""
import datetime as dt
import json
import os
import subprocess
import sys
from protocol import EvidenceError, WINDOWS, git, inventory, observe, read_note, timestamp

ERRORS = (EvidenceError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError)

def observe_main(lineage, histories, tier, now, runners, repository):
    annotated = [(sha, histories[sha]) for sha in lineage if sha in histories]
    if not annotated:
        return [f"MISSING {tier} {lineage[0]}: absent run"]
    problems = []
    fresh = []
    for sha, history in annotated:
        try:
            message = observe(history, sha, tier, now, runners, repository)
            fresh.append((sha, message))
        except ERRORS as error:
            if isinstance(error, EvidenceError) and error.status == "STALE":
                continue
            typed = isinstance(error, EvidenceError) and error.operational
            if typed and fresh:
                continue
            text = str(error)
            if not (isinstance(error, EvidenceError) and
                    text.startswith(f"{error.status} {tier} {sha}:")):
                text = f"INCOMPLETE {tier} {sha}: {error}"
            problems.append(text)
    if problems:
        # All exact-SHA defects are surfaced, with failures first.
        return sorted(problems, key=lambda s: not s.startswith("FAIL "))
    if fresh:
        sha, message = fresh[0]
        return [message if sha == lineage[0] else
                f"{message}: ancestor coverage; tip {lineage[0]} requires its own evidence"]
    newest = max(annotated, key=lambda pair: max(timestamp(r["completed"]) for r in pair[1]))[0]
    return [f"STALE {tier} {newest}: no fresh PASS"]

def main():
    environment = dict(os.environ)
    failed = False
    try:
        repository = environment["GITHUB_REPOSITORY"]
        lineage = git("rev-list", "--first-parent", "refs/remotes/origin/main", environment=environment).splitlines()
        for tier in WINDOWS:
            try:
                ref = "refs/notes/concrete-gates-" + tier
                # No inventory needed to establish genuine absence.
                refs = git("for-each-ref", "--format=%(refname)", ref, environment=environment).splitlines()
                listing = git("notes", "--ref=" + ref, "list", environment=environment) if ref in refs else ""
                shas = {line.split()[-1] for line in listing.splitlines()}
                histories = {}; malformed = []
                for sha in lineage:
                    if sha in shas:
                        try:
                            histories[sha] = read_note(sha, tier, listed=True, environment=environment)
                        except ERRORS as error:
                            malformed.append(f"INCOMPLETE {tier} {sha}: {error}")
                messages = (observe_main(lineage, histories, tier,
                                        dt.datetime.now(dt.timezone.utc).timestamp(), inventory() if histories else {}, repository) if histories or not malformed else []) + malformed
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
