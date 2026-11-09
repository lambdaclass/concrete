"""Trusted publisher: immutable attempts, idempotent retry, leased notes updates."""
import json
import subprocess
import sys
from protocol import EvidenceError, append, clean_env, git, inventory, read_note, require, validate

def publish(record, runners, repository, environment):
    sha, tier = record["sha"], record["tier"]
    validate(record, sha, tier, runners, repository, record["attempt"])
    ref = "refs/notes/concrete-gates-" + tier
    for retry in range(4):
        remote = git("ls-remote", "origin", ref, environment=environment)
        expected = remote.split()[0] if remote else ""
        if expected:
            git("fetch", "origin", "+" + ref + ":" + ref, environment=environment)
            # Lease the fetched state, never a stale ls-remote result.
            expected = git("rev-parse", ref, environment=environment)
        else:
            git("update-ref", "-d", ref, environment=environment)
        history = read_note(sha, tier, environment=environment) if expected else []
        for old in history:
            validate(old, sha, tier, runners, repository, None)
            if old["attempt"] == record["attempt"]:
                if old["status"] == "MISSING" and old["conclusion"] == "queued-too-long":
                    return  # The first overdue observation permanently closes this attempt.
                require(old == record, "replay would overwrite attempt")
                return  # Lost response after successful push, or identical collector retry.
        history = append(history, record, sha, tier, runners, repository)
        body = "\n".join(json.dumps(item, sort_keys=True) for item in history) + "\n"
        subprocess.run(["git", "notes", "--ref=" + ref, "add", "-f", "-F", "-", sha],
                       input=body, text=True, check=True, timeout=60, env=clean_env(environment))
        local = git("rev-parse", ref, environment=environment)
        try:
            subprocess.run(["git", "push", "--force-with-lease=" + ref + ":" + expected,
                        "origin", ref + ":" + ref], timeout=120, env=clean_env(environment))
        except subprocess.TimeoutExpired:
            pass  # Remote readback, then identical-attempt recovery on the next iteration.
        if git("ls-remote", "origin", ref, environment=environment).split()[0:1] == [local]:
            return
    raise EvidenceError("INCOMPLETE: notes publication contention")

def main():
    from pathlib import Path
    import os
    environment = dict(os.environ)
    repository = environment["GITHUB_REPOSITORY"]
    try:
        runners = inventory()
    except EvidenceError:
        runners = {repository: set()}
    publish(json.loads(Path(sys.argv[1]).read_text()), runners, repository, environment)

if __name__ == "__main__":
    main()
