"""Append one immutable attempt and publish notes with compare-and-swap retries."""
import json
import os
import subprocess
import sys
from pathlib import Path
from protocol import EvidenceError, append, git, inventory, read_note, require

record = json.loads(Path(sys.argv[1]).read_text())
record["artifact"] = os.environ["RESULT_ARTIFACT_URL"]
require(bool(record["artifact"]), "missing result-index artifact")
sha, tier = record["sha"], record["tier"]
ref = "refs/notes/concrete-gates-" + tier
require(tier in {"daily", "weekly"}, "wrong tier")
runners = inventory()
for retry in range(4):
    # ls-remote distinguishes absent refs from network/auth failure.
    exists = git("ls-remote", "origin", ref)
    if exists:
        git("fetch", "origin", "+" + ref + ":" + ref)
    history = read_note(sha, tier) if exists else []
    history = append(history, record, sha, tier, runners)
    body = "\n".join(json.dumps(item, sort_keys=True) for item in history) + "\n"
    subprocess.run(["git", "notes", "--ref=" + ref, "add", "-f", "-F", "-", sha],
                   input=body, text=True, check=True, timeout=60)
    pushed = subprocess.run(["git", "push", "origin", ref + ":" + ref], timeout=120)
    if pushed.returncode == 0:
        require(git("ls-remote", "origin", ref).split()[0] == git("rev-parse", ref), "notes remote did not move")
        break
else:
    raise EvidenceError("INCOMPLETE: notes publication contention")
