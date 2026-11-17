"""Verify archive identity and Codex repository-read evidence; no verdict inference."""
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys


def fail(message):
    sys.exit("review runner: " + message)


if sys.argv[1] == "tree":
    root, sha, directory = sys.argv[2:]
    tree = Path(directory)
    entries = subprocess.check_output(
        ["git", "-C", root, "ls-tree", "-rz", sha]).split(b"\0")
    expected = set()
    probe = None
    with subprocess.Popen(["git", "-C", root, "cat-file", "--batch"],
                          stdin=subprocess.PIPE, stdout=subprocess.PIPE) as batch:
        for entry in filter(None, entries):
            metadata, name = entry.split(b"\t", 1)
            mode, kind, oid = metadata.split()
            path = os.fsdecode(name)
            expected.add(path)
            target = tree / path
            if kind != b"blob":
                fail("unsupported tree entry: " + path)
            batch.stdin.write(oid + b"\n")
            batch.stdin.flush()
            object_id, object_type, size = batch.stdout.readline().split()
            if object_id != oid or object_type != b"blob":
                fail("unexpected git object: " + path)
            content = batch.stdout.read(int(size))
            if batch.stdout.read(1) != b"\n":
                fail("truncated blob: " + path)
            if mode == b"120000":
                if not target.is_symlink() or os.fsencode(os.readlink(target)) != content:
                    fail("archive symlink mismatch: " + path)
            elif mode in (b"100644", b"100755"):
                if target.is_symlink() or not target.is_file() or target.read_bytes() != content:
                    fail("archive byte mismatch: " + path)
                if bool(target.stat().st_mode & 0o111) != (mode == b"100755"):
                    fail("archive mode mismatch: " + path)
                if probe is None:
                    probe = (path, hashlib.sha256(content).hexdigest())
            else:
                fail("unsupported mode: " + path)
        batch.stdin.close()
        if batch.wait():
            fail("git cat-file failed")
    actual = {str(p.relative_to(tree)) for p in tree.rglob("*")
              if p.is_symlink() or not p.is_dir()}
    if actual != expected or probe is None:
        fail("archive paths mismatch or no regular file to read")
    path, digest = probe
    # Use Python rather than a platform-specific sha256 utility.
    code = "import hashlib; print(hashlib.sha256(open(" + repr(path) + ", 'rb').read()).hexdigest())"
    print("python3 -I -c " + shlex.quote(code))
    print("# expected repository digest: " + digest)
else:
    probe, log, output = map(Path, sys.argv[2:5])
    rc = int(sys.argv[5])
    digest = probe.read_text().splitlines()[1].split()[-1]
    read = False
    errors = []
    for line in log.read_text(errors="replace").splitlines():
        try:
            event = json.loads(line)
        except ValueError:
            fail("malformed reviewer event")
        if event.get("type") in ("error", "turn.failed"):
            errors.append(json.dumps(event.get("error", event.get("message", ""))))
        item = event.get("item", {})
        if item.get("type") == "error":
            errors.append(str(item.get("message", "")))
        if (event.get("type") == "item.completed"
                and item.get("type") == "command_execution"
                and item.get("exit_code") == 0
                and digest in item.get("aggregated_output", "").splitlines()):
            read = True
    policy = r"invalid_prompt|content[_ -](?:filter|policy)|safety (?:system|filter|policy)|cybersecurity policy"
    if errors:
        if all(re.search(policy, error, re.I) for error in errors):
            sys.exit(3)
        fail("reviewer reported an execution error")
    if rc:
        fail(f"reviewer exited {rc}")
    body = output.read_text(errors="replace")
    if not re.search(r"(?m)^\s*[*`]*VERDICT:", body) and re.search(
            policy + r"|(?:I can.t|I cannot|I won.t|unable to) (?:help|assist|comply|review|continue|provide)",
            body, re.I):
        sys.exit(3)
    if not read:
        fail("missing successful repository-read command evidence")
