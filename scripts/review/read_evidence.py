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
    git = ["git", "--no-replace-objects", "-C", root]
    object_format = subprocess.check_output(
        git + ["rev-parse", "--show-object-format"], text=True).strip()
    entries = subprocess.check_output(git + ["ls-tree", "-rz", sha]).split(b"\0")
    expected = set()
    probe = None
    for entry in filter(None, entries):
        metadata, name = entry.split(b"\t", 1)
        mode, kind, oid = metadata.split()
        path = os.fsdecode(name)
        expected.add(path)
        target = tree / path
        if kind != b"blob":
            fail("unsupported tree entry: " + path)
        if mode == b"120000":
            if not target.is_symlink():
                fail("archive symlink mismatch: " + path)
            content = os.fsencode(os.readlink(target))
        elif mode in (b"100644", b"100755"):
            if target.is_symlink() or not target.is_file():
                fail("archive file missing or wrong type: " + path)
            content = target.read_bytes()
            if bool(target.stat().st_mode & 0o111) != (mode == b"100755"):
                fail("archive mode mismatch: " + path)
            if probe is None:
                probe = (target, hashlib.sha256(content).hexdigest())
        else:
            fail("unsupported mode: " + path)
        blob_hash = hashlib.new(object_format, f"blob {len(content)}\0".encode())
        blob_hash.update(content)
        if blob_hash.hexdigest().encode() != oid:
            fail("archive byte mismatch: " + path)
    actual = {str(p.relative_to(tree)) for p in tree.rglob("*")
              if p.is_symlink() or not p.is_dir()}
    if actual != expected or probe is None:
        fail("archive paths mismatch or no regular file to read")
    target, digest = probe
    code = f"import hashlib; print(hashlib.sha256(open({str(target)!r}, 'rb').read()).hexdigest())"
    print("python3 -I -c " + shlex.quote(code))
    print("# expected repository digest: " + digest)
else:
    probe, log, output = map(Path, sys.argv[2:5])
    rc = int(sys.argv[5])
    command, expected_digest = probe.read_text().split("\n")[:2]
    digest = expected_digest.split()[-1]
    read = False
    errors = []
    for line in log.open(encoding="utf-8", newline="\n"):
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
                and item.get("status") == "completed"
                and item.get("aggregated_output", "").strip() == digest):
            # Codex records an outer shell invocation, with the probe as its
            # entire script. Reject compound commands and digest-only output.
            try:
                argv = shlex.split(item.get("command", ""))
            except ValueError:
                continue
            if (len(argv) == 3 and Path(argv[0]).name in ("sh", "bash", "zsh")
                    and argv[1] in ("-c", "-lc") and argv[2] == command):
                read = True
    policy = r"invalid[_ ]prompt\b|content[_ -](?:filter|policy)|safety (?:system|filter|policy)|cybersecurity policy"
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
