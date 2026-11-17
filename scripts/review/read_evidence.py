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
    for entry in filter(None, entries):
        metadata, name = entry.split(b"\t", 1)
        mode, kind, oid = metadata.split()
        path = os.fsdecode(name)
        expected.add(path)
        target = tree / path
        if kind != b"blob":
            fail("unsupported tree entry: " + path)
        content = subprocess.check_output(["git", "-C", root, "cat-file", "blob", oid])
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
    actual = {str(p.relative_to(tree)) for p in tree.rglob("*")
              if p.is_symlink() or not p.is_dir()}
    if actual != expected or probe is None:
        fail("archive paths mismatch or no regular file to read")
    path, digest = probe
    # Use Python rather than a platform-specific sha256 utility.
    code = "import hashlib; print(hashlib.sha256(open(" + repr(path) + ", 'rb').read()).hexdigest())"
    print("python3 -c " + shlex.quote(code))
    print("# expected repository digest: " + digest)
else:
    probe, log, output = map(Path, sys.argv[2:])
    transcript = log.read_text(errors="replace")
    body = output.read_text(errors="replace")
    failures = r"sandbox.*(?:fail|cannot|could not|unable)|(?:fail|cannot|could not|unable).*sandbox|execution sandbox could not start|repository reads failed|bwrap:|AppArmor.*denied|failed to.*(?:namespace|startup)"
    if re.search(failures, transcript + "\n" + body, re.I):
        fail("repository access/startup failure")
    digest = probe.read_text().split("# expected repository digest: ")[1].strip()
    for line in transcript.splitlines():
        try:
            event = json.loads(line)
        except ValueError:
            continue
        item = event.get("item", {})
        if (event.get("type") == "item.completed"
                and item.get("type") == "command_execution"
                and item.get("exit_code") == 0
                and digest in item.get("aggregated_output", "").splitlines()):
            break
    else:
        fail("missing successful repository-read command evidence")
