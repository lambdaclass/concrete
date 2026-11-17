"""Verify the private archive and classify a Codex response."""
import hashlib
import json
import os
from pathlib import Path
import re
import secrets
import shlex
import shutil
import stat
import subprocess
import sys
import tempfile


def fail(message):
    sys.exit("review runner: " + message)


def check_links(links):
    for path in links:
        pending = list(reversed(path.parts))
        resolved = []
        followed = 0
        while pending:
            part = pending.pop()
            if part == "..":
                if not resolved:
                    fail(f"archive symlink escapes the private tree: {path}")
                resolved.pop()
                continue
            link = links.get(Path(*resolved, part))
            if link is None:
                resolved.append(part)
                continue
            if link.is_absolute():
                fail(f"archive symlink has an absolute target: {path}")
            followed += 1
            if followed > 40:
                fail(f"archive symlink chain is too deep or cyclic: {path}")
            pending.extend(reversed(link.parts))


def verify_tree(root, sha, directory):
    tree = Path(directory)
    # This verifier runs in the caller's directory, even when inspecting an archive.
    git_path = shutil.which("git")
    if git_path is None:
        fail("git executable not found")
    git = [os.path.abspath(git_path), "--no-replace-objects", "-C", root]
    object_format = subprocess.check_output(
        git + ["rev-parse", "--show-object-format"], text=True).strip()
    entries = subprocess.check_output(git + ["ls-tree", "-rz", sha]).split(b"\0")

    actual = {}
    links = {}
    for target in tree.rglob("*"):
        path = target.relative_to(tree)
        info = target.lstat()
        if stat.S_ISDIR(info.st_mode):
            continue
        if info.st_nlink != 1 or not (stat.S_ISREG(info.st_mode) or stat.S_ISLNK(info.st_mode)):
            fail(f"archive hardlink or special file: {path}")
        if stat.S_ISLNK(info.st_mode):
            links[path] = Path(os.readlink(target))
        actual[path] = info.st_mode
    check_links(links)

    probe = None
    for entry in filter(None, entries):
        metadata, name = entry.split(b"\t", 1)
        mode, kind, oid = metadata.split()
        path = Path(os.fsdecode(name))
        target = tree / path
        if kind != b"blob":
            fail(f"unsupported tree entry: {path}")
        actual_mode = actual.pop(path, 0)
        if mode == b"120000":
            if not stat.S_ISLNK(actual_mode):
                fail(f"archive symlink mismatch: {path}")
            content = os.fsencode(os.readlink(target))
        elif mode in (b"100644", b"100755"):
            if not stat.S_ISREG(actual_mode):
                fail(f"archive file missing or wrong type: {path}")
            content = target.read_bytes()
            if bool(actual_mode & 0o111) != (mode == b"100755"):
                fail(f"archive mode mismatch: {path}")
            if probe is None:
                probe = target, content
        else:
            fail(f"unsupported mode: {path}")
        blob_hash = hashlib.new(object_format, f"blob {len(content)}\0".encode())
        blob_hash.update(content)
        if blob_hash.hexdigest().encode() != oid:
            fail(f"archive byte mismatch: {path}")
    if actual or probe is None:
        fail("archive paths mismatch or no regular file to read")
    return probe


def final_verdict(body):
    preceding, _, last = body.rstrip().rpartition("\n")
    last = re.sub(r"(\*\*|\*|`|__|_)([^*_`]+)\1", r"\2", last)
    if last in ("VERDICT: clean", "VERDICT: findings"):
        return last.split()[-1], preceding
    return "noverdict", body


def verify_events(log, output, rc):
    command, digest = sys.stdin.read().splitlines()
    read = False
    errors = []
    for number, line in enumerate(log.open(encoding="utf-8", newline="\n"), 1):
        try:
            event = json.loads(line)
        except ValueError:
            fail(f"malformed reviewer event on line {number}: invalid JSON")
        if not isinstance(event, dict) or not isinstance(event.get("type"), str):
            fail(f"malformed reviewer event on line {number}: expected an object with a string type")
        event_type = event["type"]
        if event_type in ("error", "turn.failed"):
            errors.append(json.dumps(event.get("error", event.get("message", ""))))
        if not event_type.startswith("item."):
            continue
        item = event.get("item")
        if not isinstance(item, dict) or not isinstance(item.get("type"), str):
            fail(f"malformed reviewer item on line {number}: expected an object with a string type")
        if item["type"] == "error":
            errors.append(str(item.get("message", "")))
        if event_type != "item.completed" or item["type"] != "command_execution":
            continue
        recorded = item.get("command")
        status = item.get("status")
        aggregated_output = item.get("aggregated_output")
        exit_code = item.get("exit_code")
        if aggregated_output is None:
            aggregated_output = ""
        if (not all(isinstance(value, str) for value in (recorded, status, aggregated_output))
                or (exit_code is not None and type(exit_code) is not int)):
            fail(f"malformed reviewer command item on line {number}")
        if exit_code != 0 or status != "completed" or aggregated_output.strip() != digest:
            continue
        # Codex records an outer shell invocation with the probe as its entire script.
        try:
            argv = shlex.split(recorded)
        except ValueError:
            continue
        if (len(argv) == 3 and Path(argv[0]).name in ("sh", "bash", "zsh")
                and argv[1] in ("-c", "-lc") and argv[2] == command):
            read = True

    policy = (r"invalid[_ ]prompt\b|content[_ -](?:filter|policy)|safety (?:system|filter|policy)"
              r"|cyber ?security|usage polic|flagged (?:as|for)|potentially (?:harmful|malicious)"
              r"|violat(?:es|ion of) .*polic")
    if errors:
        if all(re.search(policy, error, re.I) for error in errors):
            sys.exit(3)
        fail("reviewer reported an execution error")
    body = output.read_text(errors="replace")
    verdict, details = final_verdict(body)
    refusal = (r"\b(?:I can['’]t|I cannot|I won['’]t|I(?:['’]m| am) (?:not able|unable) to|unable to)"
               r"\s+(?:help|assist|comply|review|continue|provide)\b")
    if verdict == "noverdict" and re.search(policy + "|" + refusal, body, re.I):
        sys.exit(3)
    if rc:
        fail(f"reviewer exited {rc}")
    if not read:
        fail("missing successful repository-read command evidence")
    print(verdict)
    if verdict == "findings":
        print(details)


def main():
    action = sys.argv[1]
    if action == "tree":
        verify_tree(*sys.argv[2:])
    elif action == "probe":
        root, sha, directory, python = sys.argv[2:]
        target, content = verify_tree(root, sha, directory)
        nonce = secrets.token_bytes(32)
        with tempfile.NamedTemporaryFile(dir=directory, prefix=".concrete-review-nonce-", delete=False) as challenge:
            challenge.write(nonce)
            nonce_name = challenge.name
        code = (
            'import hashlib, sys; '
            'nonce, target = (bytes.fromhex(path) for path in sys.argv[1:]); '
            'print(hashlib.sha256(open(nonce, "rb").read() + '
            'open(target, "rb").read()).hexdigest())'
        )
        # Keep the command on one line even with newlines or non-UTF-8 path bytes.
        paths = [os.fsencode(path).hex() for path in (nonce_name, target)]
        print(shlex.join([python, "-I", "-c", code, *paths]))
        print(hashlib.sha256(nonce + content).hexdigest())
    elif action == "events":
        verify_events(*map(Path, sys.argv[2:4]), int(sys.argv[4]))


if __name__ == "__main__":
    main()
