"""Check indexed shell paths using the current tracked working-tree contents."""

import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile


SHELL_SHEBANG = re.compile(rb"^#!.*[/ \t](?:bash|sh)(?:[ \t\r\n]|$)")
SHELLCHECK = re.compile(r"^\s*shellcheck\b")


def shell_sources():
    entries = subprocess.check_output(
        ["git", "ls-files", "--stage", "-z", "--", "scripts/", ".githooks/"]
    )
    root = Path.cwd()
    for entry in entries.split(b"\0"):
        if not entry:
            continue
        metadata, name = entry.split(b"\t", 1)
        mode, _, stage = metadata.split()
        path = Path(os.fsdecode(name))
        if stage != b"0":
            raise ValueError(f"{path}: unresolved index entry")
        if mode not in (b"100644", b"100755"):
            continue
        if path.resolve() != root / path:
            raise ValueError(f"{path}: indexed regular file is reached through a symlink")
        content = path.read_bytes()
        if path.suffix == ".sh" or SHELL_SHEBANG.match(content.partition(b"\n")[0]):
            yield path, content


def comments(node):
    if isinstance(node, dict):
        if "Hash" in node and "Text" in node:
            yield node
        else:
            for value in node.values():
                yield from comments(value)
    elif isinstance(node, list):
        for item in node:
            yield from comments(item)


def without_directives(path, content):
    if b"shellcheck" not in content:
        return content
    result = subprocess.run(
        ["shfmt", "-ln", "bash", "-tojson"], input=content, capture_output=True
    )
    if result.returncode:
        raise ValueError(f"{path}: shfmt: {result.stderr.decode().strip()}")
    rewritten = bytearray(content)
    for comment in comments(json.loads(result.stdout)):
        if SHELLCHECK.match(comment["Text"]):
            offset = comment["Hash"]["Offset"]
            # A second hash makes this an ordinary comment while preserving offsets.
            rewritten[offset + 1] = ord("#")
    return rewritten


def check():
    with tempfile.TemporaryDirectory() as directory:
        paths = []
        for path, content in shell_sources():
            copy = Path(directory) / path
            copy.parent.mkdir(parents=True, exist_ok=True)
            copy.write_bytes(without_directives(path, content))
            paths.append(str(path))
        if not paths:
            raise ValueError("no tracked shell sources")
        env = os.environ.copy()
        env.pop("SHELLCHECK_OPTS", None)
        result = subprocess.run(
            ["shellcheck", "--norc", "--shell=bash", "--format=json", *paths],
            cwd=directory,
            env=env,
            stdout=subprocess.PIPE,
        )
        if result.returncode not in (0, 1):
            raise ValueError(f"ShellCheck exited {result.returncode}")
        # Filter after parsing. ShellCheck's --include can suppress parser errors.
        failed = False
        for diagnostic in json.loads(result.stdout):
            if diagnostic["level"] == "error" or diagnostic["code"] == 2318:
                print(
                    "{file}:{line}:{column}: {message} [SC{code}]".format(**diagnostic),
                    file=sys.stderr,
                )
                failed = True
        return int(failed)


if __name__ == "__main__":
    try:
        sys.exit(check())
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"shell_assignments: {error}", file=sys.stderr)
        sys.exit(2)
