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


def nodes(node):
    if isinstance(node, dict):
        yield node
        for value in node.values():
            yield from nodes(value)
    elif isinstance(node, list):
        for item in node:
            yield from nodes(item)


def parse(path, content):
    result = subprocess.run(
        ["shfmt", "-ln", "bash", "-tojson"], input=content, capture_output=True
    )
    if result.returncode:
        raise ValueError(f"{path}: shfmt: {result.stderr.decode().strip()}")
    return json.loads(result.stdout)


def without_directives(content, tree):
    rewritten = bytearray(content)
    for comment in nodes(tree):
        if "Hash" in comment and SHELLCHECK.match(comment.get("Text", "")):
            offset = comment["Hash"]["Offset"]
            # A second hash makes this an ordinary comment while preserving offsets.
            rewritten[offset + 1] = ord("#")
    return rewritten


def arithmetic_names(operand):
    if operand["Type"] == "Word":
        parts = operand["Parts"]
        if len(parts) == 1 and parts[0]["Type"] == "Lit":
            yield operand["Pos"], parts[0]["Value"]
        return
    for key in ("X", "Y"):
        if key in operand:
            yield from arithmetic_names(operand[key])


def variable_reads(node):
    for expansion in nodes(node):
        kind = expansion.get("Type")
        if kind == "ParamExp":
            yield expansion["Pos"], expansion["Param"]["Value"]
        if kind == "ArithmExp":
            yield from arithmetic_names(expansion["X"])


def declaration_dependencies(tree):
    """Find expansions that read another initializer's target in either order."""
    for declaration in nodes(tree):
        if declaration.get("Type") != "DeclClause":
            continue
        args = declaration.get("Args", [])
        initialized = {
            arg["Name"]["Value"]
            for arg in args
            if "Name" in arg and ("Value" in arg or "Array" in arg)
        }
        if len(initialized) < 2:
            continue
        for arg in args:
            if "Name" not in arg:
                continue
            name = arg["Name"]["Value"]
            for pos, param in variable_reads(arg):
                if param != name and param in initialized:
                    yield pos, name, param


def check():
    with tempfile.TemporaryDirectory() as directory:
        paths = []
        dependencies = []
        for path, content in shell_sources():
            tree = parse(path, content)
            dependencies.extend(
                (str(path), pos, name, param)
                for pos, name, param in declaration_dependencies(tree)
            )
            copy = Path(directory) / path
            copy.parent.mkdir(parents=True, exist_ok=True)
            copy.write_bytes(without_directives(content, tree))
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
        reported = set()
        for diagnostic in json.loads(result.stdout):
            if diagnostic["level"] == "error" or diagnostic["code"] == 2318:
                print(
                    "{file}:{line}:{column}: {message} [SC{code}]".format(**diagnostic),
                    file=sys.stderr,
                )
                reported.add((diagnostic["file"], diagnostic["line"], diagnostic["column"]))
                failed = True
        for path, pos, name, param in dependencies:
            line, column = pos["Line"], pos["Col"]
            if (path, line, column) not in reported:
                print(
                    f"{path}:{line}:{column}: {name} expands {param} in the same "
                    "declaration. Split the assignments. [declaration dependency]",
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
