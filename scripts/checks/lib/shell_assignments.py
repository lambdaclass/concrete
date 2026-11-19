"""Check indexed shell paths using the current tracked working-tree contents."""

import json
import os
from pathlib import Path
import re
import subprocess
import sys


SHELL_SHEBANG = re.compile(rb"^#!.*[/ \t](?:bash|sh)(?:[ \t\r\n]|$)")
DECLARATIONS = {"local", "declare", "typeset", "readonly", "export"}
EXPANSIONS = {"ParamExp", "ArithmExp", "CmdSubst"}


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


def literal_word(parts):
    text = []
    for part in parts:
        kind = part["Type"]
        if kind == "Lit":
            text.append(re.sub(r"\\(.)", r"\1", part["Value"]))
        elif kind == "SglQuoted":
            text.append(part["Value"])
        elif kind == "DblQuoted" and not part.get("Dollar"):
            contents = part.get("Parts", [])
            if any(item["Type"] != "Lit" for item in contents):
                return None
            text.extend(item["Value"] for item in contents)
        else:
            return None
    return "".join(text)


def declaration_operands(declaration):
    options = True
    for arg in declaration.get("Args", []):
        if options and arg.get("Naked") and "Name" not in arg:
            value = literal_word(arg["Value"]["Parts"])
            if value == "--":
                options = False
                continue
            if value and re.fullmatch(r"[-+][a-zA-Z]+", value):
                continue
            if value == "-" and declaration["Variant"]["Value"] == "local":
                continue
        options = False
        yield arg


def expanded_declarations(tree):
    for declaration in nodes(tree):
        if declaration.get("Type") != "DeclClause":
            continue
        if declaration["Variant"]["Value"] not in DECLARATIONS:
            continue
        operands = list(declaration_operands(declaration))
        if len(operands) > 1 and any(
            node.get("Type") in EXPANSIONS for node in nodes(operands)
        ):
            yield declaration["Pos"]


def check():
    found_source = False
    failed = False
    for path, content in shell_sources():
        found_source = True
        for pos in expanded_declarations(parse(path, content)):
            print(
                f"{path}:{pos['Line']}:{pos['Col']}: expansion in a multi-name "
                "declaration. Split into one declaration per line.",
                file=sys.stderr,
            )
            failed = True
    if not found_source:
        raise ValueError("no tracked shell sources")
    return int(failed)


if __name__ == "__main__":
    try:
        sys.exit(check())
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"shell_assignments: {error}", file=sys.stderr)
        sys.exit(2)
