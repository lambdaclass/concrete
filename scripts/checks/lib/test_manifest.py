#!/usr/bin/env python3
"""Read and validate tests/manifest.tsv; print rows for shell consumers.

Usage: test_manifest.py [validate] [MANIFEST]
       test_manifest.py rows [--kind bir|con] [MANIFEST]
       test_manifest.py self-test

The manifest's directory is the tests/ directory. Source paths are written as
con:tests/... and resolved from that directory's parent. Every defect is an
error naming its manifest line; no partial row set is printed.
"""

import re
import sys
import tempfile
from pathlib import Path, PurePosixPath
from typing import NamedTuple

HEADER = ["id", "program", "expect", "slice"]
ID_RE = re.compile(r"[a-z0-9_]+")
SLICE_RE = re.compile(r"[a-z0-9_]+")
RULE_RE = re.compile(r"[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)*(?:@[0-9]+)?")
TRAP_KIND_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")


class Row(NamedTuple):
    id: str
    program: str
    expect: str
    slice: str


class ManifestError(Exception):
    pass


def fail(path, line, message):
    where = f"{path}:{line}" if line is not None else str(path)
    raise ManifestError(f"{where}: {message}")


def natural(path, line, text):
    try:
        return int(text)
    except ValueError:
        fail(path, line, f"invalid natural number {text!r}")


def validate_expect(path, line, expect):
    parts = expect.split(" ")
    if len(parts) == 2 and parts[0] == "exit" and re.fullmatch(r"[0-9]+", parts[1]):
        status = natural(path, line, parts[1])
        if status > 255:
            fail(path, line, f"exit status {status} is outside 0..255")
        return
    if (len(parts) == 4 and parts[0] == "trap" and parts[2] == "site"
            and TRAP_KIND_RE.fullmatch(parts[1]) and re.fullmatch(r"[0-9]+", parts[3])):
        natural(path, line, parts[3])
        return
    if len(parts) == 2 and parts[0] == "refuse" and RULE_RE.fullmatch(parts[1]):
        return
    fail(path, line, f"invalid expect {expect!r}; use exit N, trap KIND site N, or refuse RULE-ID")


def validate_source(path, line, program, seen_sources):
    relative = program[4:]
    parts = relative.split("/")
    pure = PurePosixPath(relative)
    if (not relative.startswith("tests/") or not relative.endswith(".con")
            or pure.is_absolute() or "\\" in relative
            or any(part in ("", ".", "..") for part in parts)):
        fail(path, line, f"con: path {relative!r} must be a normalized tests/*.con path")
    if len(parts) > 1 and parts[1] == "golden":
        fail(path, line, f"con: path {relative!r} must not be under tests/golden/")
    tests_dir = path.parent.resolve()
    source = tests_dir.joinpath(*parts[1:])
    try:
        resolved = source.resolve(strict=True)
    except OSError:
        fail(path, line, f"con: file {relative!r} does not exist")
    except RuntimeError as error:
        fail(path, line, f"con: file {relative!r} cannot be resolved: {error}")
    if not resolved.is_file():
        fail(path, line, f"con: path {relative!r} is not a file")
    if resolved != tests_dir and tests_dir not in resolved.parents:
        fail(path, line, f"con: file {relative!r} escapes tests/")
    if relative in seen_sources or resolved in seen_sources.values():
        fail(path, line, f"con: path {relative!r} is listed more than once")
    seen_sources[relative] = resolved


def parse(path, text):
    rows = []
    ids = {}
    seen_bir = {}
    seen_sources = {}
    header_line = None
    for line_number, line in enumerate(text.splitlines(), 1):
        if not line or line.startswith("#"):
            continue
        fields = line.split("\t")
        if header_line is None:
            if fields != HEADER:
                fail(path, line_number, "header must be id<TAB>program<TAB>expect<TAB>slice")
            header_line = line_number
            continue
        if len(fields) != 4:
            fail(path, line_number, f"needs 4 tab-separated columns, found {len(fields)}")
        ident, program, expect, slice_name = fields
        if not ID_RE.fullmatch(ident):
            fail(path, line_number, f"invalid id {ident!r}; expected [a-z0-9_]+")
        if ident in ids:
            fail(path, line_number, f"duplicate id {ident!r} (first at line {ids[ident]})")
        ids[ident] = line_number
        if program.startswith("bir:"):
            name = program[4:]
            if not ID_RE.fullmatch(name):
                fail(path, line_number, f"invalid BIR example name {name!r}")
            if name in seen_bir:
                fail(path, line_number, f"duplicate bir program {name!r} (first at line {seen_bir[name]})")
            seen_bir[name] = line_number
        elif program.startswith("con:"):
            validate_source(path, line_number, program, seen_sources)
        else:
            fail(path, line_number, f"invalid program {program!r}; expected bir: or con:")
        validate_expect(path, line_number, expect)
        if program.startswith("bir:") and expect.startswith("refuse "):
            fail(path, line_number, f"bir: row {ident!r} cannot use refuse; expected exit or trap")
        if not SLICE_RE.fullmatch(slice_name):
            fail(path, line_number, f"invalid slice {slice_name!r}; expected [a-z0-9_]+")
        rows.append(Row(ident, program, expect, slice_name))
    if header_line is None:
        fail(path, 1, "missing header id<TAB>program<TAB>expect<TAB>slice")
    if not rows:
        fail(path, header_line, "manifest has no test rows")
    return rows


def load(manifest):
    path = Path(manifest)
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as error:
        raise ManifestError(f"{path}: cannot read manifest: {error}") from error
    return parse(path, text)


def self_test():
    with tempfile.TemporaryDirectory(prefix="concrete-manifest-") as tmp:
        root = Path(tmp)
        tests = root / "tests"
        tests.mkdir()
        (tests / "slice0").mkdir()
        (tests / "slice0" / "ok.con").write_text("return 0;\n", encoding="utf-8")
        valid = ("id\tprogram\texpect\tslice\n"
                 "a\tbir:answer\texit 42\tslice0\n"
                 "b\tcon:tests/slice0/ok.con\trefuse SYN-5@1\tslice0\n")
        assert len(parse(tests / "manifest.tsv", valid)) == 2
        bad = [
            valid + "a\tbir:other\texit 0\tslice0\n",
            valid + "c\tbir:other\texit 256\tslice0\n",
            valid + "c\tbir:other\ttrap foo\tslice0\n",
            valid + "c\tbir:other\ttrap arithmeticOverflow site 0\n",
            valid + "c\tbir:answer\texit 0\tslice0\n",
            valid + "c\tbir:other\trefuse SYN-5\tslice0\n",
            valid + "c\tcon:tests/slice0/missing.con\texit 0\tslice0\n",
        ]
        for text in bad:
            try:
                parse(tests / "manifest.tsv", text)
            except ManifestError:
                pass
            else:
                raise AssertionError("self-test accepted a defective manifest")
    print("test_manifest self-test: ok")


def main(argv):
    command = argv[1] if len(argv) > 1 else "validate"
    if command == "self-test" and len(argv) == 2:
        self_test()
        return
    args = argv[2:] if len(argv) > 1 else []
    kind = None
    if command == "rows" and args[:1] == ["--kind"]:
        if len(args) < 2 or args[1] not in ("bir", "con"):
            raise ManifestError("rows --kind requires bir or con")
        kind = args[1]
        args = args[2:]
    if command not in ("validate", "rows") or len(args) > 1:
        raise ManifestError(__doc__)
    rows = load(args[0] if args else "tests/manifest.tsv")
    if command == "rows":
        for row in rows:
            if kind is None or row.program.startswith(kind + ":"):
                print(f"{row.id}\t{row.program}\t{row.expect}")
    else:
        print(f"test_manifest: {len(rows)} rows valid")


if __name__ == "__main__":
    try:
        main(sys.argv)
    except ManifestError as error:
        print(f"test_manifest: {error}", file=sys.stderr)
        sys.exit(1)
