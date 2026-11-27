#!/usr/bin/env python3
"""Read and validate tests/manifest.tsv; print rows for shell consumers.

Usage: test_manifest.py [validate] [MANIFEST]
       test_manifest.py rows [MANIFEST]
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
CLI_CASES = {
    "cli:invalid-backend",
    "cli:invalid-path",
    "cli:unknown-target",
    "cli:native-helper-failure",
    "cli:build-input-write-failure",
}
SLICE_RE = re.compile(r"[a-z0-9_]+")
RULE_RE = re.compile(r"[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)*@[1-9][0-9]*")
TRAP_KINDS = frozenset(("arithmeticOverflow", "division_by_zero", "invalid_shift",
                        "out_of_bounds", "invalid_range", "abort"))
ORIGIN_RE = re.compile(r"([0-9]+):([0-9]+):([1-9][0-9]*):([1-9][0-9]*)")
CLI_CODE_RE = re.compile(r"[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)*")
LABEL_RE = re.compile(r"[a-z][a-z0-9_]*")
REFUSAL_KINDS = frozenset(("unreachable_statement", "reserved_binder"))
PENDING_REFUSALS = frozenset(("syntax", "unsupported_form", "checker"))
# Shrink this inventory as 4(c)/4(d)/4(f) expose typed refusals. New paths require typed fields.
PENDING_SOURCES = frozenset(
    "con:tests/slice0/" + name for name in (
        "overflow.con",
        "syntax_error.con",
        "unsupported.con",
        "unannotated.con",
        "suffix.con",
        "reserved.con",
        "reserved_move.con",
        "reserved_struct.con",
        "intrinsic_shadow.con",
        "radix_missing_semi.con",
        "malformed_if.con",
        "multiply.con",
        "conditional.con",
        "mutable.con",
        "contextual_multiply.con",
        "shadow_destroy.con",
        "shadow_discard.con",
        "shadow_alloc.con",
        "shadow_sizeof.con",
        "shadow_alignof.con",
        "shadow_read_owned.con",
        "shadow_get_args.con",
        "shadow_byte_at.con",
        "empty_body.con",
        "fallthrough.con",
        "shadow_wrapping_add.con",
        "shadow_saturating_add.con",
        "shadow_checked_add.con",
        "shadow_try_convert.con",
    )
)


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


def validate_origin(path, line, origin):
    match = ORIGIN_RE.fullmatch(origin)
    if not match:
        fail(path, line, f"invalid origin {origin!r}")
    start, stop, _, _ = (natural(path, line, value) for value in match.groups())
    if stop < start:
        fail(path, line, f"reversed origin {origin!r}")


def validate_stderr(path, line, data):
    if data != "-" and not re.fullmatch(r"(?:[0-9a-f]{2})+", data):
        fail(path, line, f"invalid stderr bytes {data!r}")


def validate_expect(path, line, expect):
    parts = expect.split(" ")
    match parts:
        case ["exit", status, stderr] if re.fullmatch(r"[0-9]+", status):
            status = natural(path, line, status)
            if status > 255:
                fail(path, line, f"exit status {status} is outside 0..255")
            validate_stderr(path, line, stderr)
        case ["trap", kind, site, stderr] if kind in TRAP_KINDS and re.fullmatch(r"[0-9]+", site):
            natural(path, line, site)
            validate_stderr(path, line, stderr)
            if stderr == "-":
                fail(path, line, "trap requires nonempty stderr")
        case ["refuse", "1", kind, rule, primary, related] if kind in REFUSAL_KINDS and RULE_RE.fullmatch(rule):
            validate_origin(path, line, primary)
            labels = set()
            if related != "-":
                for item in related.split(","):
                    label, _, location = item.partition("=")
                    if not LABEL_RE.fullmatch(label) or label in labels:
                        fail(path, line, f"invalid or duplicate related origin label {label!r}")
                    labels.add(label)
                    validate_origin(path, line, location)
        case ["pending-refusal", "1", kind] if kind in PENDING_REFUSALS:
            pass
        case ["diagnostic", status, code, *message] if (
                status in ("2", "3", "4") and CLI_CODE_RE.fullmatch(code) and all(message)):
            pass
        case ["llvm", "exit", status, "lcb", "resource", resource, bound, measured] if (
                LABEL_RE.fullmatch(resource) and re.fullmatch(r"[1-9][0-9]*", bound)
                and re.fullmatch(r"[1-9][0-9]*", measured)):
            if natural(path, line, measured) <= natural(path, line, bound):
                fail(path, line, "resource measurement must exceed the bound")
            validate_expect(path, line, f"exit {status} -")
        case _:
            fail(path, line, f"invalid expect {expect!r}")


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
    cur = tests_dir
    for part in parts[1:]:
        cur = cur / part
        if cur.is_symlink():
            if not cur.exists():
                fail(path, line, f"con: path {relative!r} is a dangling symlink")
            fail(path, line, f"con: path {relative!r} must not be or traverse a symlink")
    source = cur
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
        elif program not in CLI_CASES:
            fail(path, line_number, f"invalid program {program!r}; expected bir:, con: or a known cli: case")
        validate_expect(path, line_number, expect)
        if program.startswith("bir:") and expect.startswith(("refuse ", "pending-refusal ")):
            fail(path, line_number, f"bir: row {ident!r} cannot use refuse; expected exit or trap")
        if expect.startswith("pending-refusal ") and program not in PENDING_SOURCES:
            fail(path, line_number, "pending-refusal is restricted to existing untyped source fixtures")
        if program.startswith("cli:") != expect.startswith("diagnostic "):
            fail(path, line_number, "standalone diagnostic expectations require a cli: case")
        if not program.startswith("con:") and expect.startswith("llvm "):
            fail(path, line_number, "backend-specific CLI expectations require a con: source")
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
        (tests / "slice0" / "link.con").symlink_to(tests / "slice0" / "ok.con")
        (tests / "slice0" / "dangling.con").symlink_to(tests / "slice0" / "missing.con")
        valid = ("id\tprogram\texpect\tslice\n"
                 "a\tbir:answer\texit 42 -\tslice0\n"
                 "b\tcon:tests/slice0/ok.con\trefuse 1 unreachable_statement STMT-14@1 0:1:1:1 anchor=2:3:1:3\tslice0\n")
        assert len(parse(tests / "manifest.tsv", valid)) == 2
        refusal = "refuse 1 unreachable_statement STMT-14@1 0:1:1:1 anchor=2:3:1:3"
        llvm = valid.replace(refusal, "llvm exit 42 lcb resource values 488 1924")
        assert len(parse(tests / "manifest.tsv", llvm)) == 2
        cli = "c\tcli:invalid-backend\tdiagnostic 3 USAGE\tslice0\n"
        assert len(parse(tests / "manifest.tsv", valid + cli)) == 3
        bad = [
            valid + cli.replace("diagnostic 3", "diagnostic 0"),
            valid + cli.replace("diagnostic 3 USAGE", "exit 3 -"),
            valid + cli.replace("invalid-backend", "unknown-case"),
            valid.replace(refusal, "diagnostic 3 USAGE"),
            valid.replace("refuse 1", "refuse 2"),
            valid.replace(refusal, "pending-refusal 2 checker"),
            llvm.replace("488 1924", "488 488"),
            llvm.replace("exit 42 lcb", "exit 256 lcb"),
            llvm.replace("values 488", "values 0"),
            valid.replace(refusal, "pending-refusal 1 checker"),
            valid.replace("exit 42 -", "pending-refusal 1 checker"),
            valid.replace("exit 42 -", "trap arithmeticOverflow 0 -"),
            valid.replace("exit 42 -", "exit 42 0A"),
            valid.replace("STMT-14@1", "STMT-14@0"),
            valid.replace("0:1:1:1", "0:1:0:1"),
            valid.replace("0:1:1:1", "0:1:1:0"),
            valid.replace("STMT-14@1", "STMT-14"),
            valid.replace("0:1:1:1", "1:0:1:1"),
            valid.replace("anchor=2:3:1:3", "anchor=2:3:1:3,anchor=0:1:1:1"),
            valid.replace("exit 42 -", "exit 42 0"),
            valid.replace("exit 42 -", "trap overflow 0 0a"),
            valid.replace("exit 42 -", "llvm exit 42 lcb resource values 488 1924"),
            valid + "c\tcon:tests/slice0/link.con\texit 0 -\tslice0\n",
            valid + "c\tcon:tests/slice0/dangling.con\texit 0 -\tslice0\n",
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
    if command not in ("validate", "rows") or len(args) > 1:
        raise ManifestError(__doc__)
    rows = load(args[0] if args else "tests/manifest.tsv")
    if command == "rows":
        for row in rows:
            print(f"{row.id}\t{row.program}\t{row.expect}")
    else:
        print(f"test_manifest: {len(rows)} rows valid")


if __name__ == "__main__":
    try:
        main(sys.argv)
    except ManifestError as error:
        print(f"test_manifest: {error}", file=sys.stderr)
        sys.exit(1)
