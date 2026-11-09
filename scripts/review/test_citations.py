#!/usr/bin/env python3
"""Focused controls in a private git tree, never the caller working tree."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile

spec = importlib.util.spec_from_file_location("contract", Path(__file__).with_name("output_contract.py"))
contract = importlib.util.module_from_spec(spec)
spec.loader.exec_module(contract)
with tempfile.TemporaryDirectory() as directory:
    root = Path(directory)
    def git(*args):
        return subprocess.check_output(["git", "-C", directory, *args], stderr=subprocess.DEVNULL).decode().strip()
    git("init", "-q")
    (root / "source.lean").write_text("def realAnchor := 1\ndef otherAnchor := 2\n")
    (root / "lean-toolchain").write_text("leanprover/lean4:v4.34.1\n")
    target = root / "scripts/review"
    target.mkdir(parents=True)
    (target / "external-names.tsv").write_bytes(Path(__file__).with_name("external-names.tsv").read_bytes())
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "fixture")
    sha = git("rev-parse", "HEAD")
    def check(body, expected, verdict="clean"):
        actual = contract.evaluate_output(body + "\nREAD: source.lean\nREPOSITORY: readable\nVERDICT: " + verdict,
                                          "", sha, directory, ["clean", "findings", "ACCEPT"])
        assert actual == expected if isinstance(expected, tuple) else actual[0] == expected, (body, actual, expected)
    check("[`realAnchor`](source.lean#L2)", ("blocked", "citation: anchor 'realAnchor' absent from 'source.lean#L2-L2'"))
    check("[`realAnchor`](source.lean#L1-L3)", ("blocked", "citation: range 'source.lean#L1-L3' outside 1..2"))
    check("[`realAnchor`](missing.lean#L1)", ("blocked", "citation: file 'missing.lean' does not exist"))
    for verdict in ("clean", "ACCEPT"):
        check("`inventedLaw`", ("blocked", "citation: identifier 'inventedLaw' absent from reviewed tree and pinned external names"), verdict)
    check("[`realAnchor`](source.lean#L1) and `otherAnchor`", "clean")
    check("Introduce `proposed:newLaw`", "findings", "findings")
    check("`declModifiers`", "clean")
    check("`c2245c99..21ad148b`", "clean")
    check("`proposed:newLaw`", ("blocked", "citation: proposed names require a FINDINGS/REJECT verdict"))
    (root / "lean-toolchain").write_text("some-other-pin\n")
    git("add", "lean-toolchain")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "changed pin")
    old_sha = sha
    sha = git("rev-parse", "HEAD")
    check("`declModifiers`", ("blocked", "citation: identifier 'declModifiers' absent from reviewed tree and pinned external names"))
    sha = old_sha
    check("source.lean:2", ("blocked", "citation: unnamed location 'source.lean:2' requires a named source link"))
    # Working-tree edits cannot repair invalid evidence at the reviewed SHA.
    (root / "source.lean").write_text("def inventedLaw := 1\n")
    check("`inventedLaw`", ("blocked", "citation: identifier 'inventedLaw' absent from reviewed tree and pinned external names"))
print("citation controls: PASS")
