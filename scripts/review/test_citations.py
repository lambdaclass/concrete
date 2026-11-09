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
    check("[`realAnchor`](source.lean#L2)", ("invalid", "citation: anchor 'realAnchor' absent from 'source.lean#L2'"))
    check("source.lean:3", ("invalid", "citation: range 'source.lean:3' outside 1..2; citation: positive verdict requires at least one resolvable file:line citation"))
    check("source.lean:1 `inventedLaw` `mutantFake` `caseFake`", ("invalid", "citation: identifier 'inventedLaw' unresolved; citation: identifier 'mutantFake' unresolved; citation: identifier 'caseFake' unresolved"))
    check("source.lean:1–2,2 `realAnchor`", "clean")
    check("[source.lean:1](file:///arbitrary/export/source.lean:1)", "clean")
    check("source.lean:1 `declModifiers`", "clean")
    check("source.lean:1 `c2245c99..21ad148b` `plainword`", "clean")
    check("source.lean:1 `missingName`\nABSENT: missingName", "clean")
    check("source.lean:1 Introduce `proposed:newLaw`", "findings", "findings")
    check("No citations", ("invalid", "citation: positive verdict requires at least one resolvable file:line citation"))
    base = sha
    (root / "source.lean").write_text("def realAnchor := 1\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "remove name")
    sha = git("rev-parse", "HEAD")
    result = contract.evaluate_output("source.lean:1 `otherAnchor`\nREAD: source.lean\nREPOSITORY: readable\nVERDICT: clean", "", sha, directory, ["clean"], base=base)
    assert result[0] == "clean", result
print("citation controls: PASS")
