#!/usr/bin/env python3
"""Focused controls in a private git tree, never the caller working tree."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import os

sandbox_spec = importlib.util.spec_from_file_location("git_sandbox", Path(__file__).resolve().parents[1] / "checks/lib/git_sandbox.py")
sandbox_module = importlib.util.module_from_spec(sandbox_spec)
sandbox_spec.loader.exec_module(sandbox_module)

spec = importlib.util.spec_from_file_location("contract", Path(__file__).with_name("output_contract.py"))
contract = importlib.util.module_from_spec(spec)
spec.loader.exec_module(contract)
with tempfile.TemporaryDirectory() as directory:
    config = Path(directory) / "sandbox.config"
    config.write_text("[core]\n hooksPath = /dev/null\n[commit]\n gpgSign = false\n")
    env = sandbox_module.git_sandbox(config)
    os.environ.clear()
    os.environ.update(env)
    root = Path(directory)
    def git(*args):
        return subprocess.check_output(["git", "-C", directory, *args], env=env, stderr=subprocess.PIPE).decode().strip()
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
    check("source.lean:1 `proposed:newLaw`", ("invalid", "citation: proposed name 'proposed:newLaw' requires a FINDINGS/REJECT verdict"))
    check("No citations", ("invalid", "citation: positive verdict requires at least one resolvable file:line citation"))
    check("source.lean:1 `fictionalPart.realAnchor`", ("invalid", "citation: identifier 'fictionalPart.realAnchor' unresolved"))
    check("source.lean:1 `inventedLaw`", ("invalid", "citation: identifier 'inventedLaw' unresolved"), "findings")
    check("source.lean:1 old mode 100644 new mode 100755", "clean")
    check("[bad](source.lean#L1-L2-L3)", ("invalid", "citation: unsupported location grammar 'source.lean#L1-L2-L3'"))
    # An absolute prefix cannot redirect a missing script to a root/legacy basename.
    (root / "README.md").write_text("root file\n")
    (root / "ISSUES.tsv").write_text("issue row\n")
    (root / "ROADMAP.md").write_text("roadmap row\n")
    (root / "legacy").mkdir()
    (root / "legacy/source.lean").write_text("legacyAnchor\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "duplicate basename")
    sha = git("rev-parse", "HEAD")
    check("ROADMAP.md:1 README.md:1 ISSUES.tsv:1", "clean")
    check("[source.lean:1](file:///unusual/absolute/source.lean#L1)", "clean")
    # Exact root path wins even when a legacy copy exists.
    check("[`realAnchor`](source.lean#L1)", "clean")
    check("source.lean:1 file:///tmp/scripts/missing/README.md:1", ("invalid", "citation: file 'file:///tmp/scripts/missing/README.md' does not resolve uniquely"))
    (root / "lean-toolchain").write_text("different-pin\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "different toolchain pin")
    sha = git("rev-parse", "HEAD")
    check("source.lean:1 `declModifiers`", ("invalid", "citation: identifier 'declModifiers' unresolved"))
    base = sha
    (root / "source.lean").write_text("def realAnchor := 1\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "remove name")
    sha = git("rev-parse", "HEAD")
    result = contract.evaluate_output("source.lean:1 `otherAnchor`\nREAD: source.lean\nREPOSITORY: readable\nVERDICT: clean", "", sha, directory, ["clean"], base=base)
    assert result[0] == "clean", result
print("citation controls: PASS")
