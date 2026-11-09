#!/usr/bin/env python3
"""Focused controls in a private git tree, never the caller working tree."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import os
import shlex

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
    (root / ".claude").mkdir()
    (root / ".claude/marker.txt").write_text("checkout parent collision control\n")
    (root / "source.lean").write_text("def realAnchor := 1\ndef otherAnchor := 2\n")
    (root / "source.123").write_text("numeric extension\n")
    (root / "lean-toolchain").write_text("leanprover/lean4:v4.34.1\n")
    target = root / "scripts/review"
    target.mkdir(parents=True)
    (target / "external-names.tsv").write_bytes(Path(__file__).with_name("external-names.tsv").read_bytes())
    (target / "test_contamination.py").write_text("inventedLaw mutantFake caseFake\n")
    (target / "fixtures").mkdir(exist_ok=True)
    (target / "fixtures/fake.txt").write_text("inventedLaw mutantFake caseFake\n")
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
    check(f"[source.lean:1](file://{directory}/source.lean:1)", "clean")
    check("source.lean:1 `declModifiers`", "clean")
    check("source.lean:1 `c2245c99..21ad148b` `plainword`", "clean")
    check("source.lean:1 `missingName`\nABSENT: missingName", "clean")
    check("source.lean:1 Introduce `proposed:newLaw`", "findings", "findings")
    check("source.lean:1 `proposed:newLaw`", ("invalid", "citation: proposed name 'proposed:newLaw' requires a FINDINGS/REJECT verdict"))
    check("source.lean:1 `proposed:`", "clean")
    check("[`text:inventedLaw`](scripts/review/fixtures/fake.txt:1) source.lean:1", "clean")
    check("[`text:missingName`](scripts/review/fixtures/fake.txt:1) source.lean:1", ("invalid", "citation: text anchor 'missingName' absent from 'scripts/review/fixtures/fake.txt:1'"))
    check("No citations", ("invalid", "citation: positive verdict requires at least one resolvable file:line citation"))
    check("source.lean:1 `fictionalPart.realAnchor`", ("invalid", "citation: identifier 'fictionalPart.realAnchor' unresolved"))
    check("source.lean:1 `inventedLaw`", ("invalid", "citation: identifier 'inventedLaw' unresolved"), "findings")
    check("source.lean:1 old mode 100644 new mode 100755", "clean")
    check("[bad](source.lean#L1-L2-L3)", ("invalid", "citation: unsupported location grammar 'source.lean#L1-L2-L3'"))
    check("[numeric](source.123:1)", "clean")
    check("[numeric](source.123:1junk)", ("invalid", "citation: unsupported location grammar 'source.123:1junk'"))
    # Harness declarations authorize READ only, never citations or identifiers.
    def harness(body, reads="REVIEW.diff", declared=("REVIEW.diff",)):
        return contract.evaluate_output(body + "\nREAD: " + reads + "\nREPOSITORY: readable\nVERDICT: clean",
            "", sha, directory, ["clean"], harness_files=declared)
    (root / "REVIEW.diff").write_text("helperOnlyName\n")
    assert harness("source.lean:1")[0] == "clean"
    for name in ("extra.txt", "DELTA.diff"):
        (root / name).write_text("helperOnlyName\n")
        assert harness("source.lean:1", name) == ("blocked", f"READ: path '{name}' does not exist as a file in commit {sha}")
    assert harness("REVIEW.diff:1") == ("invalid", "citation: harness file 'REVIEW.diff' is not evidence; citation: positive verdict requires at least one resolvable file:line citation")
    assert harness(f"file://{directory}/REVIEW.diff:1") == ("invalid", "citation: harness file 'REVIEW.diff' is not evidence; citation: positive verdict requires at least one resolvable file:line citation")
    assert harness("source.lean:1 `helperOnlyName`") == ("invalid", "citation: identifier 'helperOnlyName' unresolved")
    check("[`text:inventedLaw`](scripts/review/fixtures/fake.txt:1)", ("invalid", "citation: positive verdict requires at least one resolvable file:line citation"))
    check("`inventedLaw` source.lean:1 scripts/review/fixtures/fake.txt:1", ("invalid", "citation: identifier 'inventedLaw' unresolved"))
    # Exercise the actual builder, declaration sidecar, integrity check and CLI.
    collector = Path(__file__).with_name("collect_output.sh")
    with tempfile.TemporaryDirectory() as context:
        ctx = Path(context)
        (ctx / "prompt").write_text("review prompt\n")
        (ctx / "diff").write_text("deltaOnlyName\n")
        (ctx / "out").write_text("source.lean:1\nREAD: DELTA.diff\nREPOSITORY: readable\nVERDICT: clean\n")
        (ctx / "log").write_text("")
        q = lambda value: shlex.quote(str(value))
        code = "source " + q(collector) + "\n"
        code += "prepare_review_archive " + " ".join(map(q, [directory, sha, ctx / "tree", ctx / "manifest"])) + "\n"
        code += "prepare_review_context " + " ".join(map(q, [ctx / "tree", ctx / "manifest", ctx / "prompt", ctx / "diff", sha, sha, directory])) + "\n"
        code += "validate_reviewer_output " + " ".join(map(q, [directory, sha, ctx / "tree", ctx / "manifest", ctx / "out", ctx / "log", "clean,findings", 0]))
        proc = subprocess.run(["bash", "-e", "-c", code], capture_output=True, text=True)
        assert proc.returncode == 0 and proc.stdout == "status: clean\nreason: verified 1 READ path(s), REPOSITORY: readable, VERDICT: clean\n", (proc.stdout, proc.stderr)
        assert (ctx / "manifest.harness").read_text().splitlines() == ["PROMPT.md", "REVIEW.diff", "DELTA.diff", "RANGE.txt"]
        assert (ctx / "tree/DELTA.diff").read_bytes() == (ctx / "diff").read_bytes()
        subprocess.run(["chmod", "-R", "u+w", str(ctx / "tree")], check=True)
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
    check(f"[source.lean:1](file://{directory}/source.lean#L1)", "clean")
    check("[source.lean:1–2,2](source.lean#L1)", "clean")
    check("source.lean:1", "clean")
    check("[source.lean:1–2,2](legacy/source.lean#L1)", ("invalid", "citation: range 'source.lean:1–2,2' outside 1..1"))
    check("[source.lean:2](source.lean#L1)", ("invalid", "citation: basename label 'source.lean:2' disagrees with link target"))
    (root / "legacy/only.lean").write_text("legacyOnly\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "legacy-only basename")
    sha = git("rev-parse", "HEAD")
    check("only.lean:1", ("invalid", "citation: file 'only.lean' does not resolve uniquely; citation: positive verdict requires at least one resolvable file:line citation"))
    # The reviewed tree defines filenames, including dotfiles and arbitrary suffixes.
    for name in ["LICENSE", ".envrc", ".githooks/pre-push", "scripts/review/allowed_writer_log_signers",
                 "SHA256SUMS", "flake.nix", "flake.lock", "smoke.con", "sub/README.md",
                 "active/unit.lean", "legacy/unit.lean", "active/unique.lean", "legacy/unique.lean", "legacy/README.md", "other/unit.lean"]:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("first\nsecond\n")
    for name in ["REVIEW.diff", "DELTA.diff", "PROMPT.md", "RANGE.txt", "_review/export.txt"]:
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("exportOnlyName\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "tracked path authority")
    sha = git("rev-parse", "HEAD")
    for name in ["lean-toolchain", "LICENSE", ".envrc", ".githooks/pre-push",
                 "scripts/review/allowed_writer_log_signers", "SHA256SUMS", "flake.nix", "flake.lock", "smoke.con"]:
        check(f"{name}:1", "clean")
        check(f"[`{name}`]({name}#L1)", "clean")
        count = 1 if name == "lean-toolchain" else 2
        check(f"source.lean:1 {name}:999", ("invalid", f"citation: range '{name}:999' outside 1..{count}"))
    check("README.md:1", "clean")
    check("unique.lean:1", "clean")
    check("source.lean:1 UTC-5:30", "clean")
    check("[missing](unknown#L1)", ("invalid", "citation: file 'unknown' does not resolve uniquely; citation: positive verdict requires at least one resolvable file:line citation"))
    check("[README.md:2](sub/README.md#L2)", "clean")
    check("unit.lean:1", ("invalid", "citation: file 'unit.lean' does not resolve uniquely; citation: positive verdict requires at least one resolvable file:line citation"))
    for body in ["[unit.lean](active/unit.lean#L1) (`unit.lean:1`)",
                 "(`unit.lean:1`) [unit.lean](active/unit.lean#L1)"]:
        check(body, "clean")
    check("[unit.lean](active/unit.lean#L1) (`unit.lean:2`)",
          ("invalid", "citation: basename label 'unit.lean:2' disagrees with link target"))
    check("source.lean:1 `exportOnlyName`", ("invalid", "citation: identifier 'exportOnlyName' unresolved"))
    # Untracked helper text cannot authorize identifiers either.
    (root / "untracked.txt").write_text("untrackedOnlyName\n")
    check("source.lean:1 `untrackedOnlyName`", ("invalid", "citation: identifier 'untrackedOnlyName' unresolved"))
    export_base = sha
    for name in ["REVIEW.diff", "DELTA.diff", "PROMPT.md", "RANGE.txt", "_review/export.txt"]:
        (root / name).write_text("replacement\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "remove export identifiers")
    sha = git("rev-parse", "HEAD")
    result = contract.citation_error("source.lean:1 `exportOnlyName`", directory, sha,
                                     contract.commit_blob_paths(directory, sha), True, base=export_base)
    assert result == "citation: identifier 'exportOnlyName' unresolved", result
    # Explicit link targets can name an exact root file despite legacy copies.
    check("[`realAnchor`](source.lean#L1)", "clean")
    check("[source.lean](source.lean:1) file:///tmp/scripts/missing/README.md:1", ("invalid", "citation: link 'file:///tmp/scripts/missing/README.md' is outside the reviewed roots"))
    check("[README.md](file:///etc/arbitrary/nonexistent/dir/README.md:1)", ("invalid", "citation: link 'file:///etc/arbitrary/nonexistent/dir/README.md' is outside the reviewed roots; citation: positive verdict requires at least one resolvable file:line citation"))
    # Parent directories matching tracked top-level directories cannot mask the root.
    archive_root = "/tmp/.claude/worktrees/topic"
    result = contract.citation_error("file://" + archive_root + "/source.lean:1", directory, sha, contract.commit_blob_paths(directory, sha), True, review_root=archive_root)
    assert result is None, result
    (root / "namespaced.lean").write_text("namespace Ghost\ndef realAnchor := 1\nend Ghost\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "namespace declaration")
    sha = git("rev-parse", "HEAD")
    check("[`Ghost.realAnchor`](namespaced.lean#L2)", "clean")
    check("[`Ghost.realAnchor`](namespaced.lean#L1)", ("invalid", "citation: anchor 'Ghost.realAnchor' absent from 'namespaced.lean#L1'"))
    check("[`Missing.realAnchor`](namespaced.lean#L2)", ("invalid", "citation: anchor 'Missing.realAnchor' absent from 'namespaced.lean#L2'; citation: identifier 'Missing.realAnchor' unresolved"))
    (root / "lean-toolchain").write_text("different-pin\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "different toolchain pin")
    sha = git("rev-parse", "HEAD")
    check("[source.lean](source.lean:1) `declModifiers`", ("invalid", "citation: identifier 'declModifiers' unresolved"))
    base = sha
    (root / "source.lean").write_text("def realAnchor := 1\n")
    git("add", ".")
    git("-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "remove name")
    sha = git("rev-parse", "HEAD")
    result = contract.evaluate_output("[source.lean](source.lean:1) `otherAnchor`\nREAD: source.lean\nREPOSITORY: readable\nVERDICT: clean", "", sha, directory, ["clean"], base=base)
    assert result[0] == "clean", result
print("citation controls: PASS")

# The actual branch tree must never gain authority from its fabricated evidence.
repo = str(Path(__file__).resolve().parents[2])
result = contract.citation_error("[README.md](README.md:1) `alwaysEmitSteps` `invalidTwo` `mutateStoreGhostStep` `register_generation` `verify_saved_replay`", repo, "HEAD", contract.commit_blob_paths(repo, "HEAD"), True)
assert result == "citation: identifier 'alwaysEmitSteps' unresolved; citation: identifier 'invalidTwo' unresolved; citation: identifier 'mutateStoreGhostStep' unresolved; citation: identifier 'register_generation' unresolved; citation: identifier 'verify_saved_replay' unresolved", result
