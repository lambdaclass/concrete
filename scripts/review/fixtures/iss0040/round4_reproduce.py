#!/usr/bin/env python3
"""Read-only baseline reproduction: timeout 60 python3 scripts/review/fixtures/iss0040/round4_reproduce.py"""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import shlex
import os
ROOT = Path(__file__).resolve().parents[4]
BASE = "5980b90a2"
def baseline(path):
    return subprocess.check_output(["git", "-C", str(ROOT), "show", BASE + ":" + path], text=True, timeout=10)
with tempfile.TemporaryDirectory() as directory:
    spec = importlib.util.spec_from_file_location("sandbox", ROOT / "scripts/checks/lib/git_sandbox.py")
    sandbox = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(sandbox)
    config = Path(directory) / "sandbox.config"
    config.write_text("[core]\n hooksPath = /dev/null\n[commit]\n gpgSign = false\n")
    env = sandbox.git_sandbox(config)
    os.environ.clear()
    os.environ.update(env)
    old = Path(directory) / "contract.py"
    old.write_text(baseline("scripts/review/output_contract.py"))
    spec = importlib.util.spec_from_file_location("baseline", old)
    c = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(c)
    def probe(number, repo, sha, body, expected):
        actual = c.citation_error(body, str(repo), sha, c.commit_blob_paths(str(repo), sha), True)
        print(f"{number}: CONFIRMED\n input: {body!r}\n observed: {actual!r}\n expected: {expected}")
        return actual
    assert probe(2, ROOT, BASE, "README.md:1 `alwaysEmitSteps` `invalidTwo` `mutateStoreGhostStep`", "three unresolved-identifier diagnostics") is None
    framed = "README.md:1 `alwaysEmitSteps` `invalidTwo` `mutateStoreGhostStep`\nREAD: README.md\nREPOSITORY: readable\nVERDICT: clean"
    accepted = c.evaluate_output(framed, "", BASE, str(ROOT), ["clean"])
    assert accepted[0] == "clean", accepted
    print("2 framed CLEAN observed:", accepted, "; expected: invalid")
    repo = Path(directory) / ".claude/worktrees/topic"
    repo.mkdir(parents=True)
    for name, content in {
        "README.md": "root\nsecond\n", "legacy/README.md": "legacy\n",
        "legacy/only.lean": "legacyOnly\n", ".claude/marker.txt": "marker\n",
        "scripts/a.py": "anchor\n", "source.lean": "namespace Ghost\ndef realAnchor := 1\nend Ghost\n",
        "a/unit.lean": "first\nsecond\n", "b/unit.lean": "first\nsecond\n",
        "source.123": "numeric extension\n",
    }.items():
        p = repo / name
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(content)
    def git(*args):
        return subprocess.check_output(["git", "-C", str(repo), *args], stderr=subprocess.PIPE, timeout=10)
    git("init", "-q")
    git("add", ".")
    git("-c", "core.hooksPath=/dev/null", "-c", "commit.gpgSign=false", "-c", "user.name=Control", "-c", "user.email=control@example.invalid", "commit", "-qm", "baseline reproduction")
    assert probe("3a", repo, "HEAD", "only.lean:1", "unresolved basename; implicit legacy selection forbidden") is None
    assert probe("3b", repo, "HEAD", "[README.md:1-2](legacy/README.md#L1)", "label range outside legacy file 1..1") is None
    assert "does not resolve uniquely" in probe("3c", repo, "HEAD", "[unit.lean:1–2,2](a/unit.lean#L1)", "valid label, normalized start line 1")
    assert "does not resolve uniquely" in probe("4a", repo, "HEAD", "file://" + str(repo) + "/scripts/a.py:1", "valid exact checkout path")
    assert probe("4b", repo, "HEAD", "file:///etc/arbitrary/nonexistent/dir/README.md:1", "unresolved absolute URL") is None
    assert "anchor 'Ghost.realAnchor' absent" in probe(5, repo, "HEAD", "[`Ghost.realAnchor`](source.lean#L2)", "valid namespaced declaration")
    signoff = baseline("scripts/land/signoff.sh")
    a = signoff.index('  if [ "$c_status" = invalid ]; then', signoff.index("cmd_collect()"))
    b = signoff.index('  echo "signoff: collected', a)
    out = Path(directory) / "rejected.out"
    out.write_text("rejected evidence")
    code = "out=" + shlex.quote(str(out)) + '; c_status=invalid; c_reason="planted invalid"; rc=0\nprobe() {\n' + signoff[a:b] + "\n}\nprobe"
    result = subprocess.run(["bash", "-c", code], capture_output=True, text=True, timeout=10)
    assert result.returncode == 1 and result.stderr == "signoff: collect invalid reviewer evidence (planted invalid)\n" and out.exists()
    print("6: CONFIRMED\n input: production cmd_collect rejection tail; invalid status, existing --out\n observed: rc=1; exact invalid-evidence diagnostic; --out still exists\n expected: rc=1; same diagnostic; --out absent")
    assert probe("audit", repo, "HEAD", "[numeric](source.123:1junk)", "unsupported location grammar") is None
