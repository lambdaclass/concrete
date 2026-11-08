"""Light signature, revision and executable-identity adversarial controls."""
import contextlib
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]
def module(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / "scripts/checks/lib" / (name + ".py"))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m
approval = module("trusted_approval")
identity = module("trusted_identity")
def run(*a):
    # A pre-push hook may export GIT_DIR/INDEX_FILE for the real checkout.
    # Synthetic repositories must never inherit that relocation or global hooks.
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    if a[0] == "git": a = ("git", "-c", "core.hooksPath=/dev/null", *a[1:])
    return subprocess.run(a, env=env, check=True, capture_output=True, text=True, timeout=15).stdout.strip()
def rejects(call, expected):
    try: call()
    except ValueError as e:
        assert str(e).startswith(expected), str(e)
    else: raise AssertionError("accepted " + expected)

with tempfile.TemporaryDirectory() as tmp:
    tmp = Path(tmp)
    old = Path.cwd()
    try:
        os.chdir(tmp)
        (tmp / "Concrete").mkdir()
        for p in ("Concrete/Premises.lean", "flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json"):
            Path(p).write_text("fixture\n")
        run("git", "init", "-q")
        run("git", "-c", "user.name=fixture", "-c", "user.email=fixture@example.org", "add", ".")
        run("git", "-c", "user.name=fixture", "-c", "user.email=fixture@example.org", "-c", "commit.gpgsign=false", "commit", "-qm", "approved fixture")
        revision = run("git", "rev-parse", "HEAD")
        report = tmp / "report"
        report.write_text("component test [fixture]\npremise P [fixture]\n")
        record = tmp / "approval.json"
        record.write_text(json.dumps({"schema": 1, "registry_revision": revision,
            "registry_sha256": hashlib.sha256(approval.registry_rows(report.read_text())).hexdigest(),
            "approval": "fixture owner approval"}))
        run("ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(tmp / "key"))
        signers = tmp / "signers"
        signers.write_text("owner " + (tmp / "key.pub").read_text())
        rejects(lambda: approval.verify(report, record, str(signers)), "invalid owner approval signature")
        run("ssh-keygen", "-Y", "sign", "-f", str(tmp / "key"), "-n", "concrete-trusted-base", str(record))
        approval.verify(report, record, str(signers))
        report.write_text(report.read_text() + "component growth [fixture]\n")
        rejects(lambda: approval.verify(report, record, str(signers)), "registry differs from owner-approved revision")
        Path("flake.lock").write_text("changed\n")
        rejects(lambda: approval.verify(report, record, str(signers)), "trust input flake.lock differs")
        pkg = tmp / ".lake/packages/control"
        pkg.mkdir(parents=True)
        run("git", "init", "-q", str(pkg))
        (pkg / "source").write_text("source")
        run("git", "-C", str(pkg), "add", ".")
        run("git", "-C", str(pkg), "-c", "user.name=fixture", "-c", "user.email=fixture@example.org",
            "-c", "commit.gpgsign=false", "commit", "-qm", "pinned")
        pin = run("git", "-C", str(pkg), "rev-parse", "HEAD")
        Path("lake-manifest.json").write_text(json.dumps({"packages": [{"name": "control", "rev": pin}]}))
        identity.verify_packages()
        (pkg / "source").write_text("modified")
        rejects(identity.verify_packages, "dependency control is not clean at pinned revision")
        (pkg / "source").write_text("source")
        Path("lake-manifest.json").write_text(json.dumps({"packages": [{"name": "control", "rev": "0" * 40}]}))
        rejects(identity.verify_packages, "dependency control is not clean at pinned revision")
        allowed = tmp / "allowed"
        allowed.write_text("clang\n")
        evidence = tmp / "identity.json"
        evidence.write_text(json.dumps({"inputs": {}, "closure": {}}))
        fake = tmp / "clang"
        shutil.copyfile(shutil.which("true"), fake)
        log = tmp / "execve"
        log.write_text(f'1 execve("{fake}", ["clang"], 0x0) = 0\n')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = identity.check(log, allowed, evidence, tmp / "lcb")
        assert rc == 1 and out.getvalue().splitlines() == [f"FAIL tools: unexpected executable 'clang': {fake}"], out.getvalue()
    finally: os.chdir(old)
print("PASS signed approval, trust growth, pin mutation and wrong-path clang controls")
