"""Light owner-approval, pin, package and executable-identity adversarial controls."""
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
    if a[0] == "git":
        a = ("git", "-c", "core.hooksPath=/dev/null", *a[1:])
    return subprocess.run(a, env=env, check=True, capture_output=True, text=True, timeout=15).stdout.strip()


def rejects(call, expected):
    try:
        call()
    except ValueError as e:
        assert str(e).startswith(expected), f"expected prefix {expected!r}, got {str(e)!r}"
    else:
        raise AssertionError("accepted " + expected)


with tempfile.TemporaryDirectory() as tmp:
    tmp = Path(tmp)
    old = Path.cwd()
    try:
        os.chdir(tmp)
        (tmp / "docs").mkdir()
        decisions = tmp / "docs/decisions.md"
        decisions_text = (
            '# Owner Decisions\n\n'
            '- <a id="decided-2026-11-07-early-evidence"></a>**Five cheap early items** '
            '(owner, 2026-11-07: "ok let\'s do 1, 2, 3, 4, 5", approving the list):\n'
            '  - *Trusted-base statement.*\n'
        )
        decisions.write_text(decisions_text)
        for p in ("flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json"):
            Path(p).write_text(f"fixture {p}\n")
        report = tmp / "report"
        base_report = (
            "component test [fixture]\n"
            "premise P [fixture]\n"
            "theorem T axioms: propext\n"
            "axiom propext : Prop\n"
        )
        report.write_text(base_report)
        record = tmp / "approval.json"
        rejects(lambda: approval.verify(report, record), "owner approval record missing")

        def make_record():
            return {
                "schema": 1,
                "decisions": [{
                    "anchor": "docs/decisions.md#decided-2026-11-07-early-evidence",
                    "date": "2026-11-07",
                    "quote": "ok let's do 1, 2, 3, 4, 5",
                    "scope": "Fixture trusted-base statement.",
                }],
                "registry_sha256": hashlib.sha256(approval.registry_rows(report.read_text())).hexdigest(),
                "theorems_sha256": hashlib.sha256(approval.theorem_rows(report.read_text())).hexdigest(),
                "inputs": approval.input_digests(),
            }

        record.write_text(json.dumps(make_record()))
        approval.verify(report, record)

        # Tampered decision quote or missing anchor in docs/decisions.md must be refused.
        decisions.write_text(decisions_text.replace("ok let's do 1, 2, 3, 4, 5", "forged quote"))
        rejects(lambda: approval.verify(report, record),
                "owner decision record in docs/decisions.md does not match docs/decisions.md#decided-2026-11-07-early-evidence")
        decisions.write_text("# Owner Decisions\n")
        rejects(lambda: approval.verify(report, record),
                "owner decision anchor missing in docs/decisions.md: docs/decisions.md#decided-2026-11-07-early-evidence")
        decisions.write_text(decisions_text)
        approval.verify(report, record)

        # Unapproved component/premise growth must be refused.
        report.write_text(base_report + "component growth [fixture]\n")
        rejects(lambda: approval.verify(report, record), "registry differs from owner-approved digest")
        report.write_text(base_report)

        # Unapproved theorem/axiom closure change must be refused.
        report.write_text(base_report + "axiom extra : Prop\n")
        rejects(lambda: approval.verify(report, record), "theorem axiom closure differs from owner-approved digest")
        report.write_text(base_report)

        # Toolchain and dependency pin mutations must each be refused.
        for pinned in ("flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json"):
            orig = Path(pinned).read_text()
            Path(pinned).write_text("changed\n")
            rejects(lambda: approval.verify(report, record), f"trust input {pinned} differs from owner-approved digest")
            Path(pinned).write_text(orig)
        approval.verify(report, record)

        # Lake package checkout integrity controls.
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

        # Tool identity controls: wrong-path binary, symlink-to-store-binary, and symlink-to-lcb.
        allowed = tmp / "allowed"
        allowed.write_text("clang\nbash\nlcb\n")
        lcb_bin = tmp / "lcb"
        shutil.copyfile(shutil.which("true"), lcb_bin)
        real_bash = os.path.realpath(shutil.which("bash"))
        bash_root = "/".join(real_bash.split("/")[:4])
        evidence = tmp / "identity.json"
        evidence.write_text(json.dumps({
            "inputs": {},
            "closure": {bash_root: {"narHash": "sha256-fixture"}},
            "tool_closures": {
                "clang": ["/nix/store/00000000000000000000000000000000-clang"],
                "bash": [bash_root],
            },
            "lcb": str(lcb_bin.resolve()),
            "lcb_sha256": hashlib.sha256(lcb_bin.read_bytes()).hexdigest(),
        }))

        # 1. Wrong-path copy of true named clang.
        fake = tmp / "clang"
        shutil.copyfile(shutil.which("true"), fake)
        log = tmp / "execve"
        log.write_text(f'1 execve("{fake}", ["clang"], 0x0) = 0\n')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = identity.check(log, allowed, evidence, lcb_bin)
        assert rc == 1 and out.getvalue().splitlines() == [f"FAIL tools: unexpected executable 'clang': {fake.resolve()}"], out.getvalue()

        # 2. Symlink named clang pointing to a store binary (bash) in the closure.
        sym_store = tmp / "sym-store"
        sym_store.mkdir()
        clang_to_bash = sym_store / "clang"
        clang_to_bash.symlink_to(real_bash)
        log.write_text(f'1 execve("{clang_to_bash}", ["clang"], 0x0) = 0\n')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = identity.check(log, allowed, evidence, lcb_bin)
        assert rc == 1 and out.getvalue().splitlines() == [f"FAIL tools: unexpected executable 'clang': {real_bash}"], out.getvalue()

        # 3. Symlink named clang pointing to the local lcb binary.
        sym_lcb = tmp / "sym-lcb"
        sym_lcb.mkdir()
        clang_to_lcb = sym_lcb / "clang"
        clang_to_lcb.symlink_to(lcb_bin)
        log.write_text(f'1 execve("{clang_to_lcb}", ["clang"], 0x0) = 0\n')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = identity.check(log, allowed, evidence, lcb_bin)
        assert rc == 1 and out.getvalue().splitlines() == [f"FAIL tools: unexpected executable 'clang': {lcb_bin.resolve()}"], out.getvalue()
    finally:
        os.chdir(old)
print("PASS owner decision approval, trust growth, pin mutation and tool-identity spoofing controls")
