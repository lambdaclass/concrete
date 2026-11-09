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
from unittest.mock import patch

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


def rejects(call, expected, exact=False):
    try:
        call()
    except ValueError as e:
        assert (str(e) == expected if exact else str(e).startswith(expected)), f"expected prefix {expected!r}, got {str(e)!r}"
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
        rejects(lambda: approval.verify_candidate(report, record), "owner approval record missing")

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
        approval.verify_candidate(report, record)

        # Reviewer bypass: writer grows report AND regenerates JSON with old citations.
        report.write_text(base_report + "component growth [fixture]\n")
        record.write_text(json.dumps(make_record()))
        rejects(lambda: approval.verify(report, record),
                "owner authorization is not established (T1): authenticator and initial digest await owner decision", exact=True)
        report.write_text(base_report)
        record.write_text(json.dumps(make_record()))
        rejects(lambda: approval.verify(report, record),
                "owner authorization is not established (T1): authenticator and initial digest await owner decision", exact=True)

        # Tampered decision quote or missing anchor in docs/decisions.md must be refused.
        decisions.write_text(decisions_text.replace("ok let's do 1, 2, 3, 4, 5", "forged quote"))
        rejects(lambda: approval.verify_candidate(report, record),
                "owner decision record in docs/decisions.md does not match docs/decisions.md#decided-2026-11-07-early-evidence")
        decisions.write_text("# Owner Decisions\n")
        rejects(lambda: approval.verify_candidate(report, record),
                "owner decision anchor missing in docs/decisions.md: docs/decisions.md#decided-2026-11-07-early-evidence")
        decisions.write_text(decisions_text)
        approval.verify_candidate(report, record)

        # Unapproved component/premise growth must be refused.
        report.write_text(base_report + "component growth [fixture]\n")
        rejects(lambda: approval.verify_candidate(report, record), "registry differs from candidate digest")
        report.write_text(base_report)

        # Unapproved theorem/axiom closure change must be refused.
        report.write_text(base_report + "axiom extra : Prop\n")
        rejects(lambda: approval.verify_candidate(report, record), "theorem axiom closure differs from candidate digest")
        report.write_text(base_report + "unrecognised continuation or injected line\n")
        rejects(lambda: approval.verify_candidate(report, record), "line 5: unrecognised trusted-base report row", exact=True)
        report.write_text(base_report)
        decisions.write_text(decisions_text + "\n" + decisions_text)
        rejects(lambda: approval.verify_candidate(report, record),
                "owner decision anchor missing in docs/decisions.md: docs/decisions.md#decided-2026-11-07-early-evidence", exact=True)
        decisions.write_text(decisions_text)

        # Toolchain and dependency pin mutations must each be refused.
        for pinned in ("flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json"):
            orig = Path(pinned).read_text()
            Path(pinned).write_text("changed\n")
            rejects(lambda: approval.verify_candidate(report, record), f"trust input {pinned} differs from candidate digest")
            Path(pinned).write_text(orig)
        approval.verify_candidate(report, record)

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
        allowed.write_text("clang\nbash\nlcb\nld\n")
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
            "role_closures": {"native": {
                "clang": ["/nix/store/00000000000000000000000000000000-clang"],
                "bash": [bash_root],
            }},
            "role_compilers": {"native": ["/nix/store/00000000000000000000000000000000-clang/bin/clang"]},
            "lcb": str(lcb_bin.resolve()),
            "lcb_sha256": hashlib.sha256(lcb_bin.read_bytes()).hexdigest(),
        }))

        # 1. Wrong-path copy of bash named clang (including real strace with adversarial arguments).
        fake = tmp / "clang"
        shutil.copyfile(real_bash, fake)
        os.chmod(fake, 0o755)
        log = tmp / "execve"
        log.write_text(f'1 execve("{fake}", ["clang"], 0x0) = 0\n')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = identity.check(log, allowed, evidence, lcb_bin, "native")
        assert rc == 1 and out.getvalue().splitlines() == [f"FAIL tools: identity-mismatch role 'native' program 'clang': {fake.resolve()}"], out.getvalue()
        if shutil.which("strace"):
            real_trace = tmp / "real-fake-clang.execve"
            long_arg = ") = -1 <unfinished ...> <... execve resumed>) = -1 " + ("A" * 5000)
            subprocess.run([
                "strace", "-f", "-qq", "-s", "4096", "-e", "trace=execve,execveat",
                "-o", str(real_trace), str(fake), "--version",
                ") = -1 ", "<unfinished ...>", "<... execve resumed>) = -1 ", 'a"b\\"c\\\\") = -1 ', long_arg,
            ], check=True, capture_output=True, text=True, timeout=30)
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                rc = identity.check(real_trace, allowed, evidence, lcb_bin, "native")
            assert rc == 1 and out.getvalue().splitlines() == [
                f"FAIL tools: identity-mismatch role 'native' program 'clang': {fake.resolve()}"
            ], out.getvalue()

        # 2. Symlink named clang pointing to a store binary (bash) in the closure.
        sym_store = tmp / "sym-store"
        sym_store.mkdir()
        clang_to_bash = sym_store / "clang"
        clang_to_bash.symlink_to(real_bash)
        log.write_text(f'1 execve("{clang_to_bash}", ["clang"], 0x0) = 0\n')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = identity.check(log, allowed, evidence, lcb_bin, "native")
        assert rc == 1 and out.getvalue().splitlines() == [f"FAIL tools: identity-mismatch role 'native' program 'clang': {real_bash}"], out.getvalue()

        # 3. Symlink named clang pointing to the local lcb binary.
        sym_lcb = tmp / "sym-lcb"
        sym_lcb.mkdir()
        clang_to_lcb = sym_lcb / "clang"
        clang_to_lcb.symlink_to(lcb_bin)
        log.write_text(f'1 execve("{clang_to_lcb}", ["clang"], 0x0) = 0\n')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = identity.check(log, allowed, evidence, lcb_bin, "native")
        assert rc == 1 and out.getvalue().splitlines() == [f"FAIL tools: identity-mismatch role 'native' program 'clang': {lcb_bin.resolve()}"], out.getvalue()
        # Synthetic immutable-store paths: no writes to /nix/store. Only realpath
        # is modeled, so the invoked and resolved roots independently exercise
        # the production per-component policy (path_root alone would accept).
        native_root = "/nix/store/00000000000000000000000000000000-native"
        lean_root = "/nix/store/11111111111111111111111111111111-lean"
        other_root = "/nix/store/22222222222222222222222222222222-other-clang"
        native = native_root + "/bin/clang"
        data = {
            "inputs": {},
            "closure": {r: {"narHash": "sha256-fixture"}
                        for r in (native_root, lean_root, other_root)},
            "role_closures": {
                "native": {"clang": [native_root], "bash": [native_root], "ld": [native_root]},
                "lean-build": {"clang": [lean_root, other_root]},
            },
            "role_linkers": {"native": {"ld": [native_root + "/bin/ld"]}},
            "role_compilers": {"native": [native],
                               "lean-build": [r + "/bin/clang" for r in (lean_root, other_root)]},
        }
        evidence.write_text(json.dumps(data))

        def check_trace(path, role, resolved=None):
            log.write_text(f'1 execve("{path}", ["tool"], 0x0) = 0\n')
            out = io.StringIO()
            original = os.path.realpath
            with patch.object(identity.os.path, "realpath",
                              side_effect=lambda p: resolved if str(p) == path and resolved else original(p)):
                with contextlib.redirect_stdout(out):
                    rc = identity.check(log, allowed, evidence, lcb_bin, role)
            return rc, out.getvalue().splitlines()

        assert check_trace(native, "native")[0] == 0
        for root in (lean_root, other_root):
            substitution = root + "/bin/clang"
            rc, lines = check_trace(substitution, "native")
            assert rc == 1 and lines == [
                f"FAIL tools: identity-mismatch role 'native' program 'clang': {substitution}"], lines
            assert check_trace(substitution, "lean-build")[0] == 0
        assert check_trace(native_root + "/bin/ld", "native")[0] == 0
        rc, lines = check_trace(lean_root + "/bin/ld", "native")
        assert rc == 1 and lines == [
            f"FAIL tools: identity-mismatch role 'native' program 'ld': {lean_root}/bin/ld"], lines
        # Same component closure, different linker identity must also fail.
        data["role_closures"]["native"]["ld"].append(other_root)
        evidence.write_text(json.dumps(data))
        rc, lines = check_trace(other_root + "/bin/ld", "native")
        assert rc == 1 and lines == [
            f"FAIL tools: identity-mismatch role 'native' program 'ld': {other_root}/bin/ld"], lines
        # Both roots belong to the union closure and the invoked root belongs to
        # bash's component. Its resolved root alone violates that component.
        invoked = native_root + "/bin/bash"
        resolved = other_root + "/bin/bash"
        rc, lines = check_trace(invoked, "native", resolved)
        assert rc == 1 and lines == [
            f"FAIL tools: identity-mismatch role 'native' program 'bash': {resolved}"], lines
        # A '..' traversal out of an allowed store prefix must not spoof path_root.
        traversed = native_root + "/../22222222222222222222222222222222-other-clang/bin/bash"
        rc, lines = check_trace(traversed, "native", invoked)
        assert rc == 1 and lines == [
            f"FAIL tools: identity-mismatch role 'native' program 'bash': {invoked}"], lines

        # Live flake.nix trustedIdentity role-boundary controls.
        os.chdir(ROOT)
        real_ev = identity.store_evidence()
        real_ev["inputs"] = {}
        real_ev["lcb"] = str(lcb_bin.resolve())
        real_ev["lcb_sha256"] = hashlib.sha256(lcb_bin.read_bytes()).hexdigest()
        evidence.write_text(json.dumps(real_ev))
        allowed.write_text("clang\nld.lld\nlean\nleanc\nlake\ngit\nlcb\nbash\n")
        real_lean = real_ev["lean"]
        real_git = os.path.realpath(shutil.which("git"))
        for bad_path, bad_role, prog in [
            (real_lean + "/bin/clang", "native", "clang"),
            (real_lean + "/bin/ld.lld", "native", "ld.lld"),
            (real_lean + "/bin/lean", "native", "lean"),
            (real_lean + "/bin/leanc", "native", "leanc"),
            (real_lean + "/bin/lake", "native", "lake"),
            (real_git, "native", "git"),
            (real_lean + "/bin/leanc", "lean-emit", "leanc"),
            (real_lean + "/bin/lake", "lean-emit", "lake"),
            (real_git, "lean-emit", "git"),
            (str(lcb_bin), "lean-emit", "lcb"),
            (str(lcb_bin), "lean-build", "lcb"),
        ]:
            rc, lines = check_trace(bad_path, bad_role)
            expected_real = os.path.realpath(bad_path)
            assert rc == 1 and lines == [
                f"FAIL tools: identity-mismatch role '{bad_role}' program '{prog}': {expected_real}"], (bad_path, bad_role, lines)
        assert check_trace(real_lean + "/bin/lean", "lean-emit")[0] == 0
        assert check_trace(str(lcb_bin), "native")[0] == 0
    finally:
        os.chdir(old)
print("PASS candidate consistency, pending owner authentication, writer-regeneration bypass, pins and role identity controls")
