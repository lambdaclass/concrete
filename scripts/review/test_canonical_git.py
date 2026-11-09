"""Slice A controls: real planted views and an exhaustive runner routing guard."""
import ast
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile

source = Path(__file__).with_name("writer_provenance.py")
spec = importlib.util.spec_from_file_location("writer_provenance", source)
wp = importlib.util.module_from_spec(spec)
spec.loader.exec_module(wp)

tree = ast.parse(source.read_text())
functions = {n.name: n for n in tree.body if isinstance(n, ast.FunctionDef)}
sites = ("git_run", "cmd_stamp_commit", "batch_cat_objects", "sync_notes_refs", "cmd_publish_notes")
for name in sites:
    calls = [n for n in ast.walk(functions[name]) if isinstance(n, ast.Call)]
    assert any(isinstance(n.func, ast.Name) and n.func.id == "canonical_git_run" for n in calls), name
# Only the canonical runner may spawn Git. Other subprocesses are SSH signing tools.
for name, fn in functions.items():
    for n in ast.walk(fn):
        if (isinstance(n, ast.Call) and isinstance(n.func, ast.Attribute)
                and isinstance(n.func.value, ast.Name)
                and n.func.value.id == "subprocess"):
            assert name == "canonical_git_run" or (
                n.args and isinstance(n.args[0], ast.List)
                and isinstance(n.args[0].elts[0], ast.Constant)
                and n.args[0].elts[0].value == "ssh-keygen"), name

def git(repo, *args):
    return subprocess.check_output(["git", "-C", str(repo), *args],
                                   env=wp.canonical_git_env(), stderr=subprocess.PIPE).decode().strip()

def refused(repo, expected):
    try:
        wp.git_run(str(repo), ["rev-parse", "HEAD"])
    except wp.ProvenanceError as exc:
        assert str(exc) == expected, str(exc)
    else:
        raise AssertionError("canonical view accepted")

with tempfile.TemporaryDirectory() as tmp:
    repo = Path(tmp) / "repo"
    repo.mkdir()
    git(repo, "init", "-q")
    git(repo, "-c", "user.name=Test", "-c", "user.email=test@example.invalid",
        "commit", "--allow-empty", "-qm", "root")
    sha = git(repo, "rev-parse", "HEAD")
    assert wp.git_run(str(repo), ["rev-parse", "HEAD"]).stdout.strip() == sha
    shallow = Path(tmp) / "shallow"
    git(repo, "clone", "-q", "--depth=1", repo.as_uri(), str(shallow))
    refused(shallow, "repository is shallow (git rev-parse --is-shallow-repository is true): "
            "writer-notes continuity and ancestry require full commit history")
    grafts = repo / ".git/info/grafts"
    grafts.write_text(sha + "\n")
    refused(repo, f"repository has grafted history ({grafts}): "
            "writer-notes continuity and ancestry forbid git grafts")
    grafts.unlink()
    git(repo, "update-ref", "refs/replace/" + sha, sha)
    refused(repo, f"repository has git replacement refs (refs/replace/{sha}): "
            "writer-notes continuity and ancestry forbid replacement objects")
    git(repo, "update-ref", "-d", "refs/replace/" + sha)
    git(repo, "config", "remote.origin.url", "repository-url")
    # Obtain the exact encoding Git exports for -c, including its shell quoting.
    exported = git(repo, "-c", "remote.origin.url=ambient-parameters",
                   "-c", "alias.canonical-env=!env", "canonical-env")
    parameters = next(line.partition("=")[2] for line in exported.splitlines()
                      if line.startswith("GIT_CONFIG_PARAMETERS="))
    controls = {
        "GIT_CONFIG_PARAMETERS": {"GIT_CONFIG_PARAMETERS": parameters},
        "GIT_CONFIG_COUNT/KEY_0/VALUE_0": {
            "GIT_CONFIG_COUNT": "1", "GIT_CONFIG_KEY_0": "remote.origin.url",
            "GIT_CONFIG_VALUE_0": "ambient-count"},
    }
    for label, planted in controls.items():
        old = os.environ.copy()
        try:
            os.environ.update(planted)
            actual = wp.git_run(str(repo), ["config", "--get", "remote.origin.url"]).stdout.strip()
            assert actual == "repository-url", (
                f"{label} leaked into canonical Git: expected repository value "
                f"'repository-url', got {actual!r}")
        finally:
            os.environ.clear()
            os.environ.update(old)
    # One capability per operation: later calls share it, but a new operation
    # must notice view mutations. Nothing is cached by repository path.
    operation = wp._VerifiedRepo(str(repo))
    assert wp.git_run(operation, ["rev-parse", "HEAD"]).stdout.strip() == sha
    grafts.write_text(sha + "\n")
    try:
        wp._VerifiedRepo(str(operation))
    except wp.ProvenanceError as exc:
        assert str(exc) == (f"repository has grafted history ({grafts}): "
                            "writer-notes continuity and ancestry forbid git grafts")
    else:
        raise AssertionError("new operation reused stale repository-view verification")
    grafts.unlink()
    assert wp.git_run(wp._VerifiedRepo(str(repo)), ["rev-parse", "HEAD"]).stdout.strip() == sha
    # Ambient redirections cannot change the selected repository or history.
    old = os.environ.copy()
    try:
        for key in wp.GIT_VIEW_ENV:
            os.environ[key] = "/nonexistent-canonical-control"
        os.environ["GIT_NO_REPLACE_OBJECTS"] = "0"
        os.environ["GIT_OPTIONAL_LOCKS"] = "1"
        assert wp.git_run(str(repo), ["rev-parse", "HEAD"]).stdout.strip() == sha
        assert wp.batch_cat_objects(str(repo), [sha])[sha][0] == "commit"
    finally:
        os.environ.clear()
        os.environ.update(old)

# The sandbox's deliberate isolation settings survive; all other config family
# members and every Git-owned local variable disappear (apart from forced flags).
local_vars = set(git(Path.cwd(), "rev-parse", "--local-env-vars").splitlines())
planted = {key: "ambient" for key in local_vars | {"GIT_CONFIG_KEY_99", "GIT_CONFIG_VALUE_99"}}
planted.update(GIT_CONFIG_GLOBAL="sandbox-config", GIT_CONFIG_NOSYSTEM="1")
clean = wp.canonical_git_env(planted)
assert clean == {"GIT_CONFIG_GLOBAL": "sandbox-config", "GIT_CONFIG_NOSYSTEM": "1",
                 "GIT_NO_REPLACE_OBJECTS": "1", "GIT_OPTIONAL_LOCKS": "0"}, clean

# Assert flags/environment at the actual spawn boundary, including probe commands.
real_run = wp.subprocess.run
seen = []
def inspect(argv, **kwargs):
    assert argv[:5] == ["git", "--no-replace-objects", "-c", "core.commitGraph=false", "-C"]
    assert all(k not in kwargs["env"] for k in wp.GIT_VIEW_ENV)
    assert kwargs["env"]["GIT_NO_REPLACE_OBJECTS"] == "1"
    assert kwargs["env"]["GIT_OPTIONAL_LOCKS"] == "0"
    seen.append(argv)
    return subprocess.CompletedProcess(argv, 0, "" if "for-each-ref" in argv else "/nonexistent/git\n/nonexistent/common\nfalse\n", "")
wp.subprocess.run = inspect
try:
    wp.git_run(".", ["status"])
    wp.canonical_git_run(".", ["cat-file", "--batch"], text=False, input=b"")
    assert len(seen) == 6
    seen.clear()
    operation = wp._VerifiedRepo(".")
    wp.git_run(operation, ["status"])
    wp.canonical_git_run(operation, ["cat-file", "--batch"], text=False, input=b"")
    assert len(seen) == 4, "operation must verify once, not once per Git call"
    # Commands mint fresh handles without changing the caller's namespace.
    original_validate = wp.validate_notes_ref
    def validate(**kwargs):
        assert isinstance(kwargs["repo"], wp._VerifiedRepo)
        wp.git_run(kwargs["repo"], ["status"])
        wp.git_run(kwargs["repo"], ["status"])
        return 0
    wp.validate_notes_ref = validate
    try:
        args = wp.argparse.Namespace(repo=".", notes_rev=None, old_notes_rev=None,
                                     allowed_signers=None)
        seen.clear()
        assert wp.cmd_validate_notes(args) == 0
        assert wp.cmd_validate_notes(args) == 0
        assert len(seen) == 8, "each command must verify afresh exactly once"
        assert type(args.repo) is str, "verified handles must remain operation-local"
    finally:
        wp.validate_notes_ref = original_validate
finally:
    wp.subprocess.run = real_run
print("canonical Git runner controls passed")
