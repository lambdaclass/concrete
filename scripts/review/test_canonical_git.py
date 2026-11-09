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
finally:
    wp.subprocess.run = real_run
print("canonical Git runner controls passed")
