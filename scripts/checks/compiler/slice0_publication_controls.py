"""Exercise publication with actual or simulated case-insensitive lookups."""

from contextlib import nullcontext
import os
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))
import slice0_publish as publisher


def check_publication(root):
    output = root / "out"
    output.mkdir()
    old = output / "LLVM.O"
    old.write_text("old")
    os.link(old, output / "keep")
    (output / "lcb.exe").write_text("lcb")
    case_insensitive = (output / "llvm.o").exists()
    original_stat = os.stat

    def resolve_artifact(path, *, dir_fd=None, follow_symlinks=True):
        # Supply the lookup a case-insensitive volume would return. Listing,
        # staging, hard-linking and exchange still use the real filesystem.
        if dir_fd is not None and path == "llvm.o":
            path = "LLVM.O"
        path = Path(path)
        if path.name == "LLVM.O" and path.parent.name == "candidate":
            path = path.with_name("llvm.o")
        return original_stat(path, dir_fd=dir_fd, follow_symlinks=follow_symlinks)

    lookup = nullcontext() if case_insensitive else patch.object(os, "stat", resolve_artifact)
    with lookup:
        staging = Path(publisher.prepare(output, ["llvm.o"]))
        (staging / "llvm.o").write_text("new")
        publisher.publish(output, staging, ["llvm.o"])
    assert set(os.listdir(output)) == {"llvm.o", "keep", "lcb.exe"}
    assert (output / "llvm.o").read_text() == "new"
    assert (output / "keep").read_text() == "old"
    assert (output / "lcb.exe").read_text() == "lcb"
    return case_insensitive


def check_project_guard(root, case_insensitive):
    project = root / "Project"
    child = project / "Child"
    child.mkdir(parents=True)
    alias = root / "PROJECT"
    for cwd in (project, child):
        os.chdir(cwd)
        original_open = os.open

        def resolve_directory(path, flags):
            return original_open(project if Path(path) == alias else path, flags)

        lookup = nullcontext() if case_insensitive else patch.object(os, "open", resolve_directory)
        try:
            with lookup:
                publisher.main(["publish", str(alias), str(root / "unused"), "llvm.o"])
            raise AssertionError(f"accepted the current project or its ancestor: {alias}")
        except ValueError as error:
            assert str(error).startswith("build output directory must not contain the current project:")
        finally:
            os.chdir(root)


if __name__ == "__main__":
    original_cwd = Path.cwd()
    with TemporaryDirectory() as directory:
        root = Path(directory).resolve()
        try:
            real = check_publication(root)
            check_project_guard(root, real)
        finally:
            os.chdir(original_cwd)
    mode = "real case-insensitive filesystem" if real else "simulated case-insensitive lookups"
    print(f"ok   publication and project-directory aliases ({sys.platform}, {mode})")
