"""Exercise publication with actual or simulated case-insensitive lookups."""

from contextlib import nullcontext
import os
import errno
from pathlib import Path
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))
import slice0_publish as publisher


def check_publication(root, *, mixed=False, ambiguous=False):
    root.mkdir()
    output = root / "out"
    output.mkdir()
    chattr = shutil.which("chattr")
    if mixed and sys.platform == "linux" and chattr:
        # ext4 can enable casefold on this empty directory alone.
        subprocess.run([chattr, "+F", output], capture_output=True, timeout=5)

    old = output / "LLVM.O"
    old.write_text("old")
    if ambiguous:
        os.link(old, output / "keep")
    (output / "lcb.exe").write_text("lcb")
    os.link(output / "lcb.exe", output / "keep-lcb")
    names = ["llvm.o", "lcb.exe"]
    case_insensitive = (output / "llvm.o").exists()
    staging_probe = root / "Stage"
    staging_probe.touch()
    staging_insensitive = (root / "stage").exists()
    real = case_insensitive and (not mixed or not staging_insensitive)
    original_stat = os.stat
    output_identity = output.stat()

    def resolve_artifact(path, *, dir_fd=None, follow_symlinks=True):
        if (dir_fd is not None and path == "llvm.o"
                and os.path.samestat(os.fstat(dir_fd), output_identity)):
            path = "LLVM.O"
        if dir_fd is None and Path(path).parent.name == "candidate":
            raise AssertionError("replacement lookup used the candidate directory")
        return original_stat(path, dir_fd=dir_fd, follow_symlinks=follow_symlinks)

    lookup = nullcontext() if real else patch.object(os, "stat", resolve_artifact)
    with lookup:
        staging = Path(publisher.prepare(output, names))
        for name in names:
            (staging / name).write_text("new")
        if ambiguous:
            try:
                publisher.publish(output, staging, names)
            except ValueError as error:
                assert str(error) == "cannot identify the build output entry: llvm.o"
            else:
                raise AssertionError("accepted an ambiguous artifact alias")
        else:
            publisher.publish(output, staging, names)

    if ambiguous:
        assert set(os.listdir(output)) == {"LLVM.O", "keep", "lcb.exe", "keep-lcb"}
        assert (output / "LLVM.O").read_text() == (output / "keep").read_text() == "old"
        assert (output / "lcb.exe").read_text() == "lcb"
        assert (staging / "llvm.o").read_text() == "new"
    else:
        assert set(os.listdir(output)) == {"llvm.o", "lcb.exe", "keep-lcb"}
        assert (output / "llvm.o").read_text() == (output / "lcb.exe").read_text() == "new"
    assert (output / "keep-lcb").read_text() == "lcb"
    return real


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


def check_staging_collision(root, *, nested):
    output = root / "out"
    output.mkdir(parents=True)
    source = output / "nested" if nested else output
    source.mkdir(exist_ok=True)
    (source / "Keep").mkdir()
    (source / "Keep" / "first").write_text("first")
    (source / "keep").mkdir()
    (source / "keep" / "second").write_text("second")
    (output / "llvm.o").write_text("old")
    names = ["llvm.o", "lcb.exe"]
    staging = Path(publisher.prepare(output, names))
    for name in names:
        (staging / name).write_text("new")
    candidate = staging / "candidate"
    original_mkdir = os.mkdir

    def casefold_mkdir(path, mode=0o777, *, dir_fd=None):
        path = Path(path)
        if candidate in path.parents and any(
            entry.casefold() == path.name.casefold() for entry in os.listdir(path.parent)
        ):
            raise FileExistsError(errno.EEXIST, os.strerror(errno.EEXIST), path)
        return original_mkdir(path, mode, dir_fd=dir_fd)

    try:
        with patch.object(os, "mkdir", casefold_mkdir), \
                patch.object(Path, "rename", side_effect=AssertionError("moved an artifact")), \
                patch.object(publisher, "exchange", side_effect=AssertionError("exchanged output")):
            publisher.publish(output, staging, names)
    except FileExistsError as error:
        target_parent = candidate / "nested" if nested else candidate
        assert Path(error.filename).parent == target_parent
        assert Path(error.filename).name.casefold() == "keep"
    else:
        raise AssertionError("accepted colliding staging directory names")

    assert set(os.listdir(source)) == ({"Keep", "keep"} if nested else {"Keep", "keep", "llvm.o"})
    assert set(os.listdir(source / "Keep")) == {"first"}
    assert set(os.listdir(source / "keep")) == {"second"}
    assert (source / "Keep" / "first").read_text() == "first"
    assert (source / "keep" / "second").read_text() == "second"
    assert (output / "llvm.o").read_text() == "old"
    assert not (output / "lcb.exe").exists()
    for name in names:
        assert (staging / name).read_text() == "new"


if __name__ == "__main__":
    original_cwd = Path.cwd()
    with TemporaryDirectory() as directory:
        root = Path(directory).resolve()
        try:
            real = check_publication(root / "ordinary")
            check_project_guard(root, real)
            mixed = check_publication(root / "mixed", mixed=True)
            ambiguous = check_publication(root / "ambiguous", mixed=True, ambiguous=True)
            if sys.platform == "linux":
                check_staging_collision(root / "collision-root", nested=False)
                check_staging_collision(root / "collision-nested", nested=True)
        finally:
            os.chdir(original_cwd)
    mode = "real" if real else "simulated"
    mixed_mode = "real" if mixed else "simulated"
    ambiguous_mode = "real" if ambiguous else "simulated"
    print(f"ok   publication and project-directory aliases ({sys.platform}, {mode})")
    print(f"ok   mixed directory lookup rules ({mixed_mode}), ambiguous alias refusal ({ambiguous_mode})")
    if sys.platform == "linux":
        print("ok   casefold staging refuses top-level and nested directory collisions before moves")
