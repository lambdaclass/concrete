"""Exercise publication with actual or simulated case-insensitive lookups."""

from contextlib import ExitStack, nullcontext
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


def lookup_probe():
    return "linux_lookup_rules" if sys.platform == "linux" else "macos_lookup_rules"


def check_publication(root, *, insensitive, ambiguous=False):
    root.mkdir()
    if insensitive and sys.platform == "linux" and shutil.which("chattr"):
        # If the temp filesystem supports casefold, exercise the real
        # inheritance rule by enabling it before creating any children.
        subprocess.run(["chattr", "+F", root], capture_output=True, timeout=5)
    output = root / "out"
    output.mkdir()
    nested = output / "Nested"
    nested.mkdir()
    (nested / "Keep").write_text("nested")
    (nested / "alias").symlink_to("keep")
    old = output / ("LLVM.O" if insensitive else "llvm.o")
    old.write_text("old")
    if ambiguous:
        os.link(old, output / "keep")
    (output / "lcb.exe").write_text("lcb")
    os.link(output / "lcb.exe", output / "keep-lcb")
    names = ["llvm.o", "lcb.exe"]
    real = (nested / "keep").exists() == insensitive
    original_stat = os.stat
    output_identity = output.stat()

    def resolve_artifact(path, *, dir_fd=None, follow_symlinks=True):
        if (dir_fd is not None and path == "llvm.o"
                and os.path.samestat(os.fstat(dir_fd), output_identity)):
            path = "LLVM.O"
        if dir_fd is None and Path(path).parent.name == "candidate":
            raise AssertionError("replacement lookup used the candidate directory")
        return original_stat(path, dir_fd=dir_fd, follow_symlinks=follow_symlinks)

    with ExitStack() as mocks:
        if not real:
            mocks.enter_context(patch.object(
                publisher, lookup_probe(), return_value=("simulated", insensitive)
            ))
            if insensitive:
                mocks.enter_context(patch.object(os, "stat", resolve_artifact))
                # shutil.copystat checks the callable's identity before
                # passing follow_symlinks=False.
                mocks.enter_context(patch.object(
                    os, "supports_follow_symlinks", os.supports_follow_symlinks | {resolve_artifact}
                ))
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
        assert set(os.listdir(output)) == {"LLVM.O", "keep", "lcb.exe", "keep-lcb", "Nested"}
        assert (output / "LLVM.O").read_text() == (output / "keep").read_text() == "old"
        assert (output / "lcb.exe").read_text() == "lcb"
        assert (staging / "llvm.o").read_text() == "new"
    else:
        assert set(os.listdir(output)) == {"llvm.o", "lcb.exe", "keep-lcb", "Nested"}
        assert (output / "llvm.o").read_text() == (output / "lcb.exe").read_text() == "new"
    assert (output / "keep-lcb").read_text() == "lcb"
    assert (output / "Nested" / "Keep").read_text() == "nested"
    assert os.readlink(output / "Nested" / "alias") == "keep"
    if real:
        alias = output / "Nested" / "alias"
        assert alias.exists() == insensitive
        if insensitive:
            assert alias.read_text() == "nested"
    return "real" if real else "simulated"


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


def check_lookup_refusal(root, *, fault, location, staging_insensitive):
    output = root / "out"
    nested = output / "nested"
    nested.mkdir(parents=True)
    (nested / "Keep").write_text("keep")
    (nested / "alias").symlink_to("keep")
    (output / "llvm.o").write_text("old")
    names = ["llvm.o", "lcb.exe"]
    staging = Path(publisher.prepare(output, names))
    for name in names:
        (staging / name).write_text("new")
    target = {"staging": staging, "output": output, "nested": nested}[location]
    identity = target.stat()

    def probe(fd, *_):
        if os.path.samestat(os.fstat(fd), identity):
            if fault == "unknown":
                raise OSError(errno.EIO, "injected probe failure")
            return "simulated", not staging_insensitive
        return "simulated", staging_insensitive

    expected = "cannot determine name-lookup rules" if fault == "unknown" else "name-lookup rules differ"
    try:
        with patch.object(publisher, lookup_probe(), side_effect=probe), \
                patch.object(Path, "mkdir", side_effect=AssertionError("created candidate")), \
                patch.object(publisher, "carry_forward", side_effect=AssertionError("copied output")), \
                patch.object(Path, "rename", side_effect=AssertionError("moved an artifact")), \
                patch.object(publisher, "exchange", side_effect=AssertionError("exchanged output")):
            publisher.publish(output, staging, names)
    except ValueError as error:
        assert str(error).startswith(expected)
        assert str(target) in str(error)
    else:
        raise AssertionError(f"accepted {fault} lookup rules for {target}")

    assert set(os.listdir(output)) == {"llvm.o", "nested"}
    assert (nested / "Keep").read_text() == "keep"
    assert os.readlink(nested / "alias") == "keep"
    assert (output / "llvm.o").read_text() == "old"
    assert set(os.listdir(staging)) == set(names)
    for name in names:
        assert (staging / name).read_text() == "new"


def check_staging_collision(root, *, nested):
    output = root / "out"
    output.mkdir(parents=True)
    source = output / "nested" if nested else output
    source.mkdir(exist_ok=True)
    (source / "Keep").mkdir()
    if (source / "keep").exists():
        print(f"SKIP unsupported host: {root.name} requires distinct Keep and keep entries")
        return
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
        # Inject a collision after a successful preflight to exercise the
        # exclusive-creation defense independently of the lookup gate.
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
    print(f"ok   exclusive creation refuses {root.name} before moves")


if __name__ == "__main__":
    original_cwd = Path.cwd()
    with TemporaryDirectory() as directory:
        root = Path(directory).resolve()
        try:
            sensitive = check_publication(root / "sensitive", insensitive=False)
            insensitive = check_publication(root / "insensitive", insensitive=True)
            ambiguous = check_publication(root / "ambiguous", insensitive=True, ambiguous=True)
            check_project_guard(root, (root / "SENSITIVE").exists())
            for insensitive_staging in (False, True):
                for location in ("output", "nested"):
                    check_lookup_refusal(
                        root / f"mismatch-{insensitive_staging}-{location}", fault="mismatch",
                        location=location, staging_insensitive=insensitive_staging,
                    )
            for location in ("staging", "output", "nested"):
                check_lookup_refusal(root / f"unknown-{location}", fault="unknown",
                                     location=location, staging_insensitive=False)
            check_staging_collision(root / "root collision", nested=False)
            check_staging_collision(root / "nested collision", nested=True)
        finally:
            os.chdir(original_cwd)
    print(f"ok   equal sensitive ({sensitive}) and equal insensitive ({insensitive}) publication")
    print(f"ok   project-directory aliases and ambiguous artifact alias refusal ({ambiguous})")
    print("ok   mismatched and unknown lookup rules refused before changes")
