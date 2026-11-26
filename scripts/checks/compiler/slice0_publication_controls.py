"""Exercise publication path resolution, concurrent commits and name lookup rules."""

from contextlib import ExitStack, nullcontext
import os
import errno
import fcntl
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


def check_path_resolution(root):
    nested = root / "target" / "nested"
    nested.mkdir(parents=True)
    (root / "link").symlink_to("target/nested")
    names = ["llvm.o"]
    output = root / "target"
    staging = publisher.prepare(root / "link/..", names)
    (staging / "llvm.o").write_text("terminal dot-dot")
    publisher.publish(root / "link/..", staging, names)
    assert (output / "llvm.o").read_text() == "terminal dot-dot"

    missing = root / "missing/../target"
    staging = publisher.prepare(output, names)
    (staging / "llvm.o").write_text("replacement")
    for operation in (publisher.prepare, publisher.publish):
        args = (names,) if operation is publisher.prepare else (staging, names)
        try:
            operation(missing, *args)
        except FileNotFoundError:
            pass
        else:
            raise AssertionError("accepted dot-dot after a missing component")
    assert not (root / "missing").exists()
    assert (output / "llvm.o").read_text() == "terminal dot-dot"
    assert (staging / "llvm.o").read_text() == "replacement"


def check_relative_staging(root):
    root.mkdir()
    output = root / "out"
    output.mkdir()
    (output / "lcb.exe").write_text("previous backend")
    names = ["llvm.o"]
    staging = publisher.prepare(output, names)
    (staging / "llvm.o").write_text("new")
    cwd = Path.cwd()
    try:
        os.chdir(root)
        publisher.main(["publish", "out", staging.name, *names])
    finally:
        os.chdir(cwd)
    assert (output / "llvm.o").read_text() == "new"
    assert (output / "lcb.exe").read_text() == "previous backend"


def check_symlink_limit(root):
    root.mkdir()
    step = root / "step"
    step.symlink_to(".")
    out = root / "out"
    out.mkdir()
    (out / "llvm.o").write_text("old")
    names = ["llvm.o"]
    staging = publisher.prepare(out, names)
    (staging / "llvm.o").write_text("new")

    supplied = root
    for _ in range(256):
        supplied /= "step"
        try:
            (supplied / "out").stat()
        except OSError as error:
            assert error.errno == errno.ELOOP
            break
    else:
        raise AssertionError("could not reach the OS symlink traversal limit")
    supplied /= "out"

    for operation in (publisher.prepare, publisher.publish):
        args = (names,) if operation is publisher.prepare else (staging, names)
        with patch.object(publisher.tempfile, "mkdtemp",
                          side_effect=AssertionError("staged through a rejected path")):
            try:
                operation(supplied, *args)
            except OSError as error:
                assert error.errno == errno.ELOOP
            else:
                raise AssertionError("published through a rejected path")
        assert (out / "llvm.o").read_text() == "old"
        assert (staging / "llvm.o").read_text() == "new"


def check_concurrent_publication(root, *, sibling=False, redirect=False):
    root.mkdir()
    first = root / "missing/nested/out"
    second = first.with_name("other") if sibling else first
    llvm = ["llvm.o", "llvm.exe"]
    lcb = ["lcb_body.o", "lcb_rt.s", "lcb_rt.o", "lcb.exe"]
    llvm_stage = publisher.prepare(first, llvm)
    lcb_stage = publisher.prepare(second, lcb)
    for staging, names in ((llvm_stage, llvm), (lcb_stage, lcb)):
        for name in names:
            (staging / name).write_text(name)
    assert not (root / "missing").exists()

    flock = publisher.fcntl.flock
    locks = []
    waiting_fd = None

    def take_lock(fd, operation):
        nonlocal waiting_fd
        if waiting_fd is not None:
            # The winner has its own handle to the same directory. Prove the
            # contender would block before allowing the winner to commit.
            flock(fd, operation | fcntl.LOCK_NB)
            try:
                flock(waiting_fd, operation | fcntl.LOCK_NB)
            except BlockingIOError:
                return
            raise AssertionError("the contender acquired the winner's lock")

        locks.append(os.fstat(fd))
        if len(locks) == 1:
            assert os.path.samestat(locks[0], root.stat())
            if redirect:
                (root / "elsewhere/nested").mkdir(parents=True)
                (root / "missing").symlink_to("elsewhere")
            else:
                waiting_fd = fd
                try:
                    publisher.publish(first, llvm_stage, llvm)
                finally:
                    waiting_fd = None
        flock(fd, operation | fcntl.LOCK_NB)

    with patch.object(publisher.fcntl, "flock", side_effect=take_lock):
        if redirect:
            try:
                publisher.publish(second, lcb_stage, lcb)
            except ValueError as error:
                assert "parent changed during publication" in str(error)
            else:
                raise AssertionError("followed a newly installed ancestor symlink")
            assert not list((root / "elsewhere/nested").iterdir())
            assert set(os.listdir(lcb_stage)) == set(lcb)
            return
        publisher.publish(second, lcb_stage, lcb)

    assert len(locks) == 2 and os.path.samestat(locks[1], second.parent.stat())
    for output, names in ((first, llvm), (second, lcb)):
        for name in names:
            assert (output / name).read_text() == name
    assert set(os.listdir(second)) == set(lcb if sibling else llvm + lcb)


if __name__ == "__main__":
    original_cwd = Path.cwd()
    with TemporaryDirectory() as directory:
        root = Path(directory).resolve()
        try:
            check_path_resolution(root / "path-resolution")
            check_relative_staging(root / "relative-staging")
            check_symlink_limit(root / "symlink-limit")
            check_concurrent_publication(root / "concurrent")
            check_concurrent_publication(root / "siblings", sibling=True)
            check_concurrent_publication(root / "redirect", redirect=True)
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
    print("ok   kernel symlink limit, terminal dot-dot, missing ancestor and relative staging")
    print("ok   concurrent first publications retain both backends and refuse symlink redirects")
    print(f"ok   equal sensitive ({sensitive}) and equal insensitive ({insensitive}) publication")
    print(f"ok   project-directory aliases and ambiguous artifact alias refusal ({ambiguous})")
    print("ok   mismatched and unknown lookup rules refused before changes")
