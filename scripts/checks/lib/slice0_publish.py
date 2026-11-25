"""Prepare and atomically publish the slice-0 CLI's native build outputs."""

from contextlib import contextmanager, suppress
import ctypes
import errno
import fcntl
import os
from pathlib import Path
import shutil
import stat
import sys
import tempfile


@contextmanager
def output_directory(output):
    try:
        fd = os.open(output, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    except OSError as error:
        if error.errno in (errno.ENOTDIR, errno.ELOOP):
            raise ValueError(f"build output directory must be a real directory: {output}") from error
        raise
    try:
        yield fd
    finally:
        os.close(fd)


def check_destinations(output, fd, names):
    for name in names:
        try:
            mode = os.stat(name, dir_fd=fd, follow_symlinks=False).st_mode
        except FileNotFoundError:
            continue
        if not stat.S_ISREG(mode):
            raise ValueError(f"build output is not a regular file: {output / name}")


def exchange(parent_fd, candidate, output):
    """Exchange two directories with one filesystem operation."""
    libc = ctypes.CDLL(None, use_errno=True)
    if sys.platform == "linux":
        rename, flags = libc.renameat2, 2  # RENAME_EXCHANGE
    elif sys.platform == "darwin":
        rename, flags = libc.renameatx_np, 2  # RENAME_SWAP
    else:
        raise ValueError("atomic build directory exchange is unavailable on this platform")
    rename.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.c_uint]
    rename.restype = ctypes.c_int
    if rename(parent_fd, os.fsencode(candidate), parent_fd, os.fsencode(output), flags):
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error), output)


def prepare(output, names):
    output.parent.mkdir(parents=True, exist_ok=True)
    try:
        output.mkdir()
    except FileExistsError:
        pass
    with output_directory(output) as fd:
        check_destinations(output, fd, names)
    return tempfile.mkdtemp(prefix=".concrete-build.", dir=output.parent)


def publish(output, staging, names):
    # Lock the parent inode, which stays in place across exchanges. Concurrent
    # builds take their snapshot from the latest committed output directory.
    parent_fd = os.open(output.parent, os.O_RDONLY | os.O_DIRECTORY)
    try:
        fcntl.flock(parent_fd, fcntl.LOCK_EX)
        with output_directory(output) as output_fd:
            check_destinations(output, output_fd, names)
            candidate = staging / "candidate"
            candidate.mkdir()
            for name in names:
                source = staging / name
                if not stat.S_ISREG(source.lstat().st_mode):
                    raise ValueError(f"expected a regular build output: {name}")
                source.rename(candidate / name)

            # Anchor the snapshot to the directory opened with O_NOFOLLOW.
            # Release the snapshot directory before attempting the exchange.
            os.fchdir(output_fd)
            shutil.copytree(
                ".", candidate, symlinks=True, copy_function=os.link,
                ignore=lambda directory, _: names if directory == "." else (),
                dirs_exist_ok=True,
            )
            os.fchdir(parent_fd)
            current = os.stat(output.name, dir_fd=parent_fd, follow_symlinks=False)
            if not os.path.samestat(current, os.fstat(output_fd)):
                raise ValueError(f"build output directory changed during publication: {output}")
        exchange(parent_fd, candidate.relative_to(output.parent), output.name)
    finally:
        # Closing a read-only lock descriptor cannot undo a committed exchange.
        with suppress(OSError):
            os.close(parent_fd)


def main(args):
    operation, directory, *names = args
    # Strip trailing separators and dot components so they cannot hide a symlink.
    output = Path(os.path.abspath(directory))
    parent = output.parent.resolve()
    output = parent / output.name
    if output in (*Path.cwd().parents, Path.cwd()):
        raise ValueError(f"build output directory must not contain the current project: {output}")
    if operation == "prepare":
        print(prepare(output, names))
    elif operation == "publish":
        staging, *names = names
        publish(output, Path(staging), names)
    else:
        raise ValueError(f"unknown build publication operation: {operation}")


if __name__ == "__main__":
    try:
        main(sys.argv[1:])
    except (OSError, ValueError, AttributeError) as error:
        sys.exit(str(error))
