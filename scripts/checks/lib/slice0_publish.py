"""Prepare and atomically publish the slice-0 CLI's native build outputs."""

from array import array
from contextlib import contextmanager, nullcontext, suppress
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
def output_directory(output, *, guard_project=True, allow_missing=False):
    try:
        fd = os.open(output, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    except FileNotFoundError:
        if not allow_missing:
            raise
        yield None
        return
    except OSError as error:
        if error.errno in (errno.ENOTDIR, errno.ELOOP):
            raise ValueError(f"build output directory must be a real directory: {output}") from error
        raise
    try:
        if guard_project:
            check_project_directory(output, fd)
        yield fd
    finally:
        os.close(fd)


def check_project_directory(output, fd):
    destination = os.fstat(fd)
    cwd = Path.cwd()
    for ancestor in (cwd, *cwd.parents):
        if os.path.samestat(destination, ancestor.stat()):
            raise ValueError(f"build output directory must not contain the current project: {output}")


def check_destinations(output, fd, names):
    existing = {}
    for name in names:
        try:
            entry = os.stat(name, dir_fd=fd, follow_symlinks=False)
        except FileNotFoundError:
            continue
        if not stat.S_ISREG(entry.st_mode):
            raise ValueError(f"build output is not a regular file: {output / name}")
        existing[name] = entry
    return existing


def replaced_entries(fd, existing):
    """Find the directory entries addressed by the selected artifact names."""
    entries = set(os.listdir(fd))
    replaced = entries.intersection(existing)
    unresolved = existing.keys() - replaced
    if not unresolved:
        return replaced

    # The output lookup supplies the inode. A directory listing supplies
    # its stored spelling. Several hard links can share the lookup result.
    by_inode = {}
    for entry in entries:
        info = os.stat(entry, dir_fd=fd, follow_symlinks=False)
        by_inode.setdefault((info.st_dev, info.st_ino), []).append(entry)

    for name in unresolved:
        info = existing[name]
        aliases = by_inode.get((info.st_dev, info.st_ino), ())
        if len(aliases) != 1 or aliases[0] in replaced:
            raise ValueError(f"cannot identify the build output entry: {name}")
        replaced.add(aliases[0])
    return replaced


def linux_lookup_rules(fd, mount_types):
    with open(f"/proc/self/fdinfo/{fd}") as info:
        mount = next((line.split()[1] for line in info if line.startswith("mnt_id:")), None)
    filesystem = mount_types.get(mount)
    if filesystem not in {"ext2", "ext3", "ext4", "f2fs", "tmpfs"}:
        raise ValueError(f"unsupported filesystem {filesystem or 'unknown'}")

    # FS_IOC_GETFLAGS is _IOR('f', 1, long), but returns an int.
    # These are the Linux architectures supported by the native build.
    if os.uname().machine not in {"x86_64", "aarch64"}:
        raise ValueError("unsupported FS_IOC_GETFLAGS ABI")
    flags = array("i", [0])
    request = 0x80006601 | (ctypes.sizeof(ctypes.c_long) << 16)
    fcntl.ioctl(fd, request, flags)
    if flags[0] & 0x00000800:  # FS_ENCRYPT_FL
        raise ValueError("encrypted directory")
    # On these filesystems newly created directories inherit the casefold
    # flag. The mount also fixes the encoding and normalization tables.
    return mount, bool(flags[0] & 0x40000000)  # FS_CASEFOLD_FL


def macos_lookup_rules(fd):
    class AttrList(ctypes.Structure):
        _fields_ = [("count", ctypes.c_uint16), ("reserved", ctypes.c_uint16),
                    ("common", ctypes.c_uint32), ("volume", ctypes.c_uint32),
                    ("directory", ctypes.c_uint32), ("file", ctypes.c_uint32),
                    ("fork", ctypes.c_uint32)]

    volume_info = 0x80000000
    volume_capabilities = 0x00020000
    case_sensitive = 0x00000100
    attrs = AttrList(5, 0, 0, volume_info | volume_capabilities)
    # Returned length, four capability words, and their four validity masks.
    result = (ctypes.c_uint32 * 9)()
    getattrlist = ctypes.CDLL(None, use_errno=True).fgetattrlist
    getattrlist.argtypes = [ctypes.c_int, ctypes.POINTER(AttrList), ctypes.c_void_p,
                            ctypes.c_size_t, ctypes.c_ulong]
    getattrlist.restype = ctypes.c_int
    if getattrlist(fd, ctypes.byref(attrs), result, ctypes.sizeof(result), 0):
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error))
    if result[0] != ctypes.sizeof(result) or not result[5] & case_sensitive:
        raise ValueError("volume does not report case sensitivity")
    return os.fstat(fd).st_dev, not bool(result[1] & case_sensitive)


def lookup_rules_probe():
    if sys.platform == "linux":
        with open("/proc/self/mountinfo") as info:
            mount_types = {
                line.split()[0]: line.split(" - ", 1)[1].split()[0] for line in info
            }

        def probe(fd):
            return linux_lookup_rules(fd, mount_types)
    elif sys.platform == "darwin":
        probe = macos_lookup_rules
    else:
        raise ValueError("cannot determine build output name-lookup rules on this platform")

    def rules(fd, path):
        try:
            return probe(fd)
        except (OSError, ValueError, AttributeError) as error:
            raise ValueError(f"cannot determine name-lookup rules for {path}: {error}") from error

    return rules


def check_lookup_tree(fd, path, expected, rules):
    """Verify inheritance can reproduce the entire output tree before copying."""
    def check_tree(fd, path):
        if rules(fd, path) != expected:
            raise ValueError(f"name-lookup rules differ between staging and build output: {path}")
        with os.scandir(fd) as entries:
            for entry in entries:
                if entry.is_dir(follow_symlinks=False):
                    child = os.open(entry.name, directory_flags, dir_fd=fd)
                    try:
                        check_tree(child, path / entry.name)
                    finally:
                        os.close(child)

    directory_flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
    check_tree(fd, path)


def rename_directory(parent_fd, candidate, output, *, replace):
    """Publish with an atomic exchange or an exclusive first installation."""
    libc = ctypes.CDLL(None, use_errno=True)
    if sys.platform == "linux":
        rename, flags = libc.renameat2, 2 if replace else 1  # RENAME_EXCHANGE / RENAME_NOREPLACE
    elif sys.platform == "darwin":
        rename, flags = libc.renameatx_np, 2 if replace else 4  # RENAME_SWAP / RENAME_EXCL
    else:
        raise ValueError("atomic build directory publication is unavailable on this platform")
    rename.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.c_uint]
    rename.restype = ctypes.c_int
    if rename(parent_fd, os.fsencode(candidate), parent_fd, os.fsencode(output), flags):
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error), output)


def exchange(parent_fd, candidate, output):
    rename_directory(parent_fd, candidate, output, replace=True)


def resolve_parent(path):
    """Resolve existing ancestors in filesystem order and retain a missing suffix."""
    anchor = Path(path.anchor) if path.is_absolute() else Path.cwd()
    parts = path.parts[1:] if path.is_absolute() else path.parts
    for index, part in enumerate(parts):
        child = anchor / part
        try:
            child.lstat()
        except FileNotFoundError:
            missing = parts[index:]
            if ".." in missing:
                raise
            return anchor, missing
        if not stat.S_ISDIR(child.stat().st_mode):
            raise NotADirectoryError(errno.ENOTDIR, os.strerror(errno.ENOTDIR), child)
        anchor = child.resolve(strict=True)
    return anchor, ()


def publication_path(output):
    # A terminal ".." must participate in filesystem resolution, too.
    if output.name == ".." or not output.name:
        parent, missing = resolve_parent(output)
        assert not missing
        return parent.parent, (parent.name or ".",)
    parent, missing = resolve_parent(output.parent)
    return parent, (*missing, output.name)


def prepare(output, names):
    parent, suffix = publication_path(output)
    output = parent.joinpath(*suffix)
    rules = lookup_rules_probe()
    with output_directory(parent, guard_project=False) as parent_fd:
        expected = rules(parent_fd, parent)
        if len(suffix) == 1:
            with output_directory(output, allow_missing=True) as fd:
                if fd is not None:
                    check_destinations(output, fd, names)
                    check_lookup_tree(fd, output, expected, rules)
    staging = Path(tempfile.mkdtemp(prefix=".concrete-build.", dir=parent))
    try:
        with output_directory(staging, guard_project=False) as staging_fd:
            if rules(staging_fd, staging) != expected:
                raise ValueError(f"name-lookup rules differ between staging and build output: {staging}")
    except Exception:
        with suppress(OSError):
            staging.rmdir()
        raise
    return staging


def carry_forward(source, destination, *, replaced=()):
    """Populate a private directory using only exclusive entry creation."""
    with os.scandir(source) as entries:
        for entry in entries:
            if entry.name in replaced:
                continue
            target = destination / entry.name
            if entry.is_symlink():
                target.symlink_to(os.readlink(entry.path))
                shutil.copystat(entry.path, target, follow_symlinks=False)
            elif entry.is_dir(follow_symlinks=False):
                target.mkdir()
                carry_forward(entry.path, target)
                shutil.copystat(entry.path, target)
            else:
                os.link(entry.path, target)


def build_candidate(candidate, staging, names, *, output_fd=None, replaced=()):
    candidate.mkdir(parents=True)
    for name in names:
        source = staging / name
        if not stat.S_ISREG(source.lstat().st_mode):
            raise ValueError(f"expected a regular build output: {name}")
        # Reserve all names before copying any old entry or moving an artifact.
        os.close(os.open(candidate / name, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600))
    if output_fd is not None:
        os.fchdir(output_fd)
        carry_forward(".", candidate, replaced=replaced)
    for name in names:
        (staging / name).rename(candidate / name)
    if output_fd is not None:
        shutil.copystat(".", candidate)


def publish(output, staging, names):
    parent, suffix = publication_path(output)
    output = parent.joinpath(*suffix)
    # Lock the existing ancestor across the snapshot and commit.
    parent_fd = os.open(parent, os.O_RDONLY | os.O_DIRECTORY)
    try:
        fcntl.flock(parent_fd, fcntl.LOCK_EX)
        # A concurrent build can have installed part or all of a missing path
        # while this process waited for the lock.
        current_parent, current_suffix = publication_path(output)
        if (current_parent, current_suffix) != (parent, suffix):
            raise ValueError(f"build output parent changed during publication: {output}")
        destination = output_directory(output, allow_missing=True) if len(suffix) == 1 else nullcontext()
        with destination as output_fd:
            rules = lookup_rules_probe()
            with output_directory(staging, guard_project=False) as staging_fd:
                expected = rules(staging_fd, staging)
            if rules(parent_fd, parent) != expected:
                raise ValueError(f"name-lookup rules differ between staging and build output: {parent}")
            candidate = staging / "candidate"
            first = suffix[0]
            if output_fd is None:
                # Build missing ancestors privately and install the entire suffix
                # in one operation, with exclusive creation of its first entry.
                build_candidate(candidate.joinpath(*suffix[1:]), staging, names)
                rename_directory(parent_fd, candidate, first, replace=False)
                return

            existing = check_destinations(output, output_fd, names)
            check_lookup_tree(output_fd, output, expected, rules)
            replaced = replaced_entries(output_fd, existing)
            build_candidate(candidate, staging, names, output_fd=output_fd, replaced=replaced)
            os.fchdir(parent_fd)
            current = os.stat(first, dir_fd=parent_fd, follow_symlinks=False)
            if not os.path.samestat(current, os.fstat(output_fd)):
                raise ValueError(f"build output directory changed during publication: {output}")
        exchange(parent_fd, candidate, first)
    finally:
        # Closing a read-only lock descriptor cannot undo a committed publication.
        with suppress(OSError):
            os.close(parent_fd)


def main(args):
    operation, directory, *names = args
    # Strip trailing separators and dot components so they cannot hide a symlink.
    output = Path(directory)
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
