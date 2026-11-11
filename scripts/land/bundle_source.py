"""Resolve the canonical fetch URL or a verified caller-relative bundle."""
import pathlib
import re
import subprocess
import sys

def fetch_source(source, repo, ref=None):
    if source == 'https://github.com/lambdaclass/concrete.git':
        return source
    # Resolve once against the caller, refusing symlinks in every path component.
    path = pathlib.Path(source).absolute()
    if any(part.is_symlink() for part in (path, *path.parents)):
        raise ValueError('symlink fetch source refused')
    if not path.is_file():
        raise ValueError('fetch source must be a canonical repository URL or a git bundle file; local clones and remote names are refused')
    verified = subprocess.run(['git', '-C', repo, 'bundle', 'verify', str(path)],
                              capture_output=True, text=True)
    if verified.returncode:
        raise ValueError('invalid git bundle: ' + verified.stderr.strip())
    heads = subprocess.run(['git', 'bundle', 'list-heads', str(path)],
                           capture_output=True, text=True, check=True)
    entries = [line.split() for line in heads.stdout.splitlines()]
    if not entries or any(len(row) != 2 or not re.fullmatch(r'[0-9a-f]{40}|[0-9a-f]{64}', row[0])
                          or (row[1] != 'HEAD' and subprocess.run(
                              ['git', 'check-ref-format', row[1]], capture_output=True).returncode)
                          for row in entries):
        raise ValueError('git bundle has invalid or missing refs')
    if ref is not None and not any(ref in row for row in entries):
        raise ValueError('requested ref is not advertised by git bundle: ' + ref)
    return str(path)


if __name__ == '__main__':
    try:
        print(fetch_source(*sys.argv[1:]))
    except ValueError as error:
        print(error, file=sys.stderr)
        sys.exit(1)
