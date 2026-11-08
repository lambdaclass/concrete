"""Closure membership without lexical erasure of filesystem traversal."""
import os
from collections import deque
from pathlib import Path
import sys


def allowed(path, local=False):
    if local:
        if (path and Path(path).name == path and path not in ('.', '..')
                and path.endswith('.o') and not Path(path).is_symlink()
                and Path(path).is_file()):
            return
        sys.exit('link purity: undeclared local object: ' + path)
    # Check raw components, before Path/normalization can erase them. Redundant
    # slashes are harmless; dot components change symlink traversal semantics.
    if not path.startswith('/') or any(p in ('.', '..') for p in path.split('/')):
        sys.exit('link purity: noncanonical path: ' + path)
    closure = Path(os.environ['CONCRETE_SLICE0_LINK_CLOSURE']).read_text().splitlines()
    def member(p):
        return any(p == root or p.startswith(root + '/') for root in closure)
    if not member(path):
        sys.exit('link purity: undeclared path: ' + path)
    def prefix(p):
        # The store's ancestors are needed to reach a declared root, but an
        # undeclared sibling is rejected before even probing its filesystem.
        return member(p) or p == '/' or any(root.startswith(p + '/') for root in closure)
    try:
        current = Path('/')
        pending = deque(path.split('/')[1:])
        links = 0
        while pending:
            part = pending.popleft()
            if part in ('', '.'):
                continue
            candidate = current.parent if part == '..' else current / part
            if not prefix(str(candidate)):
                sys.exit('link purity: undeclared symlink traversal: ' + str(candidate))
            if part != '..' and candidate.is_symlink():
                links += 1
                if links > 40:
                    sys.exit('link purity: excessive symlink traversal: ' + path)
                # Do not resolve a target wholesale: that erases intermediate
                # symlinks and directories that can leave and re-enter closure.
                target = os.readlink(candidate)
                if target.startswith('/'):
                    current = Path('/')
                pending.extendleft(reversed(target.split('/')))
            else:
                current = candidate
        if member(str(current)):
            return
    except (OSError, RuntimeError, ValueError) as error:
        sys.exit('link purity: invalid path: ' + str(error)[:500])
    sys.exit('link purity: undeclared path: ' + path)
