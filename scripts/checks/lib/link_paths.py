"""Closure membership without lexical erasure of filesystem traversal."""
import os
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
    try:
        # Each symlink prefix must stay in the closure, even if a later symlink
        # would return to it. resolve() alone checks only the final destination.
        current = Path('/')
        for part in path.split('/')[1:]:
            if not part:
                continue
            current = current / part
            if current.is_symlink() and not member(str(current.resolve())):
                sys.exit('link purity: undeclared symlink prefix: ' + str(current))
        if member(str(Path(path).resolve())):
            return
    except (OSError, RuntimeError, ValueError) as error:
        sys.exit('link purity: invalid path: ' + str(error)[:500])
    sys.exit('link purity: undeclared path: ' + path)
