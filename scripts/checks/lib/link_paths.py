"""Closure membership shared by invocation and consumed-input validation."""
import os
from pathlib import Path
import sys


def allowed(path, local=False):
    lexical = os.path.abspath(path)
    try:
        resolved = str(Path(path).resolve())
    except (OSError, RuntimeError, ValueError) as error:
        sys.exit('link purity: invalid path: ' + str(error)[:500])
    if local and lexical == resolved and Path(resolved).parent == Path.cwd().resolve() and Path(path).is_file() and path.endswith('.o'):
        return
    if not path.startswith('/') or os.path.normpath(path) != path:
        sys.exit('link purity: noncanonical path: ' + path)
    closure = Path(os.environ['CONCRETE_SLICE0_LINK_CLOSURE']).read_text().splitlines()
    def member(p):
        return any(p == root or p.startswith(root + '/') for root in closure)
    if member(path) and member(resolved):
        return
    sys.exit('link purity: undeclared path: ' + path)
