"""Audit Python Git subprocesses: repository location must be explicitly sanitized."""
import ast
from pathlib import Path
import sys


def scan(root):
    scheduled_root = Path(root) / 'scripts/checks/scheduled'
    if scheduled_root.is_dir():
        # The sandbox gate projects the WHOLE boundary census, including shell
        # adapters and workflow command positions, rather than inventing a
        # second rule for literal Git calls in repository-making test modules.
        sys.path.insert(0, str(scheduled_root))
        from process_coverage import check, DIAGNOSTIC
        try: check(Path(root))
        except (ValueError, OSError, SyntaxError) as error:
            yield f"FAIL\t{scheduled_root.relative_to(root)}\t{error}"
        else:
            yield f"ok\t{scheduled_root.relative_to(root)}\tscheduled process boundary census"
    for path in sorted((Path(root) / 'scripts').rglob('*.py')):
        tree = ast.parse(path.read_text())
        makes_repository = any(isinstance(n, ast.Call) and any(
            isinstance(a, ast.Constant) and a.value in ('init', 'clone')
            for a in (n.args + [e for a in n.args if isinstance(a, (ast.List, ast.Tuple)) for e in a.elts]))
            for n in ast.walk(tree))
        scheduled = path.parent == Path(root) / 'scripts/checks/scheduled'
        if scheduled:
            continue
        if not makes_repository: continue
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call) or not node.args:
                continue
            argv = node.args[0]
            if not isinstance(argv, (ast.List, ast.Tuple)) or not argv.elts:
                continue
            first = argv.elts[0]
            if not isinstance(first, ast.Constant) or first.value != 'git':
                continue
            env = next((k.value for k in node.keywords if k.arg == 'env'), None)
            # Shared owners, never a local dictionary claiming to be clean.
            safe = env is not None and ('clean_env(' in ast.unparse(env) or
                                        ast.unparse(env) == 'GIT_ENVIRONMENT')
            verdict = 'ok' if safe else 'FAIL'
            yield f"{verdict}\t{path.relative_to(root)}\tPython git subprocess requires protocol.clean_env environment (line {node.lineno})"

if __name__ == '__main__':
    for row in scan(Path(sys.argv[1])): print(row)
