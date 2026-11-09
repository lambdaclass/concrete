"""Fail closed on launches outside the scheduled process owner/adapters."""
import ast
import json
import re
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[3]
DIAGNOSTIC = 'FAIL scheduled process boundary: unmanaged launch'


def check(root):
    paths = [*(root / 'scripts/checks/scheduled').glob('*.py'),
             root / 'scripts/checks/lib/gate_manifest.py']
    for path in paths:
        tree = ast.parse(path.read_text())
        aliases = {}
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                aliases.update((x.asname or x.name, x.name) for x in node.names)
            elif isinstance(node, ast.ImportFrom):
                aliases.update((x.asname or x.name, (node.module or '') + '.' + x.name) for x in node.names)
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call): continue
            name = ast.unparse(node.func).split('.')
            full = '.'.join([aliases.get(name[0], name[0]), *name[1:]])
            if full in {'subprocess.run', 'subprocess.Popen', 'subprocess.call',
                        'subprocess.check_call', 'subprocess.check_output',
                        'os.system', 'os.popen', 'os.posix_spawn', 'os.posix_spawnp'} or full.startswith(('os.exec', 'os.spawn')):
                raise ValueError(DIAGNOSTIC)
    # Shell adapters must enter the owner before utility children; workflow
    # controllers explicitly select the scheduler dispatcher, never generic policy.
    for relative in ('scripts/checks/scheduled_gates.sh', 'scripts/checks/scheduled/with_slot.sh'):
        text = (root / relative).read_text()
        if 'exec python3 -I "$ROOT/scripts/checks/lib/scheduled_process.py" --cwd "$PWD" --timeout ' not in text:
            raise ValueError(DIAGNOSTIC)
        if 'source "$ROOT/scripts/checks/lib/scheduled_process.sh"' not in text:
            raise ValueError(DIAGNOSTIC)
        for line in text.splitlines():
            if re.search(r'(?:^|[;$(])\s*(?:python3|git|nix|bash) ', line):
                raise ValueError(DIAGNOSTIC)
    for path in (root / '.github/workflows').glob('gate_*.yml'):
        import yaml
        document = yaml.safe_load(path.read_text())
        for job in document['jobs'].values():
            for step in job['steps']:
                script = step.get('run', '')
                if not script or 'Refuse substitute runner' == step.get('name'): continue
                managed = ('source scripts/checks/lib/scheduled_process.sh' in script or
                           'python3 scripts/checks/lib/scheduled_process.py --cwd "$PWD" --timeout ' in script)
                if not managed or 'lib/bounded_exec.py' in script or 'lib/bounded.sh' in script:
                    raise ValueError(DIAGNOSTIC)
                # Execution commands are bounded; shell builtins/utilities inherit
                # the adapter context. Reject raw controller/tool launches.
                for line in script.splitlines():
                    if re.match(r'\s*(?:python3|git|nix|bash) ', line) and 'lib/scheduled_process.py --cwd' not in line:
                        raise ValueError(DIAGNOSTIC)


def main():
    if len(sys.argv) == 3 and sys.argv[1] == '--spec':
        sys.path.insert(0, str(ROOT / 'scripts/checks/lib'))
        from scheduled_process import validate
        validate(json.loads(Path(sys.argv[2]).read_text()))
    else:
        check(Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT)
    return 0


if __name__ == '__main__':
    try: sys.exit(main())
    except ValueError as error: sys.exit(str(error))
