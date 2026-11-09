"""Fail closed on launches outside the scheduled process owner/adapters."""
import ast
import json
import re
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[3]
DIAGNOSTIC = 'FAIL scheduled process boundary: unmanaged launch'


def check_python_launches(path):
    tree = ast.parse(path.read_text())
    aliases = {}
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            aliases.update((x.asname or x.name, x.name) for x in node.names)
        elif isinstance(node, ast.ImportFrom):
            aliases.update((x.asname or x.name, (node.module or '') + '.' + x.name) for x in node.names)
    allowed = set()
    exceptions = {'stdin_control.py': 'main',
                  'test_process.py': 'test_supervisor_charges_startup_to_declared_deadline'}
    for node in ast.walk(tree):
        if isinstance(node, ast.FunctionDef) and node.name == exceptions.get(path.name):
            allowed.update(id(n) for n in ast.walk(node) if isinstance(n, ast.Call))
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call): continue
        name = ast.unparse(node.func).split('.')
        full = '.'.join([aliases.get(name[0], name[0]), *name[1:]])
        if full == 'bounded_exec.supervise' and id(node) not in allowed:
            raise ValueError(DIAGNOSTIC)
        if full in {'subprocess.run', 'subprocess.Popen', 'subprocess.call',
                    'subprocess.check_call', 'subprocess.check_output',
                    'subprocess.getoutput', 'subprocess.getstatusoutput',
                    'asyncio.create_subprocess_exec', 'asyncio.create_subprocess_shell',
                    'os.system', 'os.popen', 'os.posix_spawn', 'os.posix_spawnp'} or full.startswith(('os.exec', 'os.spawn')):
            raise ValueError(DIAGNOSTIC)

def check(root):
    paths = [*(root / 'scripts/checks/scheduled').glob('*.py'),
             root / 'scripts/checks/lib/gate_manifest.py']
    for path in paths:
        check_python_launches(path)
    check_clock_reads(root)
    # Shell adapters must enter the owner before utility children; workflow
    # controllers explicitly select the scheduler dispatcher, never generic policy.
    for relative in ('scripts/checks/scheduled_gates.sh', 'scripts/checks/scheduled/with_slot.sh'):
        text = (root / relative).read_text()
        if 'exec python3 -I "$ROOT/scripts/checks/lib/scheduled_process.py" --cwd "$PWD" --timeout ' not in text:
            raise ValueError(DIAGNOSTIC)
        if 'source "$ROOT/scripts/checks/lib/scheduled_process.sh"' not in text:
            raise ValueError(DIAGNOSTIC)
        check_shell_launches(text, bootstrap=True)
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
                check_shell_launches(script)


def check_shell_launches(script, *, bootstrap=False):
    # Scan command segments, including substitutions and every shell separator.
    # Erase the explicitly managed argv tail, not an entire line containing it:
    # a second command after `then`, &&, || or | must still be checked.
    for line in script.splitlines():
        if line.lstrip().startswith('#'): continue
        # A complete [[ expression ]] has no executable positions except its
        # substitutions. Do not misclassify variables after its logical ||/&&.
        if (re.fullmatch(r'\s*\[\[(?:(?!\]\]).)*\]\]\s*', line) and
                '$(' not in line and '`' not in line): continue
        segments = re.split(r';|&&|\|\||\|(?!=)|\$\(|[(){}]', line)
        for segment in segments:
            segment = re.sub(r'\bbounded_run\s+[^;]*?--(?:role\s+\S+\s+--)?\s+.*$', '', segment)
            # The sole bootstrap is the explicit process owner itself.
            segment = re.sub(r'(?:exec\s+)?python3\s+(?:-I\s+)?["\S]*scripts/checks/lib/scheduled_process\.py["\S]*\s+--cwd\s+.*$', '', segment)
            tool = r"(?<![\w/.-])[\"']?(?:[^\s\"'(){};|&]*/)?(?:python3|git|nix|bash|sh|env)[\"']?(?:\s|$)"
            dynamic = r"(?:^|\bthen\s+|\belse\s+|\bdo\s+)\s*(?:(?:if|elif|while|until|exec|command|!)\s+)*[\"']?\$(?:@|[A-Za-z_0-9]|\{)"
            if re.search(tool, segment) or re.search(dynamic, segment):
                raise ValueError(DIAGNOSTIC)


def check_clock_reads(root):
    """Kernel clock access is a declared capability, not an environment promise."""
    owner = root / 'scripts/checks/lib/scheduled_process.py'
    paths = [p for p in (root / 'scripts/checks/scheduled').glob('*.py')
             if not p.name.startswith('test_')]
    paths.append(owner)
    for path in paths:
        tree = ast.parse(path.read_text())
        aliases = {}
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                aliases.update((x.asname or x.name, x.name) for x in node.names)
            elif isinstance(node, ast.ImportFrom):
                aliases.update((x.asname or x.name, (node.module or '') + '.' + x.name) for x in node.names)
        allowed = set()
        if path == owner:
            functions = [n for n in tree.body if isinstance(n, ast.FunctionDef) and n.name == '_utc_now']
            if len(functions) == 1:
                allowed = {id(n) for n in ast.walk(functions[0]) if isinstance(n, ast.Call)}
        reads = []
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call): continue
            name = ast.unparse(node.func).split('.')
            full = '.'.join([aliases.get(name[0], name[0]), *name[1:]])
            if full in {'datetime.datetime.now', 'datetime.datetime.utcnow', 'datetime.datetime.today',
                        'datetime.date.today', 'time.time', 'time.time_ns', 'time.clock_gettime',
                        'time.clock_gettime_ns'}:
                reads.append(node)
        if any(id(n) not in allowed for n in reads) or (path == owner and len(reads) != 1):
            raise ValueError('FAIL scheduled clock: undeclared read')


def main():
    check(Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT)
    return 0


if __name__ == '__main__':
    try: sys.exit(main())
    except ValueError as error: sys.exit(str(error))
