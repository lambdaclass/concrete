"""C controls must produce identical evidence with clean and hostile CI identity."""
import os
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[3]
PREFIXES = ('GITHUB_', 'SELECTED_', 'SCHEDULED_', 'RUNNER_')


def main(environment):
    clean = {k: v for k, v in environment.items() if not k.startswith(PREFIXES)}
    keys = {k for k in environment if k.startswith(PREFIXES)}
    for path in (ROOT/'scripts/checks/compiler').glob('*'):
        if path.suffix in ('.py', '.sh'):
            keys.update(re.findall(r'((?:GITHUB_|SELECTED_|SCHEDULED_|RUNNER_)[A-Z_]+)', path.read_text()))
    keys.update(prefix+'FOREIGN_CONTROL' for prefix in PREFIXES)
    hostile = dict(clean, **{key: 'foreign/'+key for key in keys})
    for script in ('campaign_wiring.py', 'campaign_controls.py', 'mode_controls.py', 'manifest_controls.py', 'scheduled_evidence_controls.py'):
        results = []
        for env in (clean, hostile):
            result = subprocess.run([sys.executable, '-I', str(ROOT/'scripts/checks/compiler'/script)],
                                    cwd=ROOT, env=env, capture_output=True, text=True, timeout=60)
            if result.returncode:
                sys.exit(f'FAIL C hermeticity {script}: {result.stdout}{result.stderr}')
            results.append((result.stdout, result.stderr))
        if results[0] != results[1]:
            sys.exit('FAIL C hermeticity: clean and hostile evidence differ: '+script)
        print(results[0][0], end='')
        print('PASS C hermeticity: identical clean/hostile evidence: '+script)
    return 0


if __name__ == '__main__':
    sys.exit(main(dict(os.environ)))
