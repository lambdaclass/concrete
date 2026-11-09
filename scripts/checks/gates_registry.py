"""Gate commands and required-check identities have one executable registry."""
import argparse
import copy
import json
from pathlib import Path
import shlex
import subprocess
import sys
import yaml

ROOT = Path(__file__).resolve().parents[2]


def invocation(name, tier):
    return f'nix develop .#gates --command python3 scripts/checks/gates_registry.py --run {name} --tier {tier}'


def validate(registry, workflows, required):
    errors = []
    for name, gate in registry['gates'].items():
        if gate['check'] not in required:
            errors.append(f'FAIL registry-required-identity {name}')
        job = workflows[gate['workflow']].get('jobs', {}).get(gate['job'], {})
        runs = [step.get('run', '').strip() for step in job.get('steps', [])]
        if job.get('name') != gate['check'] or invocation(name, 'pr') not in runs:
            errors.append(f'FAIL registry-required-invocation {name}')
        daily = workflows['.github/workflows/proof_vacuity.yml']['jobs']['daily']
        if invocation(name, 'daily') not in [s.get('run', '').strip() for s in daily['steps']]:
            errors.append(f'FAIL registry-daily-invocation {name}')
        for tier in ('pr', 'daily'):
            if not gate[tier] or any(not cmd for cmd in gate[tier]):
                errors.append(f'FAIL registry-empty-tier {name} {tier}')
    return errors


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--run')
    parser.add_argument('--tier', choices=['pr', 'daily'])
    args = parser.parse_args()
    registry = json.loads((ROOT / 'scripts/checks/gates.json').read_text())
    workflows = {str(p.relative_to(ROOT)): yaml.safe_load(p.read_text())
                 for p in (ROOT / '.github/workflows').glob('*.yml')}
    required = (ROOT / 'required-checks.txt').read_text().splitlines()
    errors = validate(registry, workflows, required)
    if errors:
        print('\n'.join(errors)); return 1
    if args.run:
        if not args.tier:
            parser.error('--run requires --tier')
        gate = registry['gates'][args.run]
        commands = list(gate[args.tier])
        if args.tier == 'pr':
            if gate['scopes']:
                commands.append(['bash', 'scripts/proofs/vacuity.sh', '--scope', ','.join(gate['scopes'])])
            else:
                print('N2 structural coverage: NOT MIGRATED; required PR check covers controls only', flush=True)
        for cmd in commands:
            print('GATE ' + shlex.join(cmd), flush=True)
            result = subprocess.run(['bash', str(ROOT / 'scripts/checks/lib/bounded.sh'), '3000', *cmd], cwd=ROOT)
            if result.returncode:
                return result.returncode
        return 0
    # A green unrelated command under the required identity must be rejected.
    planted = copy.deepcopy(workflows)
    gate = registry['gates']['proof-vacuity']
    planted[gate['workflow']]['jobs'][gate['job']]['steps'] = [{'run': 'bash scripts/proofs/axioms.sh'}]
    expected = ['FAIL registry-required-invocation proof-vacuity']
    observed = validate(registry, planted, required)
    if observed != expected:
        print(f'FAIL registry-wrong-command-control {observed!r}'); return 1
    print('PASS registry-required-invocation control (exact diagnostic)')
    print('PASS gates_registry')
    return 0


if __name__ == '__main__':
    sys.exit(main())
