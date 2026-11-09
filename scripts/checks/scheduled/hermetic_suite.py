"""Run identical test inventories under clean and foreign scheduler identities."""
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest
sys.path.insert(0, str(Path(__file__).resolve().parent))
from protocol import clean_env, git

ROOT = Path(__file__).resolve().parent
PREFIXES = ('GITHUB_', 'SELECTED_', 'SCHEDULED_', 'RUNNER_')


def environments(environment):
    clean = {k: v for k, v in clean_env(environment).items() if not k.startswith(PREFIXES)}
    # Include all ambient keys, every literal identity key in controller/library code,
    # and unknown future keys in each namespace. Values are intentionally unusable.
    keys = {k for k in environment if k.startswith(PREFIXES)}
    for path in [*ROOT.glob('*.py'), *(ROOT.parent / 'lib').glob('*.py')]:
        keys.update(re.findall(r'[\'\"]((?:GITHUB_|SELECTED_|SCHEDULED_|RUNNER_)[A-Z_]+)[\'\"]', path.read_text()))
    keys.update(prefix + 'FOREIGN_CONTROL' for prefix in PREFIXES)
    hostile = dict(clean, **{key: 'foreign/' + key for key in keys})
    return clean, hostile


def compare(clean, hostile):
    if clean != hostile:
        raise ValueError('FAIL scheduled hermeticity: clean and hostile test results differ')


def compare_canary(before, after):
    if before != after:
        raise ValueError('FAIL scheduled hermeticity: canary repository changed')


class Results(unittest.TextTestResult):
    def __init__(self, *args):
        super().__init__(*args)
        self.outcomes = []

    def addSuccess(self, test):
        super().addSuccess(test)
        self.outcomes.append([test.id(), 'PASS'])

    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        self.outcomes.append([test.id(), 'SKIP', reason])


def child(destination):
    suite = unittest.defaultTestLoader.discover(str(ROOT), pattern='test_*.py')
    result = unittest.TextTestRunner(resultclass=Results).run(suite)
    Path(destination).write_text(json.dumps(sorted(result.outcomes)))
    return int(not result.wasSuccessful())


def main(environment):
    with tempfile.TemporaryDirectory(prefix='scheduled-hermetic-') as scratch:
        canary = Path(scratch) / 'canary'
        canary.mkdir()
        git('-C', str(canary), 'init', '-q')
        (canary / 'staged.txt').write_text('canary index contents\n')
        git('-C', str(canary), 'add', 'staged.txt')
        git('-C', str(canary), '-c', 'user.name=Canary', '-c', 'user.email=canary@example.invalid',
            '-c', 'core.hooksPath=/dev/null', 'commit', '--allow-empty', '-qm', 'canary')
        def snapshot():
            return {str(p.relative_to(canary)): p.read_bytes() for p in canary.rglob('*') if p.is_file()}
        before = snapshot()
        reports = []
        for name, env in zip(('clean', 'hostile'), environments(environment)):
            if name == 'hostile':
                env.update(GIT_DIR=str(canary / '.git'), GIT_WORK_TREE=str(canary),
                           GIT_INDEX_FILE=str(canary / '.git/index'),
                           GIT_OBJECT_DIRECTORY=str(canary / '.git/objects'))
            destination = str(Path(scratch) / (name + '.json'))
            result = subprocess.run([sys.executable, '-I', str(Path(__file__).resolve()), '--child', destination],
                                    env=env, capture_output=True, text=True, timeout=45)
            if result.returncode:
                print(result.stdout, end=''); print(result.stderr, end='', file=sys.stderr)
                raise ValueError(f'FAIL scheduled hermeticity: {name} suite failed')
            compare_canary(before, snapshot())
            reports.append(json.loads(Path(destination).read_text()))
        compare(*reports)
        print(f'PASS scheduled hermeticity: {len(reports[0])} identical clean/hostile results')
    return 0


if __name__ == '__main__':
    try:
        sys.exit(child(sys.argv[2]) if len(sys.argv) == 3 and sys.argv[1] == '--child'
                 else main(dict(os.environ)))
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        sys.exit(str(error))
