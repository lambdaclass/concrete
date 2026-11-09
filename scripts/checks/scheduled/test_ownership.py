import runpy
from pathlib import Path
runpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))

import shutil
import sys
import tempfile
import unittest
from process_adapter import process_run
from runtime_support import runtime_environment
from scheduler_policy import Identity, POLICY, TIERS, WINDOWS, allocate_deadline, note_ref
from workflow_projection import check, ROOT


class Ownership(unittest.TestCase):
    def invoke(self, root, fact=None):
        result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/ownership.py'), str(root)],
                             env=runtime_environment(), cwd=ROOT, timeout=10, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0 if fact is None else 1)
        self.assertEqual(result.stdout, '')
        self.assertEqual(result.stderr, '' if fact is None else f'FAIL scheduled ownership: {fact}\n')

    def copy(self, root):
        shutil.copytree(ROOT / 'scripts/checks/scheduled', root / 'scripts/checks/scheduled',
                        ignore=shutil.ignore_patterns('__pycache__'))
        shutil.copytree(ROOT / '.github/workflows', root / '.github/workflows')
        lib = root / 'scripts/checks/lib'; lib.mkdir()
        shutil.copy2(ROOT / 'scripts/checks/lib/gate_manifest.py', lib)
        shutil.copy2(ROOT / 'scripts/checks/scheduled_gates.sh', root / 'scripts/checks/scheduled_gates.sh')

    def test_each_divergent_consumer_fails_for_its_fact(self):
        plants = dict(history="order = '--date-order'", identity="branch = 'foreign'",
                      occurrence="identity = dict(occurrence='replay/other')",
                      schedule='cron = ' + repr('0 0 * * *'),
                      tier='tier = ' + repr('monthly'), window='window = ' + str(next(iter(WINDOWS.values())) + 1),
                      budget='timeout = ' + str(POLICY['budgets']['campaign'] + 1),
                      retention='days = ' + str(POLICY['retention_days'] + 1))
        plants['note-ref'] = 'ref = ' + repr('refs/notes/foreign-monthly')
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            self.copy(root)
            self.invoke(root)
            target = root / 'scripts/checks/scheduled/divergent_consumer.py'
            for fact, source in plants.items():
                with self.subTest(fact=fact):
                    target.write_text(source + '\n')
                    self.invoke(root, fact)
            target.unlink()
            self.invoke(root)

    def test_workflow_projection_drift_is_exact(self):
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch); self.copy(root)
            for name, before, after, fact in (
                ('schedule', 'retention-days: 90', 'retention-days: 89', 'retention'),
                ('schedule', '--timeout 80000', '--timeout 79999', 'budget'),
                ('schedule', "'17 2 * * *'", "'18 2 * * *'", 'schedule'),
                ('schedule', 'options: [daily, weekly]', 'options: [daily]', 'tier'),
                ('schedule', 'ref: main', 'ref: foreign', 'identity'),
                ('freshness', 'ls-remote origin', 'ls-remote foreign', 'note-ref')):
                with self.subTest(fact=fact):
                    path = root / f'.github/workflows/gate_{name}.yml'
                    original = path.read_text(); self.assertIn(before, original)
                    path.write_text(original.replace(before, after))
                    self.invoke(root, fact)
                    path.write_text(original)

    def test_execution_and_collector_share_identity_without_trusting_worker(self):
        # Both adapters supply independently observed Actions inputs to this owner.
        for event in ('schedule', 'workflow_dispatch'):
            first = Identity.actions('example/repo', 'a'*40, TIERS[0], 42, 1, event)
            retry = Identity.actions('example/repo', 'a'*40, TIERS[0], 42, 2, event)
            self.assertEqual(first.occurrence, retry.occurrence)
            self.assertNotEqual(first.attempt, retry.attempt)
        self.assertNotEqual(first.occurrence, Identity.actions('example/repo', 'a'*40, TIERS[0], 42, 1, 'schedule').occurrence)

    def test_selection_crosses_real_process_boundary(self):
        with tempfile.TemporaryDirectory() as scratch:
            output = Path(scratch) / 'output'
            env = dict(runtime_environment(), GITHUB_SHA='a'*40, GITHUB_OUTPUT=str(output))
            for tier, cron in POLICY['cadence'].items():
                result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/selection.py'),
                                      'schedule', cron, '', '', 'a'*40], env=env, cwd=ROOT,
                                     timeout=10, capture_output=True, text=True)
                self.assertEqual((result.returncode, result.stdout, result.stderr),
                                 (0, f'SCHEDULE_SELECTION {"a"*40} {tier}\n', ''))
            self.assertEqual(output.read_text(), ''.join(f'sha={"a"*40}\ntier={tier}\n' for tier in TIERS))

    def test_deadline_composition_uses_one_fake_monotonic_sample(self):
        samples = iter([100, 130, 281])
        now = lambda: next(samples)
        parent = 280
        first = allocate_deadline(parent, 2, monotonic=now)
        second = allocate_deadline(parent, monotonic=now)
        self.assertEqual(first, 181)
        self.assertEqual(second, 265)
        with self.assertRaisesRegex(ValueError, '^INCOMPLETE scheduled suite: deadline exhausted$'):
            allocate_deadline(parent, monotonic=now)
        self.assertLess(first, parent); self.assertLess(second, parent)


if __name__ == '__main__':
    from suite_adapter import direct_main
    direct_main()
