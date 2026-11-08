#!/usr/bin/env python3
"""Focused process controls for execution budgets and opt-in absolute deadlines."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest

RUNNER = Path(__file__).with_name('bounded_exec.py')


class BudgetControls(unittest.TestCase):
    def run_attempt(self, budget, grace, deadline, command):
        with tempfile.TemporaryDirectory() as scratch:
            status = Path(scratch) / 'status'
            args = [sys.executable, '-I', str(RUNNER), '--timeout', str(budget),
                    '--kill-after', str(grace), '--status', str(status)]
            if deadline is not None:
                args += ['--deadline-mono', str(time.monotonic() + deadline)]
            env = dict(os.environ)
            env.pop('BOUNDED_DEADLINE_MONO', None)
            start = time.monotonic()
            subprocess.run(args + ['--', sys.executable, '-c', command],
                           env=env, check=True, timeout=8)
            return status.read_text().strip(), time.monotonic() - start

    def test_short_execution_long_grace(self):
        # Previously budget .2 expanded to ~3, incorrectly producing exit 0.
        status, _ = self.run_attempt(.2, 5, 4, 'import time; time.sleep(.8)')
        self.assertEqual(status, 'timeout', 'execution budget expanded under deadline')

    def test_cleanup_clamped_independently(self):
        status, elapsed = self.run_attempt(.2, 5, .8,
            'import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); time.sleep(30)')
        self.assertEqual(status, 'timeout', 'stubborn child must time out')
        self.assertLess(elapsed, 1.5, 'cleanup exceeded absolute deadline')

    def test_no_deadline_preserves_budget(self):
        status, _ = self.run_attempt(.2, 5, None, 'import time; time.sleep(.8)')
        self.assertEqual(status, 'timeout', 'legacy execution budget changed')
        status, _ = self.run_attempt(2, 5, None, 'pass')
        self.assertEqual(status, 'exit 0', 'legacy successful attempt changed')


if __name__ == '__main__':
    unittest.main()
