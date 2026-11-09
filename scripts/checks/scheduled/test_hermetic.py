from runtime_support import runtime_environment
"""The environment control must detect changed outcomes, not merely two exits."""
from process_adapter import process_run
import unittest
from hermetic_suite import compare, compare_canary, environments, PREFIXES


class Hermeticity(unittest.TestCase):
    def test_planted_result_drift(self):
        with self.assertRaisesRegex(ValueError, '^FAIL scheduled hermeticity: clean and hostile test results differ$'):
            compare([['planted', 'PASS']], [['planted', 'FAIL']])

    def test_foreign_namespace_inventory(self):
        clean, hostile = environments({'GITHUB_NEW_KEY': 'caller', 'PATH': '/runtime'})
        self.assertEqual(clean, {'PATH': '/runtime'})
        self.assertEqual(hostile['GITHUB_NEW_KEY'], 'foreign/GITHUB_NEW_KEY')
        for key in ('GITHUB_REPOSITORY', 'GITHUB_RUN_ID', 'GITHUB_RUN_ATTEMPT',
                    'SELECTED_SHA', 'SELECTED_TIER', 'SCHEDULED_ATTEMPT_ID',
                    'SCHEDULED_OUTCOME_FILE', 'RUNNER_NAME', 'RUNNER_ENVIRONMENT'):
            self.assertEqual(hostile[key], 'foreign/' + key)
        for prefix in PREFIXES:
            self.assertEqual(hostile[prefix + 'FOREIGN_CONTROL'], 'foreign/' + prefix + 'FOREIGN_CONTROL')

    def test_entry_environment_preserves_selected_git(self):
        import os
        from pathlib import Path
        import subprocess
        import sys
        import tempfile
        with tempfile.TemporaryDirectory() as scratch:
            shim = Path(scratch) / 'git'
            shim.write_text('#!/bin/sh\nprintf "selected gate git\\n"\n')
            shim.chmod(0o755)
            environment = dict(os.environ, PATH=scratch + os.pathsep + os.environ['PATH'],
                               GIT_DIR='/foreign', SELECTED_SHA='foreign')
            program = ("import sys; sys.path.insert(0, sys.argv[1]); import protocol; "
                       "assert not hasattr(protocol, 'GIT_ENVIRONMENT'); "
                       "assert sys.stdin.buffer.read() == b''; "
                       "print(protocol.git('--version'))")
            result = process_run([sys.executable, '-I', '-c', program, str(Path(__file__).resolve().parent)],
                                    env=environment, text=True, capture_output=True, timeout=10, check=True, cwd=Path(__file__).resolve().parents[3])
            self.assertEqual(result.stdout, 'selected gate git\n')

    def test_planted_canary_index_corruption(self):
        with self.assertRaisesRegex(ValueError, '^FAIL scheduled hermeticity: canary repository changed$'):
            compare_canary({'.git/index': b'original'}, {'.git/index': b'corrupted'})


if __name__ == "__main__":
    from suite_adapter import direct_main
    direct_main()
