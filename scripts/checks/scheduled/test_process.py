"""Exact source/adapter and specification regression controls."""
import json
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
from process_adapter import process_run
from process_coverage import check, ROOT
from test_support import runtime_environment


class ProcessCoverage(unittest.TestCase):
    def test_census(self):
        check(ROOT)

    def test_unmanaged_launch(self):
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            shutil.copytree(ROOT / 'scripts/checks/scheduled', root / 'scripts/checks/scheduled')
            shutil.copytree(ROOT / '.github/workflows', root / '.github/workflows')
            (root / 'scripts/checks/lib').mkdir()
            shutil.copy(ROOT / 'scripts/checks/lib/gate_manifest.py', root / 'scripts/checks/lib')
            with (root / 'scripts/checks/scheduled/planted.py').open('w') as stream:
                stream.write('from subprocess import run as launch\nlaunch(["true"])\n')
            result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/process_coverage.py'), str(root)],
                                 env=runtime_environment(), capture_output=True, text=True, timeout=5)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (1, '', 'FAIL scheduled process boundary: unmanaged launch\n'))

    def test_unmanaged_shell_adapter(self):
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            shutil.copytree(ROOT / 'scripts/checks/scheduled', root / 'scripts/checks/scheduled')
            shutil.copytree(ROOT / '.github/workflows', root / '.github/workflows')
            (root / 'scripts/checks/lib').mkdir()
            for relative in ('scripts/checks/lib/gate_manifest.py', 'scripts/checks/scheduled_gates.sh'):
                shutil.copy(ROOT / relative, root / relative)
            with (root / 'scripts/checks/scheduled/with_slot.sh').open('a') as stream:
                stream.write('python3 -c "pass"\n')
            result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/process_coverage.py'), str(root)],
                                 env=runtime_environment(), capture_output=True, text=True, timeout=5)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (1, '', 'FAIL scheduled process boundary: unmanaged launch\n'))

    def test_missing_cwd(self):
        with tempfile.TemporaryDirectory() as scratch:
            spec = Path(scratch) / 'spec.json'
            spec.write_text(json.dumps(dict(argv=['true'], env={}, timeout=1)))
            result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/process_coverage.py'), '--spec', str(spec)],
                                 env=runtime_environment(), capture_output=True, text=True, timeout=5)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (1, '', 'FAIL scheduled process specification: missing cwd\n'))


class ProcessStdin(unittest.TestCase):
    def test_production_and_planted_inheritance(self):
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            helper = ROOT / 'scripts/checks/lib/scheduled_process.py'
            command = [sys.executable, str(ROOT / 'scripts/checks/scheduled/stdin_control.py')]
            good = root / 'good'; good.mkdir()
            result = process_run([*command, str(helper), str(good)], env=runtime_environment(),
                                 capture_output=True, text=True, timeout=5)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, '', ''))
            planted = root / 'scheduled_process.py'
            source = helper.read_text()
            self.assertEqual(source.count("stdin=source,"), 1)
            planted.write_text(source.replace('stdin=source,', 'stdin=None,'))
            shutil.copy(ROOT / 'scripts/checks/lib/bounded_exec.py', root)
            bad = root / 'bad'; bad.mkdir()
            result = process_run([*command, str(planted), str(bad)], env=runtime_environment(),
                                 capture_output=True, text=True, timeout=5)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (1, '', 'FAIL scheduled process stdin: inherited open pipe\n'))

    def test_unrelated_timeout_is_incomplete(self):
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            source = (ROOT / 'scripts/checks/lib/scheduled_process.py').read_text()
            planted = root / 'scheduled_process.py'
            planted.write_text(source.replace("outcome = supervise(argv,", "argv = [sys.executable, '-c', 'import time; time.sleep(5)']\n        outcome = supervise(argv,"))
            shutil.copy(ROOT / 'scripts/checks/lib/bounded_exec.py', root)
            result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/stdin_control.py'),
                                  str(planted), str(root)], env=runtime_environment(),
                                 capture_output=True, text=True, timeout=5)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (124, '', 'INCOMPLETE scheduled process stdin: unproven timeout\n'))

    def test_eof_through_adapters(self):
        import os
        import run
        from protocol import git, _registry
        from unittest.mock import patch
        from test_slot import Slot
        reader, writer = os.pipe()
        previous = os.dup(0)
        try:
            os.dup2(reader, 0)
            command = [sys.executable, '-c', 'import sys; assert sys.stdin.buffer.read() == b""; print("EOF")']
            for adapter in (process_run, run.call):
                result = adapter(command, env=runtime_environment(), capture_output=True, text=True, timeout=5)
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'EOF\n', ''))
            # Keep the real stdin-sensitive Git operation; write-tree would hide the defect.
            with tempfile.TemporaryDirectory() as scratch:
                git('-C', scratch, 'init', '-q')
                self.assertEqual(git('-C', scratch, 'mktree'), '4b825dc642cb6eb9a060e54bf8d69288fbee4904')
            for argv in (
                [sys.executable, '-I', str(ROOT / 'scripts/checks/lib/scheduled_process.py'), '--cwd', str(ROOT), '--timeout', '3', '--', *command],
                ['bash', '-c', 'source "$1"; bounded_run 3 -- "${@:2}"', 'bash', str(ROOT / 'scripts/checks/lib/scheduled_process.sh'), *command],
            ):
                result = process_run(argv, env=runtime_environment(), capture_output=True, text=True, timeout=5)
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'EOF\n', ''))
            result = Slot().invoke(command)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'EOF\n', ''))
            row = dict(id='stdin-probe', command=command, budget_seconds=3, artifacts=[])
            with patch.object(_registry, 'tier_rows', return_value=[row]):
                self.assertEqual(_registry.execute('stdin-probe', 'daily', runtime_environment()), 0)
        finally:
            os.dup2(previous, 0)
            os.close(previous); os.close(reader); os.close(writer)
