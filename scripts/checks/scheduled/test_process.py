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
            shutil.copy(ROOT / 'scripts/checks/lib/scheduled_process.py', root / 'scripts/checks/lib')
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
            shutil.copy(ROOT / 'scripts/checks/lib/scheduled_process.py', root / 'scripts/checks/lib')
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


class ProcessContext(unittest.TestCase):
    def test_context_and_exact_plants(self):
        helper = ROOT / 'scripts/checks/lib/scheduled_process.py'
        source = helper.read_text()
        plants = {
            'env.SCHEDULER_FOREIGN': ("yield environment", "environment['SCHEDULER_FOREIGN'] = 'planted'\n        yield environment"),
            'cwd': ("cwd=spec['cwd'],", "cwd='/',"),
            'HOME': ("HOME=str(home)", "HOME='/'"),
            'umask': ('umask=0o077,', 'umask=0o022,'),
            'locale': ("LC_ALL='C'", "LC_ALL='POSIX'"),
            'timezone': ("TZ='UTC'", "TZ='Pacific/Honolulu'"),
            'temp': ("TMPDIR=str(temporary)", "TMPDIR='/'"),
            'fd.9': ('pass_fds=slot_descriptors(environment),',
                     "pass_fds=(9,),"),
            'credential scope': ("yield environment", "environment['GH_TOKEN'] = 'publisher-control-token'\n        yield environment"),
        }
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            shutil.copy(ROOT / 'scripts/checks/lib/bounded_exec.py', root)
            for field in [None, *plants]:
                with self.subTest(field=field):
                    planted = root / 'scheduled_process.py'
                    text = source
                    if field:
                        before, after = plants[field]
                        self.assertEqual(text.count(before), 1)
                        text = text.replace(before, after)
                    if field == 'fd.9':
                        # Plant a real unrelated descriptor, not a made-up report.
                        text = text.replace('outcome = supervise(argv,',
                            "os.dup2(source.fileno(), 9)\n        outcome = supervise(argv,")
                    planted.write_text(text)
                    result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/context_control.py'),
                                          str(planted), scratch], capture_output=True, text=True, timeout=5)
                    expected = (1, '', 'FAIL scheduled process context: ' + field + '\n') if field else (0, '', '')
                    self.assertEqual((result.returncode, result.stdout, result.stderr), expected)

    def test_same_probe_through_adapters(self):
        import os
        import run
        from context_control import PROBE, detect
        from test_slot import Slot
        from protocol import _registry
        from unittest.mock import patch
        command = [sys.executable, '-I', '-c', PROBE]
        reader, writer = os.pipe()
        previous = os.dup(9) if Path('/proc/self/fd/9').exists() else None
        try:
            os.dup2(reader, 9)
            environment = dict(runtime_environment(), SCHEDULER_FOREIGN='planted',
                               GH_TOKEN='publisher-control-token', HOME='/', TMPDIR='/',
                               LC_ALL='POSIX', TZ='Pacific/Honolulu')
            for adapter in (process_run, run.call):
                result = adapter(command, cwd=ROOT, env=environment,
                                 capture_output=True, text=True, timeout=5)
                self.assertEqual((result.returncode, result.stderr), (0, ''))
                detect(json.loads(result.stdout), ROOT)
            for argv in (
                [sys.executable, '-I', str(ROOT / 'scripts/checks/lib/scheduled_process.py'), '--cwd', str(ROOT), '--timeout', '3', '--', *command],
                ['bash', '-c', 'source "$1"; bounded_run 3 -- "${@:2}"', 'bash', str(ROOT / 'scripts/checks/lib/scheduled_process.sh'), *command],
            ):
                result = process_run(argv, cwd=ROOT, env=environment, capture_output=True, text=True, timeout=5)
                self.assertEqual((result.returncode, result.stderr), (0, ''))
                detect(json.loads(result.stdout), ROOT)
            # Slot.invoke owns a separate cwd; have the child report its explicit cwd.
            result = Slot().invoke(command)
            self.assertEqual((result.returncode, result.stderr), (0, ''))
            observed = json.loads(result.stdout)
            self.assertTrue(observed['slot_valid'])
            detect(observed, observed['cwd'])
            with tempfile.TemporaryDirectory() as scratch:
                report = Path(scratch) / 'observation.json'
                program = ('import contextlib,sys; from pathlib import Path\n'
                           'with Path(sys.argv[1]).open("w") as f, contextlib.redirect_stdout(f):\n' +
                           '\n'.join(' ' + line for line in PROBE.splitlines()))
                row = dict(id='context-probe', command=[sys.executable, '-c', program, str(report)], budget_seconds=3, artifacts=[])
                with patch.object(_registry, 'tier_rows', return_value=[row]):
                    self.assertEqual(_registry.execute('context-probe', 'daily', environment), 0)
                detect(json.loads(report.read_text()), ROOT)
        finally:
            if previous is not None:
                os.dup2(previous, 9); os.close(previous)
            else:
                os.close(9)
            os.close(reader); os.close(writer)

    def test_gate_credentials_are_removed_at_selection(self):
        from run import gate_environment
        self.assertFalse({'GH_TOKEN', 'GITHUB_TOKEN'} & gate_environment('a'*40, 'daily', '1/1',
                         {'GH_TOKEN': 'secret', 'GITHUB_TOKEN': 'secret'}).keys())


    def test_private_git_config_hooks_and_publisher_transport(self):
        from protocol import git
        import os
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            home = root / 'hostile-home'; home.mkdir()
            hooks = root / 'hooks'; hooks.mkdir()
            marker = root / 'hook-ran'
            hook = hooks / 'pre-commit'
            hook.write_text('#!/bin/sh\ntouch "' + str(marker) + '"\nexit 73\n'); hook.chmod(0o755)
            config = home / '.gitconfig'
            config.write_text('[scheduled]\n foreign = global-secret\n[core]\n hooksPath = ' + str(hooks) + '\n')
            env = dict(runtime_environment(), HOME=str(home), GIT_CONFIG_GLOBAL=str(config),
                       GIT_CONFIG_COUNT='1', GIT_CONFIG_KEY_0='scheduled.foreign', GIT_CONFIG_VALUE_0='injected')
            repo = root / 'repo'; repo.mkdir()
            git('-C', str(repo), 'init', '-q')
            git('-C', str(repo), 'config', 'core.hooksPath', str(hooks))
            git('-C', str(repo), 'config', 'remote.origin.url', 'https://github.com/example/repo')
            git('-C', str(repo), 'config', 'http.https://github.com/.extraheader', 'AUTHORIZATION: control-transport')
            for role in ('gate', 'publisher'):
                result = process_run(['git', '-C', str(repo), 'config', '--get', 'scheduled.foreign'],
                                     env=env, role=role, capture_output=True, text=True, timeout=3)
                self.assertEqual((result.returncode, result.stdout, result.stderr), (1, '', ''))
            # The trusted publisher declares its checkout transport locally (as
            # actions/checkout does). Private global config must not erase it.
            for key, expected in (('remote.origin.url', 'https://github.com/example/repo'),
                                  ('http.https://github.com/.extraheader', 'AUTHORIZATION: control-transport')):
                result = process_run(['git', '-C', str(repo), 'config', '--get', key], role='publisher',
                                     env=env, capture_output=True, text=True, timeout=3)
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, expected + '\n', ''))
            result = process_run(['git', '-C', str(repo), '-c', 'user.name=Control', '-c',
                                  'user.email=control@example.invalid', 'commit', '--allow-empty', '-qm', 'control'],
                                 env=env, role='publisher', capture_output=True, text=True, timeout=3)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, '', ''))
            self.assertFalse(marker.exists())
            remote = root / 'declared-transport.git'
            remote.mkdir()
            git('-C', str(remote), 'init', '--bare', '-q')
            git('-C', str(repo), 'config', 'remote.origin.url', remote.as_uri())
            with config.open('a') as stream:
                stream.write('[url "file:///nonexistent/hostile-transport/"]\n insteadOf = file://\n')
            result = process_run(['git', '-C', str(repo), 'ls-remote', 'origin'],
                                 env=env, role='publisher', capture_output=True, text=True, timeout=3)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, '', ''))

    def test_publisher_credentials_require_explicit_role(self):
        import os
        command = [sys.executable, '-c', 'import os; print(os.environ.get("GH_TOKEN", "absent"))']
        env = dict(runtime_environment(), GH_TOKEN='publisher-control-token')
        for role, expected in (('gate', 'absent'), ('publisher', 'publisher-control-token')):
            adapters = [command,
                [sys.executable, '-I', str(ROOT / 'scripts/checks/lib/scheduled_process.py'),
                 '--cwd', str(ROOT), '--timeout', '3', '--role', role, '--', *command],
                ['bash', '-c', 'source "$1"; bounded_run 3 --role "$2" -- "${@:3}"',
                 'bash', str(ROOT / 'scripts/checks/lib/scheduled_process.sh'), role, *command]]
            for argv in adapters:
                result = process_run(argv, role=role, env=env, capture_output=True, text=True, timeout=5)
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, expected + '\n', ''))


    def test_forged_slot_exact_helper_diagnostic(self):
        helper = ROOT / 'scripts/checks/lib/scheduled_process.py'
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            for number in (1, 2):
                (root / ('slot-' + str(number))).touch()
            (root / 'unrelated').touch()
            script = ('exec 9<>"$1/unrelated"; export CONCRETE_HEAVY_SLOT_DIR="$1" '
                      'CONCRETE_HEAVY_SLOT_FD=9; exec "$2" -I "$3" --cwd "$1" '
                      '--timeout 2 -- true')
            result = process_run(['bash', '-c', script, 'bash', scratch, sys.executable, str(helper)],
                                 capture_output=True, text=True, timeout=3)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (1, '', 'INCOMPLETE heavy slots: invalid inherited slot descriptor\n'))


class ProcessClock(unittest.TestCase):
    def test_single_observation_and_second_read_plant(self):
        helper = ROOT / 'scripts/checks/lib/scheduled_process.py'
        source = helper.read_text()
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            shutil.copy(ROOT / 'scripts/checks/lib/bounded_exec.py', root)
            planted = root / 'scheduled_process.py'
            for plant in (False, True):
                text = source
                if plant:
                    self.assertEqual(text.count('observed = source.read()'), 1)
                    text = text.replace('observed = source.read()', 'observed = source.read(); source.read()')
                planted.write_text(text)
                result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/clock_control.py'),
                                      str(planted)], capture_output=True, text=True, timeout=3)
                expected = (1, '', 'FAIL scheduled clock: undeclared read\n') if plant else (0, '', '')
                self.assertEqual((result.returncode, result.stdout, result.stderr), expected)

    def test_no_unmediated_wall_clock_reads(self):
        from process_coverage import check_clock_reads
        check_clock_reads(ROOT)
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            shutil.copytree(ROOT / 'scripts/checks/scheduled', root / 'scripts/checks/scheduled')
            (root / 'scripts/checks/lib').mkdir()
            shutil.copy(ROOT / 'scripts/checks/lib/scheduled_process.py', root / 'scripts/checks/lib')
            target = root / 'scripts/checks/scheduled/observe.py'
            target.write_text(target.read_text() + '\nfrom time import time as second_read\nsecond_read()\n')
            with self.assertRaisesRegex(ValueError, '^FAIL scheduled clock: undeclared read$'):
                check_clock_reads(root)

    def test_deadline_does_not_restart_at_execution(self):
        import subprocess
        import scheduled_process as owner
        from unittest.mock import patch
        context = owner.Invocation.capture(runtime_environment(), ROOT)
        with patch.object(owner.time, 'monotonic', return_value=100):
            spec = context.specification(['true'], timeout=10)
        self.assertEqual(spec['deadline'], 110)
        with patch.object(owner.time, 'monotonic', return_value=107), \
             patch.object(owner, 'supervise', return_value='exit 0') as supervise:
            self.assertEqual(owner.execute(spec).returncode, 0)
            self.assertEqual(supervise.call_args.args[1], 3)
        with patch.object(owner.time, 'monotonic', return_value=111), \
             patch.object(owner, 'supervise') as supervise:
            with self.assertRaises(subprocess.TimeoutExpired):
                owner.execute(spec)
            supervise.assert_not_called()


    def test_supervisor_charges_startup_to_declared_deadline(self):
        import bounded_exec
        from unittest.mock import Mock, patch
        ticks = [100]
        child = Mock(pid=12345)
        child.wait.return_value = 0
        def started(pid):
            self.assertEqual(pid, 12345)
            ticks[0] = 111
        with patch.object(bounded_exec.subprocess, 'Popen', return_value=child), \
             patch.object(bounded_exec.time, 'monotonic', side_effect=lambda: ticks[0]):
            self.assertEqual(bounded_exec.supervise(['true'], 10, 0, deadline=110, on_started=started), 'exit 0')
        child.wait.assert_called_once_with(timeout=0)
