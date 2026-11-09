import runpy
from pathlib import Path
runpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))

"""Exact source/adapter and specification regression controls."""
import json
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
from process_adapter import process_run
from process_coverage import check, ROOT
from runtime_support import runtime_environment
from protocol import clean_env


class ProcessCoverage(unittest.TestCase):
    def test_adopted_sessions_and_exact_plants(self):
        for mode in ('exit', 'cancel'):
            with self.subTest(mode=mode), tempfile.TemporaryDirectory() as scratch:
                directory = Path(scratch)
                helper = ROOT / 'scripts/checks/lib/supervisor_control.py'
                shutil.copy(helper, directory)
                source = (ROOT / 'scripts/checks/lib/bounded_exec.py').read_text()
                for planted in (False, True):
                    text = source
                    if planted:
                        before = ('                self.reap_adopted()' if mode == 'exit'
                                  else '        self.drain_adopted(grace)')
                        self.assertIn(before, text)
                        text = text.replace(before, before.replace('self.reap_adopted()', 'None').replace('self.drain_adopted(grace)', 'None'))
                    (directory / 'bounded_exec.py').write_text(text)
                    result = process_run([sys.executable, str(directory / helper.name), mode],
                                         cwd=ROOT, env=runtime_environment(), timeout=4,
                                         capture_output=True, text=True)
                    diagnostic = ('mid-run orphan not reaped' if mode == 'exit'
                                  else 'cancelled orphan not reaped')
                    expected = (1, '', 'FAIL supervisor orphan: ' + diagnostic + '\n') if planted else (
                        0, 'PASS supervisor orphan: ' + mode + '; tasks returned to baseline\n', '')
                    self.assertEqual((result.returncode, result.stdout, result.stderr), expected)

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
            shutil.copy(ROOT / 'scripts/checks/scheduled_gates.sh', root / 'scripts/checks/scheduled_gates.sh')
            with (root / 'scripts/checks/scheduled/planted.py').open('w') as stream:
                stream.write('from subprocess import run as launch\nlaunch(["true"])\n')
            result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/process_coverage.py'), str(root)],
                                 env=runtime_environment(), capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
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
                                 env=runtime_environment(), capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (1, '', 'FAIL scheduled process boundary: unmanaged launch\n'))

    def test_sandbox_projection_covers_workflows_and_variable_argv(self):
        from python_git_sandbox import scan
        for plant in ('workflow', 'variable argv'):
            with self.subTest(plant=plant), tempfile.TemporaryDirectory() as scratch:
                root = Path(scratch)
                shutil.copytree(ROOT / 'scripts/checks/scheduled', root / 'scripts/checks/scheduled')
                shutil.copytree(ROOT / '.github/workflows', root / '.github/workflows')
                (root / 'scripts/checks/lib').mkdir()
                for relative in ('scripts/checks/lib/scheduled_process.py',
                                 'scripts/checks/lib/gate_manifest.py',
                                 'scripts/checks/scheduled_gates.sh'):
                    shutil.copy(ROOT / relative, root / relative)
                self.assertFalse([row for row in scan(root) if row.startswith('FAIL\t')])
                if plant == 'workflow':
                    path = root / '.github/workflows/gate_freshness.yml'
                    source = path.read_text()
                    before = 'source scripts/checks/lib/scheduled_process.sh'
                    self.assertIn(before, source)
                    path.write_text(source.replace(before, before + '\n          if true; then git status; fi'))
                else:
                    (root / 'scripts/checks/scheduled/planted.py').write_text(
                        'import subprocess\ncommand = ["git", "status"]\nsubprocess.run(command)\n')
                self.assertEqual([row for row in scan(root) if row.startswith('FAIL\t')],
                                 ['FAIL\tscripts/checks/scheduled\tFAIL scheduled process boundary: unmanaged launch'])

    def test_missing_cwd(self):
        result = process_run([sys.executable, str(ROOT / 'scripts/checks/lib/scheduled_process.py'),
                              '--timeout', '1', '--', 'true'], cwd=ROOT,
                             env=runtime_environment(), capture_output=True, text=True, timeout=5)
        self.assertEqual((result.returncode, result.stdout, result.stderr),
                         (1, '', 'FAIL scheduled process specification: missing cwd\n'))

    def test_required_raw_fields_at_production_boundaries(self):
        from scheduled_process import Invocation, run as launch
        valid = dict(cwd=ROOT, env=runtime_environment(), timeout=1)
        for field in ('cwd', 'env', 'timeout'):
            for value in ('omit', None):
                options = dict(valid)
                if value == 'omit': options.pop(field)
                else: options[field] = value
                for adapter in (process_run, launch):
                    with self.subTest(field=field, value=value, adapter=adapter):
                        with self.assertRaisesRegex(ValueError, '^FAIL scheduled process specification: missing ' + field + '$'):
                            adapter(['true'], **options)
        with self.assertRaisesRegex(ValueError, '^FAIL scheduled process specification: missing cwd$'):
            Invocation.capture({})
        for cwd in ('relative', 7):
            with self.assertRaisesRegex(ValueError, '^FAIL scheduled process specification: invalid cwd$'):
                Invocation.capture({}, cwd)

    def test_tool_resolution_uses_declared_cwd(self):
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            (root / 'bin').mkdir()
            tool = root / 'bin/selected'
            tool.write_text('#!/bin/sh\nprintf "declared tool\\n"\n')
            tool.chmod(0o755)
            for command in (['selected'], ['bin/selected']):
                result = process_run(command, cwd=root, env={'PATH': 'bin'}, timeout=2,
                                     capture_output=True, text=True)
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'declared tool\n', ''))

    def test_shell_positions_and_supervisor_bypass(self):
        from process_coverage import check_shell_launches, DIAGNOSTIC
        positions = ['x=$(git status)', 'if true; then git status; fi',
                     'if false; then :; else python3 x; fi',
                     'while true; do sh x; done', 'true && env git status',
                     'false || bash x', 'echo x | nix x', '{ git status; }',
                     'if true; then "$@"; fi', 'exec "$@"', '( "$@" )',
                     'if "$@"; then :; fi', 'git', '"/usr/bin/git" status',
                     'if "$tool"; then :; fi', '[[ "$(git status)" = yes ]]']
        for script in positions:
            with self.subTest(script=script):
                with self.assertRaisesRegex(ValueError, '^' + DIAGNOSTIC + '$'):
                    check_shell_launches(script)
        with tempfile.TemporaryDirectory() as scratch:
            path = Path(scratch) / 'planted.py'
            path.write_text('from bounded_exec import supervise as launch\nlaunch(["git"], 1, 0)\n')
            from process_coverage import check_python_launches
            with self.assertRaisesRegex(ValueError, '^' + DIAGNOSTIC + '$'):
                check_python_launches(path)

    def test_scheduled_retry_status_has_one_owner(self):
        cases = [
            ('bounded_run 0.03 -- python3 -c "import time; time.sleep(2)"',
             '124 incomplete incomplete'),
            ('bounded_run 1 -- /nonexistent/scheduled-process-control',
             '124 incomplete runner-failed'),
            ("bounded_run 0.1 -- bash -c 'if [ ! -e \"$1/attempt\" ]; then touch \"$1/attempt\"; sleep 2; fi' bash \"$2\"",
             '0 done recovered'),
        ]
        for command, expected in cases:
            with self.subTest(expected=expected), tempfile.TemporaryDirectory() as scratch:
                script = ('source "$1"; ' + command + '; rc=$?; '
                          'printf "%s %s %s\\n" "$rc" "$(cat "$BOUNDED_OUTCOME_FILE")" "$(cut -d " " -f1 "$BOUNDED_LOG")"; '
                          'rm -rf "$BOUNDED_DIR"')
                result = process_run(['bash', '-c', script, 'bash',
                                      str(ROOT / 'scripts/checks/lib/scheduled_process.sh'), scratch],
                                     cwd=ROOT, env=runtime_environment(), timeout=5,
                                     capture_output=True, text=True)
                self.assertEqual((result.returncode, result.stdout.strip()), (0, expected))
                diagnostic = ('bounded_run: the runner reported no outcome for: /nonexistent/scheduled-process-control\n'
                              if expected.endswith('runner-failed') else '')
                self.assertEqual(result.stderr, diagnostic)

    def test_explicit_input_replays_through_scheduled_retry(self):
        script = r'''source "$1"
printf 'declared payload' > "$2/payload"
bounded_run -i "$2/payload" 0.05 -- bash -c 'cat > "$1/observed"; if [ ! -e "$1/attempt" ]; then touch "$1/attempt"; sleep 2; fi; cat "$1/observed"' bash "$2"
rc=$?
printf '\n%s %s %s\n' "$rc" "$(cat "$BOUNDED_OUTCOME_FILE")" "$(cut -d ' ' -f1 "$BOUNDED_LOG")"
rm -rf "$BOUNDED_DIR"
'''
        with tempfile.TemporaryDirectory() as scratch:
            result = process_run(['bash', '-c', script, 'bash',
                                  str(ROOT / 'scripts/checks/lib/scheduled_process.sh'), scratch],
                                 cwd=ROOT, env=runtime_environment(), timeout=5,
                                 capture_output=True, text=True)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (0, 'declared payload\n0 done recovered\n', ''))


class ProcessStdin(unittest.TestCase):
    def test_production_and_planted_inheritance(self):
        with tempfile.TemporaryDirectory() as scratch:
            root = Path(scratch)
            helper = ROOT / 'scripts/checks/lib/scheduled_process.py'
            command = [sys.executable, str(ROOT / 'scripts/checks/scheduled/stdin_control.py')]
            good = root / 'good'; good.mkdir()
            result = process_run([*command, str(helper), str(good)], env=runtime_environment(),
                                 capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, '', ''))
            planted = root / 'scheduled_process.py'
            source = helper.read_text()
            self.assertEqual(source.count("stdin=source,"), 1)
            planted.write_text(source.replace('stdin=source,', 'stdin=None,'))
            shutil.copy(ROOT / 'scripts/checks/lib/bounded_exec.py', root)
            bad = root / 'bad'; bad.mkdir()
            result = process_run([*command, str(planted), str(bad)], env=runtime_environment(),
                                 capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
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
                                 capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
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
                result = adapter(command, env=runtime_environment(), capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'EOF\n', ''))
            # Keep the real stdin-sensitive Git operation; write-tree would hide the defect.
            with tempfile.TemporaryDirectory() as scratch:
                git('-C', scratch, 'init', '-q')
                self.assertEqual(git('-C', scratch, 'mktree'), '4b825dc642cb6eb9a060e54bf8d69288fbee4904')
            adapters = [
                [sys.executable, '-I', str(ROOT / 'scripts/checks/lib/scheduled_process.py'), '--cwd', str(ROOT), '--timeout', '3', '--'],
                ['bash', '-c', 'source "$1"; bounded_run 3 -- "${@:2}"', 'bash', str(ROOT / 'scripts/checks/lib/scheduled_process.sh')],
                ['bash', str(ROOT / 'scripts/checks/scheduled/with_slot.sh'), '3'],
                ['bash', str(ROOT / 'scripts/checks/lib/bounded.sh'), '3'],
            ]
            for index, argv in enumerate(adapters):
                with tempfile.TemporaryDirectory() as scratch:
                    pool = Path(scratch) / 'pool'; pool.mkdir()
                    for n in (1, 2): (pool / f'slot-{n}').touch()
                    result = process_run([sys.executable, str(ROOT / 'scripts/checks/scheduled/stdin_control.py'),
                                          'unused', scratch, *argv], cwd=ROOT,
                                         env=dict(runtime_environment(), CONCRETE_HEAVY_SLOT_DIR=str(pool)),
                                         capture_output=True, text=True, timeout=5)
                    expected = (1, '', 'FAIL scheduled process stdin: inherited open pipe\n') if index == 3 else (0, '', '')
                    self.assertEqual((result.returncode, result.stdout, result.stderr), expected)
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
                                          str(planted), scratch], capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3], env=runtime_environment())
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
                                     env=clean_env(env), role=role, capture_output=True, text=True, timeout=3, cwd=Path(__file__).resolve().parents[3])
                self.assertEqual((result.returncode, result.stdout, result.stderr), (1, '', ''))
            # The trusted publisher declares its checkout transport locally (as
            # actions/checkout does). Private global config must not erase it.
            for key, expected in (('remote.origin.url', 'https://github.com/example/repo'),
                                  ('http.https://github.com/.extraheader', 'AUTHORIZATION: control-transport')):
                result = process_run(['git', '-C', str(repo), 'config', '--get', key], role='publisher',
                                     env=clean_env(env), capture_output=True, text=True, timeout=3, cwd=Path(__file__).resolve().parents[3])
                self.assertEqual((result.returncode, result.stdout, result.stderr), (0, expected + '\n', ''))
            result = process_run(['git', '-C', str(repo), '-c', 'user.name=Control', '-c',
                                  'user.email=control@example.invalid', 'commit', '--allow-empty', '-qm', 'control'],
                                 env=clean_env(env), role='publisher', capture_output=True, text=True, timeout=3, cwd=Path(__file__).resolve().parents[3])
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, '', ''))
            self.assertFalse(marker.exists())
            remote = root / 'declared-transport.git'
            remote.mkdir()
            git('-C', str(remote), 'init', '--bare', '-q')
            git('-C', str(repo), 'config', 'remote.origin.url', remote.as_uri())
            with config.open('a') as stream:
                stream.write('[url "file:///nonexistent/hostile-transport/"]\n insteadOf = file://\n')
            result = process_run(['git', '-C', str(repo), 'ls-remote', 'origin'],
                                 env=clean_env(env), role='publisher', capture_output=True, text=True, timeout=3, cwd=Path(__file__).resolve().parents[3])
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
                result = process_run(argv, role=role, env=clean_env(env), capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
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
                                 capture_output=True, text=True, timeout=3, cwd=Path(__file__).resolve().parents[3], env=runtime_environment())
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
                                      str(planted)], capture_output=True, text=True, timeout=3, cwd=Path(__file__).resolve().parents[3], env=runtime_environment())
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
        import os
        import subprocess
        import time
        with tempfile.TemporaryDirectory() as scratch:
            marker = Path(scratch) / 'started'
            began = time.monotonic()
            outcome = bounded_exec.supervise(
                [sys.executable, '-c', 'import time; time.sleep(5)'], 10, .05,
                deadline=began + .1, on_started=lambda pid: marker.touch(),
                env=runtime_environment())
            self.assertEqual(outcome, 'timeout')
            self.assertTrue(marker.exists())
            self.assertLess(time.monotonic() - began, 1)


if __name__ == "__main__":
    from suite_adapter import direct_main
    direct_main()
