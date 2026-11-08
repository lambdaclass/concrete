"""Cheap controls execute production orchestration with native toolchain stubs."""
import pathlib
import subprocess
import tempfile
import unittest

SOURCE = pathlib.Path(__file__).with_name('spec_diff.sh').read_text()

def function(name):
    start = SOURCE.index('\n' + name + '()') + 1
    end = SOURCE.index('\n}\n', start) + 3
    return SOURCE[start:end]

class Orchestration(unittest.TestCase):
    def run_shell(self, body, expected, diagnostic, check_fn=None):
        with tempfile.TemporaryDirectory() as tmp:
            work = pathlib.Path(tmp) / 'work'
            regressions = pathlib.Path(tmp) / 'regressions'
            work.mkdir()
            functions = '\n'.join(function(n) for n in (
                'parse_replay', 'replay_command', 'persist_candidate_bundle',
                'record_candidate_failure', 'report_mismatch', 'judge_evaluator',
                'native_one', 'absorb_native_result', 'collect_available',
                'finish_diff', 'run_control'))
            setup = f'''set -euo pipefail
work={work}
seed=test
regressions={regressions}
replay=""
fail=0; deadline_stopped=0; min_candidate_secs=1
BOUNDED_LOG=$work/bounded.log; : >"$BOUNDED_LOG"
PLANTED_FAIL_RC=99
remaining() {{ echo 100; }}
bounded_summary() {{ :; }}
judge() {{ cmp -s "$1/status" "$2.status"; }}
summary_value() {{ echo 1; }}
record_judged() {{ :; }}
{functions}
'''
            result = subprocess.run(['bash', '-c', setup + body], text=True,
                                    capture_output=True, timeout=5)
            self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
            diagnostics = diagnostic if isinstance(diagnostic, tuple) else (diagnostic,)
            for line in diagnostics:
                self.assertIn(line, (result.stdout + result.stderr).splitlines())
            if check_fn is not None:
                check_fn(work, regressions)

    def assert_bundle(self, regressions, leg, seed, index, kind, diag_lines, artifacts=()):
        bundle = regressions / leg / f'{seed}-{index}'
        self.assertTrue(bundle.is_dir(), f'missing bundle {bundle}')
        self.assertEqual((bundle / 'leg').read_text().strip(), leg)
        self.assertEqual((bundle / 'seed').read_text().strip(), seed)
        self.assertEqual((bundle / 'index').read_text().strip(), str(index))
        self.assertIn(kind, (bundle / 'failure-kinds.txt').read_text().splitlines())
        records = (bundle / 'failure-record.tsv').read_text().splitlines()
        self.assertTrue(any(r.startswith(f'{kind}\t') for r in records), records)
        saved_diags = (bundle / 'diagnostics.txt').read_text().splitlines()
        for line in diag_lines:
            self.assertIn(line, saved_diags)
        replay_sh = (bundle / 'replay.sh').read_text().strip()
        self.assertTrue(replay_sh.startswith('SPEC_DIFF_REPLAY='), replay_sh)
        for art in artifacts:
            self.assertTrue((bundle / art).exists(), f'missing {art} in {bundle}')

    def test_build_failure_both_loops(self):
        start = SOURCE.index('bir_accepted=$(summary_value')
        end = SOURCE.index('\nprint_judged_counts bir', start)
        for mode in ('build', 'missing_eval', 'missing_native_ready'):
            with self.subTest(mode=mode):
                expected_diags = (
                    ('FAIL bir-0 [lcb_eval]: missing evaluator observation',
                     'FAIL spec-0 [lcb_eval]: missing evaluator observation')
                    if mode == 'missing_eval' else
                    ('planted compiler error', 'FAIL bir-0: native build/setup failed',
                     'FAIL spec leg: compiled program lacks native-ready marker')
                    if mode == 'missing_native_ready' else
                    ('planted compiler error', 'FAIL bir-0: native build/setup failed',
                     'FAIL spec-0: native build/setup failed')
                )
                def check(work, regressions, mode=mode):
                    self.assertFalse(work.exists(), 'finish_diff did not remove scratch work')
                    if mode == 'missing_eval':
                        for leg in ('bir', 'spec'):
                            self.assert_bundle(
                                regressions, leg, 'test', 0, 'native_setup',
                                (f'FAIL {leg}-0 [lcb_eval]: missing evaluator observation',),
                                ('program.lean.txt',))
                    elif mode == 'missing_native_ready':
                        self.assert_bundle(
                            regressions, 'bir', 'test', 0, 'native_compilation',
                            ('FAIL bir-0: native build/setup failed', 'planted compiler error'),
                            ('program.lean.txt', 'build.log'))
                        self.assert_bundle(
                            regressions, 'spec', 'test', 0, 'native_setup',
                            ('FAIL spec leg: compiled program lacks native-ready marker',),
                            ('program.lean.txt', 'program.con'))
                    else:
                        for leg in ('bir', 'spec'):
                            self.assert_bundle(
                                regressions, leg, 'test', 0, 'native_compilation',
                                (f'FAIL {leg}-0: native build/setup failed', 'planted compiler error'),
                                ('program.lean.txt', 'build.log'))

                self.run_shell('''
trap 'finish_diff $?' EXIT
for leg in bir spec; do
 mkdir -p "$work/$leg/p0"
 printf 'program\\t0\\texit\\ttrue\\n' >"$work/$leg/summary.tsv"
 echo 0 >"$work/$leg/p0/status"
 : >"$work/$leg/p0/stderr"
 echo "term-$leg" >"$work/$leg/p0/program.lean.txt"
 [ "$leg" != spec ] || echo "fn main() -> u8 { return 0; }" >"$work/$leg/p0/program.con"
''' + ('' if mode == 'missing_native_ready' else ' touch "$work/$leg/p0/native-ready"\n') + (
    ' [ "$leg" != bir ] || touch "$work/bir/p0/native-ready"\n' if mode == 'missing_native_ready' else ''
) + ('' if mode == 'missing_eval' else ' echo 0 >"$work/$leg/p0/lcb_eval.status"\n : >"$work/$leg/p0/lcb_eval.stderr"\n : >"$work/$leg/p0/lcb_eval.stdout"\n') + '''done
native_build() { echo 'planted compiler error'; return 7; }
''' + SOURCE[start:end] + '''
[ "$fail" -eq 1 ] && [ "$deadline_stopped" -eq 0 ] || exit 9
exit "$fail"
''', 1, expected_diags, check)

    def test_failure_log_cannot_claim_timeout(self):
        self.run_shell('''
control_error() { echo 'INCOMPLETE: planted misleading compiler output'; return 1; }
run_control error '^unused$' control_error
[ "$fail" -eq 1 ] && [ "$deadline_stopped" -eq 0 ] || exit 9
exit "$fail"
''', 1, "INCOMPLETE: planted misleading compiler output")

    def test_observer_setup_failure(self):
        def check(work, regressions):
            self.assertFalse(work.exists())
            self.assert_bundle(
                regressions, 'bir', 'test', 0, 'observation',
                ('FAIL bir seed test index 0 [llvm]: observation failed',
                 'FAIL planted observer setup'),
                ('program.lean.txt', 'llvm.obs.log', 'lcb.obs.log'))

        self.run_shell('''
trap 'finish_diff $?' EXIT
mkdir -p "$work/bir/p0"
echo 0 >"$work/bir/p0/status"
: >"$work/bir/p0/stderr"
echo 0 >"$work/bir/p0/lcb_eval.status"
: >"$work/bir/p0/lcb_eval.stderr"
: >"$work/bir/p0/lcb_eval.stdout"
echo "term-bir" >"$work/bir/p0/program.lean.txt"
native_build() { return 0; }
observe_remaining() { echo 'FAIL planted observer setup' >&2; return 4; }
rc=0
native_one bir test 0 "$work/bir/p0" || rc=$?
absorb_native_result "$rc"
[ "$fail" -eq 1 ] && [ "$native_complete" -eq 0 ] && [ "$deadline_stopped" -eq 0 ] || exit 9
exit "$fail"
''', 1, ('FAIL planted observer setup',
         'FAIL bir seed test index 0 [llvm]: observation failed'), check)

    def test_collector_failure_beats_control_timeout(self):
        for kind in ('mismatch', 'lowering', 'emission', 'assembly'):
            with self.subTest(kind=kind):
                leg = 'spec' if kind == 'lowering' else 'bir'
                diag = (
                    'FAIL bir seed test index 0 [lcb_eval]: observation mismatch'
                    if kind == 'mismatch' else
                    f'FAIL {leg} {kind}: planted {kind} failure'
                )
                def check(work, regressions, leg=leg, kind=kind, diag=diag):
                    self.assertFalse(work.exists())
                    self.assert_bundle(
                        regressions, leg, 'test', 0, kind, (diag,),
                        ('program.lean.txt', 'observed-lcb_eval/status') if kind == 'mismatch'
                        else ('program.lean.txt',))

                body_prep = '''
echo 1 >"$work/bir/p0/lcb_eval.status"
: >"$work/bir/p0/lcb_eval.stderr"
: >"$work/bir/p0/lcb_eval.stdout"
''' if kind == 'mismatch' else f'''
echo "{kind}" >"$work/{leg}/p0/failure-kinds.txt"
printf '{kind}\\tsubject\\t{diag}\\n' >"$work/{leg}/p0/failure-record.tsv"
echo "{diag}" >"$work/{leg}/p0/diagnostics.txt"
echo "{diag}" >&2
'''
                self.run_shell(f'''
mkdir -p "$work/{leg}/p0"
echo 0 >"$work/{leg}/p0/status"
: >"$work/{leg}/p0/stderr"
echo "term-{leg}" >"$work/{leg}/p0/program.lean.txt"
{body_prep}
control_timeout() {{ return 3; }}
trap 'finish_diff $?' EXIT
run_control timeout '^unused$' control_timeout
''', 1, diag, check)

if __name__ == '__main__':
    unittest.main()
