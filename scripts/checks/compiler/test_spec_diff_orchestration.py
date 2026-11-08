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
    def run_shell(self, body, expected, diagnostic):
        with tempfile.TemporaryDirectory() as work:
            functions = '\n'.join(function(n) for n in (
                'judge_evaluator', 'native_one', 'absorb_native_result',
                'collect_available', 'finish_diff', 'run_control'))
            setup = f'''set -euo pipefail
work={work}
seed=test
regressions=$work/regressions
replay=""
fail=0; deadline_stopped=0; min_candidate_secs=1
BOUNDED_LOG=$work/bounded.log; : >"$BOUNDED_LOG"
PLANTED_FAIL_RC=99
remaining() {{ echo 100; }}
bounded_summary() {{ :; }}
judge() {{ cmp -s "$1/status" "$2.status"; }}
report_mismatch() {{
 mkdir -p "$regressions/$1/$3"
 cp "$4/$5.status" "$regressions/$1/$3/status"
 echo "FAIL $1 seed $2 index $3 [$5]: observation mismatch"
}}
replay_command() {{ :; }}
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

    def test_build_failure_both_loops(self):
        start = SOURCE.index('bir_accepted=$(summary_value')
        end = SOURCE.index('\nprint_judged_counts bir', start)
        for missing in (False, True):
            with self.subTest(missing=missing):
                self.run_shell('''
for leg in bir spec; do
 mkdir -p "$work/$leg/p0"
 printf 'program\\t0\\texit\\ttrue\\n' >"$work/$leg/summary.tsv"
 echo 0 >"$work/$leg/p0/status"
 touch "$work/$leg/p0/native-ready"
''' + ('' if missing else ' echo 0 >"$work/$leg/p0/lcb_eval.status"\n') + '''done
native_build() { echo 'planted compiler error'; return 7; }
''' + SOURCE[start:end] + '''
[ "$fail" -eq 1 ] && [ "$deadline_stopped" -eq 0 ] || exit 9
exit "$fail"
''', 1, ('FAIL bir-0 [lcb_eval]: missing evaluator observation',
                   'FAIL spec-0 [lcb_eval]: missing evaluator observation') if missing else
                   ('planted compiler error', 'FAIL bir-0: native build/setup failed',
                    'FAIL spec-0: native build/setup failed'))

    def test_failure_log_cannot_claim_timeout(self):
        self.run_shell('''
control_error() { echo 'INCOMPLETE: planted misleading compiler output'; return 1; }
run_control error '^unused$' control_error
[ "$fail" -eq 1 ] && [ "$deadline_stopped" -eq 0 ] || exit 9
exit "$fail"
''', 1, "INCOMPLETE: planted misleading compiler output")

    def test_observer_setup_failure(self):
        self.run_shell('''
mkdir -p "$work/candidate"
echo 0 >"$work/candidate/status"
echo 0 >"$work/candidate/lcb_eval.status"
native_build() { return 0; }
observe_remaining() { echo 'FAIL planted observer setup'; return 4; }
rc=0
native_one bir test 0 "$work/candidate" || rc=$?
absorb_native_result "$rc"
[ "$fail" -eq 1 ] && [ "$native_complete" -eq 0 ] && [ "$deadline_stopped" -eq 0 ] || exit 9
exit "$fail"
''', 1, 'FAIL planted observer setup')

    def test_collector_failure_beats_control_timeout(self):
        self.run_shell('''
mkdir -p "$work/bir/p0"
echo 0 >"$work/bir/p0/status"
echo 1 >"$work/bir/p0/lcb_eval.status"
control_timeout() { return 3; }
# Verify the saved observation before allowing scratch cleanup.
rm() { test "$(cat "$regressions/bir/0/status")" = 1 || exit 9; command rm "$@"; }
trap 'finish_diff $?' EXIT
run_control timeout '^unused$' control_timeout
''', 1, 'FAIL bir seed test index 0 [lcb_eval]: observation mismatch')

if __name__ == '__main__':
    unittest.main()
