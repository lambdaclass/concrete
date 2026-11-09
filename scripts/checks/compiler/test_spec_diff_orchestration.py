"""Cheap controls execute production orchestration with native toolchain stubs."""
import hashlib
import pathlib
import subprocess
import tempfile
import unittest

def safe(seed):
    return 'seed_' + hashlib.sha256(seed.encode()).hexdigest()

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
                'parse_replay', 'replay_command', 'register_candidate', 'register_generation', 'candidate_identity_path', 'candidate_bundle_path', 'persist_candidate_bundle',
                'record_candidate_failure', 'report_mismatch', 'judge_evaluator',
                'judge_available', 'reset_observations', 'control_candidate', 'native_one', 'absorb_native_result', 'collect_available',
                'reconcile_worker_exit', 'finish_diff', 'run_control', 'run_replay', 'first_outcome_index',
                'control_drop_overflow', 'control_lcb_off_by_one', 'control_spec_oracle',
                'control_match', 'verify_failure_bundle_and_replay', 'collect_control_failures', 'control_expected_record'))
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
            self.assertNotIn('orchestration assertion failed', result.stdout + result.stderr)
            diagnostics = diagnostic if isinstance(diagnostic, tuple) else (diagnostic,)
            for line in diagnostics:
                self.assertIn(line, (result.stdout + result.stderr).splitlines())
            if check_fn is not None:
                check_fn(work, regressions)

    def assert_bundle(self, regressions, leg, seed, index, kind, diag_lines, artifacts=()):
        matches = [e / 'latest' for p in regressions.rglob(f'{safe(seed)}-{index}')
                   if p.parent.name == leg for e in (p / 'executions').iterdir()]
        self.assertEqual(len(matches), 1, matches)
        bundle = matches[0]
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

    def test_nested_verifier_extra_mismatch_then_replay_timeout(self):
        emission = 'FAIL BIR emission: seed test index 0: planted selective emission refusal'
        mismatch = 'FAIL bir seed test index 0 [lcb_eval]: observation mismatch'
        def check(work, regressions):
            self.assertFalse(work.exists())
            bundles = list(regressions.rglob('latest'))
            self.assertTrue(bundles, 'nested evidence deleted by outer cleanup')
            self.assertTrue(any(mismatch in (b / 'diagnostics.txt').read_text()
                                for b in bundles))
        self.run_shell('''
trap 'finish_diff $?' EXIT
verify_saved_replay() { echo 'INCOMPLETE: planted replay timeout' >&2; return 3; }
first_nested() {
 d="$work/bir/p0"
 register_candidate bir test 0 "$d" emission
 echo term >"$d/program.lean.txt"; echo construct >"$d/constructs.txt"
 echo manifest >"$d/manifest-row.txt"
 echo 0 >"$d/status"; : >"$d/stderr"; echo 9 >"$d/lcb_eval.status"
 record_candidate_failure bir test 0 "$d" emission emission \
   'FAIL BIR emission: seed test index 0: planted selective emission refusal'
 exit 1
}
control_nested() {
 verify_failure_bundle_and_replay nested bir test 0 emission \
   'FAIL BIR emission: seed test index 0: planted selective emission refusal' '' '' '' first_nested || return $?
 return 99
}
run_control nested '^unused$' control_nested
''', 1, (emission, mismatch, 'INCOMPLETE: planted replay timeout'), check)

    def test_verified_nested_control_acknowledges_only_exact_plant(self):
        for extra in (False, True):
            with self.subTest(extra=extra):
                self.run_shell('''
trap 'finish_diff $?' EXIT
control_nested() {
 (
  work="$work/inner"; mkdir -p "$work"
  regressions="$control_evidence/saved"
  trap 'finish_diff $?' EXIT
  d="$work/bir/p0"
  register_candidate bir test 0 "$d" emission
  echo term >"$d/program.lean.txt"
  record_candidate_failure bir test 0 "$d" emission lcb_emit \
    'FAIL BIR emission: seed test index 0: planted selective emission refusal'
''' + ('''
  echo 0 >"$d/status"; : >"$d/stderr"; echo 9 >"$d/lcb_eval.status"
''' if extra else '') + '''
  exit 1
 ) || :
 echo 'FAIL planted: exact emission verified'
 return 99
}
run_control selective-refusal '^FAIL planted: exact emission verified$' control_nested
exit "$fail"
''', int(extra),
                    'FAIL bir seed test index 0 [lcb_eval]: observation mismatch' if extra else
                    'ok   control selective-refusal: failed for the planted reason',
                    lambda work, regressions: self.assertTrue(list(regressions.rglob('latest'))))

    def test_observer_plant_contract_checks_each_exact_backend(self):
        self.run_shell('''
b="$work/bundle"; mkdir -p "$b"
echo observation >"$b/control-replay"
echo bir >"$b/leg"; echo test >"$b/seed"; echo 0 >"$b/index"
for backend in llvm lcb; do
 control_expected_record observation-failure "$b" observation "$backend" \
   "FAIL bir seed test index 0 [$backend]: observation failed" || exit 9
done
if control_expected_record observation-failure "$b" observation lcb_eval \
 'FAIL bir seed test index 0 [lcb_eval]: observation failed'; then exit 9; fi
echo 'ok observer plant requires exact backend diagnostic'
''', 0, 'ok observer plant requires exact backend diagnostic')

    def test_spec_oracle_wrong_expectation_uses_lifecycle_and_replay(self):
        for mode in ('control', 'replay'):
            with self.subTest(mode=mode):
                diagnostics = tuple(f'FAIL spec seed control-spec-oracle index 0 [{b}]: observation mismatch'
                                    for b in ('lcb_eval', 'llvm', 'lcb'))
                def check(work, regressions):
                    self.assertFalse(work.exists())
                    self.assert_bundle(regressions, 'spec', 'control-spec-oracle', 0,
                                       'mismatch', diagnostics,
                                       ('expected/status', 'observed-lcb_eval/status',
                                        'observed-llvm/status', 'observed-lcb/status'))
                    b = next(regressions.rglob('latest'))
                    self.assertEqual((b / 'expected/status').read_text().strip(), '43')
                    self.assertIn('SPEC_DIFF_REPLAY_CONTROL=spec-oracle', (b / 'replay.sh').read_text())
                self.run_shell('''
trap 'finish_diff $?' EXIT
target=stub; driver=(stub)
native_build() { return 0; }
observe_remaining() { echo 42 >"$2.status"; : >"$2.stderr"; : >"$2.stdout"; }
control_bounded() {
 local dest=$3
 register_candidate spec control-spec-oracle 0 "$dest" spec-oracle
 mkdir -p "$dest/spec-expected" "$dest/wrong-expected"
 echo 42 >"$dest/spec-expected/status"; : >"$dest/spec-expected/stderr"
 echo 43 >"$dest/wrong-expected/status"; : >"$dest/wrong-expected/stderr"
 echo 42 >"$dest/status"; : >"$dest/stderr"
 echo 42 >"$dest/lcb_eval.status"; : >"$dest/lcb_eval.stderr"
 echo term >"$dest/program.lean.txt"
}
require_bounded() { control_bounded "$@"; }
verify_saved_replay() {
 [ "$(cat "$1/expected/status")" = 43 ] || return 1
 [ "$(wc -l <"$1/failure-record.tsv")" -eq 3 ] || return 1
 printf '%s\\n' "$1" >"$2"
 echo 'ok spec-oracle saved wrong expectation and all exact mismatches'
}
''' + ('''
run_control spec-oracle '^FAIL spec-oracle: judge rejected planted exit 43 against spec exit 42 on all 3 legs$' control_spec_oracle
exit "$fail"
''' if mode == 'control' else '''
replay=spec:control-spec-oracle:0
SPEC_DIFF_REPLAY_CONTROL=spec-oracle
run_replay
'''), 0 if mode == 'control' else 1,
                    ('ok   control spec-oracle: failed for the planted reason',)
                    if mode == 'control' else diagnostics, check)

    def test_unrelated_exit_cannot_reclassify_timed_out_candidate(self):
        self.run_shell('''
trap 'finish_diff $?' EXIT
d="$work/bir/p0"
register_candidate bir test 0 "$d"
printf 'emission\\tstarted\\n' >"$d/stage-journal.tsv"
echo 3 >"$d/worker-result"
reconcile_worker_exit 7
[ "$fail" -eq 0 ] && [ ! -f "$d/failure-kinds.txt" ] || exit 9
echo 'ok timeout candidate retains its worker outcome'
exit 1
''', 1, 'ok timeout candidate retains its worker outcome')

    def test_completed_retry_does_not_inherit_interrupted_stage(self):
        self.run_shell('''
trap 'finish_diff $?' EXIT
d="$work/bir/p0"
register_candidate bir test 0 "$d"
printf 'emission\\tstarted\\t100\\nemission\\tstarted\\t101\\nemission\\tcompleted\\t101\\n' >"$d/stage-journal.tsv"
reconcile_worker_exit 7
[ "$fail" -eq 0 ] || exit 9
echo 'ok completed retry has no active stage'
''', 0, 'ok completed retry has no active stage')

    def test_unmarked_worker_exit(self):
        for stage in ('lowering', 'emission', 'assembly'):
            for later_timeout in (False, True):
                with self.subTest(stage=stage, later_timeout=later_timeout):
                    diag = f'FAIL bir seed test index 0 [{stage}]: unexpected worker exit (rc 7)'
                    def check(work, regressions):
                        self.assertFalse(work.exists())
                        self.assert_bundle(regressions, 'bir', 'test', 0, stage,
                                           (diag,), ('stage-journal.tsv', 'program.lean.txt'))
                    self.run_shell('''
trap 'finish_diff $?' EXIT
d="$work/bir/p0"
register_candidate bir test 0 "$d"
echo term >"$d/program.lean.txt"
''' + f"printf '{stage}\\tstarted\\n' >\"$d/stage-journal.tsv\"\n" +
                        ('reconcile_worker_exit 7\nexit 3\n' if later_timeout else 'exit 7\n'),
                        1, diag, check)

    def test_mismatch_timeout_lifecycle(self):
        for path in ('candidate', 'wrong-result', 'off-by-one', 'spec-oracle',
                     'wrong-result-mutated', 'off-by-one-mutated'):
            for reverse in (False, True):
                with self.subTest(path=path, reverse=reverse):
                    bad = 'lcb' if reverse else 'llvm'
                    diag = f'FAIL bir seed test index 0 [{bad}]: observation mismatch'
                    def check(work, regressions):
                        self.assertFalse(work.exists())
                        self.assert_bundle(regressions, 'bir', 'test', 0, 'mismatch',
                                           (diag,), (f'observed-{bad}/status', 'replay.sh'))
                    invocation = ('native_one bir test 0 "$d"' if path == 'candidate'
                                  else {'wrong-result': 'control_drop_overflow',
                                        'off-by-one': 'control_lcb_off_by_one',
                                        'spec-oracle': 'control_spec_oracle',
                                        'wrong-result-mutated': 'control_drop_overflow',
                                        'off-by-one-mutated': 'control_lcb_off_by_one'}[path])
                    self.run_shell('''
trap 'finish_diff $?' EXIT
mkdir -p "$work/bir/p0"
d="$work/bir/p0"
''' + ('' if path == 'candidate' else
                        'd="$work/control-fixture"; mkdir -p "$d"\n') + '''
echo 0 >"$d/status"
: >"$d/stderr"
echo 0 >"$d/lcb_eval.status"
echo term >"$d/program.lean.txt"
echo original >"$d/llvm.ll"
echo original >"$d/lcb_body.s"
native_build() { return 0; }
target=stub; driver=(stub); fixture=$d
first_outcome_index() { echo 0; }
control_bounded() {
 local dest
 case "$2" in
  replay) dest=$5 ;;
  control-spec-oracle) dest=$3 ;;
  control-emit) dest=$7 ;;
  *) echo 'unexpected control stage' >&2; return 8 ;;
 esac
 cp -R "$fixture" "$dest"
 d=$dest
 if [ "$2" = control-emit ]; then
  echo mutated >"$d/llvm.ll"
  echo mutated >"$d/lcb_body.s"
 fi
 mkdir -p "$d/spec-expected"
 cp "$d/status" "$d/spec-expected/status"
 cp "$d/stderr" "$d/spec-expected/stderr"
}
observe_remaining() {
 case "$1" in
''' + (' */*original/*.exe) echo 0 >"$2.status"; return 0 ;;\n'
       if path.endswith('-mutated') else '') + f' */{bad}.exe) echo 9 >"$2.status"; return 0 ;;\n' + '''
 *) echo incomplete >"$2.status"; echo 'INCOMPLETE: planted later leg'; return 3 ;;
 esac
}
rc=0
''' + invocation + ''' || rc=$?
[ "$rc" -eq 1 ] && [ "$native_failed" -eq 1 ] && [ "$native_incomplete" -eq 1 ] || { echo 'orchestration assertion failed' >&2; exit 9; }
exit "$rc"
''', 1, (diag, 'INCOMPLETE: planted later leg'), check)

    def test_exit_collector_judges_uncommitted_native_observation(self):
        diag = 'FAIL bir seed test index 0 [llvm]: observation mismatch'
        def check(work, regressions):
            self.assertFalse(work.exists())
            self.assert_bundle(regressions, 'bir', 'test', 0, 'mismatch', (diag,),
                               ('observed-llvm/status',))
        self.run_shell('''
trap 'finish_diff $?' EXIT
mkdir -p "$work/control-interrupted"
d="$work/control-interrupted"
echo bir >"$d/leg"; echo test >"$d/seed"; echo 0 >"$d/index"
echo 0 >"$d/status"; : >"$d/stderr"; echo 9 >"$d/llvm.status"
echo term >"$d/program.lean.txt"
printf '%s\\n' "$d" >"$work/lifecycle-dirs"
# Simulate an exit between observer completion and normal judgment.
exit 3
''', 1, diag, check)

    def test_generation_controls_registered_before_artifacts(self):
        for mode in ('failure', 'mismatch'):
            with self.subTest(mode=mode):
                kind = 'emission' if mode == 'failure' else 'mismatch'
                diag = ('FAIL BIR emission: seed test index 0: planted selective emission refusal'
                        if mode == 'failure' else
                        'FAIL bir seed test index 0 [lcb_eval]: observation mismatch')
                def check(work, regressions):
                    self.assertFalse(work.exists())
                    self.assert_bundle(regressions, 'bir', 'test', 0, kind, (diag,))
                    self.assertTrue((next(regressions.glob(f'bir/{safe("test")}-0/executions/*/latest/control-owner'))).is_file())
                plant = (f'''echo emission >"$d/failure-kinds.txt"
printf 'emission\\tsubject\\t{diag}\\n' >"$d/failure-record.tsv"
echo '{diag}' >"$d/diagnostics.txt"
echo '{diag}' >&2
''' if mode == 'failure' else 'echo 9 >"$d/lcb_eval.status"\n')
                self.run_shell('''
trap 'finish_diff $?' EXIT
control_generation() {
 register_generation env lake env lean --run scripts/checks/compiler/spec_diff.lean replay test 0 "$work/control-drop-original" stub
 d="$work/control-drop-original"
 [ "$(cat "$d/control-owner")" = "$control_registry" ] || exit 8
 echo 0 >"$d/status"; : >"$d/stderr"
 echo term >"$d/program.lean.txt"
''' + plant + '''
''' + (" return 1\n" if mode == 'failure' else
       " echo 'INCOMPLETE: planted generation deadline'\n return 3\n") + '''}
run_control generation '^unused$' control_generation
''', 1, (diag,) if mode == 'failure' else
         (diag, 'INCOMPLETE: planted generation deadline'), check)

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
[ "$fail" -eq 1 ] && [ "$deadline_stopped" -eq 0 ] || { echo 'orchestration assertion failed' >&2; exit 9; }
exit "$fail"
''', 1, expected_diags, check)

    def test_successful_control_cannot_exempt_baseline_mismatch(self):
        diag = 'FAIL bir seed test index 0 [lcb_eval]: observation mismatch'
        def check(work, regressions):
            self.assertFalse(work.exists())
            self.assert_bundle(regressions, 'bir', 'test', 0, 'mismatch', (diag,))
        self.run_shell('''
trap 'finish_diff $?' EXIT
control_baseline() {
 register_generation lake env lean --run scripts/checks/compiler/spec_diff.lean replay test 0 "$work/control-baseline" stub
 d="$work/control-baseline"
 echo 0 >"$d/status"; : >"$d/stderr"; echo 9 >"$d/lcb_eval.status"
 echo 'FAIL planted: verified distinct effect'
 return 99
}
run_control baseline '^FAIL planted: verified distinct effect$' control_baseline
exit "$fail"
''', 1, diag, check)

    def test_oracle_publication_and_native_setup_are_distinct(self):
        diag = 'FAIL bir-0: missing oracle observation'
        def check(work, regressions):
            self.assertFalse(work.exists())
            self.assert_bundle(regressions, 'bir', 'test', 0, 'native_setup', (diag,))
            bundle = next(regressions.glob(f'bir/{safe("test")}-0/executions/*/latest'))
            self.assertNotIn('mismatch', (bundle / 'failure-kinds.txt').read_text().splitlines())
        self.run_shell('''
trap 'finish_diff $?' EXIT
d="$work/control-oracle"
register_candidate bir test 0 "$d"
echo 0 >"$d/lcb_eval.status"
# A completed evaluator result is not a mismatch against an unpublished oracle.
collect_available
[ "$fail" -eq 0 ] || { echo 'orchestration assertion failed' >&2; exit 9; }
# Once native execution starts, absence of that oracle is a setup failure.
native_build() { echo 'orchestration assertion failed: unexpected build'; return 9; }
rc=0
native_one bir test 0 "$d" || rc=$?
[ "$rc" -eq 4 ] && [ "$native_failed" -eq 1 ] || { echo 'orchestration assertion failed' >&2; exit 9; }
exit 1
''', 1, diag, check)

    def test_scenarios_keep_distinct_bundle_snapshots(self):
        def check(work, regressions):
            self.assertFalse(work.exists())
            for scenario in ('emission', 'assembly'):
                bundle = next(regressions.glob(f'controls/{scenario}/bir/{safe("test")}-0/executions/*/latest'))
                self.assertTrue(bundle.is_dir())
                self.assertEqual((bundle / 'failure-kinds.txt').read_text().strip(), scenario)
                self.assertIn(f'SPEC_DIFF_REPLAY_CONTROL={scenario}',
                              (bundle / 'replay.sh').read_text())
        self.run_shell('''
trap 'finish_diff $?' EXIT
for scenario in emission assembly; do
 d="$work/control-$scenario"
 register_candidate bir test 0 "$d" "$scenario"
 record_candidate_failure bir test 0 "$d" "$scenario" stage "FAIL planted $scenario"
done
exit 1
''', 1, ('FAIL planted emission', 'FAIL planted assembly'), check)

    def test_bundle_collision_refused_without_touching_original(self):
        self.run_shell('''
register_candidate bir test 0 "$work/first"
echo original >"$work/first/program.lean.txt"
record_candidate_failure bir test 0 "$work/first" generation worker_exit 'FAIL first generation'
bundle=$(candidate_bundle_path bir "$work/first")
candidate=$(candidate_identity_path bir "$work/first")
cp -RL "$bundle" "$work/snapshot"
register_candidate bir test 0 "$work/second"
echo different >"$work/second/program.lean.txt"
rc=0
record_candidate_failure bir test 0 "$work/second" generation worker_exit 'FAIL second generation' 2>"$work/collision.log" || rc=$?
[ "$rc" -eq 1 ] && [ "$fail" -eq 1 ] || exit 9
grep -qxF "FAIL regression candidate identity collision: $candidate" "$work/collision.log" || exit 9
diff -r "$work/snapshot" "$bundle" || exit 9
echo 'ok collision preserved original bundle'
''', 0, ('FAIL first generation',
                'ok collision preserved original bundle'),
            lambda work, regressions: None)

    def test_same_candidate_reexecution_and_append_only_snapshots(self):
        self.run_shell('''
register_candidate bir test 0 "$work/first"
echo term >"$work/first/program.lean.txt"
record_candidate_failure bir test 0 "$work/first" emission stage 'FAIL planted emission'
first=$(candidate_bundle_path bir "$work/first")
original=$(readlink -f "$first")
fingerprint=$(python3 -I scripts/checks/compiler/spec_diff_identity.py digest "$original")
record_candidate_failure bir test 0 "$work/first" assembly stage 'FAIL planted assembly'
[ "$(readlink -f "$first")" != "$original" ] || exit 9
[ "$(python3 -I scripts/checks/compiler/spec_diff_identity.py digest "$original")" = "$fingerprint" ] || exit 9
cp -R "$work/first" "$work/copied"
register_candidate bir test 0 "$work/copied"
[ "$(cat "$work/first/execution-id")" != "$(cat "$work/copied/execution-id")" ] || exit 9
record_candidate_failure bir test 0 "$work/copied" emission stage 'FAIL planted emission'
register_candidate bir test 0 "$work/second"
echo term >"$work/second/program.lean.txt"
record_candidate_failure bir test 0 "$work/second" emission stage 'FAIL planted emission'
second=$(candidate_bundle_path bir "$work/second")
[ "$first" != "$second" ] && [ -f "$first/failure-record.tsv" ] && [ -f "$second/failure-record.tsv" ] || exit 9
[ "$(find "$regressions" -type l -name latest | wc -l)" -eq 3 ] || exit 9
echo 'ok independent executions and immutable snapshots'
''', 0, ('FAIL planted emission', 'FAIL planted assembly',
                'ok independent executions and immutable snapshots'))

    def test_distinct_seeds_at_forced_candidate_path_collide(self):
        self.run_shell('''
candidate_identity_path() { echo "$regressions/forced"; }
register_candidate bir first 0 "$work/first"
record_candidate_failure bir first 0 "$work/first" generation stage 'FAIL first generation'
bundle=$(candidate_bundle_path bir "$work/first")
fingerprint=$(python3 -I scripts/checks/compiler/spec_diff_identity.py digest "$bundle")
register_candidate bir second 0 "$work/second"
rc=0
record_candidate_failure bir second 0 "$work/second" generation stage 'FAIL second generation' 2>"$work/collision.log" || rc=$?
[ "$rc" -eq 1 ] || exit 9
grep -qxF "FAIL regression candidate identity collision: $regressions/forced" "$work/collision.log" || exit 9
[ "$(python3 -I scripts/checks/compiler/spec_diff_identity.py digest "$bundle")" = "$fingerprint" ] || exit 9
echo 'ok exact candidate collision preserved evidence'
''', 0, ('FAIL first generation', 'ok exact candidate collision preserved evidence'))

    def test_populated_legacy_root_preserves_claim_and_evidence(self):
        self.run_shell('''
register_candidate bir test 0 "$work/first"
echo term >"$work/first/program.lean.txt"
candidate=$(candidate_identity_path bir "$work/first")
mkdir -p "$candidate"
cp -R "$work/first/." "$candidate/"
echo 'FAIL legacy emission' >"$candidate/diagnostics.txt"
record_candidate_failure bir test 0 "$work/first" emission stage 'FAIL planted emission'
grep -qxF 'FAIL legacy emission' "$candidate/diagnostics.txt" || exit 9
register_candidate bir test 0 "$work/different"
echo other >"$work/different/program.lean.txt"
rc=0
record_candidate_failure bir test 0 "$work/different" emission stage 'FAIL different emission' 2>"$work/error" || rc=$?
[ "$rc" -eq 1 ] || exit 9
grep -qxF "FAIL regression candidate identity collision: $candidate" "$work/error" || exit 9
echo 'ok legacy root evidence preserved and collision checked'
''', 0, ('FAIL planted emission', 'ok legacy root evidence preserved and collision checked'))

    def test_missing_content_claim_can_be_learned_but_cannot_change(self):
        self.run_shell('''
register_candidate bir test 0 "$work/first"
record_candidate_failure bir test 0 "$work/first" generation stage 'FAIL generation'
echo term >"$work/first/program.lean.txt"
record_candidate_failure bir test 0 "$work/first" emission stage 'FAIL emission'
first=$(candidate_bundle_path bir "$work/first")
echo assembly >"$work/first/control-replay"
record_candidate_failure bir test 0 "$work/first" assembly stage 'FAIL assembly'
second=$(candidate_bundle_path bir "$work/first")
[ "$first" != "$second" ] && [ -f "$first/diagnostics.txt" ] || exit 9
echo changed >"$work/first/program.lean.txt"
candidate=$(candidate_identity_path bir "$work/first")
rc=0
record_candidate_failure bir test 0 "$work/first" assembly stage 'FAIL changed assembly' 2>"$work/error" || rc=$?
[ "$rc" -eq 1 ] || exit 9
grep -qxF "FAIL regression candidate identity collision: $candidate" "$work/error" || exit 9
echo 'ok monotone content claims and separate compound plant execution'
''', 0, ('FAIL generation', 'FAIL emission', 'FAIL assembly',
                'ok monotone content claims and separate compound plant execution'))

    def test_control_identity_is_not_sanitized(self):
        self.run_shell('''
register_candidate bir test 0 "$work/control" 'Early:A'
rc=0
candidate_bundle_path bir "$work/control" >"$work/path" 2>"$work/error" || rc=$?
[ "$rc" -eq 1 ] && [ ! -s "$work/path" ] || exit 9
cat "$work/error"
''', 0, 'FAIL invalid regression control identity')

    def test_early_generation_colliding_seeds_preserve_both(self):
        seeds = ('Early:A', 'Early?A')
        diags = tuple(f'FAIL bir seed {s} index 0 [generation]: unexpected worker exit (rc 7)'
                      for s in seeds)
        def check(work, regressions):
            for seed, diag in zip(seeds, diags):
                self.assert_bundle(regressions, 'bir', seed, 0, 'generation', (diag,))
        self.run_shell('''
for item in A B; do
 d="$work/$item"
 s='Early:A'; [ "$item" = A ] || s='Early?A'
 register_candidate bir "$s" 0 "$d"
 [ -s "$d/safe-seed.txt" ] && [ -s "$d/regression-name.txt" ] && [ -s "$d/execution-id" ] || exit 9
 printf 'generation\\tstarted\\t123\\n' >"$d/stage-journal.tsv"
 echo 7 >"$d/worker-result"
done
reconcile_worker_exit 7
[ "$(find "$regressions" -type l -name latest | wc -l)" -eq 2 ] || exit 9
''', 0, diags, check)

    def test_failure_log_cannot_claim_timeout(self):
        self.run_shell('''
control_error() { echo 'INCOMPLETE: planted misleading compiler output'; return 1; }
run_control error '^unused$' control_error
[ "$fail" -eq 1 ] && [ "$deadline_stopped" -eq 0 ] || { echo 'orchestration assertion failed' >&2; exit 9; }
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
[ "$fail" -eq 1 ] && [ "$native_complete" -eq 0 ] && [ "$deadline_stopped" -eq 0 ] || { echo 'orchestration assertion failed' >&2; exit 9; }
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
