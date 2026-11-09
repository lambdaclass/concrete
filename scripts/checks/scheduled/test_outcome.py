import runpy
from pathlib import Path
runpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))

import datetime as dt
import tempfile
import sys
import unittest
from process_adapter import process_run
import protocol
import observer_watch
from test_protocol import RegistryFixture, record, SHA

ROOT = Path(__file__).resolve().parent
LIB = ROOT.parent / 'lib'

class Outcome(RegistryFixture, unittest.TestCase):
    def command(self, *args):
        return process_run([sys.executable, str(ROOT / 'outcome_control.py'), *map(str, args)],
                           cwd=ROOT, env={}, capture_output=True, text=True, timeout=10)

    def test_reducer_production_and_plants(self):
        good = self.command('precedence', LIB / 'scheduled_outcome.py')
        self.assertEqual((good.returncode, good.stdout, good.stderr), (0, '', ''))
        with tempfile.TemporaryDirectory() as scratch:
            path = Path(scratch) / 'reducer.py'
            source = (LIB / 'scheduled_outcome.py').read_text()
            for planted in (
                    source.replace('(Status.FAIL, Status.INCOMPLETE, Status.MISSING, Status.PASS)',
                                   '(Status.INCOMPLETE, Status.FAIL, Status.MISSING, Status.PASS)'),
                    source.replace('values = {Status(value) for value in outcomes}',
                                   'return Status(list(outcomes)[-1])')):
                path.write_text(planted)
                bad = self.command('precedence', path)
                self.assertEqual((bad.returncode, bad.stdout, bad.stderr),
                                 (1, '', 'FAIL scheduled outcome precedence: FAIL masked\n'))

    def test_fail_absorbs_missing_and_actions_availability(self):
        from scheduled_outcome import execution_status
        self.assertEqual(execution_status([{'status': 'FAIL'}], missing=True, unavailable=True), 'FAIL')
        forged = record()
        forged.update(status='INCOMPLETE', conclusion='incomplete')
        forged['gates'][0]['status'] = 'INCOMPLETE'
        forged['gates'][0]['controls'][0]['status'] = 'FAIL'
        with self.assertRaisesRegex(protocol.EvidenceError, '^status/conclusion contradicts evidence$'):
            protocol.validate(forged, SHA, 'daily', {'example/repo': {'ci2-heavy'}}, 'example/repo', None)
        import collect
        payload = record()
        payload.update(status='FAIL', conclusion='failure')
        payload['gates'][0]['status'] = 'FAIL'
        run = dict(id=1, run_attempt=1, event='schedule')
        job = dict(conclusion='failure', runner_name='ci2-heavy', status='completed',
                   started_at=payload['started'], completed_at=payload['completed'])
        result = collect.record_for(run, job, (SHA, 'daily'), payload, payload['artifact'],
                                    'example/repo', {'example/repo': {'ci2-heavy'}})
        self.assertEqual((result['status'], result['qualification_status']), ('FAIL', 'FAIL'))

    def test_cancelled_and_skipped_keep_gate_and_qualification_failures(self):
        import collect
        from outcome_control import preserves_failure
        from unittest.mock import patch
        for conclusion in ('cancelled', 'skipped'):
            for qualification in (False, True):
                payload = record()
                if qualification:
                    payload['gates'][0]['command'] = ['false']
                else:
                    payload.update(status='FAIL', conclusion='failure')
                    payload['gates'][0]['status'] = 'FAIL'
                run = dict(id=1, run_attempt=1, event='schedule', created_at=payload['started'])
                job = dict(conclusion=conclusion, runner_name='ci2-heavy', status='completed',
                           started_at=payload['started'], completed_at=payload['completed'])
                result = collect.record_for(run, job, (SHA, 'daily'), payload, payload['artifact'],
                                            'example/repo', {'example/repo': {'ci2-heavy'}})
                preserves_failure(result['qualification_status'], conclusion)
                self.assertEqual(result['gates'], payload['gates'])
                # A newer passing attempt cannot mask either kind of failure.
                later = record(); later['attempt'] = '1/2'
                with self.assertRaises(protocol.EvidenceError) as caught:
                    protocol.observe([result, later], SHA, 'daily', protocol.timestamp(payload['completed']),
                                     {'example/repo': {'ci2-heavy'}}, 'example/repo')
                expected = 'FAIL registry: wrong command sample' if qualification else 'recorded attempt'
                self.assertEqual(str(caught.exception), f'FAIL daily {SHA}: {expected}')
                # Plant the original evidence-dropping branch in the real adapter.
                import types
                plant = types.ModuleType('planted_collect'); plant.__file__ = collect.__file__
                source = Path(collect.__file__).read_text().replace(
                    'if missing and payload is None:', 'if missing:')
                exec(compile(source, collect.__file__, 'exec'), plant.__dict__)
                masked = plant.record_for(run, job, (SHA, 'daily'), payload, payload['artifact'],
                                         'example/repo', {'example/repo': {'ci2-heavy'}})
                with self.assertRaises(ValueError) as refused:
                    preserves_failure(masked['qualification_status'], conclusion)
                self.assertEqual(str(refused.exception),
                                 f'FAIL scheduled monotonicity: {conclusion} masked FAIL')

    def test_sibling_defects_cannot_mask_fail(self):
        import copy
        import types
        from outcome_control import preserves_failure
        from test_protocol import NOW, RUNNERS
        fail = record(); fail.update(status='FAIL', conclusion='failure')
        fail['gates'][0]['status'] = 'FAIL'
        future = record(); future.update(attempt='1/2', completed=dt.datetime.fromtimestamp(NOW+1, dt.timezone.utc).isoformat())
        for name, sibling in (('future', future), ('duplicate', record()), ('invalid sibling', None)):
            with self.assertRaises(protocol.EvidenceError) as caught:
                protocol.observe([fail, sibling], SHA, 'daily', NOW, RUNNERS, 'example/repo')
            self.assertEqual(str(caught.exception), f'FAIL daily {SHA}: recorded attempt')
            preserves_failure(caught.exception.status, name)
            # Plant eager validation, the former reduction ordering.
            source = Path(protocol.__file__).read_text().replace(
                '    valid, errors = [], []',
                '    history = [validate(r, sha, tier, runners, repository, None) for r in history]\n'
                '    for r in history: require(timestamp(r["completed"]) <= now, "future completion")\n'
                '    require(len({r["attempt"] for r in history}) == len(history), "replay would overwrite attempt")\n'
                '    valid, errors = [], []')
            plant = types.ModuleType('planted_protocol'); plant.__file__ = protocol.__file__
            exec(compile(source, protocol.__file__, 'exec'), plant.__dict__)
            plant.registry_at = protocol.registry_at
            with self.assertRaises(plant.EvidenceError) as masked:
                plant.observe([fail, sibling], SHA, 'daily', NOW, RUNNERS, 'example/repo')
            with self.assertRaises(ValueError) as refused:
                preserves_failure(masked.exception.status, name)
            self.assertEqual(str(refused.exception), f'FAIL scheduled monotonicity: {name} masked FAIL')

    def test_nonoperational_history_cannot_be_recovered_by_newer_sha(self):
        from test_protocol import NOW, RUNNERS
        partial = record(); partial['gates'][0]['controls'] = []
        missing = record(); missing.update(attempt='1/2', status='MISSING', conclusion='cancelled',
                                           gates=[], runner_name=None)
        with self.assertRaises(protocol.EvidenceError) as caught:
            protocol.observe([partial, missing], SHA, 'daily', NOW, RUNNERS, 'example/repo')
        self.assertEqual(str(caught.exception), f'MISSING daily {SHA}: recorded attempt')
        self.assertFalse(caught.exception.operational)

    def test_empty_execution_and_nested_reducer_plants(self):
        source = (LIB / 'scheduled_outcome.py').read_text()
        with tempfile.TemporaryDirectory() as scratch:
            path = Path(scratch) / 'reducer.py'
            for planted in (
                    source.replace('empty=Status.INCOMPLETE)', 'empty=Status.PASS)'),
                    source.replace("*(control['status'] for gate in gates for control in gate.get('controls', [])),", ''),
                    source.replace("return reduce_outcomes([*(gate['status'] for gate in gates),",
                                   "if missing: return Status.MISSING\n    return reduce_outcomes([*(gate['status'] for gate in gates),")):
                self.assertNotEqual(source, planted)
                path.write_text(planted)
                bad = self.command('precedence', path)
                self.assertEqual((bad.returncode, bad.stdout, bad.stderr),
                                 (1, '', 'FAIL scheduled outcome precedence: FAIL masked\n'))

    def test_malformed_json_sibling_preserves_parseable_fail(self):
        import json
        from unittest.mock import patch
        from test_protocol import NOW, RUNNERS
        fail = record(); fail.update(status='FAIL', conclusion='failure')
        fail['gates'][0]['status'] = 'FAIL'
        with patch.object(protocol, 'git', return_value=json.dumps(fail) + '\n{broken'):
            history = protocol.read_note(SHA, 'daily', listed=True)
        with self.assertRaises(protocol.EvidenceError) as caught:
            protocol.observe(history, SHA, 'daily', NOW, RUNNERS, 'example/repo')
        self.assertEqual(str(caught.exception), f'FAIL daily {SHA}: recorded attempt')

    def test_observer_completion_is_independent_of_producer_verdict(self):
        from unittest.mock import patch
        import observe
        import os
        import types
        now = dt.datetime(2027, 1, 15, 12, 29, tzinfo=dt.timezone.utc)
        runs = [dict(id=n, created_at=(now.replace(minute=11)-dt.timedelta(hours=4*n)).isoformat(),
                     event='schedule',status='completed',conclusion='failure')
                for n in range(1, 15)]
        calls = []
        def pages(path, key, **kwargs):
            calls.append(path)
            if key == 'workflow_runs':
                return runs
            return [dict(steps=[dict(name=observer_watch.COMPLETION_STEP,
                                    status='completed',conclusion='success')])]
        observer_watch.check('example/repo', 'fixture', now, pages)
        self.assertEqual(calls[0], '/repos/example/repo/actions/workflows/gate_freshness.yml/runs?branch=main&created=%3E%3D2027-01-13T08%3A29%3A00Z')
        # Plant coupling to workflow conclusion; the independent control rejects it.
        observed = [dict(r, observation_complete=True) for r in runs]
        source = Path(observer_watch.__file__).read_text().replace(
            "run.get('observation_complete') is True", "run.get('conclusion') == 'success'")
        plant = types.ModuleType('planted_watch')
        exec(compile(source, observer_watch.__file__, 'exec'), plant.__dict__)
        with self.assertRaises(ValueError) as caught:
            plant.verify_occurrences(observed, now)
        self.assertEqual(str(caught.exception), 'FAIL scheduled observer: missed occurrence')
        # A successful workflow without the explicit completion step is unhealthy.
        with self.assertRaises(ValueError) as caught:
            observer_watch.check('example/repo', 'fixture', now,
                                 lambda path, key, **kw: [dict(r, conclusion='success') for r in runs] if key == 'workflow_runs' else [])
        self.assertEqual(str(caught.exception), 'FAIL scheduled observer: missed occurrence')
        # A handled producer FAIL completes observation while preserving exit 1.
        from process_adapter import operation_clock
        with tempfile.TemporaryDirectory() as work:
            output = Path(work) / 'output'
            env = dict(GITHUB_REPOSITORY='example/repo', GITHUB_OUTPUT=str(output))
            lineage = types.SimpleNamespace(first_parent=[SHA], presentation=[SHA])
            with patch.dict(os.environ, env), patch.object(observe, 'main_history', return_value=lineage), \
                 patch.object(observe, 'git', return_value=''), \
                 patch.object(observe, 'observe_main', return_value=[f'FAIL daily {SHA}: recorded attempt']), \
                 patch('builtins.print'):
                self.assertEqual(observe.main(clock=operation_clock(observation=now)), 1)
            self.assertEqual(output.read_text(), 'completed=true\n')

    def test_redirect_production_and_credential_plant(self):
        good = self.command('transport', ROOT / 'collect.py')
        self.assertEqual((good.returncode, good.stdout, good.stderr), (0, '', ''))
        with tempfile.TemporaryDirectory() as scratch:
            path = Path(scratch) / 'collect.py'
            path.write_text((ROOT / 'collect.py').read_text().replace('redirected.remove_header("Authorization")', 'pass # planted forwarding'))
            bad = self.command('transport', path)
            self.assertEqual((bad.returncode, bad.stdout, bad.stderr),
                             (1, '', 'FAIL scheduled transport: redirected credential\n'))

    def test_provenance_substitution_and_authoritative_recomputation(self):
        for field, value in (('tools', {'compiler': 'substitute'}), ('seed', 999)):
            payload = record()
            payload['gates'][0][field] = value
            payload['qualification_status'] = 'PASS'
            payload['qualification_outcomes'] = []
            qualified = protocol.validate(payload, SHA, 'daily', {'example/repo': {'ci2-heavy'}}, 'example/repo', None)
            self.assertEqual(qualified['qualification_status'], 'FAIL')
            self.assertEqual(qualified['qualification'], ['FAIL scheduled provenance: tool/seed mismatch'])
            # This CLI boundary emits the exact recomputed finding, not any failure.
            code = ('import runpy,sys; from pathlib import Path; '
                    'runpy.run_path(str(Path(sys.argv[1])/"suite_bootstrap.py")); '
                    'import protocol; from test_protocol import fixture_registry; '
                    'protocol.registry_at=lambda sha:fixture_registry(); '
                    'import json; p=json.loads(sys.argv[2]); '
                    'r=protocol.validate(p,p["sha"],"daily",{"example/repo":{"ci2-heavy"}},"example/repo",None); '
                    'print(r["qualification"][0],file=sys.stderr); sys.exit(1 if r["qualification_status"]=="FAIL" else 0)')
            import json
            result = process_run([sys.executable, '-c', code, str(ROOT), json.dumps(payload)],
                                 cwd=ROOT, env={}, capture_output=True, text=True, timeout=10)
            self.assertEqual((result.returncode, result.stdout, result.stderr),
                             (1, '', 'FAIL scheduled provenance: tool/seed mismatch\n'))

    def test_observer_independent_occurrence_census(self):
        now = dt.datetime(2027, 1, 15, 12, 29, tzinfo=dt.timezone.utc)
        runs = []
        for offset in range(1, 15):
            created = now.replace(minute=11) - dt.timedelta(hours=4 * offset)
            runs.append(dict(id=offset, created_at=created.isoformat(), event='schedule', status='completed', conclusion='success', observation_complete=True))
        runs = [dict(r, conclusion='failure') for r in runs]
        observer_watch.verify_occurrences(runs, now)
        for index in (0, 5, 10):
            missing = runs[:index] + runs[index+1:]
            with self.assertRaisesRegex(ValueError, '^FAIL scheduled observer: missed occurrence$'):
                observer_watch.verify_occurrences(missing, now)
        # Exercise the real publisher CLI, with only external Actions I/O replaced.
        import json
        code = '\n'.join([
            'import runpy,sys,json,datetime as dt',
            'from pathlib import Path',
            'runpy.run_path(str(Path(sys.argv[1])/"suite_bootstrap.py"))',
            'import collect',
            'from process_adapter import operation_clock',
            'runs=json.loads(sys.argv[2])',
            'def pages(path,key,**kwargs):',
            '    if "gate_freshness.yml" in path:',
            '        assert path == "/repos/example/repo/actions/workflows/gate_freshness.yml/runs?branch=main&created=%3E%3D2027-01-13T08%3A29%3A00Z"',
            '        return runs',
            '    if key == "jobs": return [dict(steps=[dict(name="Observer execution completed",status="completed",conclusion="success")])]',
            '    return []',
            'collect.pages=pages',
            'collect.inventory=lambda: {}',
            'collect.operation_clock=lambda: operation_clock(observation=dt.datetime.fromisoformat(sys.argv[3]))',
            'sys.exit(collect.main())',
        ])
        with tempfile.TemporaryDirectory() as scratch:
            event = Path(scratch) / 'event.json'; event.write_text('{}')
            environment = {'GITHUB_REPOSITORY':'example/repo', 'GH_TOKEN':'fixture', 'GITHUB_EVENT_PATH':str(event)}
            for history, expected in ((runs, (0, '', '')), (runs[1:], (1, 'FAIL scheduled observer: missed occurrence\n', ''))):
                result = process_run([sys.executable, '-c', code, str(ROOT), json.dumps(history), now.isoformat()],
                                     cwd=ROOT, env=environment, role='publisher', capture_output=True, text=True, timeout=10)
                self.assertEqual((result.returncode, result.stdout, result.stderr), expected)

if __name__ == '__main__':
    from suite_adapter import direct_main
    direct_main()
