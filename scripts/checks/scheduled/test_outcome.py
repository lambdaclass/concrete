import runpy
from pathlib import Path
runpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))

import datetime as dt
import tempfile
import sys
import unittest
from unittest.mock import patch
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
            runs.append(dict(id=offset, created_at=created.isoformat(), event='schedule', status='completed', conclusion='success'))
        observer_watch.verify_occurrences(runs, now)
        for index in (0, 5, 10):
            missing = runs[:index] + runs[index+1:]
            with self.assertRaisesRegex(ValueError, '^FAIL scheduled observer: missed occurrence$'):
                observer_watch.verify_occurrences(missing, now)
        # Publisher runs on another workflow and uses external Actions history.
        import collect
        import contextlib, io, json
        with tempfile.TemporaryDirectory() as scratch:
            event = Path(scratch) / 'event.json'; event.write_text('{}')
            output = io.StringIO()
            def pages(path, key, **kwargs):
                return runs[1:] if 'gate_freshness.yml' in path else []
            with patch.object(collect, 'pages', side_effect=pages), patch.object(collect, 'inventory', return_value={}), contextlib.redirect_stdout(output):
                from process_adapter import operation_clock
                result = collect.collect_event({'GITHUB_REPOSITORY':'example/repo', 'GH_TOKEN':'fixture', 'GITHUB_EVENT_PATH':str(event)}, clock=operation_clock(observation=now))
            self.assertEqual((result, output.getvalue()), (1, 'FAIL scheduled observer: missed occurrence\n'))

if __name__ == '__main__':
    from suite_adapter import direct_main
    direct_main()
