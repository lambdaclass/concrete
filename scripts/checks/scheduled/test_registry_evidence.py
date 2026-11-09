"""Exact-SHA registry qualification through collection, publication and observation."""
import copy
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

import collect
import protocol
import publish
from test_protocol import NOW, RUNNERS, fixture_registry, record


class RegistryEvidence(unittest.TestCase):
    def setUp(self):
        scratch = tempfile.TemporaryDirectory(prefix='scheduled-registry-')
        self.addCleanup(scratch.cleanup)
        self.root = Path(scratch.name)
        self.registry = fixture_registry()
        row = self.registry['gates'][0]
        row.update(id='scaling-daily', command=['bash', 'campaign.sh'],
                   artifacts=[dict(path='report', sha_file='report/selected-sha.txt')])
        row['controls'] = [dict(id=f'control-{n}', command=['false'], exit_code=1,
                               diagnostic=f'exact refusal {n}') for n in range(5)]
        path = self.root / 'scripts/checks/gates.json'
        path.parent.mkdir(parents=True)
        path.write_text(json.dumps(self.registry))
        self.git('init', '-q'); self.git('add', '.')
        self.git('-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid',
                 '-c', 'core.hooksPath=/dev/null', 'commit', '-qm', 'selected registry')
        self.sha = self.git('rev-parse', 'HEAD')
        root = patch.object(protocol, 'REGISTRY_ROOT', self.root)
        root.start(); self.addCleanup(root.stop)
        self.payload = record()
        self.payload.update(sha=self.sha, gate_revision=self.sha)
        gate = self.payload['gates'][0]
        gate.update(id=row['id'], command=row['command'],
                    controls=[dict(id=c['id'], status='PASS', diagnostic=c['diagnostic']) for c in row['controls']],
                    artifacts=[dict(path='gate-0-artifacts/report', sha=self.sha, binding_status='PASS',
                                    files=[dict(path='gate-0-artifacts/report/selected-sha.txt', bytes=41,
                                                sha256=hashlib.sha256((self.sha+'\n').encode()).hexdigest())])])
        self.run = dict(id=1, run_attempt=1, event='schedule', created_at=self.payload['started'])
        self.job = dict(conclusion='success', runner_name='ci2-heavy', status='completed',
                        started_at=self.payload['started'], completed_at=self.payload['completed'])

    def git(self, *args):
        return subprocess.run(['git', '-C', str(self.root), *args], check=True,
                              capture_output=True, text=True, timeout=10).stdout.strip()

    def validate(self, payload):
        return protocol.validate(payload, self.sha, 'daily', RUNNERS, 'example/repo', '1/1')

    def publication(self, payload):
        """Exercise the real publication validator and JSON serialization, without a push."""
        real_run = subprocess.run
        saved = []
        def command(argv, **kwargs):
            if argv[:2] == ['git', 'notes']:
                saved.extend(json.loads(line) for line in kwargs['input'].splitlines())
                return subprocess.CompletedProcess(argv, 0)
            if argv[:2] == ['git', 'push']:
                return subprocess.CompletedProcess(argv, 0)
            return real_run(argv, **kwargs)
        with patch.object(publish, 'git', side_effect=['', '', 'local', 'local ref']), \
             patch.object(publish.subprocess, 'run', side_effect=command):
            publish.publish(payload, RUNNERS, 'example/repo', {})
        self.assertEqual(len(saved), 1)
        return saved[0]

    def check_mutation(self, payload, diagnostic):
        before = copy.deepcopy(payload)
        collected = collect.record_for(self.run, self.job, (self.sha, 'daily'), payload,
                                       payload['artifact'], 'example/repo', RUNNERS)
        for result in (self.validate(payload), collected, self.publication(payload)):
            self.assertEqual(result['qualification'][0], diagnostic)
            self.assertEqual(result['qualification_status'], diagnostic.split()[0])
            self.assertEqual(result['gates'], before['gates'])
            with self.assertRaises(protocol.EvidenceError) as caught:
                protocol.observe([result], self.sha, 'daily', NOW, RUNNERS, 'example/repo')
            self.assertEqual(str(caught.exception), diagnostic)
        self.assertEqual(payload, before)

    def test_registered_evidence_qualifies(self):
        for payload in (self.payload, self.publication(self.payload)):
            self.assertEqual(self.validate(payload)['qualification'], [])
            self.assertEqual(protocol.observe([payload], self.sha, 'daily', NOW, RUNNERS,
                                              'example/repo'), 'PASS daily ' + self.sha)

    def test_unregistered_gate(self):
        self.payload['gates'][0]['id'] = 'substitute'
        self.check_mutation(self.payload, 'FAIL registry: unregistered gate substitute')

    def test_extra_gate(self):
        extra = copy.deepcopy(self.payload['gates'][0]); extra['id'] = 'extra'
        self.payload['gates'].append(extra)
        self.check_mutation(self.payload, 'FAIL registry: unregistered gate extra')

    def test_append_preserves_existing_attempt_bytes(self):
        old = copy.deepcopy(self.payload)
        old['gates'][0]['controls'].pop()
        new = copy.deepcopy(self.payload); new['attempt'] = '1/2'
        history = protocol.append([old], new, self.sha, 'daily', RUNNERS, 'example/repo')
        self.assertEqual(history[0], old)
        self.assertNotIn('qualification', history[0])
        self.assertEqual(history[1]['qualification_status'], 'PASS')

    def test_missing_gate(self):
        self.payload['gates'] = []
        self.check_mutation(self.payload, 'INCOMPLETE registry: missing gate scaling-daily')

    def test_missing_control(self):
        self.payload['gates'][0]['controls'].pop()
        self.check_mutation(self.payload, 'INCOMPLETE registry: missing control scaling-daily/control-4')

    def test_invented_control(self):
        self.payload['gates'][0]['controls'] = [dict(id='invented', status='PASS', diagnostic='invented')]
        self.check_mutation(self.payload, 'FAIL registry: unregistered control scaling-daily/invented')

    def test_wrong_diagnostic(self):
        self.payload['gates'][0]['controls'][0]['diagnostic'] = 'substitute refusal'
        self.check_mutation(self.payload, 'FAIL registry: wrong diagnostic scaling-daily/control-0')

    def test_missing_artifact(self):
        self.payload['gates'][0]['artifacts'] = []
        self.check_mutation(self.payload, 'INCOMPLETE registry: missing artifact scaling-daily/report')

    def test_wrong_sha_binding(self):
        self.payload['gates'][0]['artifacts'][0]['sha'] = 'b' * 40
        self.check_mutation(self.payload, 'FAIL registry: wrong SHA binding scaling-daily/report')

    def test_wrong_command(self):
        self.payload['gates'][0]['command'] = ['true']
        self.check_mutation(self.payload, 'FAIL registry: wrong command scaling-daily')

    def test_binding_file_cannot_be_substituted(self):
        self.payload['gates'][0]['artifacts'][0]['files'][0]['sha256'] = '0' * 64
        self.check_mutation(self.payload, 'FAIL registry: wrong binding artifact scaling-daily/report/selected-sha.txt')

    def test_missing_binding_file(self):
        self.payload['gates'][0]['artifacts'][0]['files'] = []
        self.check_mutation(self.payload, 'INCOMPLETE registry: missing binding artifact scaling-daily/report/selected-sha.txt')

    def test_missing_all_controls(self):
        self.payload['gates'][0]['controls'] = []
        self.check_mutation(self.payload, 'INCOMPLETE registry: missing control scaling-daily/control-0')

    def test_duplicate_control(self):
        controls = self.payload['gates'][0]['controls']
        controls.append(copy.deepcopy(controls[0]))
        self.check_mutation(self.payload, 'FAIL registry: invalid control inventory scaling-daily')

    def test_duplicate_artifact(self):
        artifacts = self.payload['gates'][0]['artifacts']
        artifacts.append(copy.deepcopy(artifacts[0]))
        self.check_mutation(self.payload, 'FAIL registry: duplicate artifact scaling-daily')

    def test_substituted_artifact(self):
        self.payload['gates'][0]['artifacts'][0]['path'] = 'foreign-report'
        self.check_mutation(self.payload, 'FAIL registry: unregistered artifact scaling-daily/foreign-report')

    def test_unfinished_sha_binding(self):
        artifact = self.payload['gates'][0]['artifacts'][0]
        artifact.update(sha=None, binding_status='INCOMPLETE')
        self.check_mutation(self.payload, 'INCOMPLETE registry: missing SHA binding scaling-daily/report')

    def test_unsafe_artifact_file(self):
        self.payload['gates'][0]['artifacts'][0]['files'][0]['path'] = '../foreign'
        self.check_mutation(self.payload, 'FAIL registry: invalid artifact files scaling-daily/report')

    def test_newer_missing_attempt_revokes_qualification(self):
        partial = copy.deepcopy(self.payload); partial['attempt'] = '1/2'
        partial['gates'][0]['controls'].pop()
        with self.assertRaises(protocol.EvidenceError) as caught:
            protocol.observe([self.payload, partial], self.sha, 'daily', NOW, RUNNERS, 'example/repo')
        self.assertEqual(str(caught.exception), 'INCOMPLETE registry: missing control scaling-daily/control-4')

    def test_selected_commit_owns_registry(self):
        # Both the working copy and a newer committed registry disagree with the target.
        path = self.root / 'scripts/checks/gates.json'
        self.registry['gates'][0]['command'] = ['false']
        path.write_text(json.dumps(self.registry))
        self.git('add', '.')
        self.git('-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid',
                 '-c', 'core.hooksPath=/dev/null', 'commit', '-qm', 'newer registry')
        self.assertEqual(self.validate(self.payload)['qualification'], [])

    def test_missing_registry_is_nonqualifying_and_retained(self):
        with patch.object(protocol, 'REGISTRY_ROOT', self.root / 'absent'):
            self.check_mutation(self.payload, 'INCOMPLETE registry: unavailable at ' + self.sha)

    def test_forged_cached_qualification_is_recomputed(self):
        self.payload['qualification'] = []
        self.payload['gates'][0]['command'] = ['true']
        self.check_mutation(self.payload, 'FAIL registry: wrong command scaling-daily')

    def test_partial_attempt_can_recover_but_substitution_cannot(self):
        partial = copy.deepcopy(self.payload)
        partial['gates'][0]['artifacts'] = []
        complete = copy.deepcopy(self.payload); complete['attempt'] = '1/2'
        self.assertEqual(protocol.observe([partial, complete], self.sha, 'daily', NOW, RUNNERS,
                                          'example/repo'), 'PASS daily ' + self.sha)
        partial['gates'][0]['command'] = ['false']
        with self.assertRaises(protocol.EvidenceError) as caught:
            protocol.observe([partial, complete], self.sha, 'daily', NOW, RUNNERS, 'example/repo')
        self.assertEqual(str(caught.exception), 'FAIL registry: wrong command scaling-daily')
