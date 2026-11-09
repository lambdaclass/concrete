"""C integration with the public exact-SHA scheduled-evidence validator.

Synthetic report metadata exercises qualification only; no campaign runs or notes
are published. Inventories and diagnostics come from the committed registry.
"""
import copy
import datetime as dt
import hashlib
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / 'scripts/checks/scheduled'))
import protocol
import collect


def main():
    sha = subprocess.check_output(['git', '-C', str(ROOT), 'rev-parse', 'HEAD'],
                                  env={}, text=True, timeout=10).strip()
    rows = protocol._registry.scheduled_rows('daily', protocol.registry_at(sha))
    stamp = '2027-01-15T00:00:00+00:00'
    now = dt.datetime.fromisoformat(stamp).timestamp()
    runners = {'example/repo': {'ci2-control'}}
    payload = dict(sha=sha, tier='daily', repository='example/repo', attempt='1/1',
                   runner_environment='self-hosted', runner_name='ci2-control',
                   status='PASS', conclusion='success', started=stamp, completed=stamp,
                   occurrence='daily/1', artifact='https://example.invalid/artifact/1',
                   gate_revision=sha, gates=[])
    for number, row in enumerate(rows):
        artifacts = []
        for declaration in row['artifacts']:
            prefix = f'gate-{number}-artifacts/'
            artifacts.append(dict(path=prefix+declaration['path'], sha=sha, binding_status='PASS',
                                  files=[dict(path=prefix+declaration['sha_file'], bytes=41,
                                              sha256=hashlib.sha256((sha+'\n').encode()).hexdigest())]))
        payload['gates'].append(dict(id=row['id'], status='PASS', command=row['command'],
                                    tools=row['tools'], seed=row['seed'], resources={},
                                    log=f'gate-{number}.log', artifacts=artifacts,
                                    controls=[dict(id=c['id'], status='PASS', diagnostic=c['diagnostic'])
                                              for c in row['controls']]))
    def validate(record):
        return protocol.validate(record, sha, 'daily', runners, 'example/repo', '1/1')
    assert validate(payload)['qualification_status'] == 'PASS'
    assert protocol.observe([payload], sha, 'daily', now, runners, 'example/repo') == 'PASS daily '+sha
    target = next(g for g in payload['gates'] if g['id'] == 'scaling-daily')
    cid = target['controls'][0]['id']
    artifact = next(r for r in rows if r['id'] == target['id'])['artifacts'][0]['path']
    cases = [
        ('substitute-gate', 'FAIL registry: unregistered gate substitute',
         lambda g: g.update(id='substitute')),
        ('invented-control', 'FAIL registry: unregistered control scaling-daily/invented',
         lambda g: g.update(controls=[dict(id='invented', status='PASS', diagnostic='invented')], artifacts=[])),
        ('missing-control', 'INCOMPLETE registry: missing control scaling-daily/'+cid,
         lambda g: g['controls'].pop(0)),
        ('wrong-diagnostic', 'FAIL registry: wrong diagnostic scaling-daily/'+cid,
         lambda g: g['controls'][0].update(diagnostic='invented')),
        ('missing-report', 'INCOMPLETE registry: missing artifact scaling-daily/'+artifact,
         lambda g: g.update(artifacts=[])),
        ('wrong-command', 'FAIL registry: wrong command scaling-daily',
         lambda g: g.update(command=['true'])),
    ]
    run = dict(id=1, run_attempt=1, event='schedule', created_at=stamp)
    job = dict(conclusion='success', runner_name='ci2-control', status='completed',
               started_at=stamp, completed_at=stamp)
    for label, diagnostic, mutate in cases:
        planted = copy.deepcopy(payload)
        mutate(next(g for g in planted['gates'] if g['id'] == 'scaling-daily'))
        # A producer's cached PASS must never bypass recomputation.
        planted.update(qualification=[], qualification_status='PASS')
        collected = collect.record_for(run, job, (sha, 'daily'), planted,
                                       planted['artifact'], 'example/repo', runners)
        appended = protocol.append([], planted, sha, 'daily', runners, 'example/repo')[0]
        for result in (validate(planted), collected, appended):
            assert result['qualification'][0] == diagnostic, (label, result['qualification'])
            try:
                protocol.observe([result], sha, 'daily', now, runners, 'example/repo')
            except protocol.EvidenceError as error:
                expected = diagnostic.split(' ', 1)[0]+' daily '+sha+': '+diagnostic
                assert str(error) == expected, (label, str(error))
                assert error.status == diagnostic.split(' ', 1)[0], (label, error.status)
            else:
                raise AssertionError(label+': planted evidence qualified')
        print('ok C scheduled evidence '+label+': '+diagnostic)
    return 0


if __name__ == '__main__':
    sys.exit(main())
