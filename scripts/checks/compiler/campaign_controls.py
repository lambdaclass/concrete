"""Light controls for C's real refusal commands and nested outcome lifecycle."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / 'scripts/checks/lib'))
import gate_manifest
import gate_outcome


def run(argv, env):
    return subprocess.run(argv, cwd=ROOT, env=env, stdin=subprocess.DEVNULL,
                          capture_output=True, text=True, timeout=30)


def main():
    env = dict(os.environ)
    env.pop('SCHEDULED_OUTCOME_FILE', None)
    env.pop('SCHEDULED_OUTCOME_HELPER', None)
    sha = subprocess.check_output(['git','rev-parse','HEAD'], cwd=ROOT, text=True, timeout=10).strip()
    env.update(SELECTED_SHA=sha, SELECTED_TIER='daily')
    rows = {r['id']: r for r in gate_manifest.load()['gates']}
    for gid in ('scaling-daily','determinism'):
        for control in rows[gid]['controls']:
            result = run(control['command'],env)
            diagnostic = (result.stdout+result.stderr).strip()
            assert (result.returncode,diagnostic) == (control['exit_code'],control['diagnostic']), (control['id'],result.returncode,diagnostic)
            print('ok C registry control '+control['id']+': '+diagnostic)
    with tempfile.TemporaryDirectory() as scratch:
        for label,body,want,code in (
            ('real-exit-124','bounded 1 bash -c "exit 124"', 'FAIL',1),
            ('dependency-incomplete',"campaign_outcome INCOMPLETE 'inventory unavailable'; exit 125", 'INCOMPLETE',125),
            ('nested-timeout','bounded 0.02 bash -c "sleep 1"', 'INCOMPLETE',125),
            ('failure-before-timeout','bounded 1 false || :; bounded 0.02 bash -c "sleep 1"','FAIL',1),
            ('expected-refusal','CAMPAIGN_EXPECT_FAILURE=1 bounded 1 false || :','PASS',0),
        ):
            events = Path(scratch)/label
            events.touch()
            selected = dict(env, SCHEDULED_OUTCOME_FILE=str(events), SCHEDULED_OUTCOME_HELPER=str(ROOT/'scripts/checks/lib/gate_outcome.py'))
            result = run(['bash','-euc','root=$1; source "$root/scripts/checks/compiler/campaign.sh"; '+body,'control',str(ROOT)],selected)
            status = gate_outcome.result(events,result.returncode,sha)
            assert status == code, (label,result.returncode,status,result.stdout,result.stderr)
            statuses = {e['status'] for e in map(json.loads,events.read_text().splitlines())}
            assert (want == 'PASS' and not statuses) or want in statuses, (label,statuses)
            print('ok C lifecycle '+label+': '+want)
    return 0

if __name__ == '__main__':
    sys.exit(main())
