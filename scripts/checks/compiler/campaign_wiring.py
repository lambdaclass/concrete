"""C's policy controls use the single registry's public reader; no scheduler policy."""
import copy
from pathlib import Path
import sys
import yaml

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / 'scripts/checks/lib'))
import gate_manifest as registry

HOST = '${{ github.event_name != \'pull_request\' && fromJSON(\'["self-hosted","linux","x64","concrete","concrete-ci-2"]\') || \'ubuntu-24.04\' }}'

def require(ok, diagnostic):
    if not ok:
        raise ValueError(diagnostic)

def check(data, workflow, schedule):
    rows = {r['id']: r for r in registry.load()['gates']} if data is None else {r['id']: r for r in data['gates']}
    for tier in ('pr', 'qualification', 'daily'):
        gid = 'scaling-' + tier
        require(gid in rows, f'C registry: missing {gid}')
        row = rows[gid]
        require(row['tiers'] == (['daily'] if tier == 'daily' else ['pr','qualification']), f'C registry: misrouted {gid}')
        command = ([] if tier == 'daily' else ['nix','develop','--command']) + ['env','SELECTED_TIER='+tier,'bash','scripts/checks/compiler/scaling.sh',gid+'-report']
        require(row['command'] == command, f'C registry: wrong campaign tier command: {gid}')
        require(row['artifacts'] == [dict(path=gid+'-report',sha_file=gid+'-report/sha.txt')], f'C registry: report binding drift: {gid}')
        controls = {c['id'] for c in row['controls']}
        require(controls >= {'refusal-unauthorised-plant','refusal-invalid-tier','refusal-forged-runner','refusal-unauthorized-sha','refusal-mismatched-sha'}, f'C registry: missing exact-diagnostic controls: {gid}')
    job = workflow['jobs']['scaling']
    require(job['runs-on'] == HOST, 'C workflow: unsafe scaling runner routing')
    steps = job['steps']
    checkout = next(s for s in steps if s.get('uses') == 'actions/checkout@v4')
    require(checkout.get('with') == {'ref': "${{ github.event_name != 'pull_request' && github.sha || '' }}", 'persist-credentials': False}, 'C workflow: unpinned scaling checkout')
    for gid,condition in [('scaling-pr', "github.event_name == 'pull_request'"),('scaling-qualification', "github.event_name != 'pull_request'")]:
        step = next(s for s in steps if s.get('id') == 'gate-'+gid)
        require(step.get('if') == condition, f'C workflow: unsafe event condition: {gid}')
    env = next(s for s in steps if s.get('id') == 'gate-scaling-qualification')['env']
    for key,value in [('SELECTED_SHA','${{ github.sha }}'),('RUNNER_ENVIRONMENT','${{ runner.environment }}'),('SCHEDULED_ATTEMPT_ID','${{ github.run_id }}/${{ github.run_attempt }}')]:
        require(env.get(key) == value, 'C workflow: missing binding: '+key)
    on = schedule.get('on', schedule.get(True))
    require({x['cron'] for x in on['schedule']} >= {'17 2 * * *','43 3 * * 0'}, 'C schedule: missing daily/weekly cron')
    select = next(s for s in schedule['jobs']['select']['steps'] if s.get('id') == 'select')['run']
    require('tier=daily' in select and 'SCHEDULE_SELECTION' in select and 'merge-base --is-ancestor' in select, 'C schedule: missing authorized daily selection')
    campaign = schedule['jobs']['campaign']
    require(campaign['runs-on'] == ['self-hosted','linux','x64','concrete','concrete-ci-2'], 'C schedule: unsafe runner routing')
    step = next(s for s in campaign['steps'] if s.get('name') == 'Run exact-SHA tier')
    require('python3 scripts/checks/scheduled/run.py "$SELECTED_SHA" "$SELECTED_TIER" result-index' in step['run'] and step['env'].get('SELECTED_SHA') == '${{ needs.select.outputs.sha }}' and step['env'].get('SELECTED_TIER') == '${{ needs.select.outputs.tier }}', 'C schedule: missing exact-SHA tier execution')
    registry.check_workflow(data, workflow=workflow)

def main():
    data = registry.load()
    workflow = yaml.safe_load((ROOT / '.github/workflows/lean_action_ci.yml').read_text())
    schedule = yaml.safe_load((ROOT / '.github/workflows/gate_schedule.yml').read_text())
    check(data, workflow, schedule)
    def control(label, diagnostic, mutate):
        d,w,s = copy.deepcopy((data,workflow,schedule))
        mutate(d,w,s)
        try:
            check(d,w,s)
        except ValueError as error:
            require(str(error) == diagnostic, f'{label}: wrong diagnostic: {error}')
        else:
            raise ValueError(label+': planted defect passed')
        print('ok C wiring control '+label+': '+diagnostic)
    for tier in ('pr','qualification','daily'):
        gid='scaling-'+tier
        control('missing-'+gid, 'C registry: missing '+gid, lambda d,w,s,gid=gid: d['gates'].__setitem__(slice(None), [r for r in d['gates'] if r['id']!=gid]))
        control('wrong-tier-'+gid, 'C registry: wrong campaign tier command: '+gid, lambda d,w,s,gid=gid: next(r for r in d['gates'] if r['id']==gid)['command'].__setitem__(-4,'SELECTED_TIER=invalid'))
        control('misrouted-'+gid, 'C registry: misrouted '+gid, lambda d,w,s,gid=gid: next(r for r in d['gates'] if r['id']==gid).__setitem__('tiers',['weekly']))
    control('missing-controls','C registry: missing exact-diagnostic controls: scaling-daily',lambda d,w,s: next(r for r in d['gates'] if r['id']=='scaling-daily').__setitem__('controls',[]))
    control('unsafe-host','C workflow: unsafe scaling runner routing',lambda d,w,s:w['jobs']['scaling'].__setitem__('runs-on','ubuntu-latest'))
    control('unpinned-checkout','C workflow: unpinned scaling checkout',lambda d,w,s:next(x for x in w['jobs']['scaling']['steps'] if x.get('uses')=='actions/checkout@v4').__setitem__('with',{}))
    control('heavy-pr','C workflow: unsafe event condition: scaling-qualification',lambda d,w,s:next(x for x in w['jobs']['scaling']['steps'] if x.get('id')=='gate-scaling-qualification').__setitem__('if',"github.event_name == 'pull_request'"))
    control('missing-sha','C workflow: missing binding: SELECTED_SHA',lambda d,w,s:next(x for x in w['jobs']['scaling']['steps'] if x.get('id')=='gate-scaling-qualification')['env'].pop('SELECTED_SHA'))
    control('daily-selection','C schedule: missing authorized daily selection',lambda d,w,s:next(x for x in s['jobs']['select']['steps'] if x.get('id')=='select').__setitem__('run','exit 0'))
    control('daily-execution','C schedule: missing exact-SHA tier execution',lambda d,w,s:next(x for x in s['jobs']['campaign']['steps'] if x.get('name')=='Run exact-SHA tier').__setitem__('run','exit 0'))
    control('literal-command','registry: workflow command drift: determinism',lambda d,w,s:next(x for x in w['jobs']['determinism']['steps'] if x.get('id')=='gate-determinism').__setitem__('run','nix develop --command timeout 3000 bash scripts/checks/compiler/determinism.sh'))
    return 0

if __name__ == '__main__':
    try:
        sys.exit(main())
    except (ValueError, KeyError, StopIteration) as error:
        sys.exit('FAIL C campaign wiring: '+str(error))
