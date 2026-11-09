#!/usr/bin/env python3
"""The sole gate-registry reader/validator and CI command executor."""
from scheduled_process import run as process_run
import argparse
import copy
import json
import math
import os
import uuid
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[3]
REGISTRY = ROOT / 'scripts/checks/gates.json'
TIERS = {'pr', 'qualification', 'daily', 'weekly'}
RUNNERS = {'ci', 'github-hosted', 'native-matrix', 'concrete-ci-2'}
FIELDS = {'id', 'tiers', 'command', 'budget_seconds', 'runner_class', 'artifacts', 'tools', 'seed', 'controls', 'workflow'}

class GateManifestError(ValueError):
    pass

def require(ok, message):
    if not ok:
        raise GateManifestError(message)

def exact_fields(value, fields, where):
    require(isinstance(value, dict), f'registry: invalid {where} shape')
    unknown = sorted(set(value) - fields)
    require(not unknown, f'registry: unknown {where} field: {unknown[0] if unknown else ""}')
    require(set(value) == fields, f'registry: missing {where} fields')

def argv(value):
    return isinstance(value, list) and bool(value) and all(isinstance(x, str) and x for x in value)

def relative(value):
    return isinstance(value, str) and bool(value) and not Path(value).is_absolute() and '..' not in Path(value).parts and value != '.' and '\n' not in value and '\r' not in value

def validate_manifest(data):
    exact_fields(data, {'schema', 'windows', 'gates'}, 'document')
    require(type(data['schema']) is int and data['schema'] == 1, 'registry: unsupported schema')
    require(isinstance(data['windows'], dict) and set(data['windows']) == {'daily','weekly'} and
            all(type(v) is int and v > 0 for v in data['windows'].values()), 'registry: invalid freshness windows')
    require(isinstance(data['gates'], list) and data['gates'], 'registry: empty gates')
    seen = set(); bindings = set()
    for row in data['gates']:
        exact_fields(row, FIELDS, 'gate')
        gid = row['id']
        require(isinstance(gid, str) and re.fullmatch(r'[a-z0-9][a-z0-9_-]*', gid), 'registry: invalid gate id')
        require(gid not in seen, f'registry: duplicate gate id: {gid}'); seen.add(gid)
        tiers = row['tiers']
        require(isinstance(tiers, list) and tiers and all(isinstance(t, str) and t in TIERS for t in tiers) and
                len(set(tiers)) == len(tiers), f'registry: invalid tiers: {gid}')
        require(argv(row['command']), f'registry: invalid command: {gid}')
        require(type(row['budget_seconds']) is int and row['budget_seconds'] > 0, f'registry: invalid budget: {gid}')
        require(isinstance(row['runner_class'],str) and row['runner_class'] in RUNNERS, f'registry: invalid runner class: {gid}')
        scheduled = bool(set(tiers) & {'daily','weekly'})
        require(not scheduled or row['runner_class'] == 'concrete-ci-2', f'registry: scheduled runner class mismatch: {gid}')
        require(isinstance(row['tools'], dict) and all(isinstance(k,str) and isinstance(v,str) for k,v in row['tools'].items()), f'registry: invalid tools: {gid}')
        require(row['seed'] is None or type(row['seed']) is int, f'registry: invalid seed: {gid}')
        require(isinstance(row['artifacts'], list), f'registry: invalid artifacts: {gid}')
        paths = set()
        for artifact in row['artifacts']:
            exact_fields(artifact, {'path','sha_file'}, 'artifact')
            require(relative(artifact['path']) and artifact['path'] not in paths, f'registry: invalid artifact path: {gid}')
            paths.add(artifact['path'])
            require((artifact['sha_file'] is None and not scheduled) or relative(artifact['sha_file']), f'registry: missing artifact SHA binding: {gid}')
            require(artifact['sha_file'] is None or Path(artifact['sha_file']).is_relative_to(artifact['path']), f'registry: artifact SHA binding outside artifact: {gid}')
        require(isinstance(row['controls'], list) and (row['controls'] or not scheduled), f'registry: missing scheduled controls: {gid}')
        controls = set()
        for control in row['controls']:
            exact_fields(control, {'id','command','exit_code','diagnostic'}, 'control')
            require(isinstance(control['id'],str) and control['id'] and control['id'] not in controls, f'registry: invalid control id: {gid}')
            controls.add(control['id'])
            require(argv(control['command']) and type(control['exit_code']) is int and control['exit_code'] not in (0,125) and
                    isinstance(control['diagnostic'],str) and control['diagnostic'].strip(), f'registry: invalid control: {gid}')
        binding = row['workflow']
        if binding is not None:
            exact_fields(binding, {'job','step'}, 'workflow binding')
            require(all(isinstance(v,str) and v for v in binding.values()), f'registry: invalid workflow binding: {gid}')
            pair=(binding['job'],binding['step'])
            require(pair not in bindings, f'registry: duplicate workflow binding: {gid}'); bindings.add(pair)
    return data

def load(path=REGISTRY):
    try:
        return validate_manifest(json.loads(Path(path).read_text()))
    except (OSError, json.JSONDecodeError) as error:
        raise GateManifestError(f'registry: cannot read registry: {error}') from error

def tier_rows(tier, data=None):
    require(tier in TIERS, f'registry: invalid tier: {tier}')
    return [dict(copy.deepcopy(row), tier=tier) for row in (load() if data is None else validate_manifest(data))['gates'] if tier in row['tiers']]

def scheduled_rows(tier, data=None):
    require(tier in {'daily','weekly'}, f'registry: invalid scheduled tier: {tier}')
    return tier_rows(tier, data)

TIER_EXPRESSION = "${{ github.event_name == 'pull_request' && 'pr' || 'qualification' }}"

def invocation(gid):
    return f'python3 scripts/checks/lib/gate_manifest.py execute --id {gid} --tier {TIER_EXPRESSION}'

def check_workflow(data=None, root=ROOT, workflow=None):
    # YAML is read only here; all registry parsing goes through load/validate_manifest.
    import yaml
    data = load() if data is None else validate_manifest(data)
    document = yaml.safe_load((root / '.github/workflows/lean_action_ci.yml').read_text()) if workflow is None else workflow
    jobs = document['jobs']; expected = {}
    for row in data['gates']:
        binding = row['workflow']
        if binding:
            require({'pr','qualification'} <= set(row['tiers']), f'registry: workflow tier mismatch: {row["id"]}')
            expected[(binding['job'],binding['step'])] = row
    found=set()
    for jobid,job in jobs.items():
        budget=0
        artifact_outputs=set()
        for step in job['steps']:
            if 'run' not in step: continue
            command=step['run']
            if re.fullmatch(r'sleep [0-9]+',command): continue  # install retry, not a gate
            pair=(jobid,step.get('name'))
            require(pair in expected, f'registry: unregistered workflow command: {jobid}/{step.get("name")}')
            row=expected[pair]
            require(command == invocation(row['id']), f'registry: workflow command drift: {row["id"]}')
            require(pair not in found, f'registry: duplicate workflow invocation: {row["id"]}')
            found.add(pair); budget += row['budget_seconds']
            if row['artifacts']:
                stepid='gate-'+row['id']
                require(step.get('id')==stepid, f'registry: workflow artifact producer drift: {row["id"]}')
                artifact_outputs.add('${{ steps.'+stepid+'.outputs.artifact_paths }}')
            host=job['runs-on']; kind=row['runner_class']
            require((kind=='native-matrix' and host=='${{ matrix.runner }}') or
                    (kind=='github-hosted' and isinstance(host,str) and host.startswith(('ubuntu-','macos-'))) or
                    (kind=='ci' and isinstance(host,str) and "github.event_name != 'pull_request'" in host),
                    f'registry: workflow runner class drift: {row["id"]}')
        uploads=[s.get('with',{}).get('path') for s in job['steps'] if s.get('uses','').startswith('actions/upload-artifact@')]
        require(set(uploads)==artifact_outputs and len(uploads)==len(artifact_outputs), f'registry: workflow artifact path drift: {jobid}')
        require(job.get('timeout-minutes') == math.ceil(budget*3/60)+10, f'registry: workflow budget drift: {jobid}')
    for pair,row in expected.items():
        require(pair in found, f'registry: missing workflow invocation: {row["id"]}')
    # Root checks discover files dynamically; inventory completeness prevents row deletion.
    for path in sorted((root / 'scripts/checks').glob('*.sh')):
        if path.name == 'run.sh':continue
        matched=[r for r in data['gates'] if r['command'] == ['bash',str(path.relative_to(root))]]
        require(len(matched)==1, f'registry: missing root gate row: {path.stem}')
    return data

def execute(gid,tier,environment):
    matches=[r for r in tier_rows(tier) if r['id']==gid]
    require(len(matches)==1, f'registry: gate not registered in tier: {gid}/{tier}')
    row=matches[0]
    if environment.get('GITHUB_OUTPUT'):
        delimiter='gate-artifacts-'+uuid.uuid4().hex
        with open(environment['GITHUB_OUTPUT'],'a') as stream:
            stream.write('artifact_paths<<'+delimiter+'\n'+'\n'.join(a['path'] for a in row['artifacts'])+'\n'+delimiter+'\n')
    # The same bounded runner owns CI and scheduled budgets; no command in YAML.
    return process_run(['bash',str(ROOT/'scripts/checks/lib/bounded.sh'),str(row['budget_seconds']),*row['command']],
                          cwd=ROOT, timeout=row['budget_seconds']*3+60, env=environment).returncode

def main():
    environment = dict(os.environ)
    parser=argparse.ArgumentParser(); parser.add_argument('operation',choices=['validate','check-workflow','tier','scheduled','execute','execute-root'])
    parser.add_argument('--tier'); parser.add_argument('--id'); parser.add_argument('--path');args=parser.parse_args()
    if args.operation=='validate':load()
    elif args.operation=='check-workflow':check_workflow()
    elif args.operation=='execute':return execute(args.id,args.tier,environment)
    elif args.operation=='execute-root':
        rows=[r for r in load()['gates'] if r['command']==['bash',args.path]]
        require(len(rows)==1, f'registry: missing root gate row: {Path(args.path).stem}')
        return execute(rows[0]['id'], args.tier or 'qualification',environment)
    else:print(json.dumps((scheduled_rows if args.operation=='scheduled' else tier_rows)(args.tier)))
    return 0

if __name__=='__main__':
    try:sys.exit(main())
    except GateManifestError as error:sys.exit(str(error))
