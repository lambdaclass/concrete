#!/usr/bin/env python3
"""Landing inventory: explicit assignments, checked against a fresh Lean census.

The generator is deliberately separate from the judgment: checking never fills a missing row.
"""
import argparse
from collections import Counter, defaultdict
import csv
import hashlib
import io
from functools import lru_cache
import json
import os
import re
from pathlib import Path
import subprocess
import sys
import tempfile

from landing_census import census
from landing_controls import check_census

ROOT = Path(__file__).resolve().parents[2]
SOURCE = 'aa44140a33747fa37ece59c76090972e564c65b4'
BASE = '1a50c64ff1b6f3a2899522f279eccc86d3b004a2'
EVIDENCE = '5db3018e24eb284894b3615833aa373c76944475'
DOSSIER = '2bef2b5e20dffb5b277f9451be6fef969cf6c898'
MANIFEST = ROOT / 'ConcreteSpec/landing/inventory.tsv'
FIELDS = ['record', 'name', 'kind', 'source', 'line', 'end_line', 'piece',
          'prerequisites', 'disposition', 'contract', 'digest', 'dependencies', 'migration', 'note', 'term_digest', 'regeneration']

# Plan prerequisites, not an order guessed from filenames. External answers are explicit holds.
PREREQS = {
 'BASE': [], 'L': ['BASE'], 'D1':['L'], 'D2':['D1'], 'D3':['D2'], 'D4':['D3'], 'P':['D3'],
 'T0':['D3','P','D4'], 'T3a0':['D3','P'], 'T3a1':['T3a0'], 'T3a2':['T3a1'],
 'T3a3':['T3a2'], 'T3a4':['T3a3'], 'T3b1':['T3a4'], 'T3b2':['T3b1'],
 'T1a':['T0','T3b1'], 'T1b':['T1a'], 'T1c':['T1b','T3b2'], 'T2':['T1c'],
 'F01':['P'], 'F02':['F01','D2'],
 'Activation':['T1c','T3b2'] + [f'M{i:02}' for i in range(1,13)],
 'F03':['F02','D3'], 'F03p-design':['F03','D4'], 'F03p':['F03p-design'],
 'F04':['F03'], 'F04p':['F03p','F04'], 'F05':['F03'], 'R':['F03'],
 'F06':['F05','R','D4'], 'F07':['F06'], 'F08':['F05'],
 'F09a':['F08'], 'F09b':['F09a'], 'F09c':['F09b','D4'], 'F10':['F09c'],
 'F11':['F05'], 'F12':['F08','F11'], 'F13':['F12','F08','R'],
 'F14':['F13','F10'], 'F15':['F14'], 'F16':['F15'], 'CAP':['F03'],
 'F17':['F16','R','CAP'], 'F18':['F10','F04','CAP'],
 'F19':['F10','F04p'], 'F20':['F19'], 'F20p':['F20','F03p'],
 'F21':['F20'], 'F22':['F20'], 'F23':['F20','F10'],
 'F24a':['F20','F10'], 'F24b':['F24a'], 'F25a':['F24b'], 'F25b':['F25a'],
 'F26':['F10'], 'F27':['F26'], 'F28':['F12','F19'],
 'F29':['F28','F20','F11','F10'], 'F29p':['F29','F20p'],
 'F30':['F05','F10'], 'F31':['F30'], 'F32':['F31','F16'],
 'F33':['F32','F12'], 'F34':['F32','F10','F23'],
 'F35':['F33','F34','F23'], 'F36':['F35','F16'], 'F37':['F35','F17'],
 'F38':['F36','F37','F10'], 'F39-design':['CAP','F17','F35'],
 'F39':['F39-design','F17','F35'], 'F40':['F33','F36','F37','F38','F39'],
}
for i in range(1,13):
    PREREQS[f'M{i:02}'] = ['F02','T1c']
for p in list(PREREQS):
    if p.startswith('F') and p not in ('F01','F02','F03p-design','F39-design'):
        PREREQS[p] += ['P','Activation']
for i in range(1,11):
    PREREQS[f'C{i}-close'] = ['F40','F07','F18','F21','F22','F25b','T2','F27','F29p']
PREREQS['Freeze'] = [f'C{i}-close' for i in range(1,11)]
HOLDS = {'T2':'accepted bounded helper-contradiction extension design', 'F03p':'path character/normalization owner answer', 'F04p':'path authority',
 'F06':'independent constant authority (D4)', 'F08':'renewed lifecycle answer',
 'F09c':'law-subject/determinism authority (D4)', 'F12':'representation extensions held',
 'F18':'accepted ISS-0050 redesign', 'F19':'accepted ISS-0047 redesign; repair ISS-0045/0048',
 'F20p':'approved path policy', 'F27':'wiping owner answer',
 'F28':'startup/ABI owner answer', 'F29':'startup/ABI authority',
 'F29p':'approved source policy', 'F33':'representation extensions held',
 'F39':'accepted Authority view design'}
WHOLE = {
 'Spec/Basics':'F02', 'Proofs/Basics':'M01', 'Spec/Slice1Syntax':'F31',
 'Spec/Statics':'F31','Spec/Slice1Ownership':'F31','Spec/Slice1Semantics':'F35',
 'Spec/Slice1Checker':'F35','Spec/Slice0':'F35','Proofs/Slice0':'F35',
 'Spec/Observation':'F35','Proofs/Observe':'F35', 'Spec/Examples':'F40','Proofs/Examples':'F40',
 'Spec/Rep':'F33','Proofs/Rep':'F33','Proofs/Checker':'F32','Proofs/Slice1':'F34',
 'Spec/Borrow':'F36','Proofs/Borrow':'F36','Spec/Brand':'F37','Proofs/Brand':'F37',
 'Spec/Cleanup':'F38','Proofs/Cleanup':'F38','Spec/Authority':'F39','Proofs/Authority':'F39',
 'Spec/Target':'F03','Spec/Outcome':'F03','Spec/Scalar':'F05','Proofs/Scalar':'F05',
 'Spec/Const':'F06','Proofs/Const':'F07','Spec/Exec':'F08',
 'Spec/Memory':'F11','Proofs/Memory':'F11','Spec/Hybrid':'F12','Proofs/Hybrid':'F12',
 'Spec/Own':'F13','Spec/Grant':'F17','Proofs/Grant':'F17',
 'Spec/Prim':'F18','Proofs/Prim':'F18','Spec/Observe':'F19',
 'Proofs/ObserveEnvironment':'F21','Proofs/ObserveOutput':'F21',
 'Proofs/ObserveSource':'F22','Proofs/ObserveEvidence':'F22','Proofs/ObserveTransform':'F22',
 'Proofs/ObserveReport':'F23','Spec/Adequacy':'F24a', 'Spec/Frame':'F28',
 'Spec/Rules':'F17', 'Spec/Surface0':'F36','Proofs/Surface0':'F36',
}
REGENERATE_MODULES = tuple('ConcreteSpec.'+stem.replace('/','.') for stem in WHOLE if stem.startswith('Spec/'))

CONTRACTS = {'Ident':'C9','Target':'C3','Outcome':'C1','Exec':'C1','Scalar':'C2',
 'Const':'C2','Memory':'C4','Hybrid':'C4','Own':'C5','Grant':'C5','Prim':'C9',
 'Leak':'C7','Adequacy':'C8','Frame':'C10','Authority':'C5','Brand':'C5',
 'Borrow':'C5','Cleanup':'C1','Rep':'C4','Basics':'C9','Rules':'C9'}


@lru_cache(maxsize=1)
def git_local_variables():
    env = {k:v for k,v in os.environ.items() if k not in ('GIT_DIR','GIT_WORK_TREE','GIT_COMMON_DIR')}
    return set(subprocess.check_output(['git','rev-parse','--local-env-vars'],env=env,text=True).splitlines())


def git(*args):
    env = {k:v for k,v in os.environ.items() if k not in git_local_variables()}
    return subprocess.check_output(['git', *args], cwd=ROOT, env=env, stderr=subprocess.DEVNULL)


def sha(data):
    return hashlib.sha256(data).hexdigest()


@lru_cache(maxsize=None)
def source(ref, path):
    return git('show', f'{ref}:{path}')


def piece(file, line):
    stem = file.removeprefix('ConcreteSpec/').removesuffix('.lean')
    if stem == 'Spec/Examples' and 326 <= line <= 392:
        return 'F35'
    if stem == 'Proofs/Examples' and line <= 64:
        return 'F35'
    if stem in WHOLE:
        return WHOLE[stem]
    if stem in ('Spec','ConcreteSpec') or file == 'ConcreteSpec.lean':
        return 'F40'
    if stem == 'Spec/Ident':
        if 35 <= line <= 44 or 925 <= line <= 932:
            return 'F02'
        return 'F03p' if 704 <= line <= 826 or 917 <= line <= 923 else 'F03'
    if stem == 'Proofs/Ident':
        return 'F04p' if line < 281 or 1300 <= line <= 1305  or 2341 <= line <= 2355 else 'F04'
    if stem == 'Proofs/Exec':
        for lo, hi, p in [(1,344,'F09a'),(354,1402,'F09b'),(1404,1943,'F09c'),
                          (2301,2321,'F09a'),(2323,2902,'F09b'),
                          (3753,3883,'F09a'),(3884,3908,'F09b'),(3997,4048,'F09c')]:
            if lo <= line <= hi:
                return p
        return 'F10'
    if stem == 'Proofs/Own':
        if 2170 <= line <= 4405 or 4903 <= line <= 4943:
            return 'F15'
        return 'F16' if line >= 4947 else 'F14'
    if stem == 'Proofs/Adequacy':
        for lo, hi, p in [(1,787,'F24a'),(804,1060,'F24b'),(1065,1571,'F25a'),
                          (2428,2654,'F25a'),(2674,2871,'F24a'),(2872,2879,'F24b'),
                          (2880,2893,'F24a'),(2939,2967,'F24a'),(2977,3016,'F24b'),
                          (3030,3038,'F25a')]:
            if lo <= line <= hi:
                return p
        return 'F25b'
    if stem == 'Proofs/Observe2':
        return 'F20p' if 967 <= line <= 1011 else 'F20'
    if stem == 'Proofs/Frame':
        return 'F29p' if 2334 <= line <= 2353 or 2429 <= line <= 2457 else 'F29'
    if stem == 'Spec/Leak':
        return 'F27' if line >= 417 else 'F26'
    if stem == 'Proofs/Leak':
        return 'F27' if line >= 1800 else 'F26'
    raise ValueError(f'no plan assignment for {file}:{line}')


def contract(file):
    stem = Path(file).stem
    return CONTRACTS.get(stem, 'C6' if stem.startswith('Observe') or stem == 'Observation' else 'C1')


def row(**kw):
    return {k: str(kw.get(k, '')) for k in FIELDS}


def records_from_census(data, source_root=None):
    """Stable identity and source digests independent of the manual assignment columns."""
    result = []
    example_deps = {(r['file'],r['line']):r['uses'] for r in data if r['record']=='example'}
    files = {}
    commands = defaultdict(list)
    decls = [r for r in data if r['record'] == 'declaration']
    names = {r['name'] for r in decls}
    for c in data:
        if c['record'] != 'command':
            continue
        f = c['file']
        if f not in files:
            files[f] = (source(SOURCE, f) if source_root is None else (source_root/f).read_bytes()).decode().splitlines(keepends=True)
        digest = sha((''.join(files[f]) if c['syntax']=='module-realization' else ''.join(files[f][c['line']-1:c['endLine']])).encode())
        uses = example_deps.get((f,c['line']),[])
        if c['syntax']=='Lean.Parser.Command.attribute':
            metadata = next(r for r in data if r['record']=='resolved-command' and r['file']==f and r['line']==c['line'])
            uses = metadata['targets']
        r = row(record='command', name=f"{f}:{c['line']}:{c['endLine']}",
                kind='example' if c['example'] else c['syntax'], source=f,
                line=c['line'], end_line=c['endLine'], digest=digest,
                contract=contract(f), dependencies=json.dumps(sorted(set(uses)), separators=(',',':')))
        commands[f].append(r)
        result.append(r)
    origins = {(r['file'],r['name']):r for r in data if r['record']=='origin'}
    for d in decls:
        f = d['module'].replace('.', '/') + '.lean'
        origin = origins.get((f,d['name']))
        if origin is None:
            raise ValueError(f'no observed command origin: {d["name"]}')
        command = next(c for c in commands[f]
                       if int(c['line']) == origin['line'] and int(c['end_line']) == origin['endLine'])
        result.append(row(record='declaration', name=d['name'], kind=d['kind'], source=f,
                          line=d['line'], end_line=command['end_line'], term_digest=d['shape'],
                          digest=command['digest'],
                          dependencies=json.dumps(sorted(set(d['uses']) & names), separators=(',',':')),
                          contract=contract(f), note=command['name']))
    return result


def evidence_rows():
    """Preserve each nonblank primary audit/invalidations line, not just selected findings.

    Raw audit prose is an obligation for fresh reconciliation, never inherited approval.
    Other review artifacts remain reachable through the pinned evidence tree.
    """
    result = []
    paths = git('ls-tree','-r','--name-only',EVIDENCE,'docs/proposals/spec-evidence').decode().splitlines()
    selected = paths
    for f in selected:
        c = next((f'C{i}' for i in range(10,0,-1) if f'c{i}' in f.lower()), '')
        owner = f'{c}-close' if c else 'Freeze'
        for i, text in enumerate(source(EVIDENCE,f).decode().splitlines(),1):
            if text.strip():
                result.append(row(record='finding', name=f'{EVIDENCE}:{f}:{i}', kind='audit-evidence',
                    source=f'{EVIDENCE}:{f}', line=i,end_line=i,piece=owner,
                    prerequisites=','.join(PREREQS[owner]),disposition='repair',contract=c or 'C1-C10',
                    digest=sha(text.encode()),note='Reconcile primary evidence; historical approvals invalidated; '+text.replace('\t',' ')))
    text = source(DOSSIER,'ISSUES.tsv').decode()
    for line in text.splitlines()[1:]:
        cells = line.split('\t')
        if len(cells) < 4 or cells[2] != 'spec':
            continue
        ident = cells[0]
        owner = {'ISS-0039':'F06','ISS-0045':'F19','ISS-0046':'F02','ISS-0047':'F19',
                 'ISS-0048':'F19','ISS-0049':'F08','ISS-0050':'F18','ISS-0051':'F14'}.get(ident,'Freeze')
        result.append(row(record='finding',name=ident,kind='issue',source=f'{DOSSIER}:ISSUES.tsv',
            piece=owner, prerequisites=','.join(PREREQS[owner]),disposition='repair',
            contract='C1-C10',digest=sha(line.encode()), note=cells[3]))
    return result


@lru_cache(maxsize=1)
def baseline_census():
    paths = git('ls-tree','-r','--name-only',BASE,'ConcreteSpec','ConcreteSpec.lean').decode().splitlines()
    with tempfile.TemporaryDirectory(prefix='spec-baseline-') as tmp:
        root = Path(tmp)
        for path in paths:
            if not path.endswith('.lean'):
                continue
            target = root/path
            target.parent.mkdir(parents=True,exist_ok=True)
            target.write_bytes(source(BASE,path))
        return census(root,ROOT/'scripts/spec/landing_inventory.lean',examples=False,regenerate_modules=REGENERATE_MODULES)


@lru_cache(maxsize=1)
def baseline_members():
    data = baseline_census()
    origins = {(r['file'],r['name']):r for r in data if r['record']=='origin'}
    commands = defaultdict(set)
    declarations = {}
    for r in data:
        if r['record']=='command':
            lines = source(BASE,r['file']).splitlines(keepends=True)
            commands[r['file']].add(sha(b''.join(lines if r['syntax']=='module-realization' else lines[r['line']-1:r['endLine']])))
        elif r['record']=='declaration':
            o = origins[(r['module'].replace('.','/')+'.lean',r['name'])]
            lines = source(BASE,o['file']).splitlines(keepends=True)
            declarations[r['name']] = (o['file'],sha(b''.join(lines[o['line']-1:o['endLine']])),r['shape'])
    return commands, declarations


def on_baseline(r):
    member = baseline_members()[1].get(r['name'])
    return member is not None and member[2]==r['term_digest'] and (
        r['line']=='0' or member[:2]==(r['source'],r['digest']))


def emission_choices(data):
    declarations = {r['name']:r for r in data if r['record']=='declaration'}
    observed = {(o['file'],o['name']):o for o in data if o['record']=='origin'}
    def expand(file, name, seen):
        if name in declarations:
            return {name}
        if name in seen or (file,name) not in observed:
            return set()
        seen.add(name)
        return set().union(*(expand(file,d,seen) for d in observed[(file,name)]['uses']))
    out = defaultdict(list)
    for o in data:
        if o['record']!='origin' or o['name'] not in declarations:
            continue
        d = declarations[o['name']]
        if o['shape']!=d['shape'] or (o['line']==0 and d['kind']!='theorem'):
            continue
        deps = set().union(*(expand(o['file'],n,set()) for n in o['uses']))
        out[o['name']].append((f"{o['file']}:{o['line']}:{o['endLine']}",sorted(deps)))
    return out


def configure_migrations():
    # Classifications follow the actual baseline import DAG, including paths through Spec.
    data = baseline_census()
    imports = {r['file'][:-5].replace('/','.'):r['imports'] for r in data if r['record']=='imports'}
    stems = ['Basics','Authority','Borrow','Brand','Checker','Cleanup','Examples',
             'Observe','Rep','Slice0','Slice1','Surface0']
    owners = {f'ConcreteSpec.Proofs.{stem}':f'M{i:02}' for i,stem in enumerate(stems,1)}
    def reachable(m, seen):
        if m in seen:
            return set()
        seen.add(m)
        return set(imports.get(m,[])) | set().union(*(reachable(d,seen) for d in imports.get(m,[])))
    for module, p in owners.items():
        PREREQS[p] = ['F02','T1c'] + sorted({owners[d] for d in reachable(module,set()) if d in owners})


def command_identity(r):
    return r['name'] if r['record']=='command' else r['note'].split(';',1)[0]


def shared_fixtures(commands):
    fixtures = json.loads((ROOT/'ConcreteSpec/landing/shared-fixtures.json').read_text())
    if len(fixtures) != len(set(fixtures)) or any(f not in commands for f in fixtures):
        raise ValueError('invalid declared shared fixture commands')
    return set(fixtures)


def context_note(r, destinations):
    # Replace only our machine annotation, preserving all authority/hold prose,
    # including annotations before or after it in manually maintained rows.
    note = r['note']
    annotation = 'balanced context destinations='+destinations
    match = re.search(r'(^|; )balanced context destinations=[A-Za-z0-9,\-]*', note)
    if match:
        return note[:match.start()] + match.group(1) + annotation + note[match.end():]
    return (note + '; ' if note else '') + annotation


def context_destinations(population):
    commands = [r for r in population if r['record']=='command']
    material = defaultdict(set)
    for r in population:
        if r['record']=='declaration':
            material[r['regeneration'] or command_identity(r)].add(r['piece'])
    result = {}
    for file in sorted({r['source'] for r in commands}):
        # Each scope owns its context directives. A directive travels to every later
        # material command in its scope (including nested scopes), in original order.
        scopes = [[]]
        openings = []
        for c in sorted((r for r in commands if r['source']==file),key=lambda r:int(r['line'])):
            if c['kind'] in ('Lean.Parser.Command.namespace','Lean.Parser.Command.section'):
                result[c['name']] = set()
                scopes.append([c['name']])
                openings.append(c['name'])
            elif c['kind']=='Lean.Parser.Command.end':
                if not openings:
                    raise ValueError(f'unmatched context end: {c["name"]}')
                result[c['name']] = set(result[openings.pop()])
                scopes.pop()
            elif c['name'] in material or c['kind'] in ('example', 'module-realization', 'Lean.Parser.Command.attribute'):
                destinations = material[c['name']] | {c['piece']}
                for scope in scopes:
                    for name in scope:
                        result[name].update(destinations)
            else:
                # variable/open/universe/options/local notation and other commands
                # without constants are context, not an exclusively owned policy row.
                result[c['name']] = set()
                scopes[-1].append(c['name'])
        if openings:
            raise ValueError(f'unbalanced parsed contexts: {file}')
    return {name:','.join(sorted(destinations)) for name,destinations in result.items()}


def update_contexts(rows):
    contexts = context_destinations(rows)
    for r in rows:
        if r['record']=='command' and r['name'] in contexts:
            r['note'] = context_note(r, contexts[r['name']])


def generate(data):
    rows = [row(record='snapshot',name='source',kind='git-commit',source=SOURCE,piece='P',disposition='retain'),
            row(record='snapshot',name='baseline',kind='git-commit',source=BASE,piece='BASE',disposition='retain'),
            row(record='snapshot',name='plan',kind='sha256',source='ConcreteSpec/landing/plan.md',piece='P',
                digest=sha((ROOT/'ConcreteSpec/landing/plan.md').read_bytes()),disposition='retain')]
    for p, deps in PREREQS.items():
        rows.append(row(record='piece',name=p,kind='landing-piece',piece=p,prerequisites=','.join(deps),
                        disposition='owner-held' if p in HOLDS else 'retain',note=HOLDS.get(p,'')))
    LAW_METADATA[:] = [r for r in data if r["record"]=="resolved-command"]
    population = records_from_census(data)
    anchors = {}
    for r in population:
        if r['record']=='declaration' and r['line']!='0':
            key = command_identity(r)
            anchors[key] = min(anchors.get(key,int(r['line'])),int(r['line']))
    for r in population:
        key = command_identity(r)
        p = (WHOLE[r['source'].removeprefix('ConcreteSpec/').removesuffix('.lean')]
             if key.endswith(':0:0') else piece(r['source'],anchors.get(key,int(key.rsplit(':',2)[1]))))
        r.update(piece=p, prerequisites=','.join(PREREQS[p]),
                 disposition='owner-held' if p in HOLDS else 'repair',
                 note=r['note'] + ('; '+HOLDS[p] if p in HOLDS else ''))
    for r in population:
        if r['source']=='ConcreteSpec/Spec/Ident.lean' and r['piece']=='F02':
            r['disposition'] = 'replace'
            r['note'] += '; ISS-0046: replace presentation-bearing identity with shared numeric identity; do not restore the raw-record inequality'
    byname = {r['name']:r for r in population if r['record']=='declaration'}
    baseline, baseline_decls = baseline_members()
    for r in population:
        if ((r['record']=='command' and r['digest'] in baseline[r['source']]) or
            (r['record']=='declaration' and on_baseline(r))):
            r['migration'] = r['piece']
            r.update(piece='BASE',prerequisites='',disposition='retain')
            r['note'] += '; parsed baseline command; adapt at migration piece'
    ancestry = {}
    def ancestors(p):
        if p not in ancestry:
            ancestry[p] = set(PREREQS[p])
            for d in PREREQS[p]:
                ancestry[p] |= ancestors(d)
        return ancestry[p]
    for p in PREREQS:
        ancestors(p)
    def assign(r,p,why):
        r.update(piece=p,prerequisites=','.join(PREREQS[p]),
                 disposition='owner-held' if p in HOLDS or r['disposition']=='owner-held' else 'repair')
        if why not in r['note']:
            r['note'] += '; '+why
    command_rows = {r['name']:r for r in population if r['record']=='command'}
    command_members = defaultdict(list)
    for r in population: command_members[command_identity(r)].append(r)
    choices = emission_choices(data)
    generated = {n:r for n,r in byname.items() if r['line']=='0' and r['piece']!='BASE'}
    fixtures = shared_fixtures(command_rows)
    def available(p,d):
        return p==d or d in ancestry[p]
    def candidate_piece(command):
        c = command_rows[command]
        return c['migration'] if c['piece']=='BASE' else c['piece']
    # Generated realizations may be emitted in several importers with different proofs.
    # Choose only an actually observed, kernel-identical emission, never an inferred owner.
    # Single-emission helpers (including partial bodies) remain with their command.
    effective = {n:json.loads(r['dependencies']) for n,r in byname.items()}
    for iteration in range(len(PREREQS)*2):
        changed = False
        users = defaultdict(list)
        for n,r in byname.items():
            if r['piece']=='BASE':
                continue
            for d in effective[n]:
                users[d].append(r['piece'])
        for r in population:
            if r['kind']=='example' and r['piece']!='BASE':
                for d in json.loads(r['dependencies']):
                    users[d].append(r['piece'])
        for n,r in generated.items():
            if not choices[n]:
                raise ValueError(f'no kernel-identical observed emission: {n}')
            def score(choice):
                cmd,deps = choice
                p = candidate_piece(cmd)
                violations = sum(not available(p,byname[d]['piece']) for d in deps)
                violations += sum(not available(u,p) for u in users[n])
                return violations,len(ancestry[p]),cmd
            cmd,deps = min(choices[n],key=score)
            p = candidate_piece(cmd)
            if (r['piece'],r['regeneration'],effective[n]) != (p,cmd,deps):
                assign(r,p,'regenerated at observed kernel-identical emission')
                r['regeneration'] = cmd
                effective[n] = deps
                changed = True
        consumers = [(r,effective[n]) for n,r in byname.items() if r['piece']!='BASE']
        consumers += [(r,json.loads(r['dependencies'])) for r in population if r['kind'] in ('example','Lean.Parser.Command.attribute') and r['piece']!='BASE']
        fixture_users = defaultdict(set)
        for r,deps in consumers:
            for dep in deps:
                d = byname[dep]
                cmd = d['regeneration'] or command_identity(d)
                if cmd in fixtures and command_identity(r)!=cmd:
                    fixture_users[cmd].add(r['piece'])
        for cmd, users in fixture_users.items():
            if command_rows[cmd]['piece']=='BASE': continue
            earliest = [p for p in users if all(available(q,p) for q in users)]
            if not earliest:
                continue  # Other support commands may move this pass; final judgment refuses unresolved sets.
            destination = earliest[0]
            for support in command_members[cmd]:
                if support['piece'] not in ('BASE',destination) and (support['line']!='0' or support['regeneration']==cmd):
                    assign(support,destination,'shared fixture/support moved to earliest consumer')
                    changed = True
        if not changed:
            before = [(r['piece'],r['migration']) for r in population]
            assign_law_controls(population,effective)
            # A moved command may own a generated auxiliary used by an unannotated
            # command. Move that dependent command later, then solve emissions again.
            for consumer,deps in consumers:
                if consumer['piece']=='BASE': continue
                unavailable = [d for d in deps if not available(consumer['piece'],byname[d]['piece'])
                               and command_identity(byname[d]) not in fixtures]
                if not unavailable: continue
                identity = command_identity(consumer)
                needs = {byname[d]['piece'] for d in deps if command_identity(byname[d])!=identity
                         and command_identity(byname[d]) not in fixtures}
                if consumer['piece'] in HOLDS: needs.add(consumer['piece'])
                candidates = [p for p in PREREQS if all(available(p,d) for d in needs)]
                destination = min(candidates,key=lambda p:(len(ancestry[p]),p))
                old_piece = consumer['migration'] or consumer['piece']
                for member in command_members[identity]:
                    if member['piece']=='BASE': member['migration']=destination
                    else: assign(member,destination,'dependent command follows relocated law or observed auxiliary')
                    if member['record']=='declaration': MOVED_LAWS.setdefault(member['name'],old_piece)
            if before==[(r['piece'],r['migration']) for r in population]: break
    else:
        raise ValueError('observed emission/fixture assignment did not converge')
    check_law_controls(population)
    moved = {name:{'from':old,'to':byname[name]['migration'] or byname[name]['piece']} for name,old in sorted(MOVED_LAWS.items())}
    (ROOT/'ConcreteSpec/landing/moved-laws.json').write_text(json.dumps(moved,indent=2,sort_keys=True)+'\n')
    for r in population:
        held = r['migration'] or r['piece']
        if held in HOLDS:
            r['disposition'] = 'owner-held'
            if HOLDS[held] not in r['note']: r['note'] += '; '+HOLDS[held]
    update_contexts(population)
    fixture_consumers = [(r,[] if r['piece']=='BASE' else effective[n]) for n,r in byname.items()]
    fixture_consumers += [(r,json.loads(r['dependencies'])) for r in population if r['kind'] in ('example','Lean.Parser.Command.attribute') and r['piece']!='BASE']
    fixture_placement(population,fixture_consumers,ancestry,write_basis=True)
    rows += population + evidence_rows()
    for i in range(1,11):
        c = f'C{i}'
        members = sorted(r['name'] for r in population if r['record']=='declaration' and r['contract']==c)
        rows.append(row(record='contract',name=c,kind='closure-manifest',piece=f'{c}-close',contract=c,
            prerequisites=','.join(PREREQS[f'{c}-close']),disposition='repair',
            digest=sha('\n'.join(members).encode()),
            note='Declarations selected by contract column; reconcile authority, controls, provenance, findings, invalidations and duplicate retirement before closure.'))
    return rows


def write(rows):
    with tempfile.TemporaryDirectory(prefix='.landing-write-',dir=MANIFEST.parent) as tmp:
        target = Path(tmp)/'inventory.tsv'
        with target.open('w',newline='') as f:
            writer = csv.DictWriter(f, FIELDS, delimiter='\t', lineterminator='\n')
            writer.writeheader()
            writer.writerows(sorted(rows,key=lambda r:(r['record'],r['name'])))
        os.replace(target,MANIFEST)


def read():
    with MANIFEST.open(newline='') as f:
        reader = csv.DictReader(f,delimiter='\t')
        if reader.fieldnames != FIELDS:
            raise ValueError('inventory schema mismatch')
        return list(reader)


INVENTORY_INTRODUCTION = '5b617c680b72fa1fc033dd0c909e5c4b588c5690'


@lru_cache(maxsize=None)
def validate_landed_ref(ref):
    if len(ref)!=40 or any(c not in '0123456789abcdef' for c in ref):
        raise ValueError('landed source must be a full immutable commit SHA')
    if ref in PIN_TAGS.values():
        raise ValueError('snapshot pin cannot be landed progress: '+ref)
    if git('rev-parse','--is-shallow-repository').decode().strip()=='true':
        raise ValueError('landed ancestry requires complete history; checkout with fetch-depth: 0')
    for older,newer,message in [(INVENTORY_INTRODUCTION,ref,'landed source predates inventory introduction: '),
                                (ref,'HEAD','landed source is not an ancestor of checked revision: ')]:
        try: git('merge-base','--is-ancestor',older,newer)
        except subprocess.CalledProcessError: raise ValueError(message+ref)


@lru_cache(maxsize=None)
def landed_population(ref):
    with tempfile.TemporaryDirectory(prefix='spec-landed-') as tmp:
        root = Path(tmp)
        for path in git('ls-tree','-r','--name-only',ref,'ConcreteSpec','ConcreteSpec.lean').decode().splitlines():
            if path.endswith('.lean'):
                target = root/path
                target.parent.mkdir(parents=True,exist_ok=True)
                target.write_bytes(source(ref,path))
        data = census(root, ROOT/'scripts/spec/landing_inventory.lean',examples=False,regenerate_modules=REGENERATE_MODULES)
    result = {d['name']:d['shape'] for d in data if d['record']=='declaration'}
    return result


def record_landed(rows, piece, ref):
    if piece not in PREREQS or piece=='BASE':
        raise ValueError(f'invalid landed piece: {piece}')
    validate_landed_ref(ref)
    population = landed_population(ref)
    prior = {r['name'] for r in rows if r['record']=='landed'}
    targets = [r for r in rows if r['record']=='declaration' and r['piece']==piece and r['name'] not in prior]
    missing = [r['name'] for r in targets if population.get(r['name']) != r['term_digest']]
    if missing:
        raise ValueError('landed declarations missing; provide explicit renamed declaration mappings: '+', '.join(missing))
    return rows + [row(record='landed',name=r['name'],kind='landed-declaration',source=ref,
                       piece=piece,prerequisites=','.join(PREREQS[piece]),digest=population[r['name']],
                       note=r['name'],disposition='present') for r in targets]


def check_landed(rows, decls):
    for r in rows:
        if r['record']=='landed':
            if r['kind']!='landed-declaration' or r['disposition']!='present':
                raise ValueError(f'invalid landed record: {r["name"]}')
            if r['name'] not in decls or r['piece']!=decls[r['name']]['piece']:
                raise ValueError(f'invalid landed mapping: {r["name"]}')
            validate_landed_ref(r['source'])
            if (r['digest'] != decls[r['name']]['term_digest'] or
                landed_population(r['source']).get(r['note']) != r['digest'] or
                landed_population(git('rev-parse','HEAD').decode().strip()).get(r['note']) != r['digest']):
                raise ValueError(f'landed declaration mismatch: {r["name"]}')


def check_command_atomicity(rows, identity=command_identity):
    commands = {identity(r):r for r in rows if r['record']=='command'}
    for r in rows:
        if r['record']!='declaration' or r['line']=='0':
            continue
        c = commands[identity(r)]
        if r['piece']!=c['piece'] and not (c['piece']=='BASE' and r['piece']==c['migration']):
            raise ValueError('split source command: '+r['name'])


def check_identity_control():
    commands = [row(record='command',name='fixture:'+str(i),source='fixture',digest='same',piece=p)
                for i,p in enumerate(('F05','F40'))]
    decls = [row(record='declaration',name='d'+str(i),note=c['name'],line=1,
                 source='fixture',digest='same',piece=c['piece']) for i,c in enumerate(commands)]
    population = commands+decls
    check_command_atomicity(population)
    # Under collapsed source/digest keys the correctly relocated first command fails.
    try:
        check_command_atomicity(population,lambda r:(r['source'],r['digest']))
    except ValueError:
        pass
    else:
        raise ValueError('identical-text control did not detect collapsed identities')
    wrong = [dict(r) for r in population]
    wrong[2]['piece'],wrong[3]['piece'] = wrong[3]['piece'],wrong[2]['piece']
    try:
        check_command_atomicity(wrong)
    except ValueError:
        pass
    else:
        raise ValueError('sibling declarations moved without their command')
    print('control PASS: distinct identical-text commands move independently; collapsed keying fails')


def judgment(rows, data, dependencies=True, extra_root=None):
    LAW_METADATA[:] = [r for r in (data if extra_root is None else data[0]) if r["record"]=="resolved-command"]
    seen = {}
    for r in rows:
        if r['record'] not in {'snapshot','piece','command','declaration','finding','contract','landed'}:
            raise ValueError(f'unknown inventory record: {r["record"]}')
        key = (r['record'],r['name'])
        if key in seen:
            raise ValueError(f'duplicate assignment: {r["name"]}')
        seen[key] = r
    pieces = {r['name']:r for r in rows if r['record']=='piece'}
    if set(pieces) != set(PREREQS):
        raise ValueError('landing piece population mismatch')
    for p in PREREQS:
        if pieces[p]['disposition'] != ('owner-held' if p in HOLDS else 'retain') or pieces[p]['note'] != HOLDS.get(p,''):
            raise ValueError(f'plan hold changed: {p}')
        if pieces[p]['prerequisites'] != ','.join(PREREQS[p]):
            raise ValueError(f'plan prerequisites changed: {p}')
    for r in rows:
        effective_piece = r['migration'] or r['piece']
        if r['record'] in ('command','declaration') and effective_piece in HOLDS:
            if r['disposition']!='owner-held' or HOLDS[effective_piece] not in r['note']:
                raise ValueError('owner hold lost: '+r['name'])
        allowed = {'present'} if r['record']=='landed' else {'retain','repair','replace','owner-held','retire'}
        if r['disposition'] not in allowed:
            raise ValueError('invalid disposition: '+r['name'])
        if r['piece']=='BASE' and r['record'] in ('command','declaration'):
            if r['migration'] not in PREREQS or r['migration']=='BASE':
                raise ValueError('invalid BASE migration: '+r['name'])
        elif r['migration']:
            raise ValueError('unexpected migration: '+r['name'])
    for r in rows:
        if not r['piece'] or r['piece'] not in pieces:
            raise ValueError(f'unassigned {r["record"]}: {r["name"]}')
    if extra_root is None:
        expected = records_from_census(data) + evidence_rows()
    else:
        original, extra = data
        expected = records_from_census(original) + records_from_census(extra,extra_root) + evidence_rows()
    expected_keys = {(r['record'],r['name']) for r in expected}
    if len(expected_keys) != len(expected):
        raise ValueError('duplicate identity in the live Lean/evidence census')
    for e in expected:
        key = (e['record'],e['name'])
        if key not in seen:
            raise ValueError(f'unassigned {e["record"]}: {e["name"]}')
        r = seen[key]
        if e['record']=='declaration' and command_identity(r) != command_identity(e):
            raise ValueError(f'command origin mismatch: {e["name"]}')
        for k in ('kind','source','line','end_line','digest','dependencies','contract','term_digest'):
            if r[k] != e[k]:
                raise ValueError(f'census mismatch ({k}): {e["name"]}')
    for key in seen:
        if key[0] in ('declaration','command','finding') and key not in expected_keys:
            raise ValueError(f'extraneous assignment: {key[1]}')
    for i in range(1,11):
        c = f'C{i}'
        members = sorted(r['name'] for r in rows if r['record']=='declaration' and r['contract']==c)
        if seen.get(('contract',c),{}).get('digest') != sha('\n'.join(members).encode()):
            raise ValueError(f'contract manifest mismatch: {c}')
    for key, value in [('source',SOURCE),('baseline',BASE)]:
        if seen.get(('snapshot',key),{}).get('source') != value:
            raise ValueError(f'unpinned {key} snapshot')
    if seen.get(('snapshot','plan'),{}).get('digest') != sha((ROOT/'ConcreteSpec/landing/plan.md').read_bytes()):
        raise ValueError('plan digest mismatch')
    closure = {}
    def visit(p, active):
        if p in active:
            raise ValueError('piece dependency cycle: '+' -> '.join(active+[p]))
        if p in closure:
            return closure[p]
        if p not in pieces:
            raise ValueError(f'unknown prerequisite: {p}')
        ds = pieces[p]['prerequisites'].split(',') if pieces[p]['prerequisites'] else []
        out = set(ds)
        for d in ds:
            out |= visit(d,active+[p])
        closure[p] = out
        return out
    for p in pieces:
        visit(p,[])
    for r in rows:
        if r['record']=='declaration' and r['piece'] not in closure[r['contract']+'-close']:
            raise ValueError(f'contract closure omits declaration piece: {r["name"]}')
    for r in rows:
        if r['record'] != 'piece' and r['prerequisites'] and r['prerequisites'] != pieces[r['piece']]['prerequisites']:
            raise ValueError(f'prerequisites disagree with piece: {r["name"]}')
    contexts = context_destinations(rows)
    for r in rows:
        if r['record']=='command' and r['name'] in contexts and r['note'] != context_note(r, contexts[r['name']]):
            raise ValueError(f'context destinations mismatch: {r["name"]}')
    decls = {r['name']:r for r in rows if r['record']=='declaration'}
    command_rows = {r['name']:r for r in rows if r['record']=='command'}
    baseline_commands, baseline_decls = baseline_members()
    for r in rows:
        if r['piece']=='BASE' and r['record']=='command' and r['digest'] not in baseline_commands[r['source']]:
            raise ValueError(f'baseline assignment has no parsed baseline command: {r["name"]}')
    for r in decls.values():
        if r['piece']=='BASE' and not on_baseline(r):
            raise ValueError(f'baseline assignment has no baseline declaration: {r["name"]}')
    check_command_atomicity(rows)
    check_landed(rows,decls)
    choices = emission_choices(data if extra_root is None else data[0]+data[1])
    effective = {}
    for n,r in decls.items():
        if r['piece']=='BASE':
            # The freshly built baseline is a closed environment; its proof/body
            # is retained, rather than replaced by the source snapshot's version.
            effective[n] = []
        elif r['line']=='0':
            found = next((deps for cmd,deps in choices[n] if cmd==r['regeneration']),None)
            if found is None:
                raise ValueError(f'unobserved regeneration command: {n}')
            c = command_rows[r['regeneration']]
            p = c['migration'] if c['piece']=='BASE' else c['piece']
            if r['piece']!=p:
                raise ValueError(f'split source command: {n}')
            effective[n] = found
        else:
            effective[n] = json.loads(r['dependencies'])
    errors = []
    if dependencies:
        consumers = [(r,effective[n]) for n,r in decls.items()]
        consumers += [(r,json.loads(r['dependencies'])) for r in rows if r['kind'] in ('example','Lean.Parser.Command.attribute') and r['piece']!='BASE']
        for r,deps in consumers:
            for dep in deps:
                d = decls[dep]
                if d['piece'] != r['piece'] and d['piece'] not in closure[r['piece']]:
                    errors.append(f'{r["piece"]} {r["name"]} requires {d["piece"]} {dep}')
        if errors:
            raise ValueError('piece dependency order refused:\n'+'\n'.join(errors))
    if dependencies:
        check_law_controls(rows)
        fixture_placement(rows, consumers, closure)
    return closure


LAW_METADATA = []


def fixture_placement(rows, consumers, closure, write_basis=False):
    commands = {r['name']:r for r in rows if r['record']=='command'}
    decls = {r['name']:r for r in rows if r['record']=='declaration'}
    fixtures = shared_fixtures(commands)
    users = defaultdict(set)
    for r,deps in consumers:
        for dep in deps:
            cmd = decls[dep]['regeneration'] or command_identity(decls[dep])
            if cmd in fixtures and command_identity(r)!=cmd:
                users[cmd].add((r['record'],r['name'],r['piece']))
    basis = {}
    for cmd in sorted(fixtures):
        c = commands[cmd]
        pieces = {p for _,_,p in users[cmd]}
        if not pieces: raise ValueError('shared fixture has no consumer: '+cmd)
        earliest = [p for p in pieces if all(p==q or p in closure[q] for q in pieces)]
        if not earliest or c['piece'] not in earliest:
            raise ValueError('shared fixture does not accompany earliest consumer: '+cmd+' placed '+c['piece']+' consumers '+','.join(sorted(pieces)))
        basis[cmd] = {'basis':'Lean-observed constant dependencies; earliest consumer in prerequisite partial order',
                      'piece':c['piece'],'consumers':sorted(users[cmd])}
    path = ROOT/'ConcreteSpec/landing/fixture-selection.json'
    if write_basis:
        path.write_text(json.dumps(basis,indent=2,sort_keys=True)+'\n')
    elif not path.exists() or json.loads(path.read_text())!=json.loads(json.dumps(basis)):
        raise ValueError('shared fixture selection evidence is stale')


def law_control_groups(rows):
    decls = {r['name']:r for r in rows if r['record']=='declaration'}
    commands = {(r['source'],int(r['line'])):r for r in rows if r['record']=='command'}
    for metadata in LAW_METADATA:
        for law in metadata['laws']:
            yield decls[law], [commands[(metadata['file'],metadata['line'])]] + [decls[n] for n in metadata['mutants']]


def check_law_controls(rows):
    cases = law_control_schedule(rows)
    declarations_by_name = {r['name']:r for r in rows if r['record']=='declaration'}
    @lru_cache(maxsize=None)
    def closure(p): return {p}|set().union(*(closure(d) for d in PREREQS[p]))
    for case in cases:
        if case['unavailable'] and case['law_piece']!=case['control_piece']:
            law = declarations_by_name[case['law']]
            if case['control_piece'] not in closure(law['migration'] or law['piece']):
                raise ValueError('law precedes required control material: '+case['law'])
    groups = list(law_control_groups(rows))
    shared = shared_law_support(groups)
    fixtures = shared_fixtures({r['name']:r for r in rows if r['record']=='command'})
    declarations = {r['name']:r for r in rows if r['record']=='declaration'}
    if any(command_identity(declarations[n]) not in fixtures for n in shared):
        raise ValueError('shared law support missing from declared fixtures')
    for law, controls in groups:
        for control in controls:
            if control['name'] in shared:
                continue  # Reusable support is checked at its earliest consumer by the dependency judgment.
            if (control['migration'] or control['piece'])!=(law['migration'] or law['piece']):
                raise ValueError('law/control piece mismatch: '+control['name']+' law '+law['name'])


def shared_law_support(groups):
    laws = defaultdict(set)
    for law, controls in groups:
        for c in controls:
            if c['record']=='declaration':
                laws[c['name']].add(law['name'])
    return {n for n, users in laws.items() if len(users)>1}


ROUND3_SCHEDULE = '61998c215112b8ac5e011b242779a553334d74c7'


@lru_cache(maxsize=1)
def original_schedule():
    # The coordinator ruling applies to the reviewed round-3 schedule, not to
    # transient positions while fixtures and observed emissions are being solved.
    rows = list(csv.DictReader(io.StringIO(source(ROUND3_SCHEDULE,
                        'ConcreteSpec/landing/inventory.tsv').decode()),delimiter='\t'))
    for r in rows:
        if r['record'] not in ('command','declaration') or r['piece']=='BASE': continue
        origin_line = int(command_identity(r).rsplit(':',2)[1])
        if r['source']=='ConcreteSpec/Proofs/Examples.lean' and 0<origin_line<=64:
            r['piece']='F35'  # coordinator-approved stale citation correction
        if r['source']=='ConcreteSpec/Proofs/Frame.lean' and 2334<=origin_line<=2353:
            r['piece']='F29p'  # owner-held span, including empty_invalid
    return rows


def law_control_schedule(rows, write=False):
    historical = original_schedule()
    declarations = {r['name']:r for r in historical if r['record']=='declaration'}
    @lru_cache(maxsize=None)
    def closure(p): return {p}|set().union(*(closure(d) for d in PREREQS[p]))
    cases = []
    for law,controls in law_control_groups(historical):
        lp = law['migration'] or law['piece']
        for c in controls:
            cp = c['migration'] or c['piece']
            unavailable = sorted(d for d in json.loads(c['dependencies']) if declarations[d]['piece'] not in closure(lp))
            cases.append(dict(law=law['name'],control=c['name'],law_piece=lp,control_piece=cp,unavailable=unavailable))
    evidence = {'basis':'Coordinator 2026-11-11: laws move to their controls; corrected add0 citation and restored held Frame span',
                'source':SOURCE,'schedule_commit':ROUND3_SCHEDULE,'cases':cases}
    path = ROOT/'ConcreteSpec/landing/law-control-placement.json'
    if write: path.write_text(json.dumps(evidence,indent=2,sort_keys=True)+'\n')
    elif not path.exists() or json.loads(path.read_text())!=evidence:
        raise ValueError('law/control scheduling evidence is stale')
    return cases


MOVED_LAWS = {}


def assign_law_controls(rows, effective_dependencies):
    groups = list(law_control_groups(rows))
    shared = shared_law_support(groups)
    declarations = {r['name']:r for r in rows if r['record']=='declaration'}
    commands = {r['name']:r for r in rows if r['record']=='command'}
    fixtures = shared_fixtures(commands)
    command_members = defaultdict(list)
    for r in rows:
        if r['record'] in ('command','declaration'):
            identity = r['regeneration'] or command_identity(r)
            command_members[identity].append(r)
    def dependencies(r):
        names = effective_dependencies[r['name']] if r['record']=='declaration' else json.loads(r['dependencies'])
        def material(name,seen):
            if declarations[name]['line']!='0': return {name}
            if name in seen: return set()
            return set().union(*(material(d,seen|{name}) for d in effective_dependencies[name]))
        return set().union(*(material(name,set()) for name in names))
    def effective(r): return r['migration'] or r['piece']
    @lru_cache(maxsize=None)
    def closure(p):
        return {p} | set().union(*(closure(d) for d in PREREQS[p]))
    closures = {p:closure(p) for p in PREREQS}
    required_destinations = defaultdict(set)
    for case in law_control_schedule(rows):
        if case['unavailable'] and case['law_piece']!=case['control_piece']:
            required_destinations[case['law']].add(case['control_piece'])
    forced_destinations = {}
    # Fixed point: a moved law can itself be a dependency of another group.
    for _ in PREREQS:
        changed = False
        for law, controls in groups:
            members = [law]+[c for c in controls if c['name'] not in shared]
            identities = {command_identity(r) for r in members}
            members = [r for identity in identities for r in command_members[identity]]
            needs = set(required_destinations[law['name']])
            needs.update(forced_destinations[i] for i in identities if i in forced_destinations)
            for member in members:
                if effective(member) in HOLDS:
                    needs.add(effective(member))
                for dep in dependencies(member):
                    d = declarations[dep]
                    if command_identity(d) not in identities and command_identity(d) not in fixtures:
                        needs.add(d['piece'])
            candidates = [p for p in PREREQS if needs <= closures[p]]
            if not candidates: raise ValueError('no law/control destination: '+law['name'])
            destination = effective(law) if effective(law) in candidates else min(candidates,key=lambda p:(len(closures[p]),p))
            old = effective(law)
            if old != destination:
                MOVED_LAWS.setdefault(law['name'],old)
            for r in members:
                if effective(r)==destination: continue
                if effective(r) in HOLDS and effective(r) not in closures[destination]:
                    raise ValueError('cannot move owner-held material: '+r['name'])
                if r['piece']=='BASE': r['migration']=destination
                else: r.update(piece=destination,prerequisites=','.join(PREREQS[destination]))
                changed = True
        for identity, members in command_members.items():
            authored = [r for r in members if r['record']=='declaration' and r['piece']!='BASE' and r['line']!='0']
            if not authored: continue
            current = effective(authored[0])
            unavailable = {declarations[d]['piece'] for member in authored for d in dependencies(member)
                           if d in MOVED_LAWS and declarations[d]['piece'] not in closures[current]
                           and command_identity(declarations[d]) not in fixtures}
            if not unavailable: continue
            needs = {declarations[d]['piece'] for member in authored for d in dependencies(member)
                     if command_identity(declarations[d]) not in fixtures and command_identity(declarations[d])!=identity}
            candidates = [p for p in PREREQS if needs <= closures[p]]
            destination = min(candidates,key=lambda p:(len(closures[p]),p))
            for r in members:
                if r['piece']=='BASE': r['migration']=destination
                else: r.update(piece=destination,prerequisites=','.join(PREREQS[destination]))
                if r['record']=='declaration': MOVED_LAWS.setdefault(r['name'],current)
            changed = True
        fixture_consumers = defaultdict(list)
        for r in rows:
            if r['record'] not in ('declaration','command') or r['piece']=='BASE': continue
            if r['record']=='command' and r['kind'] not in ('example','Lean.Parser.Command.attribute'): continue
            for d in dependencies(r):
                identity = command_identity(declarations[d])
                if identity in fixtures and command_identity(r)!=identity:
                    fixture_consumers[identity].append(r)
        order = {p:i for i,p in enumerate(PREREQS)}
        for fixture,consumers in fixture_consumers.items():
            pieces = {r['piece'] for r in consumers}
            if any(all(p in closures[q] for q in pieces) for p in pieces): continue
            destination = max(pieces,key=order.get)
            for consumer in consumers:
                identity = command_identity(consumer)
                if consumer['piece']==destination: continue
                forced_destinations[identity] = destination
                for r in command_members[identity]:
                    old = effective(r)
                    if r['piece']=='BASE': r['migration']=destination
                    else: r.update(piece=destination,prerequisites=','.join(PREREQS[destination]))
                    if r['record']=='declaration': MOVED_LAWS.setdefault(r['name'],old)
                changed = True
        if not changed: return
    raise ValueError('law/control assignment did not converge')


def verify_plan_boundaries(rows):
    # Named plan obligations catch boundary drift in the generated assignments themselves,
    # including documentation beginning on a different line from a declaration's name.
    expected = {
        'ConcreteSpec.Core.Exec.runFrom_final_sound':'F24a',
        'ConcreteSpec.Core.Exec.internal_finite_or_infinite_events':'F25a',
        'ConcreteSpec.Core.Frame.ABI.LoadBoundaryDemo.controlInput':'F29p',
        'ConcreteSpec.Core.Exec.CoreEvidence.dropArgProtocolDestroyCall':'F10',
        'ConcreteSpec.Examples.add0_ok':'F35',
        'ConcreteSpec.Examples.add0_never_stuck':'F35',
        'ConcreteSpec.Examples.add0_never_traps':'F35',
        'ConcreteSpec.Examples.Add0Evidence.stuckBody':'F35',
        'ConcreteSpec.Examples.Add0Evidence.trapBody':'F35',
        'ConcreteSpec.Core.Frame.ABI.LoadBoundaryDemo.empty_invalid':'F29p',
        'ConcreteSpec.Core.CExpr.eval.match_5':'F06',
    }
    declarations = {r['name']:r for r in rows if r['record']=='declaration'}
    for name,piece in expected.items():
        r = declarations[name]
        assigned = r['migration'] if r['piece']=='BASE' else r['piece']
        if assigned!=piece:
            raise ValueError(f'plan boundary mismatch: {name}: {assigned}, expected {piece}')
    print('control PASS: named plan boundaries and held fixture destination')


def report(rows, detail=None):
    if detail and detail not in PREREQS and detail != 'all':
        raise ValueError(f'unknown remaining piece: {detail}')
    landed = {r['name'] for r in rows if r['record']=='landed'}
    counts = Counter(r['piece'] for r in rows if r['record']=='declaration' and r['piece']!='BASE' and r['name'] not in landed)
    examples = Counter(r['piece'] for r in rows if r['kind']=='example' and r['piece']!='BASE')
    print('remaining-examples\t'+str(sum(examples.values())))
    for p in sorted(r['name'] for r in rows if r['record']=='piece'):
        print(f'{p}\tremaining={counts[p]}\tremaining-examples={examples[p]}')
    if detail:
        for r in sorted(rows,key=lambda r:(r['piece'],r['name'])):
            if r['record']=='declaration' and r['piece']!='BASE' and r['name'] not in landed and detail in ('all',r['piece']):
                print(f'remaining\t{r["piece"]}\t{r["name"]}')


def ensure_object(ref):
    # Fetch object availability without moving refs, FETCH_HEAD or shallow boundaries.
    try:
        git('cat-file','-e',ref+'^{commit}')
    except subprocess.CalledProcessError:
        print(f'fetch pinned inventory source {ref}',flush=True)
        git('fetch','--no-tags','--no-write-fetch-head','origin',ref)
        git('cat-file','-e',ref+'^{commit}')


PIN_TAGS = {'source': SOURCE, 'baseline': BASE, 'evidence': EVIDENCE, 'dossier': DOSSIER}


def ensure_objects():
    for name, ref in PIN_TAGS.items():
        tag = 'refs/tags/spec-inventory/'+name
        try:
            local = git('rev-parse','--verify',tag).decode().strip()
        except subprocess.CalledProcessError:
            local = None
        if local is not None:
            if local != ref: raise ValueError('changed protected inventory pin: '+tag+' expected '+ref)
            continue
        remote = git('ls-remote','--refs','origin',tag).decode().split()
        if remote != [ref,tag]:
            raise ValueError('missing or changed protected inventory pin: '+tag+' expected '+ref)
        git('fetch','--no-tags','--no-write-fetch-head','origin',tag)
        git('cat-file','-e',ref+'^{commit}')


def extract(root):
    files = git('ls-tree','-r','--name-only',SOURCE,'ConcreteSpec','ConcreteSpec.lean').decode().splitlines()
    for f in files:
        if f.endswith('.lean'):
            dest = root / f
            dest.parent.mkdir(parents=True,exist_ok=True)
            dest.write_bytes(source(SOURCE,f))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--write',action='store_true',help='explicitly regenerate initial plan assignments')
    parser.add_argument('--remaining', metavar='PIECE', help='list remaining declaration names for a piece, or all')
    parser.add_argument('--census-output',type=Path,help='write fresh census JSONL for independent inspection; never read as a cache')
    parser.add_argument('--record-landed',metavar='PIECE')
    parser.add_argument('--landed-ref',metavar='SHA')
    args = parser.parse_args()
    if bool(args.record_landed) != bool(args.landed_ref):
        raise ValueError('--record-landed and --landed-ref are required together')
    if args.write and args.record_landed:
        raise ValueError('--write resets initial assignments; do not combine with --record-landed')
    if os.environ.get('CONCRETE_GATES_SHELL') != '1':
        raise ValueError('run inside nix develop -c')
    if args.remaining and args.remaining not in PREREQS and args.remaining != 'all':
        raise ValueError(f'unknown remaining piece: {args.remaining}')
    check_census(ROOT/'scripts/spec/landing_inventory.lean')
    ensure_objects()
    configure_migrations()
    print(f'landing inventory: source={SOURCE}; baseline={BASE}',flush=True)
    with tempfile.TemporaryDirectory(prefix='spec-landing-') as tmp:
        root = Path(tmp)
        extract(root)
        data = census(root,ROOT/'scripts/spec/landing_inventory.lean',regenerate_modules=REGENERATE_MODULES)
        if args.census_output:
            args.census_output.write_text(''.join(json.dumps(r,sort_keys=True)+'\n' for r in data))
        rows = generate(data) if args.write else read()
        if args.record_landed:
            rows = record_landed(rows,args.record_landed,args.landed_ref)
        closure = judgment(rows,data,dependencies=False)
        verify_plan_boundaries(rows)
        check_law_controls(rows)
        # Mutate only fixture placement: dependency availability still holds when moved earlier.
        fixture_commands = shared_fixtures({r['name']:r for r in rows if r['record']=='command'})
        fixture = next(r for r in rows if r['record']=='command' and r['name'] in fixture_commands and r['piece'] not in ('BASE','F02'))
        bad = [dict(r) for r in rows]
        for r in bad:
            if command_identity(r)==fixture['name']: r['piece']='F02'
        fixture_consumers = [(r,json.loads(r['dependencies'])) for r in bad if r['record']=='declaration' and r['piece']!='BASE']
        fixture_consumers += [(r,json.loads(r['dependencies'])) for r in bad if r['kind'] in ('example','Lean.Parser.Command.attribute') and r['piece']!='BASE']
        observed_consumers = [(r,json.loads(r['dependencies'])) for r in rows if r['record']=='declaration' and r['piece']!='BASE']
        observed_consumers += [(r,json.loads(r['dependencies'])) for r in rows if r['kind'] in ('example','Lean.Parser.Command.attribute') and r['piece']!='BASE']
        fixture_placement(rows,observed_consumers,closure)
        try: fixture_placement(bad,fixture_consumers,closure)
        except ValueError as exc:
            if not str(exc).startswith('shared fixture does not accompany earliest consumer: '+fixture['name']+' placed F02 '): raise
            print('control PASS: fixture placed before its earliest consumer refused')
        else: raise ValueError('premature fixture accepted')
        control_decls = {r['name']:r for r in rows if r['record']=='declaration'}
        unused_consumers = [(r,[d for d in deps if (control_decls[d]['regeneration'] or command_identity(control_decls[d]))!=fixture['name']])
                            for r,deps in observed_consumers]
        try: fixture_placement(rows,unused_consumers,closure)
        except ValueError as exc:
            if str(exc)!='shared fixture has no consumer: '+fixture['name']: raise
            print('control PASS: declared fixture without consumers refused')
        else: raise ValueError('unused fixture accepted')
        older_main = git('rev-parse',BASE+'^').decode().strip()
        try: record_landed(rows,'M01',older_main)
        except ValueError as exc:
            if not str(exc).startswith('landed source predates inventory introduction:'): raise
            print('control PASS: stale M01 recorded against older main refused')
        else: raise ValueError('older-main M01 receipt accepted')
        # Coverage controls exercise the same judgment, never the generator.
        duplicate = next(r for r in rows if r['record']=='declaration')
        try:
            judgment(rows+[duplicate],data)
        except ValueError as exc:
            if not str(exc).startswith('duplicate assignment:'):
                raise
            print('control PASS: duplicate assignment refused')
        else:
            raise ValueError('duplicate control accepted')
        # Plant a real compiled module, enumerate it through exactly the production exporter.
        control_root = root/'control'
        (control_root/'ConcreteSpec').mkdir(parents=True)
        (control_root/'ConcreteSpec.lean').write_text('import ConcreteSpec.InventoryControl\n')
        control = control_root/'ConcreteSpec/InventoryControl.lean'
        control.write_text('namespace ConcreteSpec.InventoryControl\ndef unassigned : Nat := 7\nend ConcreteSpec.InventoryControl\n')
        planted = census(control_root,ROOT/'scripts/spec/landing_inventory.lean')
        command_assignments = [dict(r,piece='P',disposition='repair',prerequisites=','.join(PREREQS['P']))
                               for r in records_from_census(planted,control_root)
                               if r['record']=='command']
        update_contexts(command_assignments)
        # Supply parsed commands so the coverage refusal must name the new declaration.
        try:
            judgment(rows+command_assignments,(data,planted),extra_root=control_root)
        except ValueError as exc:
            if str(exc) != 'unassigned declaration: ConcreteSpec.InventoryControl.unassigned':
                raise
            print('control PASS: planted unassigned declaration refused through census discovery')
        else:
            raise ValueError('unassigned control accepted')
        # A complete command assigned ahead of its scalar/execution prerequisites must fail
        # dependency order, rather than just the source-command atomicity check.
        scalar_names = {r['name'] for r in rows if r['record']=='declaration' and r['piece']=='F05'}
        victim = next((r for r in rows if r['record']=='declaration' and r['piece']=='F06'
                       and r['line']!='0' and scalar_names.intersection(json.loads(r['dependencies']))),None)
        if victim is None:
            raise ValueError('dependency-order control has no scalar-dependent command')
        bad = [dict(r) for r in rows]
        for r in bad:
            if (r['regeneration'] or command_identity(r)) == command_identity(victim):
                r.update(piece='F03',prerequisites=','.join(PREREQS['F03']))
        update_contexts(bad)
        try:
            judgment(bad,data)
        except ValueError as exc:
            if not str(exc).startswith('piece dependency order refused:'):
                raise
            print('control PASS: premature command assignment refused by dependency order')
        else:
            raise ValueError('dependency-order control accepted')
        def refuse(mutated, prefix, label):
            update_contexts(mutated)
            try:
                judgment(mutated,data)
            except ValueError as exc:
                if not str(exc).startswith(prefix):
                    raise
                print('control PASS: '+label)
            else:
                raise ValueError(label+' control accepted')

        # A zero-range declaration must be proved present on the pinned baseline too.
        absent = next((r for r in rows if r['record']=='declaration' and r['line']=='0'
                       and r['name'] not in baseline_members()[1]),None)
        if absent is None:
            raise ValueError('baseline control has no nonbaseline auxiliary')
        bad = [dict(r) for r in rows]
        next(r for r in bad if r['record']=='declaration' and r['name']==absent['name']).update(piece='BASE',migration='F40',prerequisites='')
        refuse(bad,'baseline assignment has no baseline declaration:','invalid zero-range BASE assignment refused')

        check_identity_control()
        bad = [dict(r) for r in rows]
        law_control = next(c for law, controls in law_control_groups(bad)
                           if law['name']=='ConcreteSpec.Examples.add0_never_stuck'
                           for c in controls if c['record']=='command')
        law_control.update(piece='F40',prerequisites=','.join(PREREQS['F40']))
        refuse(bad,'law/control piece mismatch:','law and its annotated control cannot split')
        held = next(r for r in rows if r['record']=='command' and r['disposition']=='owner-held'
                    and r['name'] in context_destinations(rows))
        held_copy = [dict(r) for r in rows]
        update_contexts(held_copy)
        if next(r for r in held_copy if r['record']=='command' and r['name']==held['name'])!=held:
            raise ValueError('context pass changed a held row')
        manual = dict(held,note='balanced context destinations=OLD; owner hold: preserve this reason')
        updated = context_note(manual,'F18')
        if updated!='balanced context destinations=F18; owner hold: preserve this reason':
            raise ValueError('context pass erased a trailing owner hold note')
        print('control PASS: held context row and hold note survive unchanged')
        for kind in ('command','declaration'):
            bad = [dict(r) for r in rows]
            next(r for r in bad if r['record']==kind and (r['migration'] or r['piece']) in HOLDS)['disposition']='retain'
            refuse(bad,'owner hold lost:','held '+kind+' cannot become retain')
        for column,value,prefix in [('migration','TYPO','invalid BASE migration:'),
                                    ('disposition','TYPO','invalid disposition:')]:
            bad = [dict(r) for r in rows]
            next(r for r in bad if r['record']=='declaration' and r['piece']=='BASE')[column] = value
            refuse(bad,prefix,'closed '+column+' schema')

        decls = {r['name']:r for r in rows if r['record']=='declaration'}
        example = next((r for r in rows if r['kind']=='example' and r['piece']!='BASE'
                        and any(decls[d]['piece'] not in closure['F03']|{'F03'} for d in json.loads(r['dependencies']))),None)
        if example is None:
            raise ValueError('anonymous-example control has no later dependency')
        bad = [dict(r) for r in rows]
        for r in bad:
            if (r['regeneration'] or command_identity(r))==command_identity(example):
                r.update(piece='F03',prerequisites=','.join(PREREQS['F03']))
        refuse(bad,'piece dependency order refused:','anonymous example premature dependency refused')
        unknown = False
        try:
            report(rows,'UNKNOWN-PIECE')
        except ValueError as exc:
            unknown = str(exc)=='unknown remaining piece: UNKNOWN-PIECE'
        if not unknown:
            raise ValueError('unknown remaining piece control accepted')
        print('control PASS: unknown remaining piece refused')
        report(rows,args.remaining)
        judgment(rows,data)
        if args.write or args.record_landed:
            write(rows)
        print(f'landing inventory PASS: source={SOURCE}; baseline={BASE}')


if __name__ == '__main__':
    try:
        main()
    except (ValueError,RuntimeError,subprocess.CalledProcessError) as exc:
        sys.exit(str(exc))
