#!/usr/bin/env python3
"""Landing inventory: explicit assignments, checked against a fresh Lean census.

The generator is deliberately separate from the judgment: checking never fills a missing row.
"""
import argparse
from collections import Counter, defaultdict
import csv
import hashlib
from functools import lru_cache
import json
import io
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


@lru_cache(maxsize=None)
def plan_citations(destination, file):
    """Read held source spans from the supplied plan, without inventing boundaries."""
    text = (ROOT/'ConcreteSpec/landing/plan.md').read_text()
    section = re.search(r'^\| '+re.escape(destination)+r' \|.*$', text, re.M)
    if section is None:
        raise ValueError('missing plan piece: '+destination)
    return [(int(a),int(b)) for a,b in re.findall(re.escape(file)+r':(\d+)[–-](\d+)',section.group())]


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
        return 'F29p' if any(lo <= line <= hi for lo,hi in plan_citations('F29p', 'ConcreteSpec/Proofs/Frame.lean')) else 'F29'
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


def records_from_census(data, source_root=None, ref=SOURCE):
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
            files[f] = (source(ref, f) if source_root is None else (source_root/f).read_bytes()).decode().splitlines(keepends=True)
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
    for name, key in command_match_keys(records_from_census(data, ref=BASE)).items():
        commands[name.rsplit(':',2)[0]].add(key)
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
    RELOCATED_DECLARATIONS.clear()
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
    command_keys = command_match_keys(population)
    for r in population:
        if ((r['record']=='command' and command_keys[r['name']] in baseline[r['source']]) or
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
                    causes = inherited_causes(byname[d] for d in unavailable)
                    annotate_move(member, ('dependent on relocated law ' if causes else 'dependent on observed auxiliary ')+
                                  ', '.join(unavailable)+('; '+causes if causes else ''))
                    if member['record']=='declaration': RELOCATED_DECLARATIONS.setdefault(member['name'],old_piece)
            if before==[(r['piece'],r['migration']) for r in population]: break
    else:
        raise ValueError('observed emission/fixture assignment did not converge')
    check_law_controls(population)
    for r in population:
        held = r['migration'] or r['piece']
        if held in HOLDS:
            r['disposition'] = 'owner-held'
            if HOLDS[held] not in r['note']: r['note'] += '; '+HOLDS[held]
    update_contexts(population)
    fixture_consumers = [(r,[] if r['piece']=='BASE' else effective[n]) for n,r in byname.items()]
    fixture_consumers += [(r,json.loads(r['dependencies'])) for r in population if r['kind'] in ('example','Lean.Parser.Command.attribute') and r['piece']!='BASE']
    fixture_placement(population,fixture_consumers,ancestry,write_basis=True)
    law_control_schedule(population,write=True)
    relocation_causes(population,normalize=True)
    relocation_evidence(population,write=True)
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


def command_match_keys(rows):
    """Content and ordinal within the enclosing namespace/section sequence.

    Snapshot line ranges only order the parsed sequence; they never enter a key.
    Equal commands remain distinct, including repeated identical scopes.
    """
    result, stacks, counts = {}, defaultdict(list), Counter()
    for r in sorted((r for r in rows if r['record']=='command'),
                    key=lambda r:(r['source'],int(r['line']),int(r['end_line']))):
        stack = stacks[r['source']]
        scope = tuple(stack)
        token = (r['source'], scope, r['kind'], r['digest'])
        ordinal = counts[token]
        counts[token] += 1
        key = (*token, ordinal)
        result[r['name']] = key
        if r['kind'] in ('Lean.Parser.Command.namespace','Lean.Parser.Command.section'):
            stack.append((r['kind'],r['digest'],ordinal))
        elif r['kind']=='Lean.Parser.Command.end' and stack:
            stack.pop()
    return result


def command_presence_slots(keys):
    """Fallback slots for fixtures without parsed statement metadata.

    Count all commands of a kind in each scope, not just equal content digests.
    """
    counts, result = Counter(), {}
    for key in keys:
        slot = key[:3]
        result[key] = (*slot,counts[slot])
        counts[slot] += 1
    return result


class Digests(dict):
    """Snapshot audit names accompanied by position-free matching identities."""
    def __init__(self, *args, commands=None, **kwargs):
        super().__init__(*args, **kwargs)
        self.commands = commands or {}
        self.presence = {}


def authored_names(data):
    tokens = {(c['file'], i['line'], i['column']): i['name']
              for c in data if c['record']=='command'
              for i in c.get('sourceIdentifiers', [])}
    authored = set()
    for d in data:
        if d['record']!='declaration': continue
        token = tokens.get((d['module'].replace('.', '/')+'.lean', d['line'], d['column']))
        if token and (d['userName']==token or d['userName'].endswith('.'+token)):
            authored.add(d['name'])
    return authored


def full_digests(data, root):
    """Authored declarations and otherwise unrepresented source commands only.

    A declaration must select its own identifier in the parsed source. Inherited
    ranges on recursors/helpers do not turn compiler output into authored work.
    """
    rows = records_from_census(data, root)
    authored = authored_names(data)
    commands = {(r['source'],int(r['line'])):r for r in rows if r['record']=='command'}
    attributes = defaultdict(list)
    for metadata in data:
        if metadata['record']=='resolved-command' and metadata['targets']:
            digest = commands[(metadata['file'],metadata['line'])]['digest']
            for target in metadata['targets']:
                attributes[target].append(digest)
    result = Digests({r['name']:sha(json.dumps([r['digest'],r['term_digest'],attributes[r['name']]],
                                    separators=(',',':')).encode())
              for r in rows if r['record']=='declaration' and r['name'] in authored})
    keys = command_match_keys(rows)
    represented = {command_identity(r) for r in rows
                   if r['record']=='declaration' and r['name'] in authored}
    for r in rows:
        if r['record']=='command' and r['kind']!='module-realization' and r['name'] not in represented:
            result['command:'+r['name']] = r['digest']
            result.commands['command:'+r['name']] = keys[r['name']]
    counts, scope_counts, scopes = Counter(), Counter(), defaultdict(list)
    metadata = {(c['file'],c['line']):c for c in data if c['record']=='command'}
    for r in sorted((r for r in rows if r['record']=='command'),
                    key=lambda r:(r['source'],int(r['line']),int(r['end_line']))):
        key = keys[r['name']]
        scope = scopes[r['source']]
        slot = (r['source'],tuple(scope),r['kind'])
        ordinal = counts[slot]
        counts[slot] += 1
        command = metadata.get((r['source'],int(r['line'])),{})
        statement = command.get('statement')
        result.presence[key] = (('statement',json.dumps(statement,sort_keys=True)) if statement is not None
                                else ('slot',slot,ordinal))
        if r['kind'] in ('Lean.Parser.Command.namespace','Lean.Parser.Command.section'):
            token = (r['source'],tuple(scope),r['kind'],json.dumps(command['scopeIdentity'],sort_keys=True))
            occurrence = scope_counts[token]
            scope_counts[token] += 1
            scope.append((r['kind'],token[-1],occurrence))
        elif r['kind']=='Lean.Parser.Command.end' and scope:
            scope.pop()
    return result


def head_digest_export(checked):
    return dict(declarations={n:d for n,d in checked.items() if not n.startswith(('command:','header:'))},
                headCommandKeys=[dict(module=k[0][:-5].replace('/','.'),scope=k[1],kind=k[2],
                                      digest=k[3],ordinal=k[4]) for k in checked.commands.values()])


def head_population():
    # Exactly one checked revision; the F01 provenance is separately ancestry-checked.
    head = git('rev-parse','HEAD^{commit}').decode().strip()
    print('progress checked revision: '+head,flush=True)
    with tempfile.TemporaryDirectory(prefix='spec-head-') as tmp:
        root = Path(tmp)
        for path in git('ls-tree','-r','--name-only',head,'ConcreteSpec','ConcreteSpec.lean').decode().splitlines():
            if path.endswith('.lean'):
                target = root/path
                target.parent.mkdir(parents=True,exist_ok=True)
                target.write_bytes(source(head,path))
        properties = []
        data = census(root, ROOT/'scripts/spec/landing_inventory.lean',examples=False,regenerate_modules=(),
                      validator=(lambda built,parsed: properties.append(header_property(built,parsed)))
                      if (ROOT/'ConcreteSpec/landing/headers.json').exists() else None)
        result = full_digests(data,root)
        if properties:
            result['header:F01'] = properties[0]
        return result


def unique_object(pairs):
    result = {}
    for key,value in pairs:
        if key in result: raise ValueError('duplicate JSON field: '+key)
        result[key] = value
    return result


def unit_name(r):
    return ('command:' if r['record']=='command' else '')+r['name']


def review_reference(review, record, units, spec):
    if not isinstance(review,dict) or not review:
        raise ValueError('missing accepting review: '+record)
    if set(review)!={'reference','verdict'}:
        raise ValueError('invalid review fields: '+record)
    if review['verdict']!='ACCEPT':
        raise ValueError('review verdict is not ACCEPT: '+record)
    ref = review['reference']
    if not isinstance(ref,dict) or set(ref)!={'path','commit'}:
        raise ValueError('invalid review reference fields: '+record)
    commit = ref['commit']
    if not isinstance(commit,str) or not re.fullmatch('[0-9a-f]{40}',commit):
        raise ValueError('invalid review commit: '+record)
    try: git('merge-base','--is-ancestor',commit,'HEAD')
    except subprocess.CalledProcessError:
        raise ValueError('review commit is not an ancestor of HEAD: '+record) from None
    path = ref['path']
    if (not isinstance(path,str) or not path.startswith('ConcreteSpec/landing/reviews/') or
        '..' in Path(path).parts or not path.endswith('.txt')):
        raise ValueError('review path outside ConcreteSpec/landing/reviews/*.txt: '+record)
    try: artifact = git('show',commit+':'+path).decode()
    except subprocess.CalledProcessError:
        raise ValueError('missing review artifact: '+path) from None
    # The artifact is committed after the reviewed revision, avoiding a self-referential SHA.
    revisions = re.findall(r'^COMMIT: ([0-9a-f]{40})$',artifact,re.M)
    if len(revisions)!=1:
        raise ValueError('review artifact requires one COMMIT: '+path)
    try: git('merge-base','--is-ancestor',revisions[0],commit)
    except subprocess.CalledProcessError:
        raise ValueError('reviewed commit is not an ancestor of artifact commit: '+path) from None
    verdicts = re.findall(r'^VERDICT: (.+)$',artifact,re.M)
    if not verdicts or verdicts[-1]!='ACCEPT':
        raise ValueError('review artifact last verdict is not ACCEPT: '+path)
    records = re.findall(r'^RECORD: (.+)$',artifact,re.M)
    reviewed_units = re.findall(r'^UNIT: (.+)$',artifact,re.M)
    if record not in records and not set(units)<=set(reviewed_units):
        raise ValueError('review artifact does not cover replacement: '+record)
    # Bind acceptance to the substantive reconciliation, not just its reusable name.
    # The review reference is added after review and cannot pin its own commit.
    expected = {k:v for k,v in spec.items() if k!='review'}
    try:
        manifest = git('show',revisions[0]+':ConcreteSpec/landing/inventory.tsv').decode()
    except subprocess.CalledProcessError:
        manifest = ''
    covered = False
    for candidate in csv.DictReader(io.StringIO(manifest),delimiter='\t'):
        if candidate.get('record')!='replacement' or candidate.get('name')!=record:
            continue
        try: reviewed = json.loads(candidate['note'],object_pairs_hook=unique_object)
        except (ValueError,TypeError): continue
        if (isinstance(reviewed,dict) and candidate.get('disposition')=='replace' and
            candidate.get('piece')==spec['_piece'] and
            {k:v for k,v in reviewed.items() if k!='review'}==
            {k:v for k,v in expected.items() if k!='_piece'}):
            covered = True
    if not covered:
        raise ValueError('reviewed commit does not contain reconciliation: '+record)



def replacement_rows(rows, authored=None):
    """Reconcile immutable snapshot units with disjoint, reviewed HEAD targets."""
    snapshot = {unit_name(r):r for r in rows if r['record'] in ('declaration','command')}
    replaced, targets, result = set(), set(), []
    for r in rows:
        if r['record']!='replacement': continue
        try:
            spec = json.loads(r['note'],object_pairs_hook=unique_object)
            if not isinstance(spec,dict):
                raise ValueError('invalid replacement JSON object: '+r['name'])
            if 'review' not in spec:
                raise ValueError('missing accepting review: '+r['name'])
            if set(spec)!={'supersedes','declarations','commands','action','reason','review'}:
                required = {'supersedes','declarations','commands','action','reason','review'}
                raise ValueError('invalid replacement schema fields: '+r['name']+
                                 ' missing='+','.join(sorted(required-set(spec)))+
                                 ' extra='+','.join(sorted(set(spec)-required)))
            old, new, commands = spec['supersedes'], spec['declarations'], spec['commands']
            def field(ok, name):
                if not ok: raise ValueError('invalid replacement field '+name+': '+r['name'])
            field(isinstance(old,list) and bool(old) and all(isinstance(n,str) for n in old)
                  and len(set(old))==len(old), 'supersedes')
            field(isinstance(new,dict) and all(isinstance(n,str) and n and
                  not n.startswith(('command:','header:')) and isinstance(d,str) and
                  re.fullmatch('[0-9a-f]{64}',d) for n,d in new.items()), 'declarations')
            field(isinstance(commands,list), 'commands')
            field(spec['action'] in ('replace','retire'), 'action')
            field(bool(new or commands)==(spec['action']=='replace'), 'action targets')
            field(spec['action']!='replace' or len(old)==1, 'one replacement obligation')
            field(isinstance(spec['reason'],str) and bool(spec['reason'].strip()), 'reason')
            keys = []
            for c in commands:
                if (not isinstance(c,dict) or set(c)!={'module','scope','kind','digest','ordinal'} or
                    not isinstance(c['module'],str) or not c['module'] or
                    not isinstance(c['kind'],str) or not c['kind'] or
                    not isinstance(c['digest'],str) or not re.fullmatch('[0-9a-f]{64}',c['digest']) or
                    type(c['ordinal']) is not int or c['ordinal']<0 or not isinstance(c['scope'],list) or
                    not all(isinstance(x,list) and len(x)==3 and isinstance(x[0],str) and
                            isinstance(x[1],str) and type(x[2]) is int and x[2]>=0 for x in c['scope'])):
                    raise ValueError('invalid replacement field commands: '+r['name'])
                keys.append((c['module'].replace('.','/')+'.lean',tuple(tuple(x) for x in c['scope']),
                             c['kind'],c['digest'],c['ordinal']))
        except (json.JSONDecodeError,TypeError):
            raise ValueError('invalid replacement JSON: '+r['name']) from None
        for n in old:
            if n not in snapshot: raise ValueError('unknown replacement row: '+n)
            if n in replaced: raise ValueError('already-replaced row: '+n)
            if authored is not None and n not in authored:
                raise ValueError('replacement is not an authored progress unit: '+n)
            original = snapshot[n]
            if original['piece']=='BASE':
                raise ValueError('replacement supersedes BASE unit: '+n)
            if original['piece']!=r['piece']:
                raise ValueError('replacement supersedes foreign piece unit: '+n)
            if original['disposition']=='owner-held':
                raise ValueError('replacement supersedes owner-held unit: '+n)
            replaced.add(n)
        review_reference(spec['review'],r['name'],old,dict(spec,_piece=r['piece']))
        for n in new:
            if n in snapshot and snapshot[n]['piece']!=r['piece']:
                raise ValueError('replacement target belongs to another piece: '+n)
            if n in snapshot and n not in old:
                raise ValueError('replacement target is independent snapshot unit: '+n)
        for target in [('declaration',n) for n in new]+[('command',k) for k in keys]:
            if target in targets: raise ValueError('shared replacement target: '+r['name'])
            targets.add(target)
        snapshot_commands = command_match_keys(rows)
        for key in keys:
            for n,k in snapshot_commands.items():
                if k==key and snapshot['command:'+n]['piece']!=r['piece']:
                    raise ValueError('replacement command belongs to another piece: '+n)
                if k==key:
                    raise ValueError('replacement command is unchanged snapshot unit: '+n)
        if not r['name'].strip() or any(r[k] for k in FIELDS if k not in ('record','name','piece','disposition','note')) or r['disposition']!='replace':
            raise ValueError('invalid replacement columns: '+r['name'])
        spec['_command_keys'] = keys
        result.append((r,spec))
    return result


def replacement_progress(rows, reasons, checked, snapshot=None):
    reasons = dict(reasons)
    command_keys = list(getattr(checked,'commands',{}).values())
    commands = set(command_keys)
    snapshot_commands = command_match_keys(rows)
    old_slots = command_presence_slots(snapshot_commands.values())
    head_slots = command_presence_slots(command_keys)
    for r,spec in replacement_rows(rows):
        refusal = None
        for n in spec['supersedes']:
            if n.startswith('command:'):
                old_key = snapshot_commands[n.removeprefix('command:')]
                old_identity = getattr(snapshot,'presence',{}).get(old_key)
                retained = [k for k in commands if
                            (getattr(checked,'presence',{}).get(k)==old_identity if old_identity is not None
                             else head_slots[k]==old_slots[old_key])]
                present = any(k not in spec['_command_keys'] for k in retained)
            else:
                present = n in checked and spec['declarations'].get(n)!=checked[n]
            if present:
                refusal = 'superseded unit still present: '+n
                break
        if refusal is None:
            for n,d in spec['declarations'].items():
                if n not in checked:
                    refusal = 'missing replacement declaration: '+n; break
                if checked[n]!=d:
                    refusal = 'replacement digest mismatch: '+n; break
        if refusal is None:
            for key in spec['_command_keys']:
                if key not in commands:
                    same_unit = any(k[:3]==key[:3] and k[4]==key[4] for k in commands)
                    refusal = ('replacement command digest mismatch: ' if same_unit else 'missing replacement command: ')+json.dumps(key)
                    break
        for n in spec['supersedes']:
            if n not in reasons: raise ValueError('replacement is not a progress unit: '+n)
            reasons[n] = refusal or ('reconciled by retirement' if spec['action']=='retire' else 'reconciled by replacement')
    return reasons


def header_targets():
    spec = json.loads((ROOT/'ConcreteSpec/landing/headers.json').read_text())
    if set(spec)!= {'revision','property'} or spec['property']!='exact-imports-and-complete-root':
        raise ValueError('invalid header target schema')
    revision = spec['revision']
    if not isinstance(revision,str) or not re.fullmatch('[0-9a-f]{40}',revision):
        raise ValueError('invalid header revision')
    try: git('merge-base','--is-ancestor',revision,'HEAD')
    except subprocess.CalledProcessError:
        raise ValueError('header revision is not an ancestor of HEAD') from None
    return {'header:F01':'exact-imports-and-complete-root'}


def header_property(root, data):
    """Run the production exact-imports algorithm against freshly built HEAD sources."""
    sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'proofs'))
    import imports as exact
    modules = {r['file'][:-5].replace('/','.') : r['imports'] for r in data if r['record']=='imports'}
    reachable = set()
    def visit(m):
        if m in reachable or m not in modules: return
        reachable.add(m)
        for dep in modules[m]: visit(dep)
    visit('ConcreteSpec')
    if reachable!=set(modules):
        return 'F01 property failure: ConcreteSpec.lean unreachable modules: '+', '.join(sorted(set(modules)-reachable))
    saved = exact.MODULES
    exact.MODULES = set(modules)
    try:
        with tempfile.TemporaryDirectory(prefix='landing-imports-') as tmp:
            probe = Path(tmp)/'Probe.lean'
            for path in exact.proof_paths(root):
                text = path.read_text()
                try:
                    module = path.relative_to(root).with_suffix('').as_posix().replace('/','.')
                    deps = exact.dependencies(text,probe,module,search_path=str(root))
                    if not exact.matches(text,deps):
                        actual,_,_ = exact.module_imports(text,exact.MODULES)
                        return f'F01 property failure: {path.relative_to(root)} expected imports {deps!r}, actual {sorted(actual)!r}'
                except exact.LexError as exc:
                    return f'F01 property failure: {path.relative_to(root)}: {exc}'
    finally:
        exact.MODULES = saved
    return 'exact-imports-and-complete-root'


def progress_reasons(snapshot, checked):
    commands = {key:checked[name] for name,key in getattr(checked,'commands',{}).items()}
    reasons = {}
    for name,digest in snapshot.items():
        key = getattr(snapshot,'commands',{}).get(name)
        value = commands.get(key) if key is not None else checked.get(name)
        reasons[name] = (value if name.startswith('header:') and value and value.startswith('F01 property failure:') else
                         'missing declaration' if value is None else
                         'full digest mismatch' if value!=digest else 'identical full digest')
    return reasons


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
    except ValueError as exc:
        if str(exc)!='split source command: d0': raise
    else:
        raise ValueError('identical-text control did not detect collapsed identities')
    wrong = [dict(r) for r in population]
    wrong[2]['piece'],wrong[3]['piece'] = wrong[3]['piece'],wrong[2]['piece']
    try:
        check_command_atomicity(wrong)
    except ValueError as exc:
        if str(exc)!='split source command: d0': raise
    else:
        raise ValueError('sibling declarations moved without their command')
    print('control PASS: distinct identical-text commands move independently; collapsed keying fails')


def judgment(rows, data, dependencies=True, extra_root=None):
    LAW_METADATA[:] = [r for r in (data if extra_root is None else data[0]) if r["record"]=="resolved-command"]
    seen = {}
    for r in rows:
        if r['record'] not in {'snapshot','piece','command','declaration','finding','contract','replacement'}:
            raise ValueError(f'unknown inventory record: {r["record"]}')
        key = (r['record'],r['name'])
        if key in seen:
            raise ValueError(f'duplicate assignment: {r["name"]}')
        seen[key] = r
    authored = authored_names(data if extra_root is None else data[0]+data[1])
    represented = {command_identity(d) for d in rows if d['record']=='declaration' and d['name'] in authored}
    replacement_rows(rows,authored | {'command:'+r['name'] for r in rows if r['record']=='command'
                                    and r['kind']!='module-realization' and r['name'] not in represented})
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
        allowed = {'retain','repair','replace','owner-held','retire'}
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
    command_keys = command_match_keys(rows)
    for r in rows:
        if r['piece']=='BASE' and r['record']=='command' and command_keys[r['name']] not in baseline_commands[r['source']]:
            raise ValueError(f'baseline assignment has no parsed baseline command: {r["name"]}')
    for r in decls.values():
        if r['piece']=='BASE' and not on_baseline(r):
            raise ValueError(f'baseline assignment has no baseline declaration: {r["name"]}')
    check_command_atomicity(rows)
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


def law_control_schedule(rows, write=False):
    declarations = {r['name']:r for r in rows if r['record']=='declaration'}
    @lru_cache(maxsize=None)
    def closure(p): return {p}|set().union(*(closure(d) for d in PREREQS[p]))
    cases = []
    for law,controls in law_control_groups(rows):
        lp = law['migration'] or law['piece']
        for c in controls:
            cp = c['migration'] or c['piece']
            unavailable = sorted(d for d in json.loads(c['dependencies'])
                                 if declarations[d]['piece'] not in closure(lp))
            cases.append(dict(law=law['name'],control=c['name'],law_piece=lp,control_piece=cp,unavailable=unavailable))
    evidence = {'basis':'Current rows: coordinator laws-with-controls ruling; dependency availability and co-landing',
                'source':SOURCE,'cases':cases}
    path = ROOT/'ConcreteSpec/landing/law-control-placement.json'
    if write: path.write_text(json.dumps(evidence,indent=2,sort_keys=True)+'\n')
    elif not path.exists() or json.loads(path.read_text())!=evidence:
        raise ValueError('law/control scheduling evidence is stale')
    return cases


def relocation_causes(rows, normalize=False):
    """Produce a closed, data-checked causal vocabulary for each moved row."""
    commands = {r['name']:r for r in rows if r['record']=='command'}
    declarations = {r['name']:r for r in rows if r['record']=='declaration'}
    fixtures = shared_fixtures(commands)
    members = defaultdict(list)
    for r in declarations.values():
        members[r['regeneration'] or command_identity(r)].append(r)
    graph = {n:{declarations[d]['regeneration'] or command_identity(declarations[d])
                for r in [c]+members[n] for d in json.loads(r['dependencies'] or '[]')
                if d in declarations} - {n} for n,c in commands.items()}
    def reachable(n):
        seen, todo = set(), [n]
        while todo:
            node = todo.pop()
            if node in seen: continue
            seen.add(node)
            todo.extend(graph.get(node,()))
        return seen
    closures = {n:reachable(n) for n in commands}
    groups = {}
    for law, controls in law_control_groups(rows):
        for control in controls:
            groups[(law['name'],control['name'])] = {
                command_identity(law),*(command_identity(c) for c in controls)}
    result = {}
    for r in rows:
        if r['record'] not in ('command','declaration'): continue
        key = r['regeneration'] or command_identity(r)
        causes = []
        note = r['note']
        unrelated = set()
        for match in re.finditer(r'law ([^;]+?) with control ([^;]+)',note):
            law, controls = match.groups()
            for control in controls.split(', '):
                pair = (law,control)
                if pair not in groups:
                    raise ValueError('unresolved relocation law/control: '+law+' with control '+control)
                group = groups[pair]
                if key in group: kind = 'law-control'
                elif closures.get(key,{key}) & group: kind = 'law-dependent'
                elif any(key in closures.get(n,{n}) for n in group): kind = 'law-support'
                else:
                    if normalize:
                        unrelated.add(pair)
                        continue
                    raise ValueError('unrelated relocation law/control: '+r['name']+'; '+law+' with control '+control)
                causes.append(dict(kind=kind,law=law,control=control))
        if unrelated:
            # Assignment propagation can inherit a union of consumer annotations.
            # Retain only pairs that actually explain this member, never borrow a
            # sibling consumer's law/control attribution through shared prose.
            def relevant(match):
                law, controls = match.groups()
                names = [c for c in controls.split(', ') if (law,c) not in unrelated]
                return 'law '+law+' with control '+', '.join(names) if names else ''
            r['note'] = re.sub(r'law ([^;]+?) with control ([^;]+)',relevant,note)
        if 'shared fixture/support moved to earliest consumer' in note:
            if key not in fixtures: raise ValueError('unverified shared fixture: '+r['name'])
            causes.append(dict(kind='shared-fixture',command=key))
        for fixture in re.findall(r'shared fixture ([^;]+?) forces consumer',note):
            if fixture not in fixtures or fixture not in closures.get(key,set()):
                raise ValueError('unverified shared fixture: '+r['name'])
            causes.append(dict(kind='shared-fixture-consumer',command=fixture))
        if 'regenerated at observed' in note:
            if not r['regeneration'] or r['regeneration'] not in commands or r['line']!='0':
                raise ValueError('unverified regenerated relocation: '+r['name'])
            causes.append(dict(kind='regenerated',command=r['regeneration']))
        for names in re.findall(r'dependent on observed auxiliary ([^;]+)',note):
            for name in names.split(', '):
                dep = declarations.get(name)
                if dep is None or not dep['regeneration'] or dep['regeneration'] not in closures.get(key,set()):
                    raise ValueError('unverified auxiliary relocation: '+r['name']+'; '+name)
                causes.append(dict(kind='auxiliary-dependent',declaration=name,command=dep['regeneration']))
        result[(r['record'],r['name'])] = causes
    return result


def relocation_evidence(rows, write=False):
    anchors = {}
    for r in rows:
        if r['record']=='declaration' and r['line']!='0':
            key = command_identity(r)
            anchors[key] = min(anchors.get(key,int(r['line'])),int(r['line']))
    verified_causes = relocation_causes(rows)
    moved = {}
    for r in rows:
        if r['record'] not in ('declaration','command'): continue
        key = command_identity(r)
        old = (WHOLE[r['source'].removeprefix('ConcreteSpec/').removesuffix('.lean')]
               if key.endswith(':0:0') else piece(r['source'],anchors.get(key,int(key.rsplit(':',2)[1]))))
        dest = r['migration'] or r['piece']
        if old==dest: continue
        causes = verified_causes[(r['record'],r['name'])]
        if not causes: raise ValueError('relocation lacks cause: '+r['name'])
        moved[r['record']+':'+r['name']] = dict(record=r['record'],name=r['name'],
                                              **{'from':old,'to':dest,'reason':causes,'note':r['note']})
    moved = dict(sorted(moved.items()))
    path = ROOT/'ConcreteSpec/landing/relocations.json'
    if write: path.write_text(json.dumps(moved,indent=2,sort_keys=True)+'\n')
    elif not path.exists() or json.loads(path.read_text())!=moved:
        raise ValueError('relocation evidence is stale')
    validation = ROOT/'ConcreteSpec/landing/VALIDATION.md'
    begin, end = '<!-- relocations:start -->', '<!-- relocations:end -->'
    table = '\n'.join(['| Record | Name | From | To | Cause |', '|---|---|---|---|---|'] +
        [f"| {r['record']} | `{r['name']}` | {r['from']} | {r['to']} | " +
         json.dumps(r['reason'],sort_keys=True).replace('|','&#124;')+' |' for r in moved.values()])
    block = begin+'\n'+table+'\n'+end
    text = validation.read_text()
    pattern = re.escape(begin)+r'.*?'+re.escape(end)
    if write:
        validation.write_text(re.sub(pattern,lambda _:block,text,flags=re.S)
                              if begin in text else text+'\n'+block+'\n')
    elif block not in text:
        raise ValueError('VALIDATION relocation table is stale')
    return moved


def law_cause(law, controls):
    return 'law '+law['name']+' with control '+', '.join(c['name'] for c in controls)


def inherited_causes(rows):
    # Normalize a named cause even when an earlier dependent-move note prefixes it.
    return '; '.join(sorted({'law '+law+' with control '+controls
                            for r in rows for law,controls in
                            re.findall(r'law ([^\s;]+) with control ([^;]+)',r['note'])}))


def annotate_move(r, cause):
    if cause not in r['note']: r['note'] += '; '+cause


RELOCATED_DECLARATIONS = {}


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
    # Derive the obligation from this invocation's current rows. Preserve a
    # control's destination when it needs material unavailable to its law.
    # An already co-located control does not pin the group to its old piece:
    # a later dependency can require the whole group to move sideways in the DAG.
    required_destinations = defaultdict(set)
    for law,controls in groups:
        for control in controls:
            if control['name'] in shared or effective(control)==effective(law): continue
            if any(declarations[d]['piece'] not in closures[effective(law)]
                   for d in dependencies(control)):
                required_destinations[law['name']].add(effective(control))
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
                RELOCATED_DECLARATIONS.setdefault(law['name'],old)
            for r in members:
                if effective(r)==destination:
                    if any(marker in r['note'] for marker in ('dependent ', 'shared fixture', 'regenerated at observed')):
                        annotate_move(r,law_cause(law,controls))
                    continue
                if effective(r) in HOLDS and effective(r) not in closures[destination]:
                    raise ValueError('cannot move owner-held material: '+r['name'])
                if r['piece']=='BASE': r['migration']=destination
                else: r.update(piece=destination,prerequisites=','.join(PREREQS[destination]))
                annotate_move(r, law_cause(law, controls))
                changed = True
        for identity, members in command_members.items():
            authored = [r for r in members if r['record']=='declaration' and r['piece']!='BASE' and r['line']!='0']
            if not authored: continue
            current = effective(authored[0])
            unavailable = {declarations[d]['piece'] for member in authored for d in dependencies(member)
                           if d in RELOCATED_DECLARATIONS and declarations[d]['piece'] not in closures[current]
                           and command_identity(declarations[d]) not in fixtures}
            if not unavailable: continue
            needs = {declarations[d]['piece'] for member in authored for d in dependencies(member)
                     if command_identity(declarations[d]) not in fixtures and command_identity(declarations[d])!=identity}
            candidates = [p for p in PREREQS if needs <= closures[p]]
            destination = min(candidates,key=lambda p:(len(closures[p]),p))
            for r in members:
                if r['piece']=='BASE': r['migration']=destination
                else: r.update(piece=destination,prerequisites=','.join(PREREQS[destination]))
                causes = inherited_causes(declarations[d] for member in authored for d in dependencies(member) if d in RELOCATED_DECLARATIONS)
                unavailable_names = sorted({d for member in authored for d in dependencies(member) if d in RELOCATED_DECLARATIONS})
                annotate_move(r, ('dependent on relocated law; '+causes) if causes else
                              'dependent on observed auxiliary '+', '.join(unavailable_names))
                if r['record']=='declaration': RELOCATED_DECLARATIONS.setdefault(r['name'],current)
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
                    annotate_move(r, 'shared fixture '+fixture+' forces consumer; '+inherited_causes(consumers))
                    if r['record']=='declaration': RELOCATED_DECLARATIONS.setdefault(r['name'],old)
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


def report(rows, detail=None, reasons=None):
    if detail and detail not in PREREQS and detail != 'all':
        raise ValueError(f'unknown remaining piece: {detail}')
    landed = {name for name,reason in (reasons or {}).items() if reason in ('identical full digest','reconciled by replacement','reconciled by retirement')}
    def identity(r):
        return ('command:' if r['record']=='command' else '')+r['name']
    units = [r for r in rows if r['record'] in ('declaration','command')
             and identity(r) in (reasons or {}) and r['piece']!='BASE']
    units += [row(record='header',name=n,piece='F01') for n in (reasons or {}) if n.startswith('header:')]
    counts = Counter(r['piece'] for r in units if identity(r) not in landed)
    examples = Counter(r['piece'] for r in units if r['kind']=='example' and identity(r) not in landed)
    print('remaining-examples\t'+str(sum(examples.values())))
    for p in sorted(r['name'] for r in rows if r['record']=='piece'):
        total = sum(r['piece']==p for r in units)
        matched = sum(r['piece']==p and identity(r) in landed for r in units)
        print(f'{p}\tlanded={matched}/{total}\tremaining={counts[p]}\tremaining-examples={examples[p]}')
    for name,reason in sorted((reasons or {}).items()):
        if reason.startswith('reconciled by '): print(reason+'\t'+name)
        elif reason.startswith(('F01 property failure:', 'superseded unit still present:',
                                'missing replacement ', 'replacement digest mismatch:',
                                'replacement command digest mismatch:')):
            print('progress refusal\t'+name+'\t'+reason)
    if detail:
        for r in sorted(units,key=lambda r:(r['piece'],r['name'])):
            if identity(r) not in landed and detail in ('all',r['piece']):
                print(f'remaining\t{r["piece"]}\t{identity(r)}\t{reasons[identity(r)]}')


PIN_TAGS = {'source': SOURCE, 'baseline': BASE, 'evidence': EVIDENCE, 'dossier': DOSSIER}


def ensure_objects(remote=None):
    if remote is None:
        try: remote = git('config','--get','landing.inventoryRemote').decode().strip()
        except subprocess.CalledProcessError: remote = 'origin'
        remote = remote or 'origin'

    missing = []
    for name, ref in PIN_TAGS.items():
        tag = 'refs/tags/spec-inventory/'+name
        try:
            local = git('rev-parse','--verify',tag+'^{commit}').decode().strip()
        except subprocess.CalledProcessError:
            local = None
        if local is not None:
            if local != ref: raise ValueError('changed protected inventory pin: '+tag+' expected '+ref)
            continue
        try: advertised = git('ls-remote',remote,tag,tag+'^{}').decode().splitlines()
        except subprocess.CalledProcessError: advertised = []
        advertised_refs = dict(line.split()[::-1] for line in advertised)
        if advertised_refs.get(tag+'^{}',advertised_refs.get(tag)) != ref:
            missing.append('missing or changed protected inventory pin: '+tag+' expected '+ref)
            continue
        git('fetch','--depth=1','--no-tags','--no-write-fetch-head',remote,tag+':'+tag)
        if git('rev-parse','--verify',tag+'^{commit}').decode().strip()!=ref:
            raise ValueError('changed protected inventory pin: '+tag+' expected '+ref)

    if missing: raise ValueError('\n'.join(missing))

def extract(root):
    files = git('ls-tree','-r','--name-only',SOURCE,'ConcreteSpec','ConcreteSpec.lean').decode().splitlines()
    for f in files:
        if f.endswith('.lean'):
            dest = root / f
            dest.parent.mkdir(parents=True,exist_ok=True)
            dest.write_bytes(source(SOURCE,f))


def check_replacement_judgment(rows, data):
    """Exercise judgment with a real committed review in a private Git clone."""
    global ROOT
    original_root = ROOT
    with tempfile.TemporaryDirectory(prefix='landing-review-control-') as tmp:
        clone = Path(tmp)/'repo'
        git('clone','--quiet','--shared',str(ROOT),str(clone))
        try:
            ROOT = clone
            victim = next(r for r in rows if r['record']=='declaration' and r['piece']!='BASE'
                          and r['disposition']!='owner-held' and r['name'] in authored_names(data))
            path = 'ConcreteSpec/landing/reviews/control.txt'
            artifact = ROOT/path
            artifact.parent.mkdir(parents=True,exist_ok=True)
            spec = dict(supersedes=[victim['name']],declarations={},commands=[],action='retire',reason='control',
                        review={})
            replacement = row(record='replacement',name='replacement-judgment-control',piece=victim['piece'],
                              disposition='replace',note=json.dumps(spec))
            with (ROOT/'ConcreteSpec/landing/inventory.tsv').open('w',newline='') as f:
                writer = csv.DictWriter(f,fieldnames=FIELDS,delimiter='\t')
                writer.writeheader()
                writer.writerows(rows+[replacement])
            def commit_fixture(message):
                git('add','ConcreteSpec/landing')
                git('-c','user.name=Inventory control','-c','user.email=control@example.invalid',
                    '-c','commit.gpgsign=false','-c','core.hooksPath=/dev/null','commit','--quiet','-m',message)
            commit_fixture('fixture reconciliation')
            artifact.write_text('COMMIT: '+git('rev-parse','HEAD').decode().strip()+
                                '\nRECORD: replacement-judgment-control\nVERDICT: ACCEPT\n')
            commit_fixture('fixture accepting review')
            spec['review'] = dict(reference=dict(path=path,commit=git('rev-parse','HEAD').decode().strip()),verdict='ACCEPT')
            replacement['note'] = json.dumps(spec)
            judgment(rows+[replacement],data,dependencies=False)
            spec['supersedes'] = ['InventoryControl.unknown']
            try: judgment(rows+[dict(replacement,note=json.dumps(spec))],data,dependencies=False)
            except ValueError as exc:
                if str(exc)!='unknown replacement row: InventoryControl.unknown': raise
            else: raise ValueError('judgment accepted unknown supersession')
        finally:
            ROOT = original_root
    print('control PASS: inventory judgment admits committed fixture review and refuses unknown supersession')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--write',action='store_true',help='explicitly regenerate initial plan assignments')
    parser.add_argument('--remaining', metavar='PIECE', help='list remaining authored source units for a piece, or all')
    parser.add_argument('--head-digests',type=Path,help='export checked HEAD full digests for reviewed replacement records')
    parser.add_argument('--census-output',type=Path,help='write fresh census JSONL for independent inspection; never read as a cache')
    args = parser.parse_args()
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
        existing_replacements = [r for r in read() if r['record']=='replacement'] if args.write else []
        rows = generate(data)+existing_replacements if args.write else read()
        closure = judgment(rows,data,dependencies=False)
        check_replacement_judgment(rows,data)
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
        relocation_evidence(rows)
        law_control_schedule(rows)
        snapshot = full_digests(data,root)
        snapshot.update(header_targets())
        checked = head_population()
        if args.head_digests:
            args.head_digests.write_text(json.dumps(head_digest_export(checked),indent=2,sort_keys=True)+'\n')
        reasons = replacement_progress(rows,progress_reasons(snapshot,checked),checked,snapshot)
        report(rows,args.remaining,reasons)
        judgment(rows,data)
        if args.write:
            write(rows)
            persisted = read()
            judgment(persisted,data)
            relocation_evidence(persisted)
            print('control PASS: regenerated inventory reread and judged against the same fresh census')
        print(f'landing inventory PASS: source={SOURCE}; baseline={BASE}')


if __name__ == '__main__':
    try:
        main()
    except (ValueError,RuntimeError,subprocess.CalledProcessError) as exc:
        sys.exit(str(exc))
