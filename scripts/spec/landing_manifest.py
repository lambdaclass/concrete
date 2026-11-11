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
import os
from pathlib import Path
import subprocess
import sys
import tempfile

from landing_census import census, run

ROOT = Path(__file__).resolve().parents[2]
SOURCE = 'aa44140a33747fa37ece59c76090972e564c65b4'
BASE = '1a50c64ff1b6f3a2899522f279eccc86d3b004a2'
EVIDENCE = '5db3018e24eb284894b3615833aa373c76944475'
DOSSIER = '2bef2b5e20dffb5b277f9451be6fef969cf6c898'
MANIFEST = ROOT / 'ConcreteSpec/landing/inventory.tsv'
FIELDS = ['record', 'name', 'kind', 'source', 'line', 'end_line', 'piece',
          'prerequisites', 'disposition', 'contract', 'digest', 'dependencies', 'migration', 'note']

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
HOLDS = {'F03p':'path character/normalization owner answer', 'F04p':'path authority',
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
CONTRACTS = {'Ident':'C9','Target':'C3','Outcome':'C1','Exec':'C1','Scalar':'C2',
 'Const':'C2','Memory':'C4','Hybrid':'C4','Own':'C5','Grant':'C5','Prim':'C9',
 'Leak':'C7','Adequacy':'C8','Frame':'C10','Authority':'C5','Brand':'C5',
 'Borrow':'C5','Cleanup':'C1','Rep':'C4','Basics':'C9','Rules':'C9'}


def git(*args):
    return subprocess.check_output(['git', *args], cwd=ROOT, stderr=subprocess.DEVNULL)


def sha(data):
    return hashlib.sha256(data).hexdigest()


@lru_cache(maxsize=None)
def source(ref, path):
    return git('show', f'{ref}:{path}')


def piece(file, line):
    stem = file.removeprefix('ConcreteSpec/').removesuffix('.lean')
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
                          (3753,3883,'F09a'),(3884,3908,'F09b'),(3997,4058,'F09c')]:
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
        return 'F29p' if line == 2334 or 2343 <= line <= 2353 or 2429 <= line <= 2457 else 'F29'
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


def records_from_census(data):
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
            files[f] = source(SOURCE, f).decode().splitlines(keepends=True)
        digest = sha(''.join(files[f][c['line']-1:c['endLine']]).encode())
        r = row(record='command', name=f"{f}:{c['line']}:{c['endLine']}",
                kind='example' if c['example'] else c['syntax'], source=f,
                line=c['line'], end_line=c['endLine'], digest=digest,
                contract=contract(f), dependencies=json.dumps(sorted(set(example_deps.get((f,c['line']),[]))), separators=(',',':')))
        commands[f].append(r)
        result.append(r)
    for d in decls:
        f = d['module'].replace('.', '/') + '.lean'
        command = next((c for c in commands[f] if int(c['line']) <= d['line'] <= int(c['end_line'])), None)
        result.append(row(record='declaration', name=d['name'], kind=d['kind'], source=f,
                          line=d['line'], end_line=command['end_line'] if command else 0,
                          digest=command['digest'] if command else sha((SOURCE+'\n'+d['name']).encode()),
                          dependencies=json.dumps(sorted(set(d['uses']) & names), separators=(',',':')),
                          contract=contract(f), note=command['name'] if command else 'generated; no source range'))
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
def baseline_command_digests():
    """Only actual parsed commands qualify as existing baseline source, never text in comments."""
    paths = git('ls-tree','-r','--name-only',BASE,'ConcreteSpec','ConcreteSpec.lean').decode().splitlines()
    paths = [p for p in paths if p.endswith('.lean')]
    with tempfile.TemporaryDirectory(prefix='spec-baseline-') as tmp:
        root = Path(tmp)
        for path in paths:
            target = root/path
            target.parent.mkdir(parents=True,exist_ok=True)
            target.write_bytes(source(BASE,path))
        parsed = [json.loads(s) for s in run(['lean','--run',str(ROOT/'scripts/spec/landing_inventory.lean'),
                  'parse',*paths],root,dict(os.environ,LEAN_PATH=str(root))).splitlines()]
    result = defaultdict(set)
    for c in parsed:
        if c['record']=='command':
            lines = source(BASE,c['file']).splitlines(keepends=True)
            result[c['file']].add(sha(b''.join(lines[c['line']-1:c['endLine']])))
    return result


def generate(data):
    rows = [row(record='snapshot',name='source',kind='git-commit',source=SOURCE,piece='P',disposition='retain'),
            row(record='snapshot',name='baseline',kind='git-commit',source=BASE,piece='BASE',disposition='retain'),
            row(record='snapshot',name='plan',kind='sha256',source='ConcreteSpec/landing/plan.md',piece='P',
                digest=sha((ROOT/'ConcreteSpec/landing/plan.md').read_bytes()),disposition='retain')]
    for p, deps in PREREQS.items():
        rows.append(row(record='piece',name=p,kind='landing-piece',piece=p,prerequisites=','.join(deps),
                        disposition='owner-held' if p in HOLDS else 'retain',note=HOLDS.get(p,'')))
    population = records_from_census(data)
    for r in population:
        p = piece(r['source'],int(r['end_line'] or r['line']))
        r.update(piece=p, prerequisites=','.join(PREREQS[p]),
                 disposition='owner-held' if p in HOLDS else 'repair',
                 note=r['note'] + ('; '+HOLDS[p] if p in HOLDS else ''))
    for r in population:
        if r['source']=='ConcreteSpec/Spec/Ident.lean' and r['piece']=='F02':
            r['disposition'] = 'replace'
            r['note'] += '; ISS-0046: replace presentation-bearing identity with shared numeric identity; do not restore the raw-record inequality'
    byname = {r['name']:r for r in population if r['record']=='declaration'}
    baseline = baseline_command_digests()
    for r in population:
        if r['line']!='0' and r['digest'] in baseline[r['source']]:
            r['migration'] = r['piece']
            r.update(piece='BASE',prerequisites='',disposition='retain')
            r['note'] += '; parsed baseline command; adapt at migration piece'
        elif r['record']=='declaration' and r['line']=='0':
            # Initial module assignment conveys no information about a compiler artifact's
            # origin. Only the actual dependency/consumer constraints below assign it.
            r.update(piece='P',prerequisites=','.join(PREREQS['P']),disposition='repair',
                     note='compiler artifact; no source command range')
    # Resolve concrete dependency closures. Anonymous compiler artifacts may be emitted by
    # a later importer; they are regenerated with their first actual consumer, not copied as
    # authored source. This is an extraction assignment, never compiler-origin attestation.
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
                 disposition='owner-held' if p in HOLDS else 'repair')
        if why not in r['note']:
            r['note'] += '; '+why
    # First move complete ranged fixture commands (the plan's earliest-consumer rule),
    # following generated intermediates as well as direct references in anonymous examples.
    def ranged_dependencies(name,seen):
        if name in seen:
            return set()
        seen.add(name)
        if byname[name]['line']!='0':
            return {name}
        out = set()
        for dep in json.loads(byname[name]['dependencies']):
            out |= ranged_dependencies(dep,seen)
        return out
    named_cache = {n:ranged_dependencies(n,set()) for n in byname}
    consumers = [r for r in population if r['kind']=='example'] + [r for r in byname.values() if r['line']!='0']
    for _ in range(len(PREREQS)):
        changed = False
        for r in consumers:
            needs = set()
            for dep in json.loads(r['dependencies']):
                needs |= named_cache[dep]
            for dep in sorted(needs):
                d = byname[dep]
                if r['piece']==d['piece'] or d['piece'] in ancestry[r['piece']]:
                    continue
                if ('.Demo.' in d['name'] or '.CoreEvidence.' in d['name'] or '.Vacuity.' in d['name']) and r['piece'] in ancestry[d['piece']]:
                    key = (d['source'],d['digest'])
                    for support in population:
                        if (support['source'],support['digest'])==key:
                            assign(support,r['piece'],'shared fixture/support moved to earliest consumer')
                    changed = True
        if not changed:
            break
    # Strongly connected components keep mutual generated auxiliaries together. Propagate the
    # actual ranged dependencies/users through their DAG; never infer authorship from names.
    generated = {n:r for n,r in byname.items() if r['line']=='0'}
    index, low, stack, onstack, components = {}, {}, [], set(), []
    sys.setrecursionlimit(max(10000,len(generated)*2))
    def connect(n):
        index[n] = low[n] = len(index)
        stack.append(n); onstack.add(n)
        for d in json.loads(generated[n]['dependencies']):
            if d not in generated:
                continue
            if d not in index:
                connect(d); low[n] = min(low[n],low[d])
            elif d in onstack:
                low[n] = min(low[n],index[d])
        if low[n]==index[n]:
            group = []
            while True:
                d = stack.pop(); onstack.remove(d); group.append(d)
                if d==n:
                    break
            components.append(group)
    for n in generated:
        if n not in index:
            connect(n)
    component = {n:i for i,g in enumerate(components) for n in g}
    outgoing, incoming = defaultdict(set), defaultdict(set)
    lower, upper = defaultdict(set), defaultdict(set)
    for n,r in byname.items():
        for dep in json.loads(r['dependencies']):
            if n in generated and dep in generated:
                a,b = component[n],component[dep]
                if a!=b:
                    outgoing[a].add(b); incoming[b].add(a)
            elif n in generated:
                lower[component[n]].add(byname[dep]['piece'])
            elif dep in generated:
                upper[component[dep]].add(r['piece'])
    for r in population:
        if r['kind']=='example':
            for dep in json.loads(r['dependencies']):
                if dep in generated:
                    upper[component[dep]].add(r['piece'])
    def reach(i,edges,anchors,cache):
        if i not in cache:
            result = set(anchors[i])
            for j in edges[i]:
                result |= reach(j,edges,anchors,cache)
            cache[i] = result
        return cache[i]
    lc,uc = {},{}
    for i,group in enumerate(components):
        needs = reach(i,outgoing,lower,lc)
        users = reach(i,incoming,upper,uc)
        candidates = [p for p in PREREQS
                      if all(d==p or d in ancestry[p] for d in needs)
                      and all(p==u or p in ancestry[u] for u in users)]
        if not candidates:
            raise ValueError(f'generated closure has incompatible pieces: {group[0]}: needs={sorted(needs)}, users={sorted(users)}')
        p = min(candidates,key=lambda p:(len(ancestry[p]),p))
        for n in group:
            assign(generated[n],p,'regenerated auxiliary; actual dependency/consumer closure, not origin attestation')
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
    with MANIFEST.open('w',newline='') as f:
        writer = csv.DictWriter(f, FIELDS, delimiter='\t', lineterminator='\n')
        writer.writeheader()
        writer.writerows(sorted(rows,key=lambda r:(r['record'],r['name'])))


def read():
    with MANIFEST.open(newline='') as f:
        reader = csv.DictReader(f,delimiter='\t')
        if reader.fieldnames != FIELDS:
            raise ValueError('inventory schema mismatch')
        return list(reader)


def judgment(rows, data, dependencies=True):
    seen = {}
    for r in rows:
        key = (r['record'],r['name'])
        if key in seen:
            raise ValueError(f'duplicate assignment: {r["name"]}')
        seen[key] = r
    pieces = {r['name']:r for r in rows if r['record']=='piece'}
    if set(pieces) != set(PREREQS):
        raise ValueError('landing piece population mismatch')
    for p in PREREQS:
        if pieces[p]['prerequisites'] != ','.join(PREREQS[p]):
            raise ValueError(f'plan prerequisites changed: {p}')
    for r in rows:
        if not r['piece'] or r['piece'] not in pieces:
            raise ValueError(f'unassigned {r["record"]}: {r["name"]}')
    expected = records_from_census(data) + evidence_rows()
    expected_keys = {(r['record'],r['name']) for r in expected}
    if len(expected_keys) != len(expected):
        raise ValueError('duplicate identity in the live Lean/evidence census')
    for e in expected:
        key = (e['record'],e['name'])
        if key not in seen:
            raise ValueError(f'unassigned {e["record"]}: {e["name"]}')
        r = seen[key]
        for k in ('kind','source','line','end_line','digest','dependencies','contract'):
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
    decls = {r['name']:r for r in rows if r['record']=='declaration'}
    command_rows = {(r['source'],r['digest']):r for r in rows if r['record']=='command'}
    for r in decls.values():
        if r['piece']=='BASE' and r['line']!='0' and r['digest'] not in baseline_command_digests()[r['source']]:
            raise ValueError(f'baseline assignment has no parsed baseline command: {r["name"]}')
        if r['line']!='0':
            c = command_rows[(r['source'],r['digest'])]
            if r['piece'] != c['piece']:
                raise ValueError(f'split source command: {r["name"]}')
    errors = []
    if dependencies:
        for r in list(decls.values()) + [r for r in rows if r['kind']=='example']:
            for dep in json.loads(r['dependencies']):
                d = decls[dep]
                if d['piece'] != r['piece'] and d['piece'] not in closure[r['piece']]:
                    errors.append(f'{r["piece"]} {r["name"]} requires {d["piece"]} {dep}')
        if errors:
            raise ValueError('piece dependency order refused:\n'+'\n'.join(errors))
    return closure


def report(rows, detail=None):
    counts = Counter(r['piece'] for r in rows if r['record']=='declaration' and r['piece']!='BASE')
    examples = Counter(r['piece'] for r in rows if r['kind']=='example')
    for p in sorted(r['name'] for r in rows if r['record']=='piece'):
        print(f'{p}\tremaining={counts[p]}\texamples={examples[p]}')
    if detail:
        for r in sorted(rows,key=lambda r:(r['piece'],r['name'])):
            if r['record']=='declaration' and r['piece']!='BASE' and detail in ('all',r['piece']):
                print(f'remaining\t{r["piece"]}\t{r["name"]}')


def ensure_objects():
    # Fresh CI checkouts need the historical extraction/evidence objects. Fetch exact commits
    # into the object database only: never move a branch, tag, FETCH_HEAD, or shallow boundary.
    for ref in (SOURCE,BASE,EVIDENCE,DOSSIER):
        try:
            git('cat-file','-e',ref+'^{commit}')
        except subprocess.CalledProcessError:
            print(f'fetch pinned inventory source {ref}',flush=True)
            git('fetch','--no-tags','--no-write-fetch-head','origin',ref)
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
    args = parser.parse_args()
    if os.environ.get('CONCRETE_GATES_SHELL') != '1':
        raise ValueError('run inside nix develop -c')
    ensure_objects()
    print(f'landing inventory: source={SOURCE}; baseline={BASE}',flush=True)
    with tempfile.TemporaryDirectory(prefix='spec-landing-') as tmp:
        root = Path(tmp)
        extract(root)
        data = census(root,ROOT/'scripts/spec/landing_inventory.lean')
        if args.write:
            write(generate(data))
        rows = read()
        judgment(rows,data,dependencies=False)
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
        control = root/'ConcreteSpec/InventoryControl.lean'
        control.write_text('namespace ConcreteSpec.InventoryControl\ndef unassigned : Nat := 7\nend ConcreteSpec.InventoryControl\n')
        env = dict(os.environ,LEAN_PATH=str(root))
        run(['lean','-o',str(control.with_suffix('.olean')),str(control)],root,env)
        mf = root/'control-modules.txt'
        mf.write_text('ConcreteSpec.InventoryControl\n')
        planted = [json.loads(s) for s in run(['lean','--run',str(ROOT/'scripts/spec/landing_inventory.lean'),
            'constants',str(mf)],root,env).splitlines()]
        # No command row is planted: demand failure on the real new constant specifically.
        try:
            judgment(rows,data+planted)
        except ValueError as exc:
            if str(exc) != 'unassigned declaration: ConcreteSpec.InventoryControl.unassigned':
                raise
            print('control PASS: planted unassigned declaration refused')
        else:
            raise ValueError('unassigned control accepted')
        # A complete command assigned ahead of its scalar/execution prerequisites must fail
        # dependency order, rather than just the source-command atomicity check.
        scalar_names = {r['name'] for r in rows if r['record']=='declaration' and r['piece']=='F05'}
        victim = next(r for r in rows if r['record']=='declaration' and r['piece']=='F06'
                      and r['line']!='0' and scalar_names.intersection(json.loads(r['dependencies'])))
        bad = [dict(r) for r in rows]
        for r in bad:
            if (r['source'],r['digest']) == (victim['source'],victim['digest']):
                r.update(piece='F03',prerequisites=','.join(PREREQS['F03']))
        try:
            judgment(bad,data)
        except ValueError as exc:
            if not str(exc).startswith('piece dependency order refused:'):
                raise
            print('control PASS: premature command assignment refused by dependency order')
        else:
            raise ValueError('dependency-order control accepted')
        report(rows,args.remaining)
        judgment(rows,data)
        print(f'landing inventory PASS: source={SOURCE}; baseline={BASE}')


if __name__ == '__main__':
    try:
        main()
    except (ValueError,RuntimeError,subprocess.CalledProcessError) as exc:
        sys.exit(str(exc))
