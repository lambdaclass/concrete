#!/usr/bin/env python3
"""One online allocator for ISS and bug identities. See docs/ISSUE_IDS.md."""
import argparse
import csv
import datetime
import json
import functools
import os
from pathlib import Path
import re
import socket
import subprocess
import sys
import tempfile
import uuid


class Refused(Exception):
    pass


def sandbox_environment():
    # Equivalent to checks/lib/git_sandbox.sh, also for direct Python CLI use under a hook.
    clean = dict(os.environ)
    for name in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR'):
        clean.pop(name, None)
    result = subprocess.run(['git', 'rev-parse', '--local-env-vars'], env=clean,
                            text=True, capture_output=True, check=True)
    for name in result.stdout.splitlines():
        os.environ.pop(name, None)
    os.environ['GIT_CONFIG_NOSYSTEM'] = '1'
    os.environ['GIT_CONFIG_SYSTEM'] = '/dev/null'
    os.environ['GIT_CONFIG_GLOBAL'] = '/dev/null'


def git(root, *args, data=None, check=True):
    p = subprocess.run(['git', '-C', str(root), *args], input=data, text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if check and p.returncode:
        raise Refused(f"git {' '.join(args[:3])}: {p.stderr.strip()}")
    return p


def ref_for(key):
    kind, number = key.split('-')
    return (f'refs/reserved/iss/{number}' if kind == 'ISS'
            else f'refs/bug-reservations/{number}')


def valid_key(key):
    if not re.fullmatch(r'(ISS-\d{4}|BUG-\d{3,})', key):
        raise Refused(f'invalid ID: {key}')
    return key


def issue_title(evidence):
    # Migration only. Once a title column exists, evidence can change independently.
    return evidence.split('. ')[0].strip()


def records(read, paths):
    found = {}
    def add(key, title, path):
        valid_key(key)
        if key in found:
            raise Refused(f'duplicate {key}: {found[key][1]} and {path}')
        if not title or any(c in title for c in '\n\r\t'):
            raise Refused(f'{key}: invalid title')
        found[key] = (title, path)
    if 'ISSUES.tsv' in paths:
        lines = [s for s in read('ISSUES.tsv').splitlines() if s and not s.startswith('#')]
        for row in csv.DictReader(lines, delimiter='\t'):
            add(row['id'], row.get('title') or issue_title(row['evidence']), 'ISSUES.tsv')
    for path in paths:
        m = re.fullmatch(r'(?:legacy/)?docs/bugs/(\d{3,})_[^/]+\.md', path)
        if re.fullmatch(r'(?:legacy/)?docs/bugs/[^/]+\.md', path) and Path(path).name[0].isdigit() and not m:
            raise Refused(f'malformed bug record path: {path}')
        if m:
            text = read(path)
            header = next((s for s in text.splitlines() if s.startswith('# ')), '')
            if not re.match(r'^# Bug ' + m[1] + r'\s*[:—–-]\s*\S', header, re.I):
                raise Refused(f'{path}: missing matching Bug heading')
            title = re.sub(r'^# Bug \d+\s*[:—–-]\s*', '', header, flags=re.I).strip()
            add('BUG-' + m[1], title, path)
    return found


@functools.lru_cache(maxsize=None)
def blob(repo, oid):
    return git(repo, 'cat-file', 'blob', oid).stdout


@functools.lru_cache(maxsize=None)
def raw_tree_records(repo, sha):
    listing = git(repo, 'ls-tree', '-r', '--format=%(objectname) %(path)', sha).stdout
    paths = dict(line.split(' ', 1)[::-1] for line in listing.splitlines())
    return records(lambda p: blob(repo, paths[p]), paths)


@functools.lru_cache(maxsize=None)
def tree_records(repo, sha):
    result = dict(raw_tree_records(repo, sha))
    header = git(repo, 'show', f'{sha}:ISSUES.tsv', check=False).stdout
    if header and 'id\ttitle\t' not in header:
        # Old branches have no immutable title. Recover it from the first introduction in
        # their history, rather than treating later evidence edits as new identities.
        history = git(repo, 'log', '--reverse', '--topo-order', '--format=%H', sha,
                      '--', 'ISSUES.tsv').stdout.splitlines()
        first = {}
        for revision in history:
            for key, value in raw_tree_records(repo, revision).items():
                if key.startswith('ISS-'):
                    first.setdefault(key, value)
        for key in result.keys() & first.keys():
            result[key] = first[key]
    return result


def work_records(root):
    paths = git(root, 'ls-files', '--cached', '--others', '--exclude-standard').stdout.splitlines()
    return records(lambda p: (Path(root) / p).read_text(), sorted(p for p in set(paths) if (Path(root) / p).is_file()))


class Snapshot:
    """Fetch into a private bare repo: never updates the caller's refs or notes."""
    def __init__(self, root, directory):
        self.root = root
        self.repo = Path(directory) / 'snapshot.git'
        git(directory, 'init', '--bare', str(self.repo))
        self.url = git(root, 'remote', 'get-url', 'origin').stdout.strip()
        git(self.repo, 'fetch', '--quiet', '--no-tags', self.url,
            '+refs/heads/main:refs/heads/main',
            '+refs/heads/wip/*:refs/heads/wip/*',
            '+refs/reserved/*:refs/reserved/*',
            '+refs/bug-reservations/*:refs/bug-reservations/*')
        self.refs = dict(line.split(' ', 1)[::-1] for line in git(
            self.repo, 'for-each-ref', '--format=%(objectname) %(refname)').stdout.splitlines())
        self.heads = {r.removeprefix('refs/heads/'): sha for r, sha in self.refs.items()
                      if r.startswith('refs/heads/')}
        self.trees = {branch: tree_records(self.repo, sha) for branch, sha in self.heads.items()}
        self.remote_heads = dict(self.heads)
        local_branch = git(root, 'symbolic-ref', '--short', 'HEAD', check=False).stdout.strip()
        git(self.repo, 'fetch', '--quiet', '--no-tags', str(root), 'HEAD:refs/local/head')
        if local_branch:
            self.heads[local_branch] = git(root, 'rev-parse', 'HEAD').stdout.strip()

    def reservation(self, key):
        ref = ref_for(key)
        oid = self.refs.get(ref)
        if oid is None:
            return None
        # Legacy commits may be imported by backfill, but never count as new-schema reservations.
        if git(self.repo, 'cat-file', '-t', oid).stdout.strip() != 'commit':
            raise Refused(f'{ref}: not a commit')
        parents = git(self.repo, 'rev-list', '--parents', '-n', '1', oid).stdout.split()
        if len(parents) != 1 or git(self.repo, 'ls-tree', oid).stdout:
            raise Refused(f'{ref}: reservation must be a parentless empty-tree commit')
        message = git(self.repo, 'show', '-s', '--format=%B', oid).stdout
        try:
            value = json.loads(message)
        except ValueError:
            return {'legacy_oid': oid}
        required = {'schema', 'id', 'title', 'branch', 'who', 'when', 'nonce'}
        if not isinstance(value, dict) or not required <= value.keys() or value['schema'] != 1:
            raise Refused(f'{ref}: malformed reservation')
        if value['id'] != key or any(not isinstance(value[k], str) or not value[k].strip()
                                     for k in required - {'schema'}):
            raise Refused(f'{ref}: invalid reservation identity')
        return value


def metadata(root, key, title, branch):
    if not title.strip() or any(c in title for c in '\n\r\t'):
        raise Refused('title must be one nonempty line without tabs')
    if branch != 'main' and not branch.startswith('wip/'):
        raise Refused('branch must be main or wip/*')
    git(root, 'check-ref-format', 'refs/heads/' + branch)
    return dict(schema=1, id=valid_key(key), title=title, branch=branch,
                who=git(root, 'config', 'user.email').stdout.strip() + '@' + socket.gethostname(),
                when=datetime.datetime.now(datetime.timezone.utc).isoformat(), nonce=str(uuid.uuid4()))


def commit(root, value):
    tree = git(root, 'hash-object', '-t', 'tree', '--stdin', '-w', data='').stdout.strip()
    return git(root, 'commit-tree', tree, data=json.dumps(value, sort_keys=True) + '\n').stdout.strip()


def publish(root, updates):
    """One transaction, exact leases. A lost response is resolved by reading every ref back."""
    if not updates:
        return
    receipt_dir = Path(git(root, 'rev-parse', '--absolute-git-dir').stdout.strip()) / 'id-reservation-attempts'
    receipt_dir.mkdir(exist_ok=True)
    receipt = receipt_dir / (str(uuid.uuid4()) + '.json')
    receipt.write_text(json.dumps(dict(updates=updates, commits={new: git(root, 'cat-file', 'commit', new).stdout
                                                               for _, _, new in updates}), indent=2) + '\n')
    print(f'ID reservation recovery receipt: {receipt}', file=sys.stderr)
    args = ['push', '--atomic', '--porcelain']
    args += [f'--force-with-lease={ref}:{old}' for ref, old, _ in updates]
    args += ['origin'] + [f'{new}:{ref}' for ref, _, new in updates]
    result = git(root, *args, check=False)
    remote = git(root, 'ls-remote', 'origin', *[r for r, _, _ in updates])
    seen = dict(line.split()[::-1] for line in remote.stdout.splitlines())
    if all(seen.get(ref) == new for ref, _, new in updates):
        return
    if any(seen.get(ref) == new for ref, _, new in updates):
        raise Refused('partial reservation observed; do not file. Preserve plan and reconcile refs.')
    raise Refused('reservation not acquired; do not file; retry after a fresh snapshot. ' + result.stderr.strip())


def check_entries(snapshot, entries, label):
    for key, (title, _) in entries.items():
        reservation = snapshot.reservation(key)
        if not reservation or 'schema' not in reservation:
            raise Refused(f'{label}: {key} has no schema-1 reservation; run backfill')
        if reservation['title'] != title:
            raise Refused(f'{label}: {key} title differs from reservation ({title!r})')


def check_snapshot(snapshot):
    # Operator audit only; ordinary gates never couple unrelated draft branches.
    for branch, entries in snapshot.trees.items():
        check_entries(snapshot, entries, branch)


def check_change(root, snapshot):
    head = git(root, 'rev-parse', 'HEAD').stdout.strip()
    base = git(snapshot.repo, 'merge-base', snapshot.heads['main'], head).stdout.strip()
    # Compare complete rows/content, not just titles: edits to an old identity activate enforcement.
    listing = git(snapshot.repo, 'ls-tree', '-r', '--format=%(objectname) %(path)', base).stdout
    old = dict(line.split(' ', 1)[::-1] for line in listing.splitlines())
    local = work_records(root)
    old_records = tree_records(snapshot.repo, base)
    changed = {}
    for key, value in local.items():
        title, path = value
        if key not in old_records or path not in old:
            changed[key] = value
        elif path == 'ISSUES.tsv':
            before = next((x for x in blob(snapshot.repo, old[path]).splitlines() if x.startswith(key + '\t')), None)
            after = next((x for x in (Path(root) / path).read_text().splitlines() if x.startswith(key + '\t')), None)
            if before != after:
                changed[key] = value
        elif blob(snapshot.repo, old[path]) != (Path(root) / path).read_text():
            changed[key] = value
    check_entries(snapshot, changed, 'change')


def legacy_signature(repo, sha, key):
    text = git(repo, 'show', f'{sha}:ISSUES.tsv', check=False).stdout
    for row in csv.DictReader((x for x in text.splitlines() if x and not x.startswith('#')), delimiter='\t'):
        if row['id'] == key:
            return tuple(row.get(k, '') for k in ('date', 'opened', 'area', 'kind'))
    return None


def make_plan(root, snapshot, local, branch):
    trees = list(snapshot.trees.items()) + [(branch, local)]
    uses = {}
    for name, entries in trees:
        for key, (title, _) in entries.items():
            uses.setdefault(key, []).append((name, title))
    conflicts = []
    updates = []
    for key, rows in sorted(uses.items()):
        titles = {title for _, title in rows}
        # Six-column pre-migration rows have mutable evidence, not immutable titles.
        # Canonical explicit titles win only when stable legacy fields agree.
        if key.startswith('ISS-') and len(titles) > 1:
            signatures = {legacy_signature(snapshot.repo, snapshot.heads[b], key)
                          for b, _ in rows if b in snapshot.heads}
            explicit = {title for b, title in rows if b in snapshot.heads and
                        'id\ttitle\t' in git(snapshot.repo, 'show', f'{snapshot.heads[b]}:ISSUES.tsv', check=False).stdout}
            if len(signatures) == 1 and None not in signatures and len(explicit) == 1:
                titles = explicit

        if len(titles) != 1:
            conflicts.append(f'{key}: ' + repr(rows))
            continue
        title = next(iter(titles))
        # Reservation provenance is stable metadata, independent of commit ancestry.
        owner = sorted((b for b, _ in rows), key=lambda b: (b != 'main', b))[0]
        old = snapshot.reservation(key)
        if old and 'schema' in old:
            if old['title'] != title:
                conflicts.append(f'{key}: existing reservation disagrees')
            continue
        value = metadata(root, key, title, owner)
        if old:
            value['legacy_oid'] = old['legacy_oid']
        updates.append(dict(ref=ref_for(key), old=snapshot.refs.get(ref_for(key), ''), value=value))
    # Preserve every unused legacy reservation too: gaps and in-flight numbers are never reused.
    for ref, oid in snapshot.refs.items():
        if ref.startswith('refs/bug-reservations/'):
            key = 'BUG-' + ref.rsplit('/', 1)[1]
            valid_key(key)
            if key not in uses:
                old = snapshot.reservation(key)
                if old and 'schema' not in old:
                    value = metadata(root, key, 'Retired legacy reservation ' + key, 'main')
                    value['legacy_oid'] = oid
                    updates.append(dict(ref=ref, old=oid, value=value))
    return dict(schema=1, heads=snapshot.remote_heads, updates=updates, conflicts=conflicts,
                collision_resolution='ISS-0017 acceptance-shim -> ISS-0033; spec-* retains ISS-0017; lexer -> ISS-0032. Branches must actually be renumbered before publishing.')


def main():
    sandbox_environment()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', default='.')
    sub = parser.add_subparsers(dest='command', required=True)
    reserve = sub.add_parser('reserve')
    reserve.add_argument('kind', choices=['iss', 'bug'])
    reserve.add_argument('title')
    reserve.add_argument('--number', type=int, help='explicit coordinator-assigned number; never overwrites')
    sub.add_parser('check')
    sub.add_parser('audit-wip')
    verify = sub.add_parser('verify-ref')
    verify.add_argument('ref')
    verify.add_argument('oid')
    verify.add_argument('old')
    backfill = sub.add_parser('backfill')
    backfill.add_argument('--output', required=True)
    apply = sub.add_parser('publish-plan')
    apply.add_argument('plan')
    args = parser.parse_args()
    root = Path(args.root).resolve()
    branch = git(root, 'symbolic-ref', '--short', 'HEAD', check=False).stdout.strip()
    if not branch:
        # CI detached head: identify it only when exactly one fetched wip head matches it; main
        # and train candidates can set the explicit identity via git config id.branch.
        branch = git(root, 'config', 'id.branch', check=False).stdout.strip()
    if args.command == 'verify-ref':
        ref, oid, old = args.ref, args.oid, args.old
        if set(oid) == {'0'}:
            raise Refused(f'reservation ref {ref} may not be deleted')
        if set(old) != {'0'} and git(root, 'cat-file', '-e', old, check=False).returncode:
            git(root, 'fetch', '--no-tags', '--no-write-fetch-head', 'origin', ref)
        message = git(root, 'show', '-s', '--format=%B', oid).stdout
        value = json.loads(message)
        if ref != ref_for(valid_key(value['id'])) or value['schema'] != 1:
            raise Refused('invalid reservation ref or schema')
        for field in ('title', 'branch', 'who', 'when', 'nonce'):
            if not isinstance(value[field], str) or not value[field].strip():
                raise Refused('missing reservation metadata')
        if git(root, 'rev-list', '--parents', '-n', '1', oid).stdout.split() != [oid] or git(root, 'ls-tree', oid).stdout:
            raise Refused('reservation must be an empty orphan commit')
        if set(old) != {'0'}:
            if value.get('legacy_oid') != old:
                raise Refused('reservations are immutable; only explicit legacy import is allowed')
            try:
                json.loads(git(root, 'show', '-s', '--format=%B', old).stdout)
            except ValueError:
                pass
            else:
                raise Refused('cannot replace a schema reservation')
        return
    with tempfile.TemporaryDirectory(prefix='concrete-ids-') as temp:
        snapshot = Snapshot(root, temp)
        if args.command in ('check', 'audit-wip'):
            # Validate reservation namespace integrity even when there are no changed records.
            for ref in snapshot.refs:
                if ref.startswith(('refs/reserved/', 'refs/bug-reservations/')):
                    key = ('ISS-' if ref.startswith('refs/reserved/iss/') else 'BUG-') + ref.rsplit('/', 1)[1]
                    valid_key(key)
                    if ref != ref_for(key):
                        raise Refused(f'malformed reservation ref: {ref}')
                    snapshot.reservation(key)
            if args.command == 'audit-wip':
                check_snapshot(snapshot)
            else:
                check_change(root, snapshot)
            print('ID reservations: PASS')
        elif args.command == 'backfill':
            plan = make_plan(root, snapshot, work_records(root), branch)
            Path(args.output).write_text(json.dumps(plan, indent=2, sort_keys=True) + '\n')
            print(f'{len(plan["updates"])} planned reservations; {len(plan["conflicts"])} conflicts. No refs pushed.')
            if plan['conflicts']:
                raise Refused('backfill blocked; resolve every conflict listed in plan; ' + plan['collision_resolution'])
        elif args.command == 'publish-plan':
            plan = json.loads(Path(args.plan).read_text())
            if plan['schema'] != 1 or plan['conflicts']:
                raise Refused('plan has conflicts or an unsupported schema')
            if plan['heads'] != snapshot.remote_heads:
                raise Refused('origin heads changed; regenerate and review the plan')
            updates = []
            for row in plan['updates']:
                value = row['value']
                if row['ref'] != ref_for(valid_key(value['id'])):
                    raise Refused('plan ref/ID mismatch')
                current = snapshot.reservation(value['id'])
                if current == value:
                    continue
                if snapshot.refs.get(row['ref'], '') != row['old']:
                    raise Refused('reservation changed; regenerate plan')
                updates.append((row['ref'], row['old'], commit(root, value)))
            publish(root, updates)
            print('backfill reservations published and read back')
        else:
            # Enforce migration and all visible conflicts before taking any fresh number.
            kind = args.kind.upper()
            used = [int(k.split('-')[1]) for entries in snapshot.trees.values() for k in entries if k.startswith(kind + '-')]
            prefix = 'refs/reserved/iss/' if kind == 'ISS' else 'refs/bug-reservations/'
            used += [int(r.rsplit('/', 1)[1]) for r in snapshot.refs if r.startswith(prefix)]
            number = args.number if args.number is not None else max(used, default=0) + 1
            if number <= 0 or (kind == 'ISS' and number > 9999):
                raise Refused('number out of range')
            key = f'{kind}-{number:04d}' if kind == 'ISS' else f'{kind}-{number:03d}'
            if ref_for(key) in snapshot.refs or any(key in t for t in snapshot.trees.values()):
                raise Refused(f'{key} already taken')
            value = metadata(root, key, args.title, branch)
            publish(root, [(ref_for(key), '', commit(root, value))])
            print(key)


if __name__ == '__main__':
    try:
        main()
    except (Refused, OSError, ValueError, KeyError, TypeError) as error:
        print(f'ids: REFUSED: {error}', file=sys.stderr)
        sys.exit(1)
