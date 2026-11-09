#!/usr/bin/env python3
"""One online allocator for ISS, BUG and roadmap identities. See docs/ISSUE_IDS.md."""
import argparse
import datetime
import json
import functools
import os
from pathlib import Path
import re
import runpy
import socket
import subprocess
import sys
import tempfile
import uuid


read_roadmap_rows = runpy.run_path(str(Path(__file__).with_name("roadmap.py")))["read_rows"]

KINDS = {"iss": ("ISS", 4, "refs/reserved/iss/"),
         "bug": ("BUG", 3, "refs/bug-reservations/"),
         "roadmap": ("R", 4, "refs/reserved/roadmap/")}

read_issue_rows = runpy.run_path(str(Path(__file__).with_name("issues.py")))["read_rows"]

canonical_branch = runpy.run_path(str(Path(__file__).with_name("branch.py")))["canonical_branch"]


class Refused(Exception):
    pass


class Incomplete(Exception):
    pass


def sandbox_environment(root=None):
    # Equivalent to checks/lib/git_sandbox.sh, also for direct Python CLI use under a hook.
    clean = dict(os.environ)
    for name in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR'):
        clean.pop(name, None)
    result = subprocess.run(['git', 'rev-parse', '--local-env-vars'], env=clean,
                            text=True, capture_output=True, check=True)
    for name in result.stdout.splitlines():
        os.environ.pop(name, None)
    # Operator transport config remains active for live fetch/push. Offline controls
    # isolate global/system configuration explicitly in their own harness.


def git(root, *args, data=None, check=True):
    p = subprocess.run(['git', '-C', str(root), *args], input=data, text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if check and p.returncode:
        error = Incomplete if args[0] in {"fetch", "ls-remote", "cat-file", "show", "ls-tree", "rev-list"} else Refused
        raise error(f"git {' '.join(args[:3])}: {p.stderr.strip()}")
    return p


def ref_for(key):
    valid_key(key)
    kind, number = key.split('-')
    return next(prefix + number for tag, _, prefix in KINDS.values() if tag == kind)


def key_for_ref(ref):
    for tag, _, prefix in KINDS.values():
        if ref.startswith(prefix):
            key = valid_key(tag + '-' + ref[len(prefix):])
            if ref_for(key) == ref:
                return key
    raise Refused(f'invalid reservation ref: {ref}')


def valid_key(key):
    if not re.fullmatch(r'(ISS-\d{4}|BUG-\d{3,}|R-\d{4})', key):
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
        try:
            rows = read_issue_rows(read('ISSUES.tsv'), allow_legacy=True)
        except ValueError as error:
            raise Refused(f'ISSUES.tsv: {error}') from error
        for row in rows.values():
            add(row['id'], row['title'] if 'title' in row else issue_title(row['evidence']), 'ISSUES.tsv')
    if 'ROADMAP.md' in paths:
        try:
            for key, (subject, _) in read_roadmap_rows(read('ROADMAP.md')).items():
                add(key, subject, 'ROADMAP.md')
        except ValueError as error:
            raise Refused(str(error)) from error
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


def tree_records(repo, sha):
    return raw_tree_records(repo, sha)


@functools.lru_cache(maxsize=None)
def historical_keys(repo, sha):
    # Old history predates today's schema and may contain then-unfixed duplicates.
    # Presence, not present-day validity or wording, is all provenance needs.
    paths = git(repo, 'ls-tree', '-r', '--format=%(path)', sha).stdout.splitlines()
    keys = set()
    for path in ('ISSUES.tsv', 'ROADMAP.md'):
        if path in paths:
            text = git(repo, 'show', f'{sha}:{path}').stdout
            pattern = r'^(ISS-\d{4})\t' if path == 'ISSUES.tsv' else r'^\s*\|\s*(R-\d{4})\s*\|'
            keys.update(re.findall(pattern, text, re.M))
    for path in paths:
        match = re.fullmatch(r'(?:legacy/)?docs/bugs/(\d{3,})_[^/]+\.md', path)
        if match:
            keys.add('BUG-' + match[1])
    return keys


def introduction_content(repo, revision, key):
    """The first record payload is display evidence, never an identity."""
    paths = git(repo, 'ls-tree', '-r', '--format=%(path)', revision).stdout.splitlines()
    if key.startswith('ISS-'):
        text = git(repo, 'show', f'{revision}:ISSUES.tsv').stdout
        return '\n'.join(line for line in text.splitlines() if line.startswith(key + '\t'))
    if key.startswith('R-'):
        text = git(repo, 'show', f'{revision}:ROADMAP.md').stdout
        return '\n'.join(line for line in text.splitlines() if re.match(r'^\s*\|\s*' + key + r'\s*\|', line))
    number = key.split('-')[1]
    return '\n'.join(git(repo, 'show', f'{revision}:{path}').stdout for path in paths
                     if re.fullmatch(r'(?:legacy/)?docs/bugs/' + number + r'_[^/]+\.md', path))


def introduced_keys(present, parents, previously_present):
    """One state-transition definition, also used for the uncommitted child of HEAD.

    A missing parent starts an introduction unless another parent supplies the ID
    and the missing parent has never held it. That exception is ordinary merge
    inheritance; restoring an ID after deletion on either lineage is a new event.
    """
    inherited = set().union(*parents) if parents else set()
    return {key for key in present if key not in inherited or
            any(key not in parent and key in history
                for parent, history in zip(parents, previously_present))}


@functools.lru_cache(maxsize=None)
def is_ancestor(repo, ancestor, descendant):
    result = git(repo, 'merge-base', '--is-ancestor', ancestor, descendant, check=False)
    if result.returncode not in (0, 1):
        raise Incomplete('cannot establish introduction ancestry: ' + result.stderr.strip())
    return result.returncode == 0


@functools.lru_cache(maxsize=None)
def introductions(repo, sha):
    """ID -> introducing commit OIDs, including distinct restorations after deletion.

    Read commit and tree plumbing, never patch paths or user-configured diff text.
    Path selection only reduces the candidates; presence is always historical_keys.
    %P reports actual parents, unlike rev-list's path-simplified parent list.
    """
    history = git(repo, 'log', '--full-history', '--topo-order', '--reverse',
                  '--format=%H %P', '--no-patch', '--no-color', '--no-ext-diff',
                  '--no-textconv', '--no-renames', '--no-notes', '--no-show-signature', sha, '--',
                  'ISSUES.tsv', 'ROADMAP.md', 'docs/bugs', 'legacy/docs/bugs').stdout
    result = {}
    for line in history.splitlines():
        revision, *parents = line.split()
        present = historical_keys(repo, revision)
        parent_keys = [historical_keys(repo, parent) for parent in parents]
        # Only missing IDs need ancestry queries. A root on another, unrelated
        # lineage does not make an ordinary merge a restoration.
        histories = []
        for parent, keys in zip(parents, parent_keys):
            histories.append({key for key in present - keys if any(
                is_ancestor(repo, oid, parent)
                for oid in result.get(key, ()))})
        for key in introduced_keys(present, parent_keys, histories):
            result.setdefault(key, set()).add(revision)
    return result


def state_introductions(snapshot, sha):
    if sha != 'worktree':
        return introductions(snapshot.repo, sha)
    result = {key: set(roots) for key, roots in introductions(snapshot.repo, snapshot.local_head).items()}
    parent = historical_keys(snapshot.repo, snapshot.local_head)
    for key in introduced_keys(set(snapshot.local_records), [parent], [set(result)]):
        result.setdefault(key, set()).add('worktree:' + snapshot.local_head)
    return result


def identity_error(snapshot, key, sha, branch):
    """Main owns existing introductions; reservations own new branch introductions."""
    roots = state_introductions(snapshot, sha).get(key, set())
    main_roots = introductions(snapshot.repo, snapshot.remote_heads['main']).get(key, set())
    if not roots:
        return f'{key}: no introducing commit for {branch} at {sha}'
    if main_roots:
        if not roots or not roots <= main_roots:
            return f'{key}: independent introduction conflicts with origin/main:{snapshot.trees["main"].get(key, ("", "history"))[1]} (main {sorted(main_roots)}, branch {sorted(roots)})'
    elif len(roots) > 1:
        return f'{key}: multiple independent introductions {sorted(roots)}'
    else:
        reservation = snapshot.reservation(key)
        if reservation and 'schema' in reservation:
            owner = reservation['branch']
            owner_sha = snapshot.remote_heads.get(owner)
            inherited = owner_sha and roots and roots <= introductions(snapshot.repo, owner_sha).get(key, set())
            if branch != owner and not inherited:
                return f'{key}: new introduction is not owned by reservation {ref_for(key)} on {owner}'
    return None


def work_records(root):
    paths = git(root, 'ls-files', '--cached', '--others', '--exclude-standard').stdout.splitlines()
    return records(lambda p: (Path(root) / p).read_text(), sorted(p for p in set(paths) if (Path(root) / p).is_file()))


class Snapshot:
    """Fetch into a private bare repo: never updates the caller's refs or notes."""
    def __init__(self, root, directory):
        self.root = root
        self.repo = Path(directory) / 'snapshot.git'
        git(directory, '-c', 'core.hooksPath=/dev/null', 'init', '--bare', '--template=', str(self.repo))
        git(self.repo, 'config', 'core.hooksPath', '/dev/null')
        git(self.repo, 'config', 'commit.gpgsign', 'false')
        # Carry effective repo-local authentication/transport into the private fetch,
        # without copying repository-location, hooks or signing settings. Global and
        # system transport settings remain inherited normally. Never print credentials.
        config = git(root, 'config', '--show-scope', '--null', '--list').stdout.split('\0')
        for scope, entry in zip(config[0::2], config[1::2]):
            if scope not in ('local', 'worktree') or '\n' not in entry:
                continue
            key, value = entry.split('\n', 1)
            if key.startswith(('credential.', 'http.', 'https.', 'url.', 'protocol.', 'remote.origin.')) or key in ('core.sshcommand', 'ssh.variant'):
                git(self.repo, 'config', '--add', key, value)
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
        self.trees = {}
        self.tree_errors = {}
        for branch, sha in self.heads.items():
            try:
                self.trees[branch] = tree_records(self.repo, sha)
            except (Refused, KeyError, ValueError) as error:
                self.tree_errors[branch] = str(error)

        # Only main and reservation refs own numbers. Unreserved drafts are audit
        # material, never an allocation lock or a source of numbering authority.
        self.occupied = set(self.trees.get('main', {}))
        self.remote_heads = dict(self.heads)
        git(self.repo, 'fetch', '--quiet', '--no-tags', str(root), 'HEAD:refs/local/head')
        self.local_head = git(root, 'rev-parse', 'HEAD').stdout.strip()
        # Every operation reads the same complete namespace, including allocation and
        # planning. A poisoned reservation cannot silently become numbering authority.
        for ref in self.refs:
            if ref.startswith(('refs/reserved/', 'refs/bug-reservations/')):
                self.reservation(key_for_ref(ref))

    @functools.lru_cache(maxsize=None)
    def reservation(self, key):
        ref = ref_for(key)
        oid = self.refs.get(ref)
        if oid is None:
            return None
        return reservation_commit(self.repo, key, oid, allow_legacy=key.startswith('BUG-'))


def validate_metadata(value, key):
    required = {'schema', 'id', 'title', 'branch', 'who', 'when', 'nonce'}
    if not isinstance(value, dict) or not required <= value.keys() or type(value['schema']) is not int or value['schema'] != 1:
        raise Refused('malformed reservation schema')
    if value['id'] != valid_key(key) or any(not isinstance(value[k], str) or not value[k].strip()
                                           for k in required - {'schema'}):
        raise Refused('invalid reservation identity or metadata')
    if any(c in value['title'] for c in '\n\r\t'):
        raise Refused('title must be one nonempty line without tabs')
    if canonical_branch(value['branch']) != value['branch']:
        raise Refused('reservation branch must be canonical main or wip/NAME')
    if 'legacy_oid' in value and (not key.startswith('BUG-') or
                                 not isinstance(value['legacy_oid'], str) or
                                 not re.fullmatch(r'[0-9a-f]{40}|[0-9a-f]{64}', value['legacy_oid'])):
        raise Refused('legacy_oid is only valid for a legacy bug import')
    return value


def reservation_commit(repo, key, oid, allow_legacy=False):
    """One validator for fetched metadata, pre-push verification and legacy imports."""
    if git(repo, 'cat-file', '-t', oid).stdout.strip() != 'commit':
        raise Refused('reservation is not a commit')
    if git(repo, 'rev-list', '--parents', '-n', '1', oid).stdout.split() != [oid] or git(repo, 'ls-tree', oid).stdout:
        raise Refused('reservation must be a parentless empty-tree commit')
    try:
        value = json.loads(git(repo, 'show', '-s', '--format=%B', oid).stdout)
    except ValueError:
        if allow_legacy and key.startswith('BUG-'):
            return {'legacy_oid': oid}
        raise Refused('non-JSON reservation is not a schema-1 identity')
    return validate_metadata(value, key)


def metadata(root, key, title, branch):
    value = dict(schema=1, id=valid_key(key), title=title, branch=canonical_branch(branch),
                 who=(git(root, 'config', 'user.email', check=False).stdout.strip() or
                      os.environ.get('GIT_AUTHOR_EMAIL', '')) + '@' + socket.gethostname(),
                 when=datetime.datetime.now(datetime.timezone.utc).isoformat(), nonce=str(uuid.uuid4()))
    return validate_metadata(value, key)


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


def check_entries(snapshot, entries, label, sha=None, branch=None):
    sha = sha or snapshot.remote_heads.get(label, snapshot.local_head)
    branch = branch or label
    for key in entries:
        error = identity_error(snapshot, key, sha, branch)
        if error:
            raise Refused(f'{label}: {error}')
        reservation = snapshot.reservation(key)
        if not reservation or 'schema' not in reservation:
            raise Refused(f'{label}: {key} has no schema-1 reservation at {ref_for(key)}; run backfill')


def check_snapshot(snapshot):
    # Operator audit only; ordinary gates never couple unrelated draft branches.
    if 'main' in snapshot.tree_errors:
        raise Incomplete('origin/main: ' + snapshot.tree_errors['main'])
    if snapshot.tree_errors:
        raise Refused('remote record audit: ' + repr(snapshot.tree_errors))
    for branch, entries in snapshot.trees.items():
        check_entries(snapshot, entries, branch)


def check_change(root, snapshot):
    # Activation requires the complete authoritative backfill; no historical exemptions.
    if 'main' in snapshot.tree_errors:
        raise Incomplete('origin/main: ' + snapshot.tree_errors['main'])
    branch = git(root, 'symbolic-ref', '--short', 'HEAD', check=False).stdout.strip()
    branch = branch or git(root, 'config', 'id.branch', check=False).stdout.strip()
    # Detached candidates can inherit recorded introductions without claiming new ones.
    branch = canonical_branch(branch) if branch else 'detached'
    check_entries(snapshot, snapshot.trees['main'], 'origin/main', snapshot.remote_heads['main'], 'main')
    check_entries(snapshot, tree_records(snapshot.repo, snapshot.local_head), 'HEAD', snapshot.local_head, branch)
    snapshot.local_records = work_records(root)
    check_entries(snapshot, snapshot.local_records, 'worktree', 'worktree', branch)


def attributed_branch(snapshot, branch):
    if branch:
        return canonical_branch(branch)
    matches = [b for b, sha in snapshot.remote_heads.items()
               if b.startswith('wip/') and sha == snapshot.local_head]
    if len(matches) != 1:
        raise Refused('detached allocation requires id.branch or exactly one matching wip head')
    return matches[0]


def make_plan(root, snapshot, local, branch, scope='all'):
    # Bootstrap authoritative records without forcing unrelated drafts to migrate together.
    trees = ({'main': snapshot.trees.get('main', {})} if scope == 'authoritative'
             else dict(snapshot.trees))
    branch = attributed_branch(snapshot, branch)
    # Replace only the writer's draft. Local main must never mask origin/main.
    if branch != 'main':
        trees.pop(branch, None)
    snapshot.local_records = local
    inventory = [*trees.items(), (branch, local)]
    uses = {}
    for name, entries in inventory:
        for key, (title, _) in entries.items():
            uses.setdefault(key, []).append((name, title, 'worktree' if entries is local else snapshot.remote_heads[name]))
    conflicts = [f'{b}: {error}' for b, error in snapshot.tree_errors.items()
                 if scope == 'all' or b == 'main']
    updates = []
    resolutions = []
    evidence = {}
    for key, rows in sorted(uses.items()):
        errors = [f'{name}: {error}' for name, _, sha in rows
                  if (error := identity_error(snapshot, key, sha, name))]
        roots = set().union(*(state_introductions(snapshot, sha).get(key, set()) for _, _, sha in rows))
        if (not snapshot.reservation(key) and
                not introductions(snapshot.repo, snapshot.remote_heads['main']).get(key) and
                len(roots) > 1):
            errors.append(f'{key}: independent unreserved introductions {sorted(roots)}')
        if any(errors):
            evidence[key] = [dict(branch=name, oid=oid,
                                 first_content=(local[key][0] if oid.startswith('worktree:') else
                                                introduction_content(snapshot.repo, oid, key)))
                             for name, _, sha in rows
                             for oid in sorted(state_introductions(snapshot, sha).get(key, set()))]
            names = ', '.join(sorted({name for name, _, _ in rows if name != 'main'}))
            resolutions.append(f'{key} ({names}): compare the listed introducing commits and first records. '
                               'For an already landed copy, reconcile the branch onto origin/main so it inherits '
                               'the main introduction, or retire its stale advertised ref. For a distinct task, '
                               'reserve a new ID through the coordinator and renumber that branch before retrying backfill.')
            conflicts.append(f'{key}: ' + repr([(name, title) for name, title, _ in rows]) + '; ' + '; '.join(sorted(set(filter(None, errors)))))
            continue
        owner, title, _ = min(rows, key=lambda row: (row[0] != 'main', row[0]))
        old = snapshot.reservation(key)
        if old and 'schema' in old:
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
    return dict(schema=1, scope=scope, heads=snapshot.remote_heads, updates=updates, conflicts=conflicts,
                introduction_evidence=evidence, collision_resolution='\n'.join(resolutions))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', default='.')
    sub = parser.add_subparsers(dest='command', required=True)
    reserve = sub.add_parser('reserve')
    reserve.add_argument('kind', choices=list(KINDS))
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
    backfill.add_argument('--scope', choices=['all', 'authoritative'], default='all',
                          help='all advertised drafts, or only origin/main and this worktree')
    apply = sub.add_parser('publish-plan')
    apply.add_argument('plan')
    args = parser.parse_args()
    root = Path(args.root).resolve()
    sandbox_environment(root)
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
        # The ref determines the identity; never trust payload fields before validation.
        key = key_for_ref(ref)
        if ref != ref_for(valid_key(key)):
            raise Refused('invalid reservation ref')
        value = reservation_commit(root, key, oid)
        if set(old) != {'0'}:
            if not key.startswith('BUG-') or value.get('legacy_oid') != old:
                raise Refused('reservations are immutable; only explicit legacy bug import is allowed')
            previous = reservation_commit(root, key, old, allow_legacy=True)
            if 'schema' in previous:
                raise Refused('cannot replace a schema reservation')
        elif 'legacy_oid' in value:
            raise Refused('legacy import requires the existing legacy ref')
        return
    with tempfile.TemporaryDirectory(prefix='concrete-ids-') as temp:
        snapshot = Snapshot(root, temp)
        if args.command in ('reserve', 'backfill'):
            branch = attributed_branch(snapshot, branch)
        if args.command in ('check', 'audit-wip'):
            # Validate reservation namespace integrity even when there are no changed records.
            for ref in snapshot.refs:
                if ref.startswith(('refs/reserved/', 'refs/bug-reservations/')):
                    key = key_for_ref(ref)
                    valid_key(key)
                    if ref != ref_for(key):
                        raise Refused(f'malformed reservation ref: {ref}')
                    if snapshot.reservation(key).get('schema') != 1:
                        raise Refused(f'{ref}: legacy reservation requires backfill before enforcement')
            if args.command == 'audit-wip':
                check_snapshot(snapshot)
            else:
                check_change(root, snapshot)
            print('ID reservations: PASS')
        elif args.command == 'backfill':
            plan = make_plan(root, snapshot, work_records(root), branch, args.scope)
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
                validate_metadata(value, value['id'])
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
            # Refuse an unreadable authoritative inventory; drafts cannot claim numbers.
            if 'main' in snapshot.tree_errors:
                raise Incomplete('origin/main: ' + snapshot.tree_errors['main'])
            kind, width, prefix = KINDS[args.kind]
            used = [int(k.split('-')[1]) for k in snapshot.occupied if k.startswith(kind + '-')]
            used += [int(r.rsplit('/', 1)[1]) for r in snapshot.refs if r.startswith(prefix)]
            number = args.number if args.number is not None else max(used, default=0) + 1
            if number <= 0 or (width == 4 and number > 9999):
                raise Refused('number out of range')
            key = f'{kind}-{number:0{width}d}'
            if ref_for(key) in snapshot.refs or key in snapshot.occupied:
                raise Refused(f'{key} already taken at {ref_for(key)} or origin/main')
            value = metadata(root, key, args.title, branch)
            publish(root, [(ref_for(key), '', commit(root, value))])
            print(key)


if __name__ == '__main__':
    try:
        main()
    except Incomplete as error:
        print(f'ids: INCOMPLETE: {error}', file=sys.stderr)
        sys.exit(2)
    except (Refused, OSError, ValueError, KeyError, TypeError) as error:
        print(f'ids: REFUSED: {error}', file=sys.stderr)
        sys.exit(1)
