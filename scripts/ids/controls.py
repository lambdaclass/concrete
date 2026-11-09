#!/usr/bin/env python3
"""Exercise the real CLI and pre-push tier only in throwaway repositories."""
import concurrent.futures
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import sys

sys.dont_write_bytecode = True

SOURCE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('allocator', SOURCE / 'allocate.py')
a = importlib.util.module_from_spec(spec)
spec.loader.exec_module(a)
a.sandbox_environment()
os.environ.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_SYSTEM="/dev/null", GIT_CONFIG_NOSYSTEM="1")


def run(root, *args, expect=0):
    result = subprocess.run(['python3', '-I', str(SOURCE / 'allocate.py'), '--root', str(root), *args],
                            text=True, capture_output=True)
    if (result.returncode == 0) != (expect == 0):
        raise AssertionError(f'{args}: exit {result.returncode}\n{result.stdout}{result.stderr}')
    return result


def save(root, key, title):
    (root / 'ISSUES.tsv').write_text('id\ttitle\tdate\tarea\tevidence\towner\tseverity\tclosure_condition\tstatus\n' +
                                   f'{key}\t{title}\t2026-11-08\tci\tevidence\tR-0542\thigh\tfixed\topen\n')
    a.git(root, 'add', 'ISSUES.tsv')
    a.git(root, 'commit', '-qm', title + ' on ' + a.git(root, 'branch', '--show-current').stdout.strip())


def main():
    with tempfile.TemporaryDirectory(prefix='id-controls-') as directory:
        base = Path(directory)
        remote = base / 'origin.git'
        a.git(base, 'init', '--bare', str(remote))
        a.git(remote, 'config', 'user.name', 'Remote')
        a.git(remote, 'config', 'user.email', 'remote@example.invalid')
        writer = base / 'writer'
        a.git(base, 'init', '-b', 'main', str(writer))
        a.git(writer, 'config', 'user.name', 'Control')
        a.git(writer, 'config', 'user.email', 'control@example.invalid')
        a.git(writer, 'commit', '--allow-empty', '-qm', 'base')
        a.git(writer, 'remote', 'add', 'origin', str(remote))
        a.git(writer, 'push', '-q', 'origin', 'main')
        # Use the real hook for every allocator push, including concurrent reservations.
        (writer / 'scripts/ids').mkdir(parents=True)
        shutil.copy(SOURCE / 'allocate.py', writer / 'scripts/ids/allocate.py')
        shutil.copy(SOURCE / 'issues.py', writer / 'scripts/ids/issues.py')
        shutil.copy(SOURCE / 'branch.py', writer / 'scripts/ids/branch.py')
        hooks = writer / '.githooks'
        hooks.mkdir()
        shutil.copy(SOURCE.parents[1] / '.githooks/pre-push', hooks / 'pre-push')
        (hooks / 'pre-push').chmod(0o755)
        a.git(writer, 'config', 'core.hooksPath', str(hooks))
        a.git(writer, 'switch', '-qc', 'wip/a')
        run(writer, 'reserve', 'iss', 'first', '--number', '1')
        save(writer, 'ISS-0001', 'first')
        a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/a')
        run(writer, 'check')
        print('PASS: reserved ID on its branch, through real hook')
        # Test both metadata namespaces alone and in mixed pushes; full-tier detection
        # uses a linked worktree refusal, so these probes never run real gates.
        wt = base / 'tier-worktree'
        a.git(writer, 'worktree', 'add', '--detach', str(wt))
        (wt / 'scripts/ids').mkdir(parents=True, exist_ok=True)
        shutil.copy(SOURCE / 'allocate.py', wt / 'scripts/ids/allocate.py')
        shutil.copy(SOURCE / 'issues.py', wt / 'scripts/ids/issues.py')
        shutil.copy(SOURCE / 'branch.py', wt / 'scripts/ids/branch.py')
        head = a.git(writer, 'rev-parse', 'HEAD').stdout.strip()
        zero = '0' * 40
        for key in ('ISS-0088', 'BUG-088'):
            oid = a.commit(writer, a.metadata(writer, key, 'tier control', 'wip/a'))
            line = f'refs/x {oid} {a.ref_for(key)} {zero}\n'
            for extra, tier, rc in [('', 'metadata push', 0),
                                    (f'refs/x {head} refs/heads/wip/tier {zero}\n', 'wip/* backup push', 0),
                                    (f'refs/x {head} refs/heads/ordinary {zero}\n', 'is a linked worktree', 1)]:
                result = subprocess.run(['bash', str(hooks / 'pre-push'), 'origin', str(remote)],
                                        cwd=wt, input=line + extra, capture_output=True, text=True)
                assert result.returncode == rc and tier in result.stdout + result.stderr, result
            result = run(writer, 'verify-ref', a.ref_for(key), zero, oid, expect=1)
            assert 'may not be deleted' in result.stderr
        a.git(writer, 'worktree', 'remove', '--force', str(wt))
        print('PASS: ISS/bug metadata, wip mixed, full mixed tiers and explicit deletion refusal')

        # Evidence updates preserve identity.
        p = writer / 'ISSUES.tsv'
        p.write_text(p.read_text().replace('\tevidence\tR-', '\tupdated evidence\tR-'))
        run(writer, 'check')
        p.write_text(p.read_text().replace('\tfirst\t', '\twrong title\t'))
        run(writer, 'check', expect=1)
        print('PASS: title mismatch refused; evidence remains editable')
        a.git(writer, 'restore', 'ISSUES.tsv')
        a.git(writer, 'switch', '-qc', 'wip/duplicate', 'main')
        save(writer, 'ISS-0001', 'first')
        run(writer, 'check')
        print('PASS: rebased/cherry-picked same identity needs no shared introduction ancestry')
        save(writer, 'ISS-0001', 'different identity')
        a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/duplicate')
        run(writer, 'check', expect=1)
        print('PASS: different titles for the same ID refused')
        conflict_plan = base / 'conflict-plan.json'
        before = a.git(writer, 'ls-remote', 'origin').stdout
        run(writer, 'backfill', '--output', str(conflict_plan), expect=1)
        assert json.loads(conflict_plan.read_text())['conflicts']
        run(writer, 'publish-plan', str(conflict_plan), expect=1)
        assert a.git(writer, 'ls-remote', 'origin').stdout == before
        print('PASS: collision backfill records conflict and refuses publication without moving refs')
        a.git(writer, 'switch', '-qc', 'wip/legacy-evolved', 'main')
        (writer / 'ISSUES.tsv').write_text('id\tdate\tarea\tevidence\towner\tstatus\n'
            'ISS-0001\t2026-11-08\tci\trewritten evidence after squash\tR-0542\topen\n')
        a.git(writer, 'add', 'ISSUES.tsv')
        a.git(writer, 'commit', '-qm', 'legacy evolving identity')
        a.git(writer, 'push', '-q', 'origin', ':refs/heads/wip/duplicate')
        a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/legacy-evolved')
        a.git(writer, 'switch', '-q', 'wip/a')
        run(writer, 'backfill', '--output', str(base / 'evolved-plan.json'), expect=1)
        assert json.loads((base / 'evolved-plan.json').read_text())['conflicts']
        print('PASS: squashed legacy title ambiguity requires reconciliation')
        for suffix, evidence in [('one', 'earlier mutable evidence'), ('two', 'edited mutable evidence')]:
            a.git(writer, 'switch', '-qc', 'wip/legacy-' + suffix, 'main')
            (writer / 'ISSUES.tsv').write_text('id\tdate\tarea\tevidence\towner\tstatus\n'
                f'ISS-0084\t2026-11-08\tci\t{evidence}\tR-0542\topen\n')
            a.git(writer, 'add', 'ISSUES.tsv')
            a.git(writer, 'commit', '-qm', 'legacy-only evolving identity')
            a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/legacy-' + suffix)
        a.git(writer, 'switch', '-q', 'wip/a')
        legacy_plan = base / 'legacy-only-plan.json'
        run(writer, 'backfill', '--output', str(legacy_plan), expect=1)
        assert any('ISS-0084' in conflict for conflict in json.loads(legacy_plan.read_text())['conflicts'])
        for suffix in ('one', 'two'):
            a.git(writer, 'push', '-q', 'origin', ':refs/heads/wip/legacy-' + suffix)
        print('PASS: distinct legacy introductions are conflicts, never majority votes')

        a.git(writer, 'push', '-q', 'origin', ':refs/heads/wip/legacy-evolved')


        a.git(writer, 'push', '-q', 'origin', ':refs/heads/wip/duplicate')
        a.git(writer, 'switch', '-q', 'wip/a')
        a.git(writer, 'switch', '-qc', 'wip/inherited')
        run(writer, 'check')
        print('PASS: inherited IDs pass on descendant branches')
        a.git(writer, 'checkout', '--detach', '-q')
        run(writer, 'check')
        a.git(writer, 'switch', '-q', 'wip/inherited')
        (writer / 'ISSUES.tsv').unlink()
        run(writer, 'check')
        a.git(writer, 'restore', 'ISSUES.tsv')
        print('PASS: detached inherited identity and deleted ledger')
        save(writer, 'ISS-0002', 'unreserved')
        run(writer, 'check', expect=1)
        print('PASS: unreserved ID refused')
        a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/stale')
        a.git(writer, 'switch', '-q', 'wip/a')
        run(writer, 'check')
        # The stale head cannot block allocations either.
        run(writer, 'reserve', 'iss', 'unrelated allocation', '--number', '80')
        a.git(writer, 'push', '-q', 'origin', ':refs/heads/wip/stale')
        # A malformed draft is audit material, not a global allocation lock.
        a.git(writer, 'switch', '-qc', 'wip/malformed')
        malformed = writer / 'docs/bugs/42_bad.md'
        malformed.parent.mkdir(parents=True, exist_ok=True)
        malformed.write_text('# Bad draft')
        a.git(writer, 'add', str(malformed))
        a.git(writer, 'commit', '-qm', 'malformed draft')
        a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/malformed')
        a.git(writer, 'switch', '-qc', 'wip/clean', 'main')
        run(writer, 'check')
        run(writer, 'reserve', 'iss', 'another unrelated allocation', '--number', '81')
        run(writer, 'audit-wip', expect=1)
        a.git(writer, 'push', '-q', 'origin', ':refs/heads/wip/malformed')
        print('PASS: unrelated stale/malformed branches cannot poison checks or reservations')
        # Recreate clean branch so the race is about reservation, not local filing.
        a.git(writer, 'switch', '-qc', 'wip/race', 'main')
        clone = base / 'other'
        a.git(base, 'clone', '-q', str(remote), str(clone))
        a.git(clone, 'config', 'user.name', 'Other')
        a.git(clone, 'config', 'user.email', 'other@example.invalid')
        a.git(clone, 'switch', '-qc', 'wip/race-other')
        def reserve(root):
            return subprocess.run(['python3', '-I', str(SOURCE / 'allocate.py'), '--root', str(root),
                                   'reserve', 'iss', 'race', '--number', '2'], capture_output=True, text=True)
        with concurrent.futures.ThreadPoolExecutor(2) as pool:
            results = list(pool.map(reserve, [writer, clone]))
        assert sorted(p.returncode for p in results) == [0, 1], results
        assert len(a.git(writer, 'ls-remote', 'origin', 'refs/reserved/iss/0002').stdout.splitlines()) == 1
        print('PASS: concurrent double reservation from separate clones has exactly one winner')
        a.git(writer, 'remote', 'set-url', 'origin', str(base / 'missing'))
        run(writer, 'reserve', 'bug', 'offline', expect=1)
        run(writer, 'check', expect=1)
        a.git(writer, 'remote', 'set-url', 'origin', str(remote))
        print('PASS: offline allocation and validation refused')
        # Import an old-style reservation with the CAS backfill; unused numbers remain burned.
        legacy = a.git(remote, 'commit-tree', a.git(remote, 'hash-object', '-t', 'tree', '--stdin', data='').stdout.strip(),
                       data='bug 300 reserved: legacy\n').stdout.strip()
        a.git(remote, 'update-ref', 'refs/bug-reservations/300', legacy)
        frozen = writer / 'legacy/docs/bugs'
        frozen.mkdir(parents=True)
        (frozen / '300_legacy.md').write_text('# Bug 300: legacy bug\n')
        a.git(writer, 'add', 'legacy/docs/bugs/300_legacy.md')
        a.git(writer, 'commit', '-qm', 'existing frozen bug')
        a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/race')
        run(writer, 'audit-wip', expect=1)
        plan = base / 'plan.json'
        run(writer, 'backfill', '--output', str(plan))
        run(writer, 'publish-plan', str(plan))
        run(writer, 'check')
        run(writer, 'publish-plan', str(plan))
        run(writer, 'reserve', 'bug', 'new bug')
        assert a.git(writer, 'ls-remote', 'origin', 'refs/bug-reservations/301').stdout
        print('PASS: legacy reservations imported idempotently; bug allocator skips legacy numbers')
        # File duplicate bug records; the same parser covers frozen and future root layouts.
        bugs = writer / 'docs/bugs'
        bugs.mkdir(parents=True)
        (bugs / '301_a.md').write_text('# Bug 301: new bug\n')
        (bugs / '301_b.md').write_text('# Bug 301: another\n')
        run(writer, 'check', expect=1)
        print('PASS: duplicate bug records refused')
        for path in bugs.glob('*.md'):
            path.unlink()
        for name, text in [('42_bad.md', '# Bug 42: bad'), ('042-bad.md', '# Bug 042: bad'),
                           ('042_bad.md', '# Something else')]:
            path = bugs / name
            path.write_text(text)
            run(writer, 'check', expect=1)
            path.unlink()
        (frozen / '300_legacy.md').unlink()
        run(writer, 'check')
        a.git(writer, 'restore', 'legacy/docs/bugs/300_legacy.md')
        print('PASS: malformed paths/headings refused; deleted tracked bug handled')
        # A malformed reservation must not receive the hook's metadata exemption.
        head = a.git(writer, 'rev-parse', 'HEAD').stdout.strip()
        result = subprocess.run(['bash', str(hooks / 'pre-push'), 'origin', str(remote)], cwd=writer,
                                input=f'refs/x {head} refs/reserved/iss/9999 {"0" * 40}\n',
                                capture_output=True, text=True)
        assert result.returncode != 0
        print('PASS: hook refuses source commits as reservations')
        # Validate transport ambiguity against actual local pushes, not matched output text.
        real_git = a.git
        value = a.metadata(writer, 'ISS-0090', 'lost response', 'wip/race')
        oid = a.commit(writer, value)
        def lost_response(root, *args, **kwargs):
            result = real_git(root, *args, **kwargs)
            if args and args[0] == 'push':
                return subprocess.CompletedProcess(result.args, 1, result.stdout, 'lost response')
            return result
        a.git = lost_response
        try:
            a.publish(writer, [(a.ref_for('ISS-0090'), '', oid)])
        finally:
            a.git = real_git
        print('PASS: failed push response is resolved by exact successful read-back')
        attempts = writer / '.git/id-reservation-attempts'
        receipts = list(attempts.glob('*.json'))
        assert receipts and any(oid in json.loads(p.read_text())['commits'] for p in receipts)
        print('PASS: recovery receipt preserves exact commit bytes before publication')
        first = a.commit(writer, a.metadata(writer, 'ISS-0091', 'partial A', 'wip/race'))
        second = a.commit(writer, a.metadata(writer, 'ISS-0092', 'partial B', 'wip/race'))
        def partial(root, *args, **kwargs):
            if args and args[0] == 'push':
                real_git(root, 'push', '--porcelain', '--force-with-lease=refs/reserved/iss/0091:',
                         'origin', first + ':refs/reserved/iss/0091')
                return subprocess.CompletedProcess([], 1, '', 'simulated broken server transaction')
            return real_git(root, *args, **kwargs)
        a.git = partial
        try:
            try:
                a.publish(writer, [(a.ref_for('ISS-0091'), '', first), (a.ref_for('ISS-0092'), '', second)])
            except a.Refused as error:
                assert 'partial reservation' in str(error)
            else:
                raise AssertionError('partial publication accepted')
        finally:
            a.git = real_git
        print('PASS: partial transaction is refused, with recovery receipt retained')
        for path in bugs.glob('*.md'):
            path.unlink()
        a.git(writer, 'merge', '--no-ff', '-m', 'integrate first ID', 'wip/a')
        a.git(remote, 'fetch', '-q', str(writer), 'HEAD:refs/integration/control')
        a.git(remote, 'update-ref', 'refs/heads/main', a.git(writer, 'rev-parse', 'HEAD').stdout.strip())
        a.git(remote, 'update-ref', '-d', 'refs/heads/wip/a')
        run(writer, 'check')
        a.git(writer, 'checkout', '--detach', '-q')
        run(writer, 'check')
        print('PASS: landed IDs survive owner branch deletion and detached integration')

    result = subprocess.run(['python3', '-I', str(SOURCE / 'round3_controls.py')])
    assert result.returncode == 0, 'round 3 regression controls failed'
    result = subprocess.run(['python3', '-I', '-B', str(SOURCE / 'round4_controls.py')])
    assert result.returncode == 0, 'round 4 regression controls failed'
    print('ID reservation controls: PASS')


if __name__ == '__main__':
    main()
