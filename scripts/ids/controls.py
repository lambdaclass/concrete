#!/usr/bin/env python3
"""Exercise the real CLI and pre-push tier only in throwaway repositories."""
import concurrent.futures
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile

SOURCE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('allocator', SOURCE / 'allocate.py')
a = importlib.util.module_from_spec(spec)
spec.loader.exec_module(a)
a.sandbox_environment()


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
        a.git(writer, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/duplicate')
        run(writer, 'check', expect=1)
        print('PASS: two independent branches using same ID/title refused')
        a.git(writer, 'push', '-q', 'origin', ':refs/heads/wip/duplicate')
        a.git(writer, 'switch', '-q', 'wip/a')
        a.git(writer, 'switch', '-qc', 'wip/inherited')
        run(writer, 'check')
        print('PASS: inherited IDs pass on descendant branches')
        save(writer, 'ISS-0002', 'unreserved')
        run(writer, 'check', expect=1)
        print('PASS: unreserved ID refused')
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
        legacy = a.git(writer, 'commit-tree', a.git(writer, 'hash-object', '-t', 'tree', '--stdin', data='').stdout.strip(),
                       data='bug 300 reserved: legacy\n').stdout.strip()
        a.git(remote, 'fetch', '-q', str(writer), f'{legacy}:refs/bug-reservations/300')
        plan = base / 'plan.json'
        run(writer, 'backfill', '--output', str(plan))
        run(writer, 'publish-plan', str(plan))
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
        # A malformed reservation must not receive the hook's metadata exemption.
        head = a.git(writer, 'rev-parse', 'HEAD').stdout.strip()
        result = subprocess.run(['bash', str(hooks / 'pre-push'), 'origin', str(remote)], cwd=writer,
                                input=f'refs/x {head} refs/reserved/iss/9999 {"0" * 40}\n',
                                capture_output=True, text=True)
        assert result.returncode != 0
        print('PASS: hook refuses source commits as reservations')
    print('ID reservation controls: PASS')


if __name__ == '__main__':
    main()
