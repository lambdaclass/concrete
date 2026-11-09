#!/usr/bin/env python3
"""Project identity regressions, against an optional before/after source root."""
import sys
sys.dont_write_bytecode = True
import concurrent.futures
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile

SOURCE = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('allocator', SOURCE / 'scripts/ids/allocate.py')
a = importlib.util.module_from_spec(spec)
spec.loader.exec_module(a)
a.sandbox_environment()
os.environ.update(GIT_CONFIG_GLOBAL='/dev/null', GIT_CONFIG_SYSTEM='/dev/null', GIT_CONFIG_NOSYSTEM='1')
HEADER = 'id\ttitle\tdate\tarea\tevidence\towner\tseverity\tclosure_condition\tstatus\n'


def cli(root, *args):
    return subprocess.run(['python3', '-I', '-B', str(SOURCE / 'scripts/ids/allocate.py'), '--root', str(root), *args], capture_output=True, text=True)


def ok(p):
    assert p.returncode == 0, (p.returncode, p.stdout, p.stderr)


def save(root, kind, title):
    path, text = ('ISSUES.tsv', HEADER + f'ISS-0020\t{title}\t2026-11-09\tci\tevidence\tR-0542\thigh\tfixed\topen\n') if kind == 'iss' else ('ROADMAP.md', f'| R-0620 | {title} | mutable obligation | — |\n')
    (root / path).write_text(text)
    a.git(root, 'add', path)
    a.git(root, 'commit', '-qm', title)


def collision(root, kind):
    base = a.git(root, 'rev-parse', 'HEAD').stdout.strip()
    key, number = ('ISS-0020', '20') if kind == 'iss' else ('R-0620', '620')
    ok(cli(root, 'reserve', kind, 'main identity', '--number', number))
    save(root, kind, 'main identity')
    a.git(root, 'push', '-q', 'origin', 'main')
    a.git(root, 'switch', '-qc', 'stale-writer', base)
    save(root, kind, 'colliding identity')
    for phase in ('branch', 'merge target'):
        p = cli(root, 'check')
        assert p.returncode == 1 and key in p.stderr and 'origin/main:' in p.stderr, (phase, p.returncode, p.stderr)
        if phase == 'branch':
            a.git(root, 'switch', '-qc', 'candidate', 'main')
            a.git(root, 'merge', '--no-ff', '-X', 'theirs', '-m', 'integrate stale branch', 'stale-writer')
    # Edits inherit the introduction, regardless of title wording or row line number.
    a.git(root, 'switch', '-qc', 'positive', 'main')
    save(root, kind, 'reworded existing item')
    ok(cli(root, 'check'))
    if kind == 'roadmap':
        p = root / 'ROADMAP.md'
        p.write_text('new prose\n' + p.read_text())
        ok(cli(root, 'check'))
    a.git(root, 'restore', '.')
    # Identical wording cannot launder an independent introduction either.
    a.git(root, 'switch', '-qc', 'same-text-reuse', base)
    save(root, kind, 'main identity')
    a.git(root, 'commit', '--amend', '-qm', 'independent identical wording introduction')
    assert cli(root, 'check').returncode == 1


def race(root):
    other = root.parent / 'other'
    a.git(root.parent, 'clone', '-q', a.git(root, 'remote', 'get-url', 'origin').stdout.strip(), str(other))
    a.git(other, 'config', 'user.name', 'Other')
    a.git(other, 'config', 'user.email', 'other@example.invalid')
    def reserve(writer):
        return cli(writer, 'reserve', 'roadmap', 'race task', '--number', '620')
    with concurrent.futures.ThreadPoolExecutor(2) as pool:
        results = list(pool.map(reserve, (root, other)))
    assert sorted(p.returncode for p in results) == [0, 1], [(p.returncode, p.stderr) for p in results]
    assert len(a.git(root, 'ls-remote', 'origin', 'refs/reserved/roadmap/0620').stdout.splitlines()) == 1
    save(root, 'roadmap', 'race task')
    ok(cli(root, 'check'))
    p = cli(root, 'reserve', 'roadmap', 'next task')
    ok(p)
    assert p.stdout.strip() == 'R-0621', p.stdout


def backfill(root):
    save(root, 'roadmap', 'historical task')
    a.git(root, 'push', '-q', 'origin', 'main')
    assert cli(root, 'check').returncode == 1, 'unreserved main task accepted'
    path = root.parent / 'plan.json'
    ok(cli(root, 'backfill', '--output', str(path)))
    value = json.loads(path.read_text())
    assert [(r['ref'], r['value']['title']) for r in value['updates']] == [('refs/reserved/roadmap/0620', 'historical task')], value
    ok(cli(root, 'publish-plan', str(path)))
    ok(cli(root, 'check'))
    ok(cli(root, 'audit-wip'))
    a.git(root, 'switch', '-qc', 'wip/conflicting')
    save(root, 'roadmap', 'different task')
    a.git(root, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/conflicting')
    a.git(root, 'switch', '-q', 'main')
    ok(cli(root, 'check'))
    ok(cli(root, 'audit-wip'))
    ok(cli(root, 'backfill', '--output', str(path)))
    assert json.loads(path.read_text())['conflicts'] == []
    save(root, 'roadmap', 'local main wording edit')
    ok(cli(root, 'backfill', '--scope', 'authoritative', '--output', str(path)))


def independent_unreserved(root):
    for kind in ('iss', 'roadmap'):
        for suffix in ('a', 'b'):
            a.git(root, 'switch', '-qc', kind + '-' + suffix, 'main')
            save(root, kind, 'identical wording')
            a.git(root, 'commit', '--amend', '-qm', kind + '-' + suffix + ' independent introduction')
            a.git(root, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/' + kind + '-' + suffix)
        plan = root.parent / 'plan.json'
        p = cli(root, 'backfill', '--output', str(plan))
        key = 'ISS-0020' if kind == 'iss' else 'R-0620'
        assert p.returncode == 1 and any(key in c for c in json.loads(plan.read_text())['conflicts']), p.stderr


def unreserved(root):
    save(root, 'roadmap', 'unreserved task')
    p = cli(root, 'check')
    assert p.returncode == 1 and 'refs/reserved/roadmap/0620' in p.stderr, p.stderr
    # Unstaged deletion must never hide the committed task.
    (root / 'ROADMAP.md').unlink()
    assert cli(root, 'check').returncode == 1


def metadata(root):
    value = a.metadata(root, 'R-0620', 'task', 'main')
    for field, bad in [('title', 'bad\ttitle'), ('branch', 'ordinary'), ('legacy_oid', '1' * 40)]:
        malformed = {**value, field: bad}
        oid = a.commit(root, malformed)
        p = cli(root, 'verify-ref', 'refs/reserved/roadmap/0620', oid, '0' * 40)
        assert p.returncode == 1, (field, p.stderr)
    ok(cli(root, 'reserve', 'roadmap', 'held task', '--number', '620'))
    save(root, 'roadmap', 'another task')
    p = cli(root, 'check')
    ok(p)  # reservation title is immutable provenance, never current wording


def incomplete(root):
    (root / 'ROADMAP.md').write_text('| R-0620 | | missing subject |\n')
    a.git(root, 'add', '.')
    a.git(root, 'commit', '-qm', 'malformed main')
    a.git(root, 'push', '-q', 'origin', 'main')
    p = cli(root, 'check')
    assert p.returncode == 2 and 'INCOMPLETE' in p.stderr and 'origin/main' in p.stderr, p.stderr
    a.git(root, 'remote', 'set-url', 'origin', str(root.parent / 'missing'))
    p = cli(root, 'check')
    assert p.returncode == 2 and 'INCOMPLETE' in p.stderr, p.stderr


def unreadable_reservation(root):
    oid = a.commit(root, a.metadata(root, 'R-0620', 'task', 'main'))
    a.git(root, 'push', '-q', 'origin', oid + ':refs/reserved/roadmap/0620')
    remote = Path(a.git(root, 'remote', 'get-url', 'origin').stdout.strip())
    (remote / 'objects' / oid[:2] / oid[2:]).unlink()
    p = cli(root, 'check')
    assert p.returncode == 2 and 'INCOMPLETE' in p.stderr, p.stderr


def parser(root):
    parse = a.read_roadmap_rows
    text = 'prose R-0620\n| heading | R-0620 |\n| R-0620 | subject with \\| pipe | obligation | — |\n'
    assert parse(text) == {'R-0620': ('subject with \\| pipe', 3)}
    for bad in [text + '| R-0620 | duplicate | obligation |\n', '| R-620 | bad | obligation |\n', '| R-0620 | | obligation |\n', '| R-0620 | missing end |']:
        try:
            parse(bad)
        except ValueError:
            pass
        else:
            raise AssertionError('malformed row accepted: ' + bad)


def main():
    failures = []
    with tempfile.TemporaryDirectory(prefix='id-round6-') as tmp:
        tests = [('ISS-0020-before-main-and-merge-target', lambda r: collision(r, 'iss')),
                 ('R-before-main-and-merge-target', lambda r: collision(r, 'roadmap')),
                 *[(f.__name__, f) for f in (race, backfill, independent_unreserved, unreserved, metadata, incomplete, unreadable_reservation, parser)]]
        for name, test in tests:
            directory = Path(tmp) / name
            directory.mkdir()
            root = directory / 'writer'
            remote = directory / 'origin.git'
            a.git(directory, 'init', '--bare', str(remote))
            a.git(remote, 'symbolic-ref', 'HEAD', 'refs/heads/main')
            a.git(directory, 'init', '-b', 'main', str(root))
            a.git(root, 'config', 'user.name', 'Control')
            a.git(root, 'config', 'user.email', 'control@example.invalid')
            a.git(root, 'commit', '--allow-empty', '-qm', 'base')
            a.git(root, 'remote', 'add', 'origin', str(remote))
            a.git(root, 'push', '-q', 'origin', 'main')
            try:
                test(root)
                print('PASS:', name)
            except Exception as error:
                failures.append(name)
                print('FAIL:', name, repr(error))
    if failures:
        raise SystemExit('regressions: ' + ', '.join(failures))
    print('Round 6 reservation controls: PASS')


if __name__ == '__main__':
    main()
