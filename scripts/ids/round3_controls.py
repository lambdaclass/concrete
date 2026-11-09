#!/usr/bin/env python3
"""Regression controls for ISS-0034, entirely in private repositories.

An optional source root permits running these same controls against an earlier revision.
"""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True

SOURCE = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('allocator', SOURCE / 'scripts/ids/allocate.py')
a = importlib.util.module_from_spec(spec)
spec.loader.exec_module(a)
a.sandbox_environment()
os.environ.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_SYSTEM="/dev/null", GIT_CONFIG_NOSYSTEM="1")
HEADER = 'id\ttitle\tdate\tarea\tevidence\towner\tseverity\tclosure_condition\tstatus\n'
def row(n, title='title'):
    return f'ISS-{n:04d}\t{title}\t2026-11-08\tci\tevidence\tR-0542\thigh\tfixed\topen\n'
def cli(root, *args, good=True):
    p = subprocess.run(['python3', '-I', str(SOURCE / 'scripts/ids/allocate.py'), '--root', str(root), *args],
                       text=True, capture_output=True)
    assert (p.returncode == 0) == good, (args, p.returncode, p.stdout, p.stderr)
    return p

def main():
    failures = []
    with tempfile.TemporaryDirectory(prefix='id-round3-') as tmp:
        tmp = Path(tmp)
        def case(name, test):
            try:
                directory = tmp / name
                directory.mkdir()
                remote = directory / 'origin.git'
                a.git(directory, 'init', '--bare', str(remote))
                root = directory / 'writer'
                a.git(directory, 'init', '-b', 'main', str(root))
                a.git(root, 'config', 'user.name', 'Control')
                a.git(root, 'config', 'user.email', 'control@example.invalid')
                a.git(root, 'commit', '--allow-empty', '-qm', 'base')
                a.git(root, 'remote', 'add', 'origin', str(remote))
                a.git(root, 'push', '-q', 'origin', 'main')
                test(root, remote)
                print('PASS:', name)
            except (AssertionError, a.Refused) as error:
                failures.append(name)
                print('FAIL:', name, str(error))
        def write(root, text):
            (root / 'ISSUES.tsv').write_text(text)
            a.git(root, 'add', '.')
            a.git(root, 'commit', '-qm', 'records')
        def origin_main(root, remote):
            write(root, HEADER + row(1))
            a.git(root, 'push', '-q', 'origin', 'main')
            cli(root, 'check', good=False)
            plan = root.parent / 'plan.json'
            cli(root, 'backfill', '--output', str(plan))
            cli(root, 'publish-plan', str(plan))
            cli(root, 'check')
        case('origin-main-rollout', origin_main)
        def local_main(root, remote):
            write(root, HEADER + row(1))
            cli(root, 'check', good=False)
        case('local-main', local_main)
        def masked(root, remote):
            write(root, HEADER)
            a.git(root, 'push', '-q', 'origin', 'main')
            a.git(root, 'switch', '-qc', 'wip/masked')
            write(root, HEADER + row(1))
            (root / 'ISSUES.tsv').write_text(HEADER)
            cli(root, 'check', good=False)
        case('committed-head-masked-by-worktree', masked)
        def legacy_records(root, remote):
            p = root / 'legacy/docs/bugs/300_old.md'
            p.parent.mkdir(parents=True)
            p.write_text('# Bug 300: old bug\n')
            a.git(root, 'add', '.')
            a.git(root, 'commit', '-qm', 'old bug')
            a.git(root, 'push', '-q', 'origin', 'main')
            cli(root, 'check', good=False)
        case('frozen-main-bug', legacy_records)
        for key in ('ISS-0001', 'BUG-300'):
            def nonjson(root, remote, key=key):
                oid = a.git(root, 'commit-tree', a.git(root, 'hash-object', '-t', 'tree', '--stdin', '-w', data='').stdout.strip(), data='old reservation').stdout.strip()
                a.git(root, 'push', '-q', 'origin', oid + ':' + a.ref_for(key))
                cli(root, 'check', good=False)
            case('nonjson-' + key, nonjson)
        def worktree(root, remote):
            # Execute the actual worktree workflow, with this revision's policy owner.
            import shutil
            shutil.copytree(SOURCE / 'scripts', root / 'scripts')
            p = subprocess.run(['bash', str(root / 'scripts/worktree-new.sh'), 'plain-name'], cwd=root, capture_output=True, text=True)
            assert p.returncode == 0, p.stderr
            wt = root / '.claude/worktrees/plain-name'
            write(wt, HEADER + row(1, 'plain title'))
            cli(wt, 'backfill', '--output', str(root.parent / 'initial.json'))
            planned = json.loads((root.parent / 'initial.json').read_text())
            assert planned['updates'][0]['value']['branch'] == 'wip/plain-name'
            # Clear the draft before reserving its explicit number.
            a.git(wt, 'reset', '--hard', 'HEAD~1')
            cli(wt, 'reserve', 'iss', 'plain title', '--number', '1')
            write(wt, HEADER + row(1, 'plain title'))
            cli(wt, 'backfill', '--output', str(root.parent / 'plain.json'))
            a.git(wt, 'checkout', '--detach', '-q')
            cli(wt, 'backfill', '--output', str(root.parent / 'detached.json'), good=False)
            a.git(wt, 'config', 'id.branch', 'plain-name')
            cli(wt, 'backfill', '--output', str(root.parent / 'detached.json'))
            assert json.loads((root.parent / 'detached.json').read_text())['updates'] == []
            cli(wt, 'check')
        case('shared-worktree-branch-policy', worktree)
        for index, (field, bad) in enumerate([('title', 'bad\ttitle'), ('title', 'bad\ntitle'), ('branch', 'ordinary'), ('branch', 'wip/bad..ref')]):
            def invalid(root, remote, field=field, bad=bad):
                value = a.metadata(root, 'ISS-0001', 'title', 'wip/a')
                value[field] = bad
                oid = a.commit(root, value)
                cli(root, 'verify-ref', 'refs/reserved/iss/0001', oid, '0' * 40, good=False)
                a.git(root, 'push', '-q', 'origin', oid + ':refs/reserved/iss/0001')
                cli(root, 'check', good=False)
            case('metadata-' + field + '-' + str(index), invalid)
            def snapshot_invalid(root, remote, field=field, bad=bad):
                value = a.metadata(root, 'ISS-0001', 'title', 'wip/a')
                value[field] = bad
                oid = a.commit(root, value)
                a.git(root, 'push', '-q', 'origin', oid + ':refs/reserved/iss/0001')
                cli(root, 'check', good=False)
            case('snapshot-metadata-' + str(index), snapshot_invalid)
        def iss_import(root, remote):
            old = a.git(root, 'commit-tree', a.git(root, 'hash-object', '-t', 'tree', '--stdin', '-w', data='').stdout.strip(), data='not legacy ISS').stdout.strip()
            value = a.metadata(root, 'ISS-0001', 'title', 'wip/a')
            value['legacy_oid'] = old
            cli(root, 'verify-ref', a.ref_for('ISS-0001'), a.commit(root, value), old, good=False)
        case('no-legacy-iss-replacement', iss_import)
        def structured_merge(root, remote):
            spec = importlib.util.spec_from_file_location('resolver', SOURCE / 'scripts/land/resolve_conflicts.py')
            r = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(r)
            base = HEADER.encode()
            ours, theirs = (HEADER + row(2)).encode(), (HEADER + row(1)).encode()
            # Use real index stages and the resolver CLI.
            def resolve(o, t):
                entries = []
                for stage, content in [(1, base), (2, o), (3, t)]:
                    oid = a.git(root, 'hash-object', '-w', '--stdin', data=content.decode()).stdout.strip()
                    entries.append(f'100644 {oid} {stage}\tISSUES.tsv\n')
                a.git(root, 'update-index', '--index-info', data=''.join(entries))
                return subprocess.run(['python3', str(SOURCE / 'scripts/land/resolve_conflicts.py'), 'resolve', 'ISSUES.tsv'], cwd=root, capture_output=True, text=True)
            p = resolve(ours, theirs)
            assert p.returncode == 0, p.stdout + p.stderr
            assert (root / 'ISSUES.tsv').read_text() == HEADER + row(1) + row(2)
            import shutil
            shutil.copytree(SOURCE / 'scripts/checks', root / 'scripts/checks')
            (root / 'scripts/ids').mkdir()
            shutil.copy(SOURCE / 'scripts/ids/issues.py', root / 'scripts/ids/issues.py')
            p = subprocess.run(['bash', 'scripts/checks/issues.sh'], cwd=root, capture_output=True, text=True)
            assert p.returncode == 0, p.stdout + p.stderr
            for other in [(HEADER + row(2, 'collision')).encode(), (HEADER + row(1)).replace('\thigh\t', '\tinvalid\t').encode()]:
                p = resolve(ours, other)
                assert p.returncode != 0, p.stdout + p.stderr
        case('canonical-validated-issue-merge', structured_merge)
    assert not failures, 'regressions: ' + ', '.join(failures)
    print('Round 3 reservation controls: PASS')

if __name__ == '__main__':
    main()
