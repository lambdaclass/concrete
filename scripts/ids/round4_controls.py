#!/usr/bin/env python3
"""Round 4 regressions; optional source root supports before/after verification."""
import sys
sys.dont_write_bytecode = True
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
OLD = HEADER.replace('title\t', '')
def row(n, title='title'):
    return f'ISS-{n:04d}\t{title}\t2026-11-08\tci\tevidence\tR-0542\thigh\tfixed\topen\n'
def cli(root, *args, env=None):
    return subprocess.run(['python3', '-I', '-B', str(SOURCE / 'scripts/ids/allocate.py'), '--root', str(root), *args], text=True, capture_output=True, env=env)
def ok(p):
    assert p.returncode == 0, (p.returncode, p.stdout, p.stderr)
def write(root, text):
    (root / 'ISSUES.tsv').write_text(text)
    a.git(root, 'add', '.')
    a.git(root, 'commit', '-qm', 'records')
def push(root, branch):
    a.git(root, 'push', '-q', 'origin', 'HEAD:refs/heads/' + branch)
def plan(root):
    path = root.parent / 'plan.json'
    p = cli(root, 'backfill', '--output', str(path))
    return p, json.loads(path.read_text()) if path.exists() else None

def collision(root):
    for branch, title in [('wip/a', 'distinct A'), ('wip/b', 'distinct B')]:
        a.git(root, 'switch', '-qc', branch, 'main')
        write(root, OLD + row(50, title).replace(title + '\t', '', 1).replace('evidence', title))
        push(root, branch)
    p, value = plan(root)
    assert p.returncode != 0 and value['conflicts'], (p.returncode, value)
    assert len(value['conflicts']) == 1 and value['conflicts'][0].count("('wip/b',") == 1, value

def malformed(root):
    a.git(root, 'switch', '-qc', 'wip/bad')
    write(root, HEADER + 'ISS-0001\n')
    push(root, 'wip/bad')
    a.git(root, 'switch', '-q', 'main')
    ok(cli(root, 'check'))
    with tempfile.TemporaryDirectory() as tmp:
        snap = a.Snapshot(root, tmp)
        assert 'wip/bad' in snap.tree_errors
    p, value = plan(root)
    assert p.returncode != 0 and value['conflicts'] and 'Traceback' not in p.stderr, p.stderr

def quoted(root):
    title = '"quoted" title'
    ok(cli(root, 'reserve', 'iss', title, '--number', '1'))
    write(root, HEADER + row(1, title))
    ok(cli(root, 'check'))
    (root / 'ISSUES.tsv').write_text(HEADER + row(1, ''))
    assert cli(root, 'check').returncode != 0

def detached(root):
    a.git(root, 'switch', '-qc', 'wip/local')
    write(root, HEADER + row(1))
    a.git(root, 'checkout', '--detach', '-q')
    p, value = plan(root)
    assert p.returncode != 0 and 'id.branch' in p.stderr, (p.returncode, value, p.stderr)
    push(root, 'wip/local')
    p, value = plan(root)
    ok(p)
    assert [(u['value']['id'], u['value']['branch']) for u in value['updates']] == [('ISS-0001', 'wip/local')], value
    push(root, 'wip/ambiguous')
    assert cli(root, 'backfill', '--output', str(root.parent / 'ambiguous.json')).returncode != 0
    a.git(root, 'config', 'id.branch', 'wip/explicit')
    p, value = plan(root)
    ok(p)
    assert any(u['value']['id'] == 'ISS-0001' for u in value['updates']), value

def draft(root):
    a.git(root, 'switch', '-qc', 'wip/draft')
    write(root, HEADER + row(33))
    push(root, 'wip/draft')
    ok(cli(root, 'reserve', 'iss', 'title', '--number', '33'))

def exhaustion(root):
    a.git(root, 'switch', '-qc', 'wip/bogus')
    write(root, HEADER + row(9999))
    push(root, 'wip/bogus')
    a.git(root, 'switch', '-q', 'main')
    p = cli(root, 'reserve', 'iss', 'first real identity')
    ok(p)
    assert p.stdout.strip() == 'ISS-0001', p.stdout

def transport(root):
    remote = a.git(root, 'remote', 'get-url', 'origin').stdout.strip()
    alias = 'https://transport.invalid/control'
    cfg = root.parent / 'transport.config'
    cfg.write_text(f'[url "{remote}"]\n insteadOf = {alias}\n[core]\n hooksPath = /nonexistent/hooks\n[commit]\n gpgsign = true\n')
    a.git(root, 'remote', 'set-url', 'origin', alias)
    env = {**os.environ, 'GIT_CONFIG_GLOBAL': str(cfg)}
    ok(cli(root, 'check', env=env))
    ok(cli(root, 'reserve', 'iss', 'transport identity', env=env))
    a.git(root, 'remote', 'set-url', 'origin', remote)
    # Repo-local transport configuration must reach the private fetch too.
    a.git(root, 'config', f'url.{remote}.insteadOf', alias)
    a.git(root, 'remote', 'set-url', 'origin', alias)
    ok(cli(root, 'check'))
    # No real network: a private SSH transport serves the local bare repository.
    import shlex
    helper = root.parent / 'ssh-transport'
    helper.write_text('#!/bin/sh\ncase "$*" in\n*git-receive-pack*) exec git-receive-pack ' + shlex.quote(remote) + ';;\n*) exec git-upload-pack ' + shlex.quote(remote) + ';;\nesac\n')
    helper.chmod(0o755)
    a.git(root, 'config', 'core.sshCommand', str(helper))
    a.git(root, 'config', 'ssh.variant', 'ssh')
    a.git(root, 'remote', 'set-url', 'origin', 'ssh://transport.invalid/repository')
    ok(cli(root, 'check'))
    ok(cli(root, 'reserve', 'iss', 'local SSH identity'))

def scanner(root):
    script = (SOURCE / 'scripts/checks/sandbox_isolation.sh').read_text()
    code = script.split("<<'PYSCAN'\n", 1)[1].split('\nPYSCAN', 1)[0]
    for text in ["import subprocess\nsubprocess.run(['git', 'init'])\nsandbox_environment()\n", "import subprocess\ndef main():\n subprocess.Popen(['git', 'init'])\n sandbox_environment()\n", "import subprocess\ndef main():\n subprocess.run(['git', 'init'])\nmain()\nsandbox_environment()\n"]:
        path = root.parent / 'maker.py'
        path.write_text(text)
        p = subprocess.run(['python3', '-B', '-', str(path)], input=code, capture_output=True, text=True)
        assert p.returncode == 1, (text, p.returncode, p.stdout, p.stderr)

def documentation(root):
    assert 'introduced or modified' not in (SOURCE / 'changelog.d/2026-11-08-issue-allocator.md').read_text()
    assert 'introduced or modified' not in (SOURCE / 'ISSUES.tsv').read_text()
    assert 'collisions across origin main and every wip branch' not in (SOURCE / 'AGENTS.md').read_text()

def bytecode(root):
    # Exercise the import prefix in a private source copy, without -B masking its policy.
    import shutil
    copy = root.parent / 'source'
    shutil.copytree(SOURCE / 'scripts', copy / 'scripts', ignore=shutil.ignore_patterns('__pycache__'))
    for name in ('controls.py', 'round3_controls.py'):
        path = copy / 'scripts/ids' / name
        prefix = path.read_text().split('\ndef ', 1)[0]
        # __file__ is needed for each harness's normal source selection.
        p = subprocess.run(['python3', '-I', '-c', '__file__=' + repr(str(path)) + '\n' + prefix], cwd=copy, capture_output=True, text=True)
        ok(p)
    assert not list(copy.rglob('__pycache__')), list(copy.rglob('__pycache__'))

def main():
    failures = []
    with tempfile.TemporaryDirectory(prefix='id-round4-') as tmp:
        for test in (collision, malformed, quoted, detached, draft, exhaustion, transport, scanner, documentation, bytecode):
            directory = Path(tmp) / test.__name__
            directory.mkdir()
            root = directory / 'writer'
            a.git(directory, 'init', '--bare', str(directory / 'origin.git'))
            a.git(directory, 'init', '-b', 'main', str(root))
            a.git(root, 'config', 'user.name', 'Control')
            a.git(root, 'config', 'user.email', 'control@example.invalid')
            a.git(root, 'commit', '--allow-empty', '-qm', 'base')
            a.git(root, 'remote', 'add', 'origin', str(directory / 'origin.git'))
            push(root, 'main')
            try:
                test(root)
                print('PASS:', test.__name__)
            except Exception as error:
                failures.append(test.__name__)
                print('FAIL:', test.__name__, repr(error))
    if failures:
        raise SystemExit('regressions: ' + ', '.join(failures))
    print('Round 4 reservation controls: PASS')

if __name__ == '__main__':
    main()
