#!/usr/bin/env python3
"""Round 7 adversarial controls; optional source root for baseline reproduction."""
import sys
sys.dont_write_bytecode = True
import runpy
from pathlib import Path
import tempfile
import json
import os

SOURCE = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else Path(__file__).resolve().parents[2]
c = runpy.run_path(str(SOURCE / 'scripts/ids/round6_controls.py'))
a, cli, ok, save = (c[n] for n in ('a', 'cli', 'ok', 'save'))
a.sandbox_environment()

def head(root):
    return a.git(root, 'rev-parse', 'HEAD').stdout.strip()

def setup_main(root):
    ok(cli(root, 'reserve', 'roadmap', 'main identity', '--number', '620'))
    save(root, 'roadmap', 'main identity')
    a.git(root, 'push', '-q', 'origin', 'main')
    return head(root)

def delete(root):
    a.git(root, 'rm', 'ROADMAP.md')
    a.git(root, 'commit', '-qm', 'delete task')

def refused(p):
    assert p.returncode == 1, (p.returncode, p.stdout, p.stderr)

def worktree_readd(root):
    setup_main(root)
    a.git(root, 'switch', '-qc', 'writer')
    delete(root)
    (root / 'ROADMAP.md').write_text('| R-0620 | unrelated worktree task | obligation | — |\n')
    refused(cli(root, 'check'))

def worktree_collision(root):
    a.git(root, 'switch', '-qc', 'other')
    save(root, 'roadmap', 'other task')
    a.git(root, 'push', '-q', 'origin', 'HEAD:refs/heads/wip/other')
    a.git(root, 'switch', '-qc', 'writer', 'main')
    (root / 'ROADMAP.md').write_text('| R-0620 | local task | obligation | — |\n')
    path = root.parent / 'plan.json'
    refused(cli(root, 'backfill', '--output', str(path)))
    assert len(json.loads(path.read_text())['conflicts']) == 1

def cherry_readd(root):
    introduced = setup_main(root)
    a.git(root, 'switch', '-qc', 'writer')
    delete(root)
    a.git(root, 'cherry-pick', introduced)
    save(root, 'roadmap', 'reworded unrelated task')
    refused(cli(root, 'check'))

def merge_readd(root):
    setup_main(root)
    a.git(root, 'switch', '-qc', 'still-present')
    (root / 'other').write_text('divergence')
    a.git(root, 'add', '.')
    a.git(root, 'commit', '-qm', 'other parent')
    a.git(root, 'switch', '-qc', 'writer', 'main')
    delete(root)
    a.git(root, 'merge', '--no-ff', '--no-commit', 'still-present')
    (root / 'ROADMAP.md').write_text('| R-0620 | restored by merge | obligation | — |\n')
    a.git(root, 'add', '.')
    a.git(root, 'commit', '-qm', 'restore through merge parent')
    refused(cli(root, 'check'))

def two_step_bug(root):
    base = head(root)
    ok(cli(root, 'reserve', 'bug', 'main bug', '--number', '20'))
    path = root / 'docs/bugs/020_main.md'
    path.parent.mkdir(parents=True)
    path.write_text('# Bug 020: main bug\n')
    a.git(root, 'add', '.')
    a.git(root, 'commit', '-qm', 'main bug')
    a.git(root, 'push', '-q', 'origin', 'main')
    a.git(root, 'switch', '-qc', 'stale', base)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text('WIP\n')
    a.git(root, 'add', '.')
    a.git(root, 'commit', '-qm', 'path first')
    path.write_text('# Bug 020: stale bug\n')
    a.git(root, 'add', '.')
    a.git(root, 'commit', '-qm', 'heading second')
    a.git(root, 'switch', '-qc', 'candidate', 'main')
    a.git(root, 'merge', '--no-ff', '-X', 'theirs', '-m', 'integrate stale', 'stale')
    refused(cli(root, 'check'))

def hostile_config(root):
    base = head(root)
    setup_main(root)
    a.git(root, 'switch', '-qc', 'stale', base)
    save(root, 'roadmap', 'colliding task')
    a.git(root, 'switch', '-q', 'main')
    a.git(root, 'merge', '--no-ff', '-X', 'theirs', '-m', 'integrate stale', 'stale')
    config = root.parent / 'hostile.config'
    config.write_text('[diff]\n noprefix = true\n mnemonicPrefix = true\n renames = copies\n external = /bin/false\n[color]\n ui = always\n')
    previous = os.environ['GIT_CONFIG_GLOBAL']
    os.environ['GIT_CONFIG_GLOBAL'] = str(config)
    try:
        refused(cli(root, 'check'))
        a.git(root, 'switch', '-qc', 'valid-edit', 'origin/main')
        save(root, 'roadmap', 'valid inherited edit')
        ok(cli(root, 'check'))
    finally:
        os.environ['GIT_CONFIG_GLOBAL'] = previous

def diagnostics(root):
    base = head(root)
    introduced = setup_main(root)
    a.git(root, 'switch', '-qc', 'stale', base)
    save(root, 'roadmap', 'colliding task')
    other = head(root)
    path = root.parent / 'plan.json'
    refused(cli(root, 'backfill', '--output', str(path)))
    plan = json.loads(path.read_text())
    text = json.dumps(plan)
    assert introduced in text and other in text, text
    assert 'ISS-0017' not in plan['collision_resolution'], plan
    assert 'R-0620' in plan['collision_resolution'] and 'wip/stale' in plan['collision_resolution'], plan
    assert 'retire' in plan['collision_resolution'] and 'reserve' in plan['collision_resolution'], plan


def main():
    failures = []
    with tempfile.TemporaryDirectory(prefix='id-round7-') as tmp:
        for test in (worktree_readd, worktree_collision, cherry_readd, merge_readd, two_step_bug, hostile_config, diagnostics):
            directory = Path(tmp) / test.__name__
            directory.mkdir()
            root, remote = directory / 'writer', directory / 'origin.git'
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
                print('PASS:', test.__name__)
            except Exception as error:
                failures.append(test.__name__)
                print('FAIL:', test.__name__, repr(error))
    if failures:
        raise SystemExit('regressions: ' + ', '.join(failures))
    print('Round 7 reservation controls: PASS')

if __name__ == '__main__':
    main()
