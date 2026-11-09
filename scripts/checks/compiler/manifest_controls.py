"""Probe determinism's production consumer of B's public manifest interface."""
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]
env = {k: v for k, v in os.environ.items()
       if not k.startswith(('GITHUB_', 'SELECTED_', 'SCHEDULED_', 'RUNNER_'))}
env.pop('CONCRETE_TEST_MANIFEST', None)
env['CAMPAIGN_EXPECT_FAILURE'] = '1'


def run(root):
    return subprocess.run(['bash', 'scripts/checks/compiler/determinism.sh', '--check-manifest'],
                          cwd=root, env=env, capture_output=True, text=True, timeout=40)


public = subprocess.run(['python3', '-I', 'scripts/checks/lib/test_manifest.py', 'rows'],
                        cwd=ROOT, capture_output=True, text=True, check=True, timeout=30)
rows = public.stdout.splitlines()
expected = ''.join(row+'\n' for row in rows if row.split('\t')[1].startswith('bir:'))
clean = run(ROOT)
assert (clean.returncode, clean.stdout, clean.stderr) == (0, expected, ''), clean
print('ok C manifest: public rows select exactly BIR rows')

with tempfile.TemporaryDirectory(prefix='determinism-manifest-') as tmp:
    root = Path(tmp)
    compiler = root/'scripts/checks/compiler'
    compiler.mkdir(parents=True)
    (root/'scripts/checks/lib').symlink_to(ROOT/'scripts/checks/lib', target_is_directory=True)
    tests = root/'tests'
    tests.mkdir()
    (tests/'source.con').write_text('return 0;\n')
    manifest = tests/'manifest.tsv'
    manifest.write_text((ROOT/'tests/manifest.tsv').read_text() +
                        '\nsource_control\tcon:tests/source.con\trefuse SYN-5\tslice0\n')
    env['CONCRETE_TEST_MANIFEST'] = str(manifest)
    mixed = run(ROOT)
    assert (mixed.returncode, mixed.stdout, mixed.stderr) == (0, expected, ''), mixed
    print('ok C manifest: source row excluded from public mixed-kind output')
    (compiler/'campaign.sh').symlink_to(ROOT/'scripts/checks/compiler/campaign.sh')
    source = (ROOT/'scripts/checks/compiler/determinism.sh').read_text()
    anchor = 'rows "$manifest"'
    assert source.count(anchor) == 1
    (compiler/'determinism.sh').write_text(source.replace(anchor, 'rows --kind bir "$manifest"'))
    planted = run(root)
    assert (planted.returncode, planted.stdout, planted.stderr) == (
        1, '', 'FAIL slice0_determinism: manifest rows interface failed\n'), planted
print('ok C manifest control: removed option refused with exact interface diagnostic')
