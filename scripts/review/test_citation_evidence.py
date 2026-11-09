#!/usr/bin/env python3
"""Original review evidence checked against portable, original-blob fixture trees."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile

sandbox_spec = importlib.util.spec_from_file_location("git_sandbox", Path(__file__).resolve().parents[1] / "checks/lib/git_sandbox.py")
sandbox_module = importlib.util.module_from_spec(sandbox_spec)
sandbox_spec.loader.exec_module(sandbox_module)

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('contract', HERE / 'output_contract.py')
c = importlib.util.module_from_spec(spec)
spec.loader.exec_module(c)
FIX = HERE / 'fixtures/iss0040'
for row in (FIX / 'SHA256SUMS').read_text().splitlines():
    digest, name = row.split('  ', 1)
    assert hashlib.sha256((FIX / name).read_bytes()).hexdigest() == digest, name
manifest = json.loads((FIX / 'provenance.json').read_text())

def run(name, review, positive=True, removed=False, framed=False):
    with tempfile.TemporaryDirectory() as directory:
        config = Path(directory) / "sandbox.config"
        config.write_text("[core]\n hooksPath = /dev/null\n[commit]\n gpgSign = false\n")
        env = sandbox_module.git_sandbox(config)
        os.environ.clear()
        os.environ.update(env)
        def git(*args):
            return subprocess.check_output(['git', '-C', directory, *args], env=env, stderr=subprocess.PIPE).decode().strip()
        git('init', '-q')
        def unpack(which):
            with tarfile.open(FIX / (which + '.tar.gz')) as archive:
                assert set(archive.getnames()) == set(manifest[which]['paths'])
                archive.extractall(directory, filter='data')
        def commit():
            git('add', '.')
            git('-c', 'user.name=Control', '-c', 'user.email=control@example.invalid', 'commit', '-qm', 'original blobs')
            return git('rev-parse', 'HEAD')
        base = None
        if removed:
            unpack('c2-base')
            base = commit()
        unpack(name)
        sha = commit()
        text = (FIX / review).read_text()
        if framed:
            row = manifest[name]
            for path, digest in row['blob_sha256'].items():
                assert hashlib.sha256((Path(directory) / path).read_bytes()).hexdigest() == digest, path
            return c.evaluate_output(text, '', sha, directory, ['clean', 'findings'],
                review_root=row['review_root'], harness_files=c.REVIEW_HARNESS_FILES)
        # Historical archive roots are fixture metadata, never inferred by the resolver.
        roots = {
            'scheduled': '/home/unbalancedparen/projects/gates-briefs/ro-S3',
            'anchors': '/home/unbalancedparen/projects/gates-briefs/ro-AKa',
            'cross-check': manifest['cross-check']['review_root'],
            'rev-c1a3': '/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/rev-c1a3/tree',
        }
        return c.citation_error(text, directory, sha, c.commit_blob_paths(directory, sha), positive, base, roots.get(name, '/home/unbalancedparen'))

assert run('rev-c1a3', 'rev-c1a3/agy.out') is None
assert run('rev-c2a4', 'rev-c2a4/codex.final', positive=False, removed=True) is None
# The later positive-evidence requirement deliberately rejects this uncited positive output.
assert run('rev-c2a4', 'rev-c2a4/codex.final', removed=True) == 'citation: positive verdict requires at least one resolvable file:line citation'
assert run('scheduled', 'S3-agy.log') is None
for name, needle in [('rev-c9r8-invalid', "identifier 'stepChecked' unresolved"), ('rev-c4a3', "identifier 'invalidTwo' unresolved"), ('rev-c7r6', "identifier 'vacuousForbidsRelease' unresolved"), ('rev-intg6', "identifier 'alwaysEmitSteps' unresolved")]:
    result = run(name, name + '/agy.out')
    assert result and needle in result, (name, result)
result = run('anchors', 'AKa-agy.log')
assert "anchor 'finish_diff' absent from 'file:///home/unbalancedparen/projects/gates-briefs/ro-AKa/scripts/checks/compiler/spec_diff.sh#L2020-L2034'" in result, result
assert run('anchors', 'AKc-codex.md') == 'citation: positive verdict requires at least one resolvable file:line citation'
# Raw AKc has no framing directives either: the uncited positive is still invalid.
status, reason = c.evaluate_output((FIX / 'AKc-codex.md').read_text(), '', 'HEAD', str(HERE.parents[1]), ['clean', 'findings'])
assert (status, reason) == ('invalid', 'citation: positive verdict requires at least one resolvable file:line citation'), (status, reason)
print('original citation evidence: PASS (hashes, honest reviews, fabricated reviews, fake anchors, uncited CLEAN)')

# CHa remains invalid for precisely two defects, with both spellings of the bad range.
cross_result = run('cross-check', 'CHa-agy.log', positive=False)
assert cross_result == (
    "citation: range 'file:///home/unbalancedparen/projects/gates-briefs/ro-CHa/scripts/checks/run.sh#L1-L59' outside 1..27; "
    "citation: range 'scripts/checks/run.sh:1-59' outside 1..27; "
    "citation: identifier 'ALL_CONTROLS_OK' unresolved"
), cross_result
print('CHa original evidence: PASS (root twins, adjacent basename, LLVM anchors, exact two defects)')

# Full contract replay: final artifacts are never normalized, repaired or reordered.
for label in ('CIa', 'I5a'):
    row = manifest['round6-' + label]
    raw = (FIX / (label + '-agy.log')).read_bytes()
    final = (FIX / row['final_message']).read_bytes()
    assert raw == final + b'agy rc=0\n'
    assert hashlib.sha256(raw).hexdigest() == row['review_sha256']
    assert hashlib.sha256(final).hexdigest() == row['final_sha256']
    expected = json.loads((FIX / row['expected']).read_text())
    result = run('round6-' + label, row['final_message'], framed=True)
    assert result == (expected['status'], expected['reason']), (label, result, expected)
    raw_result = run('round6-' + label, label + '-agy.log', framed=True)
    assert raw_result == ('noverdict', 'last non-empty line is not a valid VERDICT (clean|findings) line'), raw_result
print('round6 original evidence: PASS (CIa blocked, I5a invalid, exact diagnostics, unchanged reviewer bytes)')
