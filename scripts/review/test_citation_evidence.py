#!/usr/bin/env python3
"""Original review evidence checked against portable, original-blob fixture trees."""
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import tarfile
import tempfile

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('contract', HERE / 'output_contract.py')
c = importlib.util.module_from_spec(spec)
spec.loader.exec_module(c)
FIX = HERE / 'fixtures/iss0040'
for row in (FIX / 'SHA256SUMS').read_text().splitlines():
    digest, name = row.split('  ', 1)
    assert hashlib.sha256((FIX / name).read_bytes()).hexdigest() == digest, name
manifest = json.loads((FIX / 'provenance.json').read_text())

def run(name, review, positive=True, removed=False):
    with tempfile.TemporaryDirectory() as directory:
        def git(*args):
            return subprocess.check_output(['git', '-C', directory, *args], stderr=subprocess.DEVNULL).decode().strip()
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
        return c.citation_error(text, directory, sha, c.commit_blob_paths(directory, sha), positive, base)

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
