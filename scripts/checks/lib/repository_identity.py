"""Reject archived repository endpoints in maintained files; preserve historical evidence."""
import hashlib
import pathlib
import re
import subprocess
import sys
import tempfile

ARCHIVED = re.compile(r"unbalancedparentheses[/:]concrete2", re.IGNORECASE)


def problems(path, content):
    if path.startswith(('legacy/', 'docs/archive/', 'changelog.d/')):
        return []
    found = []
    for number, line in enumerate(content.splitlines(), 1):
        if not ARCHIVED.search(line):
            continue
        # Only the existing dated release evidence is exempt; new decision guidance is scanned.
        if path == 'docs/decisions.md' and hashlib.sha256(line.encode()).hexdigest() == '89301668fabffe82fce229eb1f7d3a5bf529c559507d92a0003bee3e51663158':
            continue
        if path == 'ISSUES.tsv' and line.split('\t')[-1] == 'closed':
            continue
        found.append(f'{path}:{number}: archived repository reference')
    return found


def controls():
    old = 'unbalancedparentheses' + '/concrete2'
    for path in ('README.md', '.github/workflows/ci.yml', 'scripts/tool.sh'):
        assert problems(path, f'https://github.com/{old}')
    for path in ('legacy/scripts/tool.sh', 'docs/archive/report.md',
                 'changelog.d/2026-11-05.md'):
        assert not problems(path, old)
    assert not problems('ISSUES.tsv', f'ISS-1\t{old}\tclosed')
    assert problems('ISSUES.tsv', f'ISS-2\t{old}\topen')
    assert problems('docs/decisions.md', old)
    assert not problems('README.md', 'https://github.com/lambdaclass/concrete')


def scan(root):
    paths = subprocess.check_output(
        ['git', '-C', str(root), 'ls-files', '-z', '--cached', '--others', '--exclude-standard']
    ).decode().split('\0')
    errors = []
    for path in sorted(set(paths) - {''}):
        file = root / path
        if file.is_file():
            errors.extend(problems(path, file.read_text(errors='replace')))
    return errors


def planted_controls():
    # Exercise discovery and file reading too: an untracked live endpoint must be found.
    with tempfile.TemporaryDirectory() as directory:
        root = pathlib.Path(directory)
        subprocess.run(['git', 'init', '-q', str(root)], check=True)
        old = 'https://github.com/' + 'unbalancedparentheses' + '/concrete2'
        (root / 'docs/archive').mkdir(parents=True)
        (root / 'docs/archive/evidence.md').write_text(old)
        assert not scan(root), 'historical evidence refused'
        (root / 'README.md').write_text(old)
        assert scan(root) == ['README.md:1: archived repository reference'], 'planted live endpoint passed'


if __name__ == '__main__':
    controls()
    planted_controls()
    errors = scan(pathlib.Path(sys.argv[1]))
    print('\n'.join(errors) if errors else 'repository_identity: historical controls pass; no live archived endpoints')
    sys.exit(bool(errors))
