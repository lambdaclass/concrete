"""Reject archived repository endpoints in maintained files; preserve historical evidence."""
import hashlib
import pathlib
import re
import subprocess
import sys
import tempfile

# Match owner/repository paths independently of protocol, host, port or SSH alias.
# The Pages spelling is the same identity with a different separator.
ARCHIVED = re.compile(r"(?<![a-z0-9_-])unbalancedparentheses(?:[/:]|\.github\.io/)concrete2(?=$|[^a-z0-9_-])", re.IGNORECASE)
HISTORICAL_PATHS = ('legacy/', 'docs/archive/', 'changelog.d/')
HISTORICAL_LINES = {
    'docs/decisions.md': {'89301668fabffe82fce229eb1f7d3a5bf529c559507d92a0003bee3e51663158'},
    'ISSUES.tsv': {
        '4429fea9f910712f42ef89666c42cdfb70df1841b6ee5bec05b7f48fe3d870cb',
        '64ac6a7f963148bf94675d1d8715a46a2d20ebc9420f934a8685b29a82b6c54f',
        'fa193ed204e8b980e234f8b944a124eb8df2b8e3ade20de250fb994c42400ccf',
    },
}


def canonical_url(url):
    normalized = url.lower().rstrip('/')
    if normalized.endswith('.git'):
        normalized = normalized[:-4]
    return bool(re.fullmatch(r'(?:https?://github\.com/|ssh://git@github\.com(?::22)?/|git@[^/:]+:)lambdaclass/concrete', normalized))


def endpoint_problem(url):
    if ARCHIVED.search(url):
        return 'archived repository destination refused'
    # Local remotes exist only for gate sandboxes. Check their resolved identity too.
    if url.startswith(('/', './', '../', 'file://')) or (':' not in url and '@' not in url):
        local = url[7:] if url.startswith('file://') else url
        if ARCHIVED.search(str(pathlib.Path(local).resolve())):
            return 'archived repository destination refused'
        return None
    if canonical_url(url):
        return None
    return 'destination is not lambdaclass/concrete'


def fetch_source(source, repo, ref=None):
    if ARCHIVED.search(source):
        raise ValueError('archived repository destination refused')
    if canonical_url(source):
        return source
    # Resolve once against the caller, refusing symlinks in every path component.
    path = pathlib.Path(source).absolute()
    if any(part.is_symlink() for part in (path, *path.parents)):
        raise ValueError('symlink fetch source refused')
    if not path.is_file():
        raise ValueError('fetch source must be a canonical repository URL or a git bundle file; local clones and remote names are refused')
    verified = subprocess.run(['git', '-C', repo, 'bundle', 'verify', str(path)],
                              capture_output=True, text=True)
    if verified.returncode:
        raise ValueError('invalid git bundle: ' + verified.stderr.strip())
    heads = subprocess.run(['git', 'bundle', 'list-heads', str(path)],
                           capture_output=True, text=True, check=True)
    entries = [line.split() for line in heads.stdout.splitlines()]
    if not entries or any(len(row) != 2 or not re.fullmatch(r'[0-9a-f]{40}|[0-9a-f]{64}', row[0])
                          or (row[1] != 'HEAD' and subprocess.run(
                              ['git', 'check-ref-format', row[1]], capture_output=True).returncode)
                          for row in entries):
        raise ValueError('git bundle has invalid or missing refs')
    if ref is not None and not any(ref in row for row in entries):
        raise ValueError('requested ref is not advertised by git bundle: ' + ref)
    return str(path)


def remote_problems(name, destination=None):
    if name != 'origin':
        return [f"refusing remote named {name!r}: only origin is permitted"]
    urls = [destination] if destination is not None else []
    for mode in ([], ['--push']):
        result = subprocess.run(['git', 'remote', 'get-url', '--all', *mode, name],
                                capture_output=True, text=True)
        if result.returncode:
            return ['origin has no configured fetch/push URL']
        urls.extend(result.stdout.splitlines())
    return [f'{endpoint_problem(url)}: {url}' for url in urls if endpoint_problem(url)]



def problems(path, content):
    if path.startswith(HISTORICAL_PATHS):
        return []
    found = []
    for number, line in enumerate(content.splitlines(), 1):
        if not ARCHIVED.search(line):
            continue
        if hashlib.sha256(line.encode()).hexdigest() in HISTORICAL_LINES.get(path, set()):
            continue
        found.append(f'{path}:{number}: archived repository reference')
    return found


def controls():
    old = 'unbalancedparentheses' + '/concrete2'
    assert problems('README.md', 'https://' + 'unbalancedparentheses' + '.github.io/concrete2')
    for path in ('README.md', '.github/workflows/ci.yml', 'scripts/tool.sh'):
        assert problems(path, f'https://github.com/{old}')
    for path in ('legacy/scripts/tool.sh', 'docs/archive/report.md',
                 'changelog.d/2026-11-05.md'):
        assert not problems(path, old)
    assert problems('ISSUES.tsv', f'ISS-1\t{old}\tclosed')
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
        for url in (old, 'https://' + 'unbalancedparentheses' + '.github.io/concrete2'):
            (root / 'README.md').write_text(url)
            assert scan(root) == ['README.md:1: archived repository reference'], 'planted live endpoint passed'
        (root / 'README.md').unlink()
        (root / 'ISSUES.tsv').write_text(f'ISS-9999\t{old}\tclosed')
        assert scan(root) == ['ISSUES.tsv:1: archived repository reference'], 'new closed issue exempted'


if __name__ == '__main__':
    if sys.argv[1:2] == ['--fetch-source']:
        try:
            print(fetch_source(sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else '.',
                               sys.argv[4] if len(sys.argv) > 4 else None))
        except ValueError as error:
            print(error, file=sys.stderr)
            sys.exit(1)
        sys.exit(0)
    if sys.argv[1:2] == ['--remote']:
        errors = remote_problems(sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else None)
        if errors: print('\n'.join(errors), file=sys.stderr)
        sys.exit(bool(errors))
    controls()
    planted_controls()
    errors = scan(pathlib.Path(sys.argv[1]))
    print('\n'.join(errors) if errors else 'repository_identity: historical controls pass; no live archived endpoints')
    sys.exit(bool(errors))
