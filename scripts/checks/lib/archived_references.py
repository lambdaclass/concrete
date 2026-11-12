"""Refuse stale archived URLs in maintained text; historical evidence is explicit."""
import hashlib
import pathlib
import re
import subprocess
import sys
import tempfile

ARCHIVED = re.compile(r"(?<![a-z0-9_-])unbalancedparentheses(?:[/:]|\.github\.io/)concrete2(?=$|[^a-z0-9_-])", re.IGNORECASE)
# Frozen evidence only: new files, even under archive directories, are scanned.
HISTORICAL_FILES = {
    'docs/archive/roadmap-2026-11/ROADMAP_TASKS.md',
    'docs/archive/site-down.md',
    'legacy/.github/workflows/lean_action_ci.yml',
    'legacy/.github/workflows/nightly.yml',
    'legacy/ISSUES.tsv',
    'legacy/README.md',
    'legacy/changelog.d/2026-11-05-restart-infra-2.md',
    'legacy/docs/archive/ROADMAP_HISTORY.md',
    'legacy/docs/archive/ROADMAP_RETIRED_TASKS.md',
    'legacy/docs/archive/roadmap-2026-11/ROADMAP_BACKLOG.md',
    'legacy/docs/archive/roadmap-2026-11/ROADMAP_TASKS.md',
    'legacy/docs/archive/site-down.md',
    'legacy/docs/archive/site-frozen.md',
    'legacy/docs/book/src/getting_started.md',
    'legacy/docs/book/src/installation.md',
    'legacy/docs/project/GATE_PROFILE.md',
    'legacy/docs/project/LANDING.md',
    'legacy/docs/project/PROMOTION_PIPELINE.md',
    'legacy/docs/project/SELF_HOSTED_RUNNERS.md',
    'legacy/docs/spec/SPEC.md',
    'legacy/docs/spec/decisions.md',
    'legacy/docs/verification/KNOWN_HOLES.md',
    'legacy/paper/evidence-carrying.typ',
    'legacy/research/meta/bend-roc-lessons.md',
    'legacy/research/proof-evidence/concrete-design.md',
    'legacy/scripts/fuzz/farm/farm.sh',
    'legacy/scripts/fuzz/farm/install-farm.sh',
    'legacy/scripts/gen/site_reference.py',
    'legacy/scripts/land/install-bot.sh',
    'legacy/scripts/land/train.sh',
    'legacy/scripts/oracle/ORACLE.lock',
    'legacy/scripts/perf/install-perf.sh',
    'legacy/scripts/push-both.sh',
    'legacy/scripts/spec/spec.py',
    'legacy/scripts/status/install.sh',
    'legacy/scripts/status/status.py',
    'legacy/site/config.toml',
    'legacy/site/content/guide/direction.md',
    'legacy/site/content/guide/getting_started.md',
    'legacy/site/content/guide/high_integrity.md',
    'legacy/site/content/guide/installation.md',
    'legacy/site/content/guide/intro.md',
    'legacy/site/content/guide/landing.md',
    'legacy/site/content/guide/status.md',
    'legacy/site/content/paper.md',
    'legacy/site/templates/base.html',
    'legacy/site/templates/index.html',
}
HISTORICAL_LINES = {
    'docs/decisions.md': {'89301668fabffe82fce229eb1f7d3a5bf529c559507d92a0003bee3e51663158'},
    'ISSUES.tsv': {
        '4429fea9f910712f42ef89666c42cdfb70df1841b6ee5bec05b7f48fe3d870cb',
        '64ac6a7f963148bf94675d1d8715a46a2d20ebc9420f934a8685b29a82b6c54f',
        'fa193ed204e8b980e234f8b944a124eb8df2b8e3ade20de250fb994c42400ccf',
    },
}


def problems(path, content):
    if path in HISTORICAL_FILES:
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
    for path in HISTORICAL_FILES:
        assert not problems(path, old)
    for path in ('legacy/new.md', 'docs/archive/new.md', 'changelog.d/new.md'):
        assert problems(path, old)
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
        (root / 'docs/archive/site-down.md').write_text(old)
        assert not scan(root), 'historical evidence refused'
        (root / 'README.md').write_text(old)
        for url in (old, 'https://' + 'unbalancedparentheses' + '.github.io/concrete2'):
            (root / 'README.md').write_text(url)
            assert scan(root) == ['README.md:1: archived repository reference'], 'planted live endpoint passed'
        (root / 'README.md').unlink()
        (root / 'changelog.d').mkdir()
        fragment = root / 'changelog.d/new.md'
        fragment.write_text(old)
        assert scan(root) == ['changelog.d/new.md:1: archived repository reference'], 'new live fragment exempted'
        fragment.unlink()
        (root / 'ISSUES.tsv').write_text(f'ISS-9999\t{old}\tclosed')
        assert scan(root) == ['ISSUES.tsv:1: archived repository reference'], 'new closed issue exempted'


if __name__ == '__main__':
    controls()
    planted_controls()
    errors = scan(pathlib.Path(sys.argv[1]))
    print('\n'.join(errors) if errors else 'archived_references: no live archived endpoints')
    sys.exit(bool(errors))
