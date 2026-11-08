#!/usr/bin/env python3
"""Compare GitHub-hosted and concrete-ci-2 slice-0 manifests, fail closed.

usage: repro_manifest_compare.py GITHUB_MANIFEST CI2_MANIFEST

Repository and runner provenance are validated but excluded from the payload comparison. Every
other line, including build identity and artifact SHA-256 records, must match exactly. The CI2
runner name must be an exact inventory entry for the repository recorded by both manifests.
"""
import difflib
import re
import sys
from pathlib import Path

PROVENANCE = ('github_repository', 'runner_environment', 'runner_name')
INVENTORY = Path(__file__).with_name('ci2_runners.txt')
ARTIFACT = re.compile(r'[0-9a-f]{64}  .+')


def fail(message):
    sys.exit(f'FAIL cross-machine reproducibility: {message}')


def read_inventory():
    try:
        lines = INVENTORY.read_text().splitlines()
    except (OSError, UnicodeError) as error:
        fail(f'{INVENTORY}: cannot read runner inventory ({error})')
    runners = {}
    pairs = set()
    for number, raw in enumerate(lines, 1):
        line = raw.strip()
        if not line or line.startswith('#'):
            continue
        fields = line.split()
        if len(fields) != 2:
            fail(f'{INVENTORY}:{number}: expected repository and one runner name')
        repository, runner_name = fields
        pair = (repository, runner_name)
        if pair in pairs:
            fail(f'{INVENTORY}:{number}: duplicate {repository} {runner_name}')
        pairs.add(pair)
        runners.setdefault(repository, set()).add(runner_name)
    if not runners:
        fail(f'{INVENTORY}: empty runner inventory')
    return runners


def read_manifest(argument):
    path = Path(argument)
    try:
        lines = path.read_text().splitlines()
    except (OSError, UnicodeError) as error:
        fail(f'{path}: cannot read manifest ({error})')
    fields = {}
    payload = []
    for line in lines:
        key, separator, value = line.partition('=')
        if separator and key in PROVENANCE:
            if key in fields:
                fail(f'{path}: duplicate {key}')
            if not value:
                fail(f'{path}: empty {key}')
            fields[key] = value
        else:
            payload.append(line)
    for key in PROVENANCE:
        if key not in fields:
            fail(f'{path}: missing {key}')
        if fields[key] == 'unknown':
            fail(f'{path}: {key} was not recorded')
    if not any(line.startswith('compiler_commit=') for line in payload):
        fail(f'{path}: missing compiler_commit')
    if not any(ARTIFACT.fullmatch(line) and line.endswith('/lcb.exe') for line in payload):
        fail(f'{path}: missing lcb.exe SHA-256')
    return fields, payload


def check_provenance(path, fields, environment):
    if fields['runner_environment'] != environment:
        fail(f"{path}: expected runner_environment={environment}, "
             f"got {fields['runner_environment']}")


def main(argv):
    if len(argv) != 3:
        sys.exit(__doc__)
    github, github_payload = read_manifest(argv[1])
    ci2, ci2_payload = read_manifest(argv[2])
    check_provenance(argv[1], github, 'github-hosted')
    check_provenance(argv[2], ci2, 'self-hosted')
    repository = github['github_repository']
    if ci2['github_repository'] != repository:
        fail(f"github.repository differs: {repository} != {ci2['github_repository']}")
    eligible = read_inventory().get(repository)
    if eligible is None:
        fail(f'{INVENTORY}: no concrete-ci-2 runner inventory for {repository}')
    if ci2['runner_name'] not in eligible:
        fail(f"{argv[2]}: runner_name={ci2['runner_name']} is not eligible for {repository} "
             f"in {INVENTORY}")
    if github_payload != ci2_payload:
        diff = difflib.unified_diff(github_payload, ci2_payload,
                                    fromfile=argv[1], tofile=argv[2], lineterm='')
        sys.stderr.write('\n'.join(diff) + '\n')
        fail('GitHub-hosted and concrete-ci-2 manifests differ')
    print('PASS cross-machine reproducibility: GitHub-hosted and concrete-ci-2 manifests match')


if __name__ == '__main__':
    main(sys.argv)
