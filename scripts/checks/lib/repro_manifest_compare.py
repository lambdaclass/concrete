#!/usr/bin/env python3
"""Compare GitHub-hosted and concrete-ci-2 slice-0 manifests, fail closed.

usage: repro_manifest_compare.py GITHUB_MANIFEST CI2_MANIFEST

Runner provenance is validated but excluded from the payload comparison. Every other line,
including build identity and artifact SHA-256 records, must match exactly.
"""
import difflib
import re
import sys
from pathlib import Path

PROVENANCE = ('runner_environment', 'runner_host', 'runner_name')
ARTIFACT = re.compile(r'[0-9a-f]{64}  .+')


def fail(message):
    sys.exit(f'FAIL cross-machine reproducibility: {message}')


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
    if not any(line.startswith('compiler_commit=') for line in payload):
        fail(f'{path}: missing compiler_commit')
    if not any(ARTIFACT.fullmatch(line) and line.endswith('/lcb.exe') for line in payload):
        fail(f'{path}: missing lcb.exe SHA-256')
    return fields, payload


def check_provenance(path, fields, environment, host):
    expected = {'runner_environment': environment, 'runner_host': host}
    for key, value in expected.items():
        if fields[key] != value:
            fail(f'{path}: expected {key}={value}, got {fields[key]}')
    if fields['runner_name'] == 'unknown':
        fail(f'{path}: runner_name was not recorded')


def main(argv):
    if len(argv) != 3:
        sys.exit(__doc__)
    github, github_payload = read_manifest(argv[1])
    ci2, ci2_payload = read_manifest(argv[2])
    check_provenance(argv[1], github, 'github-hosted', 'github')
    check_provenance(argv[2], ci2, 'self-hosted', 'concrete-ci-2')
    if github_payload != ci2_payload:
        diff = difflib.unified_diff(github_payload, ci2_payload,
                                    fromfile=argv[1], tofile=argv[2], lineterm='')
        sys.stderr.write('\n'.join(diff) + '\n')
        fail('GitHub-hosted and concrete-ci-2 manifests differ')
    print('PASS cross-machine reproducibility: GitHub-hosted and concrete-ci-2 manifests match')


if __name__ == '__main__':
    main(sys.argv)
