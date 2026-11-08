#!/usr/bin/env python3
"""The recorded policy must be exactly the effective toolchain configuration."""
import json
import os
import shlex
import sys
from pathlib import Path


def verify_policy():
    policy = json.loads(Path(os.environ['CONCRETE_SLICE0_LINK_POLICY']).read_text())
    if policy['version'] != 1:
        raise SystemExit('link policy: unsupported version')
    for key, value in policy['settings'].items():
        if os.environ.get(key) != value:
            raise SystemExit('link policy: effective setting differs: ' + key)
    return policy


if __name__ == '__main__':
    policy = verify_policy()
    if sys.argv[1:] == ['--shell']:
        def expand(s):
            return s.replace('@CWD@', os.getcwd()).replace('@TMPDIR@', os.environ.get('TMPDIR', '/tmp'))
        arrays = {'clean': ['env', '-i'] + [k + '=' + expand(v) for k, v in policy['environment'].items()],
                  'driver': [expand(v) for v in policy['compile']],
                  'link': policy['link'], 'san': policy['sanitize'], 'sanlink': policy['sanitizeLink']}
        for name, values in arrays.items():
            print(name + '=(' + ' '.join(shlex.quote(v) for v in values) + ')')
