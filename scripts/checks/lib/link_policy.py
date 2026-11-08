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


def shell_arrays(policy):
    from link_paths import allowed

    def expand(s):
        return s.replace('@CWD@', os.getcwd()).replace('@TMPDIR@', os.environ['CONCRETE_NATIVE_TMP'])
    driver = [expand(v) for v in policy['compile']]
    if driver[0] != policy['settings']['CONCRETE_SLICE0_CLANG']:
        sys.exit('link command: unexpected compiler')
    allowed(driver[0])
    literals = {'--no-default-config', '-O0', '-g0', '-w', '-rtlib=compiler-rt',
                '-unwindlib=none', '-mmacosx-version-min=15.0',
                '-ffile-prefix-map=' + os.getcwd() + '=.',
                '-fdebug-prefix-map=' + os.getcwd() + '=.', '-fdebug-compilation-dir=.'}
    for arg in driver[1:]:
        if arg in literals:
            continue
        if arg.startswith('-B'):
            allowed(arg[2:])
            if arg[2:] == policy['settings']['CONCRETE_SLICE0_LINKER_BIN']:
                continue
        if arg.startswith('--sysroot='):
            allowed(arg.split('=', 1)[1])
            if arg.split('=', 1)[1] == policy['settings']['CONCRETE_SLICE0_SYSROOT']:
                continue
        sys.exit('link command: unsupported compiler argument: ' + arg)
    if policy['sanitize'] != ['-fsanitize=address', '-fno-sanitize-recover=all']:
        sys.exit('link command: unsupported sanitizer instrumentation')
    return {'clean': ['env', '-i'] + [k + '=' + expand(v) for k, v in policy['environment'].items()],
            'driver': driver, 'link': policy['link'], 'san': policy['sanitize'],
            'sanlink': policy['sanitizeLink']}


if __name__ == '__main__':
    policy = verify_policy()
    if sys.argv[1:2] in (['--shell'], ['--driver']):
        arrays = shell_arrays(policy)
        if sys.argv[1] == '--driver':
            if sys.argv[2:] != arrays['clean'] + arrays['driver']:
                sys.exit('link command: driver differs from flake policy')
            sys.exit(0)
        for name, values in arrays.items():
            print(name + '=(' + ' '.join(shlex.quote(v) for v in values) + ')')
