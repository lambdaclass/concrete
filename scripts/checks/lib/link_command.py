#!/usr/bin/env python3
"""Validate and execute an exact real-linker template from the pinned policy.

There is no wrapper after this check, no driver discovery, and no general purpose
option parser. The policy owns the complete grammar and operand ordering; callers
can supply only local objects and choose the declared sanitizer variant.
"""
import json
import os
import re
from pathlib import Path
import sys
from link_policy import verify_policy
from link_paths import allowed


def check_grammar(actual, objects, output, target):
    """Closed subset used by the templates, with operand arity from pinned manuals.

    GNU ld 2.44 ld/ld.texi and ld64-954.16 doc/man/man1/ld-classic.1
    describe these options. We never parse arbitrary clang diagnostics or accept
    extra options merely because they are known to a linker.
    """
    allowed(actual[0])
    darwin = target == 'aarch64-macos'
    flags = ({'-dynamic', '-no_uuid', '-w', '-Z', '-t'} if darwin else
             {'--eh-frame-hdr', '-nostdlib', '-pie', '--build-id=none',
              '--hash-style=gnu', '-t', '--whole-archive', '--no-whole-archive', '--no-as-needed'})
    paths = {'-syslibroot', '-L', '-F', '-rpath'} if darwin else {'--sysroot', '-L', '-rpath', '-dynamic-linker', '--dynamic-list'}
    args = iter(actual[1:])
    try:
        for arg in args:
            if arg == '-o':
                if next(args) != output:
                    sys.exit('link command: unexpected output')
            elif arg in paths:
                allowed(next(args))
            elif not darwin and arg.startswith(('--sysroot=', '--dynamic-list=')):
                allowed(arg.split('=', 1)[1])
            elif arg.startswith(('-L', '-F')) and len(arg) > 2:
                if arg[:2] not in paths:
                    sys.exit('link command: unsupported search option: ' + arg)
                allowed(arg[2:])
            elif arg == '-arch' and darwin:
                if next(args) != 'arm64':
                    sys.exit('link command: unexpected architecture')
            elif arg == '-platform_version' and darwin:
                if next(args) != 'macos' or any(not re.fullmatch(r'[0-9]+(?:[.][0-9]+){0,2}', next(args)) for _ in range(2)):
                    sys.exit('link command: invalid platform version')
            elif arg == '-z' and not darwin:
                if next(args) != 'relro':
                    sys.exit('link command: unsupported -z operand')
            elif arg in flags or re.fullmatch(r'-l[A-Za-z0-9_+.-]+', arg):
                pass
            elif arg in objects:
                allowed(arg, local=True)
            elif arg.startswith('/'):
                allowed(arg)
            else:
                sys.exit('link command: unsupported linker argument: ' + arg)
    except StopIteration:
        sys.exit('link command: missing option operand')


def command(policy, output, objects, sanitize=False):
    if not output or Path(output).name != output or output in ('.', '..'):
        sys.exit('link command: output must be a local basename')
    if not objects:
        sys.exit('link command: no objects')
    for obj in objects:
        allowed(obj, local=True)
    result = []
    for arg in policy['link']:
        if arg == '@OUTPUT@':
            result.append(output)
        elif arg == '@OBJECTS@':
            result.extend(objects)
            if sanitize:
                result.extend(policy['sanitizeLink'])
        else:
            result.append(arg)
    check_grammar(result, objects, output, policy['settings']['CONCRETE_SLICE0_TARGET'])
    if result[0] != os.environ['CONCRETE_SLICE0_REAL_LD']:
        sys.exit('link command: unexpected real linker')
    return result


def validate(policy, actual, output, objects, sanitize=False):
    expected = command(policy, output, objects, sanitize)
    if actual != expected:
        sys.exit('link command: invocation differs from declared real-linker template')
    return actual


if __name__ == '__main__':
    policy = verify_policy()
    record, output, variant, *objects = sys.argv[1:]
    if variant not in ('normal', 'sanitize'):
        sys.exit('link command: unknown variant')
    actual = command(policy, output, objects, variant == 'sanitize')
    Path(record).write_text(json.dumps(actual) + '\n')
    validate(policy, json.loads(Path(record).read_text()), output, objects, variant == 'sanitize')
    os.execve(actual[0], actual, {'LC_ALL': 'C', 'TZ': 'UTC', 'ZERO_AR_DATE': '1',
                                   'TMPDIR': os.environ['CONCRETE_NATIVE_TMP']})
