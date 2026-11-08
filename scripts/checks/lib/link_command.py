#!/usr/bin/env python3
"""Parse clang -###'s quoted linker command, rejecting unknown syntax/options.

This validates active searches, including unused directories, before the real link.
The pinned wrapper runs in the policy environment; no caller flags are inherited.
"""
import os
from pathlib import Path
import shlex
import sys
from link_policy import verify_policy
from link_paths import allowed

verify_policy()
if sys.argv[1] == '--inputs':
    permitted = set(verify_policy()['sanitizeLink']) | set(verify_policy()['link'])
    for arg in sys.argv[2:]:
        if arg in permitted:
            continue
        if arg.endswith('.o') and not arg.startswith('-'):
            allowed(arg, local=True)
        else:
            sys.exit('link command: unapproved caller argument: ' + arg)
    sys.exit(0)
commands = []
for line in Path(sys.argv[1]).read_text().splitlines():
    if line.startswith(' "'):
        try:
            commands.append(shlex.split(line))
        except ValueError:
            sys.exit('link command: malformed quoted command')
    elif not line.startswith(('clang version ', 'Target: ', 'Thread model: ', 'InstalledDir: ')):
        sys.exit('link command: unexpected driver diagnostic: ' + line[:500])
if len(commands) != 1:
    sys.exit('link command: expected exactly one linker invocation')
command = commands[0]
expected = os.environ['CONCRETE_SLICE0_LINKER_BIN'] + '/ld'
if command[0] != expected:
    sys.exit('link command: unexpected linker: ' + command[0])
allowed(command[0])
args = iter(command[1:])
# Closed grammar for the pinned GNU/Darwin linker invocation, not arbitrary driver flags.
path_options = {'--sysroot', '-syslibroot', '-L', '-F', '-rpath', '-rpath-link',
                '-dynamic-linker', '--dynamic-linker', '-plugin', '--dynamic-list'}
value_options = {'-z', '-m', '-arch', '-macosx_version_min', '-sdk_version',
                 '-platform_version'}
flags = {'-pie', '-no_uuid', '-t', '--eh-frame-hdr', '--as-needed', '--no-as-needed',
         '--push-state', '--pop-state', '--whole-archive', '--no-whole-archive',
         '-static', '-Bstatic', '-Bdynamic', '-export-dynamic', '--export-dynamic',
         '-dead_strip', '-demangle', '-dynamic', '-search_paths_first'}
try:
    for arg in args:
        if arg == '-o':
            output = next(args)
            if Path(output).name != output:
                sys.exit('link command: output must be a local basename')
        elif arg in path_options:
            allowed(os.path.normpath(next(args)))
        elif any(arg.startswith(p + '=') for p in path_options):
            allowed(os.path.normpath(arg.split('=', 1)[1]))
        elif arg.startswith(('-L', '-F')) and len(arg) > 2:
            allowed(os.path.normpath(arg[2:]))
        elif arg in value_options:
            count = 3 if arg == '-platform_version' else 1
            for _ in range(count):
                value = next(args)
                if not value or '/' in value or value.startswith('-'):
                    sys.exit('link command: invalid option value: ' + value)
        elif arg in flags or arg in {'--build-id=none', '--hash-style=gnu'}:
            pass
        elif arg.startswith('-l') and len(arg) > 2 and '/' not in arg:
            pass
        elif not arg.startswith('-'):
            allowed(arg, local=True)
        else:
            sys.exit('link command: unsupported linker argument: ' + arg)
except StopIteration:
    sys.exit('link command: missing option operand')
