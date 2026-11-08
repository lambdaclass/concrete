#!/usr/bin/env python3
"""Exercise real-command, path and Mach-O parsers without shell pre-checks.

Mach-O headers are subprocess fixtures: run the production parser on every host.
Each negative must produce its own policy diagnostic; tool errors are failures.
"""
import os
import copy
from pathlib import Path
import runpy
import sys
import tempfile
from unittest.mock import patch

from link_command import command, validate, check_grammar
from link_paths import allowed
from link_policy import verify_policy, shell_arrays

count = 0

def refuse(label, action, expected='link '):
    global count
    try:
        action()
    except SystemExit as error:
        if not str(error).startswith(expected):
            raise AssertionError((label, str(error)))
    else:
        raise AssertionError(label + ': accepted')
    count += 1
    print('ok parser control: ' + label)


policy = verify_policy()
root = Path(__file__).resolve().parent
with tempfile.TemporaryDirectory() as scratch:
    os.chdir(scratch)
    Path('control.o').write_bytes(b'fixture')
    Path('trace').write_text('control.o\n')
    Path('image').write_bytes(b'fixture')
    actual = command(policy, 'out.exe', ['control.o'])
    validate(policy, actual, 'out.exe', ['control.o'])
    for flag in ['-L/usr/lib', '-F/usr/lib', '-B/usr/bin', '--sysroot=/usr',
                 '-rpath', '-T', '@response', '--unknown']:
        refuse('final invocation ' + flag,
               lambda flag=flag: validate(policy, actual + [flag], 'out.exe', ['control.o']))
    os.environ['CONCRETE_NATIVE_TMP'] = scratch
    shell_arrays(policy)
    for flag in ['-L/usr/lib', '-F/usr/lib', '-B/usr/bin', '--sysroot=/usr']:
        mutated = copy.deepcopy(policy)
        mutated['compile'].append(flag)
        refuse('compiler policy ' + flag, lambda mutated=mutated: shell_arrays(mutated))
    target = policy['settings']['CONCRETE_SLICE0_TARGET']
    for args in [['-L/usr/lib'], ['-L', '/usr/lib'], ['-F/usr/lib'], ['-B/usr/bin'],
                 ['--sysroot=/usr'], ['-rpath', '/usr/lib'], ['-T', '/usr/script'],
                 ['@response'], ['--unknown'], ['-L']]:
        refuse('grammar ' + repr(args), lambda args=args: check_grammar([actual[0]] + args, [], 'out.exe', target))
    # ld64's legitimate -w is covered on Linux too, independently of the host.
    check_grammar([actual[0], '-w', '-Z', '-platform_version', 'macos', '15.0', '15.0'], [], 'out.exe', 'aarch64-macos')
    store = next(p for p in Path(os.environ['CONCRETE_SLICE0_LINK_CLOSURE']).read_text().splitlines() if Path(p).is_dir())
    refuse('local closure object', lambda: allowed(store + '/crt1.o', local=True))
    refuse('local relative traversal', lambda: allowed('./control.o', local=True))
    Path('alias.o').symlink_to(Path('control.o').resolve())
    refuse('local symlink', lambda: allowed('alias.o', local=True))
    refuse('symlink dotdot traversal', lambda: allowed('/host/symlink/../../' + store.lstrip('/')))
    refuse('store dot traversal', lambda: allowed(store + '/./lib'))
    # A symlink can leave and then re-enter a declared root. The final resolved
    # target alone is insufficient: every traversed symlink prefix must be owned.
    declared = Path(scratch) / 'declared'
    declared.mkdir()
    (declared / 'safe').mkdir()
    outside = Path(scratch) / 'outside'
    outside.mkdir()
    (declared / 'leave').symlink_to(outside, target_is_directory=True)
    (outside / 'return').symlink_to(declared / 'safe', target_is_directory=True)
    closure = Path(scratch) / 'closure'
    closure.write_text(str(declared) + '\n')
    (declared / 'chain').symlink_to(outside / 'return', target_is_directory=True)
    (declared / 'safe' / 'inside').write_text('fixture')
    (declared / 'internal').symlink_to('safe/../safe/inside')
    with patch.dict(os.environ, {'CONCRETE_SLICE0_LINK_CLOSURE': str(closure)}):
        allowed(str(declared / 'internal'))
        refuse('symlink prefix leaves and reenters closure', lambda: allowed(str(declared / 'leave' / 'return')))
        refuse('symlink target chain leaves and reenters closure', lambda: allowed(str(declared / 'chain')))
    # Each Mach-O fixture begins with the legitimate fixed loader. Fixtures test
    # production handling of all path-bearing failure modes, not a duplicate parser.
    def header(commands):
        return '\n'.join(f'Load command {i}\n cmd {kind}\n name {name} (offset 12)' for i, (kind, name) in enumerate(commands))
    loader = [('LC_LOAD_DYLINKER', '/usr/lib/dyld')]
    def macho(commands):
        with patch.object(sys, 'platform', 'darwin'), patch.object(sys, 'argv', ['link_purity.py', 'image', 'trace', 'control.o']), patch('subprocess.check_output', return_value=header(commands)):
            runpy.run_path(str(root / 'link_purity.py'), run_name='__main__')
    macho(loader)
    for label, commands in [
        ('dyld environment', loader + [('LC_DYLD_ENVIRONMENT', 'DYLD_LIBRARY_PATH=/host')]),
        ('unexpected loader', [('LC_LOAD_DYLINKER', '/host/dyld')]),
        ('relative rpath', loader + [('LC_RPATH', 'relative')]),
        ('host rpath', loader + [('LC_RPATH', '/usr/lib')]),
        ('noncanonical install name', loader + [('LC_LOAD_DYLIB', '/usr/lib/../lib/libSystem.B.dylib')]),
        ('missing SDK stub', loader + [('LC_LOAD_DYLIB', '/usr/lib/concrete-missing.dylib')]),
        ('unresolved dependency', loader + [('LC_LOAD_DYLIB', '@rpath/concrete-missing.dylib')]),
        ('unparsed load command', loader + [('LC_RPATH', '')]),
    ]:
        refuse('Mach-O ' + label, lambda commands=commands: macho(commands))
print(f'PASS parser controls: negative={count} positive=6')
