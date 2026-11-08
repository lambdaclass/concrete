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
    check_grammar([actual[0], policy['compilerRuntime'] + '/lib/darwin/libclang_rt.osx.a'], [], 'out.exe', 'aarch64-macos')
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
    sdk = Path(scratch) / 'sdk'
    sdk.mkdir()
    sdk_closure = Path(scratch) / 'sdk-closure'
    sdk_closure.write_text(Path(os.environ['CONCRETE_SLICE0_LINK_CLOSURE']).read_text() + str(sdk) + '\n')
    stub = sdk / 'unrelated-filename.tbd'
    stub.write_text("""--- !tapi-tbd
tbd-version: 4
targets: [ arm64-macos ]
install-name: /usr/lib/libSystem.B.dylib
current-version: 1351.0.0
compatibility-version: 1.0.0
reexported-libraries:
  - targets: [ arm64-macos ]
    libraries: [ /usr/lib/libc++.1.dylib ]
--- !tapi-tbd-v3
archs: [ arm64 ]
platform: macosx
install-name: /usr/lib/libc++.1.dylib
exports:
  - archs: [ arm64 ]
    symbols: [ _fixture ]
...
""")
    fixture_policy = copy.deepcopy(policy)
    fixture_policy['sdkInstallNames'] = {
        'normal': ['/usr/lib/libSystem.B.dylib'],
        'sanitize': ['/usr/lib/libSystem.B.dylib', '/usr/lib/libc++.1.dylib']}
    def macho(commands, sanitize=False):
        with patch.dict(os.environ, {'CONCRETE_SLICE0_SYSROOT': str(sdk), 'CONCRETE_SLICE0_LINK_CLOSURE': str(sdk_closure)}), patch('link_policy.verify_policy', return_value=fixture_policy), patch.object(sys, 'platform', 'darwin'), patch.object(sys, 'argv', ['link_purity.py', 'image', 'trace'] + (['--sanitize'] if sanitize else []) + ['control.o']), patch('subprocess.check_output', return_value=header(commands)):
            runpy.run_path(str(root / 'link_purity.py'), run_name='__main__')
    macho(loader)
    macho(loader + [('LC_LOAD_DYLIB', '/usr/lib/libSystem.B.dylib')])
    macho(loader + [('LC_LOAD_DYLIB', '/usr/lib/libSystem.B.dylib'),
                    ('LC_LOAD_DYLIB', '/usr/lib/libc++.1.dylib')], sanitize=True)
    refuse('SDK present but undeclared for normal mode',
           lambda: macho(loader + [('LC_LOAD_DYLIB', '/usr/lib/libc++.1.dylib')]),
           'link purity: install name not declared for mode:')
    from sdk_install_names import install_names
    original_stub = stub.read_text()
    for label, text in [
        ('duplicate key', original_stub.replace('tbd-version: 4', 'tbd-version: 4\ntbd-version: 4')),
        ('unknown field', original_stub.replace('tbd-version: 4', 'tbd-version: 4\nunrecognized: true')),
        ('invalid version', original_stub.replace('1351.0.0', 'broken')),
        ('dangling reexport', original_stub.replace('libraries: [ /usr/lib/libc++.1.dylib ]', 'libraries: [ /usr/lib/missing.dylib ]')),
        ('invalid YAML', '[ malformed'),
        ('unknown YAML tag', original_stub.replace('!tapi-tbd\n', '!unknown\n', 1)),
        ('untagged stub', original_stub.replace('!tapi-tbd\n', '\n', 1)),
        ('missing targets', original_stub.replace('targets: [ arm64-macos ]', 'targets: []', 1)),
        ('invalid section type', original_stub.replace('symbols: [ _fixture ]', 'symbols: _fixture')),
        ('missing section targets', original_stub.replace('  - archs: [ arm64 ]', '  - symbols: [ _other ]')),
    ]:
        stub.write_text(text)
        refuse('SDK ' + label, lambda: macho(loader), 'link purity: ')
    stub.write_text(original_stub)
    # A valid stub for another architecture cannot authorize the arm64 load.
    stub.write_text(original_stub.replace('arm64-macos', 'x86_64-macos').replace('archs: [ arm64 ]', 'archs: [ x86_64 ]'))
    refuse('SDK wrong architecture', lambda: macho(loader + [('LC_LOAD_DYLIB', '/usr/lib/libSystem.B.dylib')]), 'link purity: install name absent from SDK:')
    stub.write_text(original_stub)
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
print(f'PASS parser controls: negative={count} positive=8')
