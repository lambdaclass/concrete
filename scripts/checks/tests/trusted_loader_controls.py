"""Focused real shared-library attack plus clean-launch and mapping-parser controls."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[3]
spec = importlib.util.spec_from_file_location('trusted_mappings', ROOT / 'scripts/checks/lib/trusted_mappings.py')
mappings = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mappings)


def run(args, **kw):
    return subprocess.run(args, check=True, text=True, capture_output=True, timeout=45, **kw)


def rejects(call, expected):
    try:
        call()
    except ValueError as error:
        assert str(error) == expected, str(error)
    else:
        raise AssertionError('accepted ' + expected)


# Parser controls include split calls, failed mappings, arbitrary library suffixes,
# anonymous code and mprotect. Each negative asserts the precise planted diagnostic.
line = '1 mmap(NULL, 4096, PROT_READ|PROT_EXEC, MAP_PRIVATE, 3</tmp/injected.payload>, 0) = 0x1000\n'
assert mappings.executable_mappings([line]) == {'/tmp/injected.payload'}
assert mappings.executable_mappings([
    '1 mmap(NULL, 4096, PROT_READ|PROT_EXEC, MAP_PRIVATE, 3</tmp/injected.payload>, 0 <unfinished ...>\n',
    '2 mmap(NULL, 1, PROT_READ, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = -1 ENOMEM (Cannot allocate memory)\n',
    '1 <... mmap resumed>) = 0x1000\n',
]) == {'/tmp/injected.payload'}
for lines, expected in [
    (['1 mmap(NULL, 1, PROT_EXEC, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x1000\n'],
     'executable anonymous or unresolved mapping'),
    (['1 mprotect(0x1000, 4096, PROT_READ|PROT_EXEC) = 0\n'],
     'executable mprotect has no pinned file identity'),
    (['1 <... mmap resumed>) = 0x1000\n'], 'resumed mapping has no unfinished call'),
    (['1 mmap(NULL, 4096, PROT_READ <unfinished ...>\n'], 'unfinished mapping calls have no result'),
    (['1 mmap(NULL, 4096, PROT_EXEC, MAP_PRIVATE, 3</tmp/x>, 0) = ?\n'], 'missing mapping result'),
]:
    rejects(lambda: mappings.executable_mappings(lines), expected)

if sys.platform != 'linux':
    print('SKIP real loader controls: Linux strace required')
    sys.exit(0)

with tempfile.TemporaryDirectory() as tmp:
    d = Path(tmp)
    lean = Path(shutil.which('lean')).resolve()
    assert str(lean).startswith('/nix/store/'), lean
    root = '/'.join(str(lean).split('/')[:4])
    closure = run(['nix', 'path-info', '--offline', '--recursive', root]).stdout.splitlines()
    evidence = {'closure': dict.fromkeys(closure)}
    marker = d / 'constructor-ran'
    source = d / 'inject.c'
    source.write_text('''#include <unistd.h>
#include <fcntl.h>
#include <string.h>
__attribute__((constructor)) static void injected(void) {
  char path[4096]; ssize_t n = readlink("/proc/self/exe", path, sizeof(path)-1);
  if (n <= 0) return; path[n] = 0;
  char *base = strrchr(path, '/');
  if (base && strcmp(base+1, "lean") == 0) {
    int fd = open(''' + json.dumps(str(marker)) + ''', O_WRONLY|O_CREAT, 0600);
    if (fd >= 0) close(fd);
  }
}
''')
    library = d / 'injected.payload'  # no .so extension: audit actual executable mappings
    helper = str(ROOT / 'scripts/checks/lib/slice0_environment.sh')
    run(['bash', '-c', 'source "$1"; slice0_clean_environment "$PATH" "$HOME" "${TMPDIR:-/tmp}"; '
         '"${clean[@]}" "$2" --no-default-config -shared -fPIC "$3" -o "$4"',
         'bash', helper, os.environ['CONCRETE_SLICE0_CLANG'], str(source), str(library)])
    trace = d / 'injected.trace'
    command = ['strace', '-f', '-qq', '-s', '4096', '-yy', '-e',
               'trace=execve,execveat,mmap,mprotect', '-o', str(trace), str(lean), '--version']
    run(command, env={**os.environ, 'LD_PRELOAD': str(library)})
    assert marker.exists(), 'injection did not run its constructor in Lean'
    names_spec = importlib.util.spec_from_file_location('execve_names', ROOT / 'scripts/checks/lib/execve_names.py')
    names = importlib.util.module_from_spec(names_spec)
    names_spec.loader.exec_module(names)
    assert names.parse_many([trace]) == ['lean'], 'injection changed the executable inventory'
    rejects(lambda: mappings.check([trace], evidence),
            'unregistered executable mapping: ' + json.dumps(str(library)))
    marker.unlink()
    # Exercise the real production boundary as well as its underlying allowlist.
    poison_bin = d / 'bin'
    poison_bin.mkdir()
    fake_lean = poison_bin / 'lean'
    fake_lean.write_text('#!/bin/sh\nexit 97\n')
    fake_lean.chmod(0o755)
    hostile = ['LD_PRELOAD', 'LD_LIBRARY_PATH', 'LD_AUDIT', 'GCONV_PATH',
               'LEAN_PATH', 'LEAN_SRC_PATH', 'LEAN_SYSROOT', 'LEAN_CC',
               'BASH_ENV', 'ENV', 'PYTHONPATH', 'NIX_CONFIG', 'NIX_PATH',
               'GIT_CONFIG_GLOBAL', 'GIT_DIR', 'CONCRETE_TEST_MANIFEST',
               'CONCRETE_TRUSTED_DECLARATIONS', 'CONCRETE_TRUSTED_BASE_WITHOUT',
               'CONCRETE_EXAMPLES_MEMORY_MAX', 'SLICE0_GATE_MEM_GB',
               'NIX_CFLAGS_COMPILE', 'NIX_LDFLAGS', 'CC', 'CFLAGS']
    probe = ('import json,os,shutil; '
             'print(json.dumps({"env":dict(os.environ),"lean":shutil.which("lean")}))')
    result = run(['bash', '-c',
        'source "$1"; probe() { case $- in *e*u*) ;; *) echo "missing shell safety options"; return 1;; esac; '
        'set -o | grep -qxE "pipefail[[:space:]]+on" || return 1; python3 -I -c "$1"; }; '
        'export PATH="$2:$PATH"; shift 2; code=$1; shift; '
        'for key in "$@"; do export "$key=ambient-poison"; done; '
        'slice0_gate_environment probe "$code"',
        'bash', helper, str(poison_bin), probe, *hostile])
    observed = json.loads(result.stdout)
    assert not (set(hostile) & set(observed['env'])), observed
    assert observed['lean'] == str(lean), observed
    assert observed['env']['HOME'] != os.environ.get('HOME'), observed
    print('ok   control: production launch removes ambient authority and replaces poisoned PATH')
    clean_trace = d / 'clean.trace'
    command[command.index(str(trace))] = str(clean_trace)
    # Export hostile ambient variables only after entering the test shell. The same
    # shared allowlist used by the production gate/native helper clears every one.
    run(['bash', '-c', 'source "$1"; shift; export LD_PRELOAD="$1" LD_LIBRARY_PATH="$1" '
         'LD_AUDIT="$1" GCONV_PATH="$1" LEAN_PATH="$1" LEAN_SYSROOT="$1" '
         'BASH_ENV="$1" CONCRETE_TEST_MANIFEST="$1" NIX_CFLAGS_COMPILE="$1"; shift; '
         'slice0_clean_environment "$PATH" "$HOME" "${TMPDIR:-/tmp}"; "${clean[@]}" "$@"',
         'bash', helper, str(library), *command])
    assert not marker.exists(), 'clean launch still loaded the planted library into Lean'
    mappings.check([clean_trace], evidence)
    print('ok   control: real injected library rejected with exact mapping diagnostic')
    print('ok   control: shared clean environment prevents injected constructor and passes mapping audit')
print('PASS focused loader and executable-mapping controls')
