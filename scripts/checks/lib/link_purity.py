#!/usr/bin/env python3
"""Validate native linker inputs and load commands against the flake closure.

Local objects are allowed only by exact argument name. Store symlinks are resolved,
so a path in a declared output cannot smuggle an undeclared input through a symlink.
"""
import os
from pathlib import Path
import re
import subprocess
import sys

exe, trace, *args = sys.argv[1:]
closure = set(Path(os.environ['CONCRETE_SLICE0_LINK_CLOSURE']).read_text().splitlines())
objects = {str(Path(a).resolve()) for a in args if a.endswith('.o')}


def allowed(path, local=False):
    resolved = str(Path(path).resolve())
    if local and resolved in objects:
        return
    if any(resolved == root or resolved.startswith(root + '/') for root in closure):
        return
    sys.exit('link purity: undeclared path: ' + path)


lines = Path(trace).read_text().splitlines()
seen = set()
for line in lines:
    line = line.strip()
    if not line:
        continue
    # GNU ld traces archives as /path/archive.a(member.o).
    path = line.split('(', 1)[0]
    if Path(path).exists():
        allowed(path, local=True)
        seen.add(str(Path(path).resolve()))
    else:
        # Unknown trace syntax is not evidence of purity.
        sys.exit('link purity: unrecognized linker trace: ' + line)
if not objects.issubset(seen):
    sys.exit('link purity: trace omitted an explicit object')
if sys.platform == 'darwin':
    tool = os.environ['CONCRETE_SLICE0_LLVM'] + '/llvm-objdump'
    headers = subprocess.check_output([tool, '--macho', '--private-headers', exe], text=True)
    # Validate each load command separately: OS ABI install names are allowed
    # only if represented by a stub in the declared SDK. RPATH has no exception.
    for block in re.split(r'Load command \d+', headers)[1:]:
        command = re.search(r'^\s*cmd (LC_\w+)', block, re.M)
        entry = re.search(r'^\s*(?:name|path) (\S+) \(offset', block, re.M)
        if not command or not entry:
            continue
        path = entry[1]
        if command[1] == 'LC_LOAD_DYLINKER' and path == '/usr/lib/dyld':
            continue  # fixed Darwin kernel ABI, not a link-time host input
        if command[1] in ('LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB'):
            sdk = Path(os.environ['CONCRETE_SLICE0_SYSROOT'])
            stub = sdk / path.lstrip('/')
            if path.startswith(('/usr/lib/', '/System/Library/')):
                candidates = [stub, stub.with_suffix('.tbd') if stub.suffix == '.dylib'
                              else Path(str(stub) + '.tbd')]
                for candidate in candidates:
                    if candidate.is_file():
                        allowed(str(candidate))
                        break
                else:
                    sys.exit('link purity: install name absent from SDK: ' + path)
                continue
        allowed(path)

else:
    tool = os.environ['CONCRETE_SLICE0_LINKER_BIN'] + '/readelf'
    headers = subprocess.check_output([tool, '-l', '-d', exe], text=True)
    loaders = re.findall(r'Requesting program interpreter: ([^\]]+)', headers)
    if len(loaders) != 1:
        sys.exit('link purity: expected one ELF interpreter')
    for path in loaders:
        allowed(path)
    for paths in re.findall(r'\((?:RUNPATH|RPATH)\).*?\[([^\]]*)\]', headers):
        for path in paths.split(':'):
            if not path.startswith('/'):
                sys.exit('link purity: relative or empty runtime search path')
            allowed(path)
    # Absolute DT_NEEDED entries also participate in runtime lookup.
    for path in re.findall(r'\(NEEDED\).*?\[([^\]]*)\]', headers):
        if '/' in path:
            allowed(path)
