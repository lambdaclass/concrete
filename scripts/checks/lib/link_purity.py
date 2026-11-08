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
    # dyld is supplied by macOS, not Nix. Its fixed OS ABI path is an explicit
    # platform exception; SDK stubs and all linked inputs still belong to closure.
    for path in re.findall(r'^\s*(?:name|path) (\S+) \(offset', headers, re.M):
        if path == '/usr/lib/dyld':
            continue
        # SDK dylib install names refer to the OS shared cache, not host link inputs.
        if path.startswith(('/usr/lib/', '/System/Library/')):
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
