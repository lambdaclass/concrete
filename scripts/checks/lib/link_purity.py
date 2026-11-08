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

from link_policy import verify_policy

verify_policy()
exe, trace, *args = sys.argv[1:]
closure = set(Path(os.environ['CONCRETE_SLICE0_LINK_CLOSURE']).read_text().splitlines())
objects = {os.path.abspath(a): str(Path(a).resolve()) for a in args if a.endswith('.o')}


def allowed(path, local=False):
    lexical = os.path.abspath(path)
    resolved = str(Path(path).resolve())
    if local and lexical in objects and resolved == objects[lexical]:
        return
    if not path.startswith('/') or os.path.normpath(path) != path:
        sys.exit('link purity: noncanonical path: ' + path)
    def member(p):
        return any(p == root or p.startswith(root + '/') for root in closure)
    if member(path) and member(resolved):
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
if not set(objects.values()).issubset(seen):
    sys.exit('link purity: trace omitted an explicit object')
if sys.platform == 'darwin':
    tool = os.environ['CONCRETE_SLICE0_LLVM'] + '/llvm-objdump'
    visited = set()
    executable_dir = Path(exe).resolve().parent

    def macho(image, inherited=()):
        image = Path(image).resolve()
        if image in visited:
            return
        visited.add(image)
        headers = subprocess.check_output([tool, '--macho', '--private-headers', str(image)], text=True)
        commands = []
        for block in re.split(r'Load command \d+', headers)[1:]:
            command = re.search(r'^\s*cmd (LC_\w+)', block, re.M)
            entry = re.search(r'^\s*(?:name|path) (.+) \(offset \d+\)\s*$', block, re.M)
            path_commands = {'LC_RPATH', 'LC_LOAD_DYLINKER', 'LC_ID_DYLIB',
                             'LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB',
                             'LC_LOAD_UPWARD_DYLIB', 'LC_LAZY_LOAD_DYLIB', 'LC_PREBOUND_DYLIB'}
            if command and command[1] in path_commands:
                if not entry:
                    sys.exit('link purity: unparsed Mach-O path command: ' + command[1])
                commands.append((command[1], entry[1]))
        if image == Path(exe).resolve() and [p for c, p in commands if c == 'LC_LOAD_DYLINKER'] != ['/usr/lib/dyld']:
            sys.exit('link purity: expected the fixed Darwin loader')

        def expand(path):
            return path.replace('@loader_path', str(image.parent)).replace(
                '@executable_path', str(executable_dir))

        rpaths = [expand(path) for command, path in commands if command == 'LC_RPATH']
        for path in rpaths:
            if not path.startswith('/'):
                sys.exit('link purity: unresolved Mach-O runtime search path: ' + path)
            allowed(path)
        rpaths += list(inherited)
        for command, path in commands:
            if command == 'LC_LOAD_DYLINKER':
                if path != '/usr/lib/dyld':
                    sys.exit('link purity: unexpected Darwin loader: ' + path)
            if command not in ('LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB', 'LC_LOAD_UPWARD_DYLIB', 'LC_LAZY_LOAD_DYLIB', 'LC_PREBOUND_DYLIB'):
                continue
            if os.path.normpath(path) != path:
                sys.exit('link purity: noncanonical install name: ' + path)
            # The OS shared cache is a runtime ABI dependency, represented at link
            # time by stubs in the pinned SDK, never by the host filesystem.
            if path.startswith(('/usr/lib/', '/System/Library/')):
                sdk = Path(os.environ['CONCRETE_SLICE0_SYSROOT'])
                stub = sdk / path.lstrip('/')
                candidates = [stub, stub.with_suffix('.tbd') if stub.suffix == '.dylib'
                              else Path(str(stub) + '.tbd')]
                for candidate in candidates:
                    if candidate.is_file():
                        allowed(str(candidate))
                        break
                else:
                    sys.exit('link purity: install name absent from SDK: ' + path)
                continue
            if path.startswith('@rpath/'):
                candidates = [Path(p) / path[len('@rpath/'):] for p in rpaths]
                resolved = next((p for p in candidates if p.is_file()), None)
                if resolved is None:
                    sys.exit('link purity: unresolved Mach-O dependency: ' + path)
            else:
                resolved = Path(expand(path))
            allowed(str(resolved))
            macho(resolved, rpaths)

    macho(exe)
else:
    tool = os.environ['CONCRETE_SLICE0_LINKER_BIN'] + '/readelf'
    headers = subprocess.check_output([tool, '-W', '-l', '-d', exe], text=True)
    def elf_dynamic(headers, image):
        if re.search(r'\((?:FILTER|AUXILIARY|AUDIT|DEPAUDIT)\)', headers):
            sys.exit('link purity: unsupported ELF dependency injection tag')
        for paths in re.findall(r'\((?:RUNPATH|RPATH)\).*?\[([^\]]*)\]', headers):
            # glibc disables an entirely empty tag; an empty component of a
            # nonempty colon list instead searches cwd and is rejected below.
            if not paths:
                continue
            for path in paths.split(':'):
                if '$ORIGIN' in path or '${ORIGIN}' in path:
                    path = os.path.normpath(path.replace('${ORIGIN}', str(Path(image).resolve().parent))
                                            .replace('$ORIGIN', str(Path(image).resolve().parent)))
                if not path.startswith('/') or '$' in path:
                    sys.exit(f'link purity: relative, empty or unresolved runtime search path in {image}: {path!r}')
                allowed(path)
        for path in re.findall(r'\(NEEDED\).*?\[([^\]]*)\]', headers):
            if '/' in path:
                allowed(path)

    elf_dynamic(headers, exe)
    loaders = re.findall(r'Requesting program interpreter: ([^\]]+)', headers)
    if len(loaders) != 1:
        sys.exit('link purity: expected one ELF interpreter')
    for path in loaders:
        allowed(path)
    # Ask the declared loader to resolve the complete dependency graph, including
    # transitive bare DT_NEEDED names. --list does not execute the program. Empty
    # environment removes LD_PRELOAD/LD_LIBRARY_PATH and other loader injections.
    resolved = subprocess.check_output([loaders[0], '--inhibit-cache', '--list', str(Path(exe).resolve())],
                                       env={'LC_ALL': 'C'}, text=True)
    for line in resolved.splitlines():
        line = line.strip()
        if re.fullmatch(r'linux-vdso\.so\.\d+ \(0x[0-9a-f]+\)', line):
            continue  # kernel-provided virtual DSO, not a filesystem input
        match = re.fullmatch(r'(?:\S+ => )?(/\S+) \(0x[0-9a-f]+\)', line)
        if not match:
            sys.exit('link purity: unresolved loader output: ' + line)
        allowed(match[1])
        dynamic = subprocess.check_output([tool, '-W', '-d', match[1]], text=True)
        elf_dynamic(dynamic, match[1])
