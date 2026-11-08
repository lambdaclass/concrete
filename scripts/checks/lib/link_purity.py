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
from link_paths import allowed

verify_policy()
exe, trace, *args = sys.argv[1:]
objects = {}
for arg in args:
    if arg.endswith('.o'):
        allowed(arg, local=True)
        objects[str(Path.cwd() / arg)] = str(Path(arg).resolve())

lines = Path(trace).read_text().splitlines()
seen = set()
for line in lines:
    line = line.strip()
    if not line:
        continue
    # GNU ld -t: a filename, or an archive filename followed by (member).
    # Darwin ld -t: a filename. Never probe arbitrary diagnostic text as a path.
    match = re.fullmatch(r'((?:/|[A-Za-z0-9_.-])[^\n()]*\.(?:o|a|so(?:\.[0-9]+)*|dylib|tbd))(?:\([^()/]+\))?', line)
    if not match or len(match[1].encode()) > 4096:
        sys.exit('link purity: unrecognized linker trace: ' + line[:500])
    path = match[1]
    try:
        if path in args:
            allowed(path, local=True)
        elif path in objects:
            allowed(Path(path).name, local=True)
        else:
            allowed(path)
        if not Path(path).is_file():
            sys.exit('link purity: missing traced input: ' + path)
        seen.add(str(Path(path).resolve()))
    except (OSError, ValueError) as error:
        sys.exit('link purity: invalid traced input: ' + str(error)[:500])
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
            if command and command[1] == 'LC_DYLD_ENVIRONMENT':
                sys.exit('link purity: unsupported LC_DYLD_ENVIRONMENT')
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
            if command not in ('LC_ID_DYLIB', 'LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB', 'LC_LOAD_UPWARD_DYLIB', 'LC_LAZY_LOAD_DYLIB', 'LC_PREBOUND_DYLIB'):
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
    def elf_path(path, image):
        origin = str(Path(image).resolve().parent)
        path = re.sub(r'\$(?:\{ORIGIN\}|ORIGIN(?=/|$))', lambda _: origin, path)
        if not path.startswith('/') or '$' in path:
            sys.exit(f'link purity: relative, empty or unresolved runtime path in {image}: {path!r}')
        # Do not normalize before allowed(): that would erase symlink/.. traversal.
        # Expanded runtime paths must satisfy the same canonical closure contract.
        allowed(path)

    def elf_dynamic(headers, image):
        if re.search(r'\((?:FILTER|AUXILIARY|AUDIT|DEPAUDIT)\)', headers):
            sys.exit('link purity: unsupported ELF dependency injection tag')
        for paths in re.findall(r'\((?:RUNPATH|RPATH)\).*?\[([^\]]*)\]', headers):
            for path in paths.split(':'):
                elf_path(path, image)
        for path in re.findall(r'\(NEEDED\).*?\[([^\]]*)\]', headers):
            if '/' in path or '$' in path:
                elf_path(path, image)

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
