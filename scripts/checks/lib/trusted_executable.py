"""Audit kernel-created executable mappings omitted from ptrace syscall events.

The kernel loads ELF PT_INTERP and shebang interpreters during exec itself. ELF
load/stack permissions also precede the first traced userspace mapping call.
"""
import importlib.util
import json
import os
from pathlib import Path
import struct
import re

_spec = importlib.util.spec_from_file_location('execve_names', Path(__file__).with_name('execve_names.py'))
strings = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(strings)


def check(path, closure, seen=None):
    seen = set() if seen is None else seen
    real = strings.checked_path(os.path.realpath(path), 'resolved executable path')
    if real in seen:
        raise ValueError('recursive kernel interpreter: ' + json.dumps(real))
    seen.add(real)
    with open(real, 'rb') as stream:
        header = stream.read(64)
        if header.startswith(b'#!'):
            stream.seek(0)
            line = stream.readline(257)
            # Linux truncates shebang buffers; never guess how that affects identity.
            if len(line) > 256 or not line.endswith(b'\n'):
                raise ValueError('unresolved kernel script interpreter')
            value = line[2:-1].strip(b' \t')
            if not value: raise ValueError('missing kernel script interpreter')
            interpreter = os.fsdecode(re.split(b'[ \t]', value, maxsplit=1)[0])
        elif header.startswith(b'\x7fELF'):
            if len(header) != 64 or header[4] != 2 or header[5] not in (1, 2):
                raise ValueError('unsupported executable ELF format')
            endian = '<' if header[5] == 1 else '>'
            offset = struct.unpack_from(endian + 'Q', header, 32)[0]
            size, count = struct.unpack_from(endian + 'HH', header, 54)
            length = os.fstat(stream.fileno()).st_size
            if size != 56 or count == 0 or count == 65535 or offset + size * count > length:
                raise ValueError('malformed executable ELF program headers')
            interpreter, stack = None, False
            for index in range(count):
                stream.seek(offset + index * size)
                kind, flags, start, _, _, filesz, _, _ = struct.unpack(endian + 'IIQQQQQQ', stream.read(size))
                if kind == 1 and flags & 1 and flags & 2:
                    raise ValueError('writable executable ELF segment has no immutable identity')
                if kind == 0x6474e551:
                    stack = True
                    if flags & 1: raise ValueError('executable ELF stack has no pinned identity')
                if kind == 3:
                    if interpreter is not None or not 1 < filesz <= 4096 or start + filesz > length:
                        raise ValueError('malformed ELF interpreter')
                    stream.seek(start); value = stream.read(filesz)
                    if value[-1:] != b'\0' or b'\0' in value[:-1]:
                        raise ValueError('malformed ELF interpreter')
                    interpreter = os.fsdecode(value[:-1])
            if not stack: raise ValueError('ELF stack permissions are unspecified')
        else:
            raise ValueError('unsupported kernel executable format: ' + json.dumps(real))
    if interpreter is not None:
        strings.checked_path(interpreter, 'kernel interpreter path')
        resolved = strings.checked_path(os.path.realpath(interpreter), 'resolved kernel interpreter path')
        def root(p):
            return '/'.join(p.split('/')[:4]) if p.startswith('/nix/store/') else ''
        if not os.path.isabs(interpreter) or root(interpreter) not in closure or root(resolved) not in closure:
            raise ValueError('unregistered kernel interpreter: ' + json.dumps(interpreter))
        check(resolved, closure, seen)
