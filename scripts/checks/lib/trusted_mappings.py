"""Audit executable mmap/mprotect events, not library filename conventions.

strace -f -yy resolves mapping fds in the tracee's namespace. Any anonymous executable
mapping or executable mprotect is refused: there is no pinned file identity for it.
Each log is a distinct session; split syscalls are reconstructed per PID.
"""
import json
import os
from pathlib import Path
import re
import sys

import importlib.util
_spec = importlib.util.spec_from_file_location('execve_names', Path(__file__).with_name('execve_names.py'))
strings = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(strings)
PREFIX = strings.PID_PREFIX
CALL = re.compile(r'^(\w+)\(')
RESUMED = re.compile(r'^<\.\.\. (\w+) resumed>')


def fields(text):
    """Split syscall fields, preserving escaped fd paths, quotes and brackets."""
    result, start, quote, angle, depth, escape = [], 0, False, False, 0, False
    for i, c in enumerate(text):
        if escape:
            escape = False
            continue
        if c == '\\':
            escape = True
            continue
        if c == '"' and not angle:
            quote = not quote
        elif not quote:
            if c == '<': angle = True
            elif c == '>': angle = False
            elif not angle:
                if c in '([{': depth += 1
                elif c in ')]}': depth -= 1
                elif c == ',' and depth == 0:
                    result.append(text[start:i].strip()); start = i + 1
    if quote or angle or depth or escape:
        raise ValueError('malformed mapping arguments')
    return result + [text[start:].strip()]


def bits(text, allowed, context):
    tokens = set(text.split('|'))
    if not tokens or not tokens <= allowed:
        raise ValueError('unknown ' + context + ': ' + text)
    return tokens


def executable_mappings(lines):
    pending, found = {}, set()
    for raw in lines:
        match = PREFIX.fullmatch(raw.rstrip('\n'))
        if not match:
            raise ValueError('malformed mapping trace line')
        pid, body = match.group(1) or match.group(2) or 'main', match.group(3)
        call, resumed = CALL.match(body), RESUMED.match(body)
        if strings.KILL_LINE.fullmatch(body):
            pending.pop(pid, None)
            continue
        if not call and not resumed:
            if strings.EXIT_LINE.fullmatch(body) and pid in pending:
                raise ValueError('normal exit during unfinished mapping')
            continue
        kind = (call or resumed).group(1)
        if kind in {'execve', 'execveat'}: continue
        if kind not in strings.MEMORY_CALLS:
            raise ValueError('unsupported traced memory syscall: ' + kind)
        if call:
            if pid in pending: raise ValueError('overlapping mapping calls')
            if body.endswith('<unfinished ...>'):
                pending[pid] = (kind, body[:-len('<unfinished ...>')]); continue
        else:
            if pid not in pending: raise ValueError('resumed mapping has no unfinished call')
            expected, start = pending.pop(pid)
            if kind != expected: raise ValueError('mismatched resumed mapping')
            body = start + body[resumed.end():]
        result = re.fullmatch(r'\w+\((.*)\)\s+=\s+(.*)', body)
        if not result: raise ValueError('missing mapping result')
        args, outcome = fields(result.group(1)), result.group(2)
        if outcome == '?':
            pending[pid] = ('no-return', ''); continue
        if re.fullmatch(r'-1 E[A-Z0-9_]+(?: \([^\n]*\))?', outcome): continue
        if not re.fullmatch(r'(?:0x[0-9a-f]+|[0-9]+)(?:<.*>)?', outcome):
            raise ValueError('missing mapping result')
        if kind in {'open', 'openat', 'openat2'}:
            fd = re.search(r'<(.*)>$', outcome)
            if not fd: raise ValueError('opened file has no resolved identity')
            path = strings.decode_fd_path(re.sub(r'<(?:char|block) [0-9]+:[0-9]+>$', '', fd.group(1)))
            if re.fullmatch(r'/proc/(?:self|thread-self|[0-9]+)(?:/task/[0-9]+)?/mem', path):
                raise ValueError('process memory file access has no pinned identity')
            continue
        if kind in {'personality', 'ptrace', 'process_vm_writev', 'userfaultfd', 'remap_file_pages'}:
            raise ValueError('unsupported memory identity mutation: ' + kind)
        if kind == 'shmat':
            if len(args) != 3: raise ValueError('malformed shmat arguments')
            flags = bits(args[2], {'0', 'SHM_RDONLY', 'SHM_RND', 'SHM_REMAP', 'SHM_EXEC'}, 'shmat flags')
            if 'SHM_EXEC' in flags: raise ValueError('executable shmat has no pinned file identity')
            continue
        if kind not in {'mmap', 'mmap2', 'old_mmap', 'mprotect', 'pkey_mprotect'}:
            continue  # known operations cannot add executable permission or change backing bytes
        if len(args) != (6 if kind in {'mmap', 'mmap2', 'old_mmap'} else 4 if kind == 'pkey_mprotect' else 3):
            raise ValueError('malformed ' + kind + ' arguments')
        prot = bits(args[2], {'PROT_NONE', 'PROT_READ', 'PROT_WRITE', 'PROT_EXEC', 'PROT_GROWSDOWN', 'PROT_GROWSUP'}, 'mapping protection')
        if 'PROT_EXEC' not in prot: continue
        if kind in {'mprotect', 'pkey_mprotect'}:
            raise ValueError('executable ' + kind + ' has no pinned file identity')
        # Anonymous always wins over the fd: Linux ignores that fd entirely.
        flag_tokens = set(args[3].split('|'))
        if flag_tokens & {'MAP_ANONYMOUS', 'MAP_ANON'}:
            raise ValueError('executable anonymous or unresolved mapping')
        if 'PROT_WRITE' in prot: raise ValueError('writable executable mapping has no pinned file identity')
        if not prot <= {'PROT_READ', 'PROT_EXEC'}:
            raise ValueError('unsupported executable mapping protection')
        flags = bits(args[3], {'MAP_PRIVATE', 'MAP_DENYWRITE', 'MAP_EXECUTABLE', 'MAP_FIXED', 'MAP_FIXED_NOREPLACE', 'MAP_POPULATE', 'MAP_NONBLOCK', 'MAP_NORESERVE', 'MAP_STACK', 'MAP_LOCKED', 'MAP_SHARED', 'MAP_SHARED_VALIDATE'}, 'mapping flags')
        if flags & {'MAP_SHARED', 'MAP_SHARED_VALIDATE'}:
            raise ValueError('shared executable mapping has no immutable file identity')
        if 'MAP_PRIVATE' not in flags: raise ValueError('executable mapping is not private')
        fd = re.fullmatch(r'[0-9]+<([^<>]*)>', args[4])
        if not fd: raise ValueError('executable anonymous or unresolved mapping')
        path = strings.decode_fd_path(fd.group(1))
        if not os.path.isabs(path): raise ValueError('unresolved executable mapping path')
        found.add(path)
    if pending: raise ValueError('unfinished mapping calls have no result')
    return found


def check(logs, evidence, role):
    if role not in evidence['role_closures']:
        raise ValueError('unknown pipeline role: ' + role)
    closure = set().union(*map(set, evidence['role_closures'][role].values()))
    found = set()
    for log in logs:
        with open(log, encoding='utf-8') as f:
            found.update(executable_mappings(f))
    if not found:
        raise ValueError('no executable library mappings')
    for path in sorted(found):
        real = strings.checked_path(os.path.realpath(path), "resolved mapping path")
        if not os.path.exists(real):
            raise ValueError("missing executable mapping path: " + json.dumps(path))
        root = '/'.join(real.split('/')[:4]) if real.startswith('/nix/store/') else ''
        invoked_root = '/'.join(path.split('/')[:4]) if path.startswith('/nix/store/') else ''
        if root not in closure or invoked_root not in closure:
            raise ValueError('unregistered executable mapping: ' + json.dumps(path))
    print('ok   libraries: every executable mapping belongs to the pinned closure')


if __name__ == '__main__':
    try:
        check(sys.argv[3:], json.loads(Path(sys.argv[1]).read_text()), sys.argv[2])
    except (ValueError, OSError) as error:
        sys.exit(f'FAIL libraries: {error}')
