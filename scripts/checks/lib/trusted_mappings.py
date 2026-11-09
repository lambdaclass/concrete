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

PREFIX = re.compile(r'^\s*(?:(?:\[pid\s+(\d+)\]|(\d+))\s+)?(.*)$')
CALL = re.compile(r'^(mmap|mprotect)\(')
RESUMED = re.compile(r'^<\.\.\. (mmap|mprotect) resumed>')


def executable_mappings(lines):
    pending = {}
    found = set()
    for raw in lines:
        match = PREFIX.fullmatch(raw.rstrip('\n'))
        if not match:
            raise ValueError('malformed mapping trace line')
        pid, body = match.group(1) or match.group(2) or 'main', match.group(3)
        call, resumed = CALL.match(body), RESUMED.match(body)
        if body.startswith('+++ killed by '):
            pending.pop(pid, None)
            continue
        if not call and not resumed:
            if body.startswith('+++ exited') and pid in pending:
                raise ValueError('normal exit during unfinished mapping')
            continue
        if call:
            if pid in pending:
                raise ValueError('overlapping mapping calls')
            if body.endswith('<unfinished ...>'):
                pending[pid] = (call.group(1), body[:-len('<unfinished ...>')])
                continue
        else:
            if pid not in pending:
                raise ValueError('resumed mapping has no unfinished call')
            kind, start = pending.pop(pid)
            if kind != resumed.group(1):
                raise ValueError('mismatched resumed mapping')
            body = start + body[resumed.end():]
        result = re.search(r'\)\s+=\s+(0x[0-9a-f]+|0|-1\s+E[A-Z0-9_]+(?: \([^\n]*\))?)$', body)
        if not result:
            raise ValueError('missing mapping result')
        if result.group(1).startswith('-1') or 'PROT_EXEC' not in body[:result.start()]:
            continue
        if body.startswith('mprotect('):
            raise ValueError('executable mprotect has no pinned file identity')
        # mmap(addr, length, prot, flags, fd<absolute path>, offset). An fd annotation
        # is mandatory even if the file has no .so suffix. Never trust a filename heuristic.
        fd = re.search(r',\s*\d+<([^<>]+)>\s*,\s*(?:0x[0-9a-f]+|[0-9]+)\s*\)$', body[:result.start()+1])
        if not fd:
            raise ValueError('executable anonymous or unresolved mapping')
        path = re.sub(r'\\([0-7]{3})', lambda m: chr(int(m.group(1), 8)), fd.group(1))
        if not os.path.isabs(path) or any(ord(c) < 32 for c in path) or '\\' in path:
            raise ValueError('unresolved executable mapping path')
        found.add(path)
    if pending:
        raise ValueError('unfinished mapping calls have no result')
    return found


def check(logs, evidence):
    closure = set(evidence['closure'])
    found = set()
    for log in logs:
        with open(log, encoding='utf-8') as f:
            found.update(executable_mappings(f))
    if not found:
        raise ValueError('no executable library mappings')
    for path in sorted(found):
        real = os.path.realpath(path)
        root = '/'.join(real.split('/')[:4]) if real.startswith('/nix/store/') else ''
        invoked_root = '/'.join(path.split('/')[:4]) if path.startswith('/nix/store/') else ''
        if root not in closure or invoked_root not in closure:
            raise ValueError('unregistered executable mapping: ' + json.dumps(path))
    print('ok   libraries: every executable mapping belongs to the pinned closure')


if __name__ == '__main__':
    try:
        check(sys.argv[2:], json.loads(Path(sys.argv[1]).read_text()))
    except (ValueError, OSError) as error:
        sys.exit(f'FAIL libraries: {error}')
