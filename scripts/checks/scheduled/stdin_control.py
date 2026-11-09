"""Independent short supervisor for a proven inherited pipe read.

A timeout alone is INCOMPLETE. Linux's wait-channel and fd identity prove the
planted reason before the existing supervisor cancels/reaps the entire group.
"""
import json
import os
import signal
from pathlib import Path
import sys
import threading
import time
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'lib'))
from bounded_exec import supervise


def main(helper, scratch, *adapter):
    scratch = Path(scratch)
    marker = scratch / 'reading.json'
    complete = scratch / 'eof'
    reader, writer = os.pipe()
    identity = os.fstat(reader)
    confirmed = threading.Event()
    stop = threading.Event()
    def observe():
        while not stop.wait(0.005):
            try:
                pid = json.loads(marker.read_text())['pid']
                fd = os.stat(f'/proc/{pid}/fd/0')
                channel = Path(f'/proc/{pid}/wchan').read_text().strip()
                if (fd.st_dev, fd.st_ino) == (identity.st_dev, identity.st_ino) and channel in {'pipe_read', 'anon_pipe_read'}:
                    confirmed.set()
            except (OSError, ValueError, KeyError):
                pass
    watcher = threading.Thread(target=observe)
    program = r"""
import json, os, signal, sys
from pathlib import Path
child = os.fork()
if child == 0:
    sys.stdin.buffer.read()
    os._exit(0)
Path(sys.argv[1]).write_text(json.dumps(dict(pid=os.getpid(), child=child, pgid=os.getpgrp())))
sys.stdin.buffer.read()
os.waitpid(child, 0)
Path(sys.argv[2]).touch()
"""
    command = list(adapter) if adapter else [sys.executable, '-I', helper, '--cwd', str(scratch), '--timeout', '5', '--']
    environment = {k: v for k, v in os.environ.items() if not k.startswith('BOUNDED_TREE_')}
    try:
        with os.fdopen(reader, 'rb') as source:
            outcome = supervise([*command, sys.executable, '-c', program, str(marker), str(complete)],
                                 0.7, 0.2, stdin=source, env=environment,
                                 on_started=lambda pid: watcher.start())
    finally:
        stop.set()
        if watcher.ident is not None: watcher.join()
        # Cleanup targets the recorded group even if a broken adapter escaped its
        # launcher. The independent supervisor is a Linux subreaper.
        if marker.exists():
            recorded = json.loads(marker.read_text())
            try: os.killpg(recorded['pgid'], signal.SIGKILL)
            except ProcessLookupError: pass
            for pid in (recorded['pid'], recorded['child']):
                try: os.waitpid(pid, os.WNOHANG)
                except ChildProcessError: pass
        os.close(writer)  # no EOF until supervision and cancellation have completed
    if outcome == 'timeout' and confirmed.is_set() and not complete.exists():
        recorded = json.loads(marker.read_text())
        if any(Path(f'/proc/{pid}').exists() for pid in (recorded['pid'], recorded['child'])):
            print('INCOMPLETE scheduled process stdin: child not reaped', file=sys.stderr)
            return 124
        print('FAIL scheduled process stdin: inherited open pipe', file=sys.stderr)
        return 1
    if outcome == 'exit 0' and complete.exists():
        return 0
    print('INCOMPLETE scheduled process stdin: unproven timeout', file=sys.stderr)
    return 124


if __name__ == '__main__':
    sys.exit(main(*sys.argv[1:]))
