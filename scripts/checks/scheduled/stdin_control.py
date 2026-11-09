"""Independent short supervisor for a proven inherited pipe read.

A timeout alone is INCOMPLETE. Linux's wait-channel and fd identity prove the
planted reason before the existing supervisor cancels/reaps the entire group.
"""
import json
import os
from pathlib import Path
import sys
import threading
import time
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'lib'))
from bounded_exec import supervise


def main(helper, scratch):
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
    program = ('import json,os,sys; from pathlib import Path; '
               'Path(sys.argv[1]).write_text(json.dumps(dict(pid=os.getpid()))); '
               'sys.stdin.buffer.read(); Path(sys.argv[2]).touch()')
    try:
        with os.fdopen(reader, 'rb') as source:
            outcome = supervise([sys.executable, '-I', helper, '--cwd', str(scratch),
                                 '--timeout', '5', '--', sys.executable, '-c', program,
                                 str(marker), str(complete)], 0.7, 0.2, stdin=source,
                                 on_started=lambda pid: watcher.start())
    finally:
        stop.set()
        if watcher.ident is not None: watcher.join()
        os.close(writer)  # no EOF until supervision and cancellation have completed
    if outcome == 'timeout' and confirmed.is_set() and not complete.exists():
        pid = json.loads(marker.read_text())['pid']
        if Path(f'/proc/{pid}').exists():
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
