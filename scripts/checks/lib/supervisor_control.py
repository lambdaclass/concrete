"""Bounded Linux orphan lifecycle probe; one fork, never a fork loop."""
import os
from pathlib import Path
import signal
import sys
import tempfile
import time
from bounded_exec import supervise, Cancelled, TREE_KEYS


def command(mode, destination):
    pid = os.fork()
    if pid == 0:
        if os.fork() != 0: os._exit(0)
        os.setsid()
        if mode == 'cancel': signal.signal(signal.SIGTERM, signal.SIG_IGN)
        Path(destination).write_text(str(os.getpid()))
        if mode == 'exit': os._exit(0)
        time.sleep(5)
        os._exit(0)
    os.waitpid(pid, 0)
    end = time.monotonic() + 1
    while not Path(destination).exists() and time.monotonic() < end: time.sleep(.005)
    pid = int(Path(destination).read_text())
    if mode == 'cancel':
        os.kill(os.getppid(), signal.SIGTERM)
        time.sleep(5)
    else:
        time.sleep(.2)
        if Path(f'/proc/{pid}').exists():
            sys.exit('FAIL supervisor orphan: mid-run orphan not reaped')


def main(mode):
    for key in TREE_KEYS: os.environ.pop(key, None)
    baseline = len(list(Path('/proc/self/task').iterdir()))
    with tempfile.TemporaryDirectory() as scratch:
        destination = Path(scratch) / 'pid'
        try:
            try:
                outcome = supervise([sys.executable, __file__, '--command', mode, str(destination)], 2, .05)
            except Cancelled as error:
                outcome = 'cancel ' + str(error.sig)
            expected = 'exit 0' if mode == 'exit' else 'cancel 15'
            if outcome != expected: return 1
            pid = int(destination.read_text())
            if Path(f'/proc/{pid}').exists():
                sys.exit('FAIL supervisor orphan: cancelled orphan not reaped')
            children = set()
            for path in Path('/proc/self/task').glob('*/children'):
                children.update(path.read_text().split())
            if children or len(list(Path('/proc/self/task').iterdir())) != baseline:
                sys.exit('FAIL supervisor orphan: task count did not return to baseline')
            print('PASS supervisor orphan: ' + mode + '; tasks returned to baseline')
            return 0
        finally:
            # Even planted regressions cannot leak a live task or zombie.
            if destination.exists():
                pid = int(destination.read_text())
                try: os.kill(pid, signal.SIGKILL)
                except ProcessLookupError: pass
                try: os.waitpid(pid, 0)
                except ChildProcessError: pass


if __name__ == '__main__':
    if sys.argv[1] == '--command': command(*sys.argv[2:])
    else: sys.exit(main(sys.argv[1]))
