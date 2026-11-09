"""One suite budget and absolute-deadline allocation policy.

Reserve ten percent of remaining time at each boundary for startup, comparison,
and cleanup. Sequential children split the remaining work allocation; modules
share their child's deadline because their costs differ substantially.
"""
from pathlib import Path
import sys
import time
sys.path.insert(0, str(Path(__file__).absolute().parent))
from process_adapter import process_run

SECONDS = 180
RESERVE = .10


def deadline():
    return time.monotonic() + SECONDS


def remaining(end):
    value = end - time.monotonic()
    if value <= 0:
        raise ValueError('INCOMPLETE scheduled suite: deadline exhausted')
    return value


def allocate(end, count=1):
    if count < 1: raise ValueError('FAIL scheduled suite: invalid allocation')
    return time.monotonic() + remaining(end) * (1 - RESERVE) / count


def launch(script, args):
    import os
    end = deadline()
    result = process_run(['bash', script, '--scheduled-context', str(end), *args],
                         env=dict(os.environ), cwd=Path.cwd(), timeout=remaining(end),
                         deadline=end)
    return result.returncode


if __name__ == '__main__':
    sys.exit(launch(sys.argv[1], sys.argv[2:]))
