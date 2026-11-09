"""Exercise an isolated clock owner with one injected observation and duration clock."""
import datetime as dt
import importlib.util
from pathlib import Path
import sys


def main(helper):
    spec = importlib.util.spec_from_file_location('isolated_clock', helper)
    owner = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = owner
    spec.loader.exec_module(owner)
    observation = dt.datetime(2026, 11, 9, tzinfo=dt.timezone.utc)
    reads = []
    def wall():
        reads.append(True)
        return observation
    ticks = iter([100, 102, 105, 109])
    clock = owner.operation_clock(wall=wall, monotonic=lambda: next(ticks))
    assert clock.observation == observation
    specification = owner.Invocation.capture({}, Path.cwd()).specification(
        ['true'], timeout=10, deadline=clock.deadline(10))
    assert specification['deadline'] == 112
    assert clock.instant() == observation + dt.timedelta(seconds=5)
    assert clock.instant() == observation + dt.timedelta(seconds=9)
    assert len(reads) == 1
    return 0


if __name__ == '__main__':
    try: sys.exit(main(sys.argv[1]))
    except ValueError as error: sys.exit(str(error))
