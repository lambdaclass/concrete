"""Python adapter to the single process specification owner."""
from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'lib'))
from scheduled_process import Invocation, execute, operation_clock


def process_run(argv, **options):
    context = Invocation.capture(options.pop('env', None),
                                 options.pop('cwd', None), options.pop('role', 'gate'))
    return execute(context.specification(argv, **options))
