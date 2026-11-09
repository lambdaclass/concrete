"""Python adapter to the single process specification owner."""
import os
from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'lib'))
from scheduled_process import Invocation, execute


def process_run(argv, **options):
    context = Invocation.capture(options.pop('env', dict(os.environ)),
                                 options.pop('cwd', Path.cwd()), options.pop('role', 'gate'))
    return execute(context.specification(argv, **options))
