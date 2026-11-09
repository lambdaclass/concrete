import os
from pathlib import Path
import subprocess
import sys
import time
import signal
import json

# Final ci-2 D15b bounds and margins are recorded in compiler/README.md.
_sequence = 0

def control_run(args, *, timeout=15, check=False, **kwargs):
    global _sequence
    _sequence += 1
    name = f"#{_sequence} " + " ".join(Path(str(a)).name if str(a).endswith(".lean") else str(a) for a in args)
    started = time.monotonic()
    with subprocess.Popen(args, start_new_session=True, **kwargs) as child:
        try:
            stdout, stderr = child.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            os.killpg(child.pid, signal.SIGKILL)
            child.communicate()
            print(f"INCOMPLETE trusted-base control: {name} (timeout {timeout}s)", file=sys.stderr)
            raise SystemExit(2)
        finally:
            print("control-timing " + json.dumps({"control": name, "seconds": round(time.monotonic()-started, 3),
                                                 "bound_seconds": timeout}), file=sys.stderr)
        result = subprocess.CompletedProcess(args, child.returncode, stdout, stderr)
        if check:
            result.check_returncode()
        return result

