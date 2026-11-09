import os
from pathlib import Path
import subprocess
import sys
import time
import signal
import json

# Per-command timings are emitted on ci-2 for calibration; no controls are sampled.
# 180s is provisional: 3x the old 60s bound exceeded by ownership.lean on D14.
# Coordinator must retain ci-2 measurements and calibrate this bound before acceptance.
def control_run(args, *, timeout=180, check=False, **kwargs):
    name = " ".join(Path(str(a)).name if str(a).endswith(".lean") else str(a) for a in args)
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

