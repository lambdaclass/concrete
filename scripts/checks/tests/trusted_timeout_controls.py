"""Exercise the real timeout path without running Lean or a heavy suite."""
import importlib.util
from pathlib import Path
import subprocess
import sys
spec = importlib.util.spec_from_file_location("runner", Path(__file__).resolve().parents[1] / "lib/trusted_control_run.py")
r = importlib.util.module_from_spec(spec)
spec.loader.exec_module(r)
result = subprocess.run([sys.executable, "-I", "-c",
    "import runpy,sys; runpy.run_path(sys.argv[1])['control_run']([sys.executable,'-c','import time; time.sleep(30)'],timeout=0.05)",
    str(Path(r.__file__))], text=True, capture_output=True, timeout=5)
assert result.returncode == 2, result
assert "INCOMPLETE trusted-base control:" in result.stderr, result.stderr
assert "(timeout 0.05s)" in result.stderr, result.stderr
assert "Traceback" not in result.stderr, result.stderr
assert "PASS" not in result.stderr, result.stderr
print("PASS named control timeout is INCOMPLETE")
