"""Real cgroup controls: small allocations pass, an over-limit child is OOM-killed.

Use the production manager path, not a mocked cgroup or a different launch env.
"""
import importlib.util
from pathlib import Path
import subprocess
import sys
import resource
spec = importlib.util.spec_from_file_location("bound", Path(__file__).resolve().parents[1] / "lib/trusted_memory_bound.py")
bound = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bound)
try:
    limit = bound.LIMIT if "--full" in sys.argv else 64*1024**2
    assert bound.run([sys.executable, "-I", "-c", "import resource; soft,hard=resource.getrlimit(resource.RLIMIT_AS); assert 0 < soft <= hard <= 64*1024**3; x=bytearray(1024*1024)"], limit=64*1024**2, seconds=15) == 0
    try:
        bound.run([sys.executable, "-I", "-c", f"x=bytearray({limit + 192*1024**2})"], limit=limit, seconds=60)
    except ValueError as error:
        assert str(error) == f"FAIL examples memory: cgroup MemoryMax exceeded ({limit} bytes)", str(error)
    else:
        raise AssertionError("over-limit control escaped the memory boundary")
    print("PASS real cgroup MemoryMax positive and over-limit controls")
except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
    print(str(error) if isinstance(error, RuntimeError) else "INCOMPLETE examples memory: manager unavailable or timed out", file=sys.stderr)
    sys.exit(2)
