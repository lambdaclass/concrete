"""Reachable aggregate examples-audit memory boundary (Linux cgroup v2).

Runs outside the audited command's syscall trace. systemd owns the whole child
cgroup, including descendants. No command from the checkout runs as a different
uid: sudo is used only for the manager, and the service explicitly drops uid/gid.
"""
import os
from pathlib import Path
import subprocess
import sys
import shutil
import uuid

LIMIT = 8 * 1024**3

def run(command, limit=LIMIT, seconds=600):
    if sys.platform != "linux":
        raise RuntimeError("INCOMPLETE examples memory: cgroup v2 requires Linux")
    if not Path("/sys/fs/cgroup/cgroup.controllers").is_file():
        raise RuntimeError("INCOMPLETE examples memory: cgroup v2 unavailable")
    env = dict(os.environ)
    # The gate's clean environment deliberately has no session bus authority.
    # Reconstruct only the standard kernel-uid-owned local user-manager address.
    env["XDG_RUNTIME_DIR"] = f"/run/user/{os.getuid()}"
    env["DBUS_SESSION_BUS_ADDRESS"] = f"unix:path={env['XDG_RUNTIME_DIR']}/bus"
    user = subprocess.run(["systemctl", "--user", "show-environment"], env=env,
                          capture_output=True, timeout=10).returncode == 0
    prefix = [] if user or os.getuid() == 0 else ["/usr/bin/sudo", "-n"]
    manager = ["--user"] if user else []
    unit = "concrete-examples-" + uuid.uuid4().hex + ".service"
    args = prefix + ["systemd-run", *manager, "--quiet", "--wait", "--pipe",
        "--service-type=exec", "--unit=" + unit,
        "--property=MemoryMax=" + str(limit), "--property=MemorySwapMax=0",
        "--property=MemoryAccounting=yes",
        "--property=OOMPolicy=kill", "--property=KillMode=control-group",
        "--property=RuntimeMaxSec=" + str(seconds)]
    if not user:
        args += ["--uid=" + str(os.getuid()), "--gid=" + str(os.getgid())]
    args += ["--working-directory=" + os.getcwd(), "--", shutil.which("env"), "-i"]
    args += [f"{k}={v}" for k, v in os.environ.items()]
    args += [shutil.which("bash"), str(Path(__file__).with_name("limited_exec.sh")), "65536", *command]
    ctl = prefix + ["systemctl", *manager]
    try:
        result = subprocess.run(args, env=env, timeout=seconds + 30)
        state = subprocess.run([*ctl, "show", unit, "--property=Result", "--value"],
                               env=env, capture_output=True, text=True, timeout=10)
        if state.stdout.strip() == "oom-kill":
            peak = subprocess.run([*ctl, "show", unit, "--property=MemoryPeak", "--value"],
                                  env=env, capture_output=True, text=True, timeout=10).stdout.strip()
            if not peak.isdecimal():
                raise RuntimeError("INCOMPLETE examples memory: OOM memory peak unavailable")
            if int(peak) < limit:
                raise RuntimeError("INCOMPLETE examples memory: OOM before MemoryMax reached")
            raise ValueError(f"FAIL examples memory: cgroup MemoryMax exceeded ({limit} bytes)")
        if state.stdout.strip() == "timeout":
            raise RuntimeError("INCOMPLETE examples memory: service timeout")
        if result.returncode and state.stdout.strip() not in ("exit-code", "signal", "core-dump"):
            raise RuntimeError("INCOMPLETE examples memory: cannot establish memory-limited service")
        return result.returncode
    finally:
        subprocess.run([*ctl, "stop", unit], env=env, capture_output=True, timeout=10)
        subprocess.run([*ctl, "reset-failed", unit], env=env, capture_output=True, timeout=10)

if __name__ == "__main__":
    try:
        sys.exit(run(sys.argv[1:]))
    except ValueError as error:
        sys.exit(str(error))
    except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
        print(str(error) if isinstance(error, RuntimeError) else
              "INCOMPLETE examples memory: manager unavailable or timed out", file=sys.stderr)
        sys.exit(2)
