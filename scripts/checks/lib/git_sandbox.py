"""Use the canonical shell repository/config sandbox for Python subprocesses."""
import json
from pathlib import Path
import subprocess


def git_sandbox(config):
    return json.loads(subprocess.check_output(
        ["bash", str(Path(__file__).with_suffix(".sh")), str(config)], text=True))
