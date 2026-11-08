#!/usr/bin/env python3
"""The recorded policy must be exactly the effective toolchain configuration."""
import json
import os
from pathlib import Path


def verify_policy():
    policy = json.loads(Path(os.environ['CONCRETE_SLICE0_LINK_POLICY']).read_text())
    if policy['version'] != 1:
        raise SystemExit('link policy: unsupported version')
    for key, value in policy['settings'].items():
        if os.environ.get(key) != value:
            raise SystemExit('link policy: effective setting differs: ' + key)
    return policy


if __name__ == '__main__':
    verify_policy()
