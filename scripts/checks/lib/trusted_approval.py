"""Verify owner authority over one registry revision, without a second component list.

The allowed-signers file is supplied by protected CI configuration, never by this
checkout. The detached SSH signature signs the exact approval JSON bytes under
namespace concrete-trusted-base, principal owner. Golden updates cannot approve.
"""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys


def registry_rows(text):
    return "".join(line + "\n" for line in text.splitlines()
                   if line.startswith(("component ", "premise "))).encode()


def verify(report, record, signers):
    record = Path(record)
    if not record.is_file(): raise ValueError("owner approval record missing")
    payload = record.read_bytes()
    approval = json.loads(payload)
    if set(approval) != {"schema", "registry_revision", "registry_sha256", "approval"} or approval["schema"] != 1:
        raise ValueError("invalid owner approval schema")
    revision = approval["registry_revision"]
    if len(revision) != 40 or any(c not in "0123456789abcdef" for c in revision):
        raise ValueError("approval must name a full registry revision")
    if not approval["approval"].strip(): raise ValueError("empty owner approval statement")
    if not signers or not Path(signers).is_file():
        raise ValueError("protected owner allowed-signers file missing")
    result = subprocess.run(["ssh-keygen", "-Y", "verify", "-f", signers, "-I", "owner",
                             "-n", "concrete-trusted-base", "-s", str(record) + ".sig"],
                            input=payload, capture_output=True, timeout=30)
    if result.returncode: raise ValueError("invalid owner approval signature")
    git_env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    subprocess.run(["git", "cat-file", "-e", revision + ":Concrete/Premises.lean"],
                   env=git_env, check=True, capture_output=True, timeout=30)
    subprocess.run(["git", "merge-base", "--is-ancestor", revision, "HEAD"],
                   env=git_env, check=True, capture_output=True, timeout=30)
    for source in ("Concrete/Premises.lean", "flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json"):
        approved_source = subprocess.run(["git", "show", revision + ":" + source],
            env=git_env, check=True, capture_output=True, timeout=30).stdout
        if approved_source != Path(source).read_bytes():
            raise ValueError("trust input " + source + " differs from owner-approved revision " + revision)
    digest = hashlib.sha256(registry_rows(Path(report).read_text())).hexdigest()
    if digest != approval["registry_sha256"]:
        raise ValueError("registry differs from owner-approved revision " + revision)
    print("ok   owner-approved registry revision " + revision)

if __name__ == "__main__":
    try:
        verify(sys.argv[1], sys.argv[2], os.environ.get("CONCRETE_OWNER_ALLOWED_SIGNERS", ""))
    except (ValueError, OSError, subprocess.SubprocessError) as e:
        sys.exit("FAIL approval: " + str(e))
