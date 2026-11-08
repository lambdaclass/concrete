"""Lock-derived pipeline identity evidence. Never derive authority from ambient PATH."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import importlib.util
spec = importlib.util.spec_from_file_location("execve_names", Path(__file__).with_name("execve_names.py"))
parser = importlib.util.module_from_spec(spec)
spec.loader.exec_module(parser)
parse = parser.parse


def run(*args):
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.run(args, env=env, check=True, capture_output=True, text=True, timeout=120).stdout.strip()


def verify_packages():
    for package in json.loads(Path("lake-manifest.json").read_text())["packages"]:
        name = package["name"].removeprefix("«").removesuffix("»")
        d = str(Path(".lake/packages") / name)
        if (run("git", "-C", d, "rev-parse", "HEAD") != package["rev"]
                or run("git", "-C", d, "status", "--porcelain", "--untracked-files=all")):
            raise ValueError(f"dependency {name} is not clean at pinned revision {package['rev']}")


def store_evidence():
    system = run("nix", "eval", "--impure", "--raw", "--expr", "builtins.currentSystem")
    attr = f".#devShells.{system}.default"
    roots = json.loads(run("nix", "eval", "--json", attr + ".trustedPipelineRoots"))
    lean = run("nix", "eval", "--raw", attr + ".trustedLeanRoot")
    native_clang = run("nix", "eval", "--raw", attr + ".trustedNativeClang")
    if os.path.realpath(os.environ.get("CONCRETE_SLICE0_CLANG", "")) != os.path.realpath(native_clang):
        raise ValueError("consumed native clang differs from the lock-derived compiler: " + os.environ.get("CONCRETE_SLICE0_CLANG", "<unset>"))
    for tool, attribute in (("ssh-keygen", "trustedApprovalVerifier"), ("git", "trustedGit")):
        expected = run("nix", "eval", "--raw", attr + "." + attribute)
        actual = os.path.realpath(shutil.which(tool) or "")
        if actual != os.path.realpath(expected):
            raise ValueError(f"consumed {tool}: {actual}, expected {expected}")
    closure = json.loads(run("nix", "path-info", "--json", "--recursive", *roots))
    # Nix versions encode path-info as either an object keyed by path or an array.
    if isinstance(closure, list): closure = {r["path"]: r for r in closure}
    for path, info in closure.items():
        if not path.startswith("/nix/store/") or not info.get("narHash"):
            raise ValueError(f"missing store hash for {path}")
    for tool in ("lean", "lake"):
        actual = os.path.realpath(shutil.which(tool) or "")
        expected = os.path.realpath(lean + "/bin/" + tool)
        if actual != expected: raise ValueError(f"consumed {tool}: {actual}, expected {expected}")
    pin = Path("lean-toolchain").read_text().strip().split(":")[-1].removeprefix("v")
    if f"version {pin}," not in run(lean + "/bin/lean", "--version"):
        raise ValueError("Lean version differs from lean-toolchain")
    return {"closure": closure, "lean": lean,
            "inputs": {p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
                       for p in ("flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json")}}


def prepare(output):
    evidence = store_evidence()
    verify_packages()
    lcb = Path(".lake/packages/lean-compiler-backend/.lake/build/bin/lcb").resolve()
    evidence.update(lcb=str(lcb), lcb_sha256=hashlib.sha256(lcb.read_bytes()).hexdigest())
    Path(output).write_text(json.dumps(evidence, sort_keys=True) + "\n")


def check(log, allowed, evidence, lcb):
    allowed = set(Path(allowed).read_text().splitlines())
    evidence = json.loads(Path(evidence).read_text())
    for p, digest in evidence["inputs"].items():
        if hashlib.sha256(Path(p).read_bytes()).hexdigest() != digest:
            raise ValueError(f"identity input changed during audit: {p}")
    failed = False
    paths = parse(Path(log).read_text().splitlines(), paths=True)
    if not paths: raise ValueError("no successful exec records")
    for path in paths:
        name = os.path.basename(path)
        if name not in allowed:
            print(f"FAIL tools: unlisted program '{name}'")
            failed = True
        real = os.path.realpath(path)
        root = "/".join(real.split("/")[:4])
        local = real == os.path.realpath(lcb)
        if local and (real != evidence.get("lcb") or
                hashlib.sha256(Path(real).read_bytes()).hexdigest() != evidence.get("lcb_sha256")):
            print(f"FAIL tools: changed local LCB executable: {real}")
            failed = True
        if root not in evidence["closure"] and not local:
            print(f"FAIL tools: unexpected executable '{name}': {real}")
            failed = True
        else:
            info = evidence["closure"].get(root, {})
            print("tool-identity " + json.dumps({"executed": path, "resolved": real,
                  "narHash": info.get("narHash"), "local": real == os.path.realpath(lcb)}, sort_keys=True))
    return 1 if failed else 0

if __name__ == "__main__":
    try:
        if sys.argv[1] == "prepare": prepare(sys.argv[2])
        elif sys.argv[1] == "check": sys.exit(check(*sys.argv[2:]))
        else: raise ValueError("unknown identity operation")
    except (ValueError, OSError, subprocess.SubprocessError) as e:
        sys.exit(f"FAIL identity: {e}")
