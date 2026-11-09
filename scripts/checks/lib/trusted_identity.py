"""Lock-derived pipeline identity evidence. Never derive authority from ambient PATH."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

spec = importlib.util.spec_from_file_location("execve_names", Path(__file__).with_name("execve_names.py"))
parser = importlib.util.module_from_spec(spec)
spec.loader.exec_module(parser)
parse = parser.parse
parse_many = parser.parse_many
map_spec = importlib.util.spec_from_file_location("trusted_mappings", Path(__file__).with_name("trusted_mappings.py"))
mappings = importlib.util.module_from_spec(map_spec)
map_spec.loader.exec_module(mappings)

def realpath(path):
    parser.checked_path(str(path))
    return parser.checked_path(os.path.realpath(path), "resolved executable path")

def display(value):
    """Keep human diagnostics on one line with literal quotes/backslashes escaped."""
    return json.dumps(str(value), ensure_ascii=True)[1:-1].replace("'", "\\u0027")


RESOLVED_NAMES = {
    "lean": {"lean"},
    "leanc": {"leanc"},
    "lake": {"lake"},
    "lcb": {"lcb"},
    "git": {"git"},
    "ar": {"ar", "llvm-ar"},
    "clang": {"clang", "clang-21", ".clang-wrapped"},
    "clang-21": {"clang-21", "clang", ".clang-wrapped"},
    "ld": {"ld", "ld.bfd", "ld.lld", "ld64", "lld", ".ld-wrapped"},
    "ld64": {"ld64", "ld64.lld", "ld", ".ld-wrapped"},
    "ld.lld": {"ld.lld", "lld"},
    "as": {"as", ".as-wrapped"},
    "nix": {"nix"},
    "bash": {"bash", ".bash-wrapped"},
    "env": {"env", "coreutils"},
    "uname": {"uname", "coreutils"},
    "mktemp": {"mktemp", "coreutils"},
    "realpath": {"realpath", "coreutils"},
    "rm": {"rm", "coreutils"},
}


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
    identity = json.loads(run("nix", "eval", "--json", attr + ".trustedIdentity"))
    roots = sorted(set(identity["roots"]))
    lean = identity["lean"]
    native_clang = identity["nativeClang"]
    if realpath(os.environ.get("CONCRETE_SLICE0_CLANG", "")) != realpath(native_clang):
        raise ValueError("consumed native clang differs from the lock-derived compiler: " + os.environ.get("CONCRETE_SLICE0_CLANG", "<unset>"))
    expected_git = identity["git"]
    actual_git = realpath(shutil.which("git") or "")
    if actual_git != realpath(expected_git):
        raise ValueError(f"consumed git: {display(actual_git)}, expected {display(expected_git)}")
    closure = json.loads(run("nix", "path-info", "--json", "--recursive", *roots))
    # Nix versions encode path-info as either an object keyed by path or an array.
    if isinstance(closure, list): closure = {r["path"]: r for r in closure}
    for path, info in closure.items():
        if not path.startswith("/nix/store/") or not info.get("narHash"):
            raise ValueError(f"missing store hash for {path}")
    package_closures = {}
    def closure_of(pkg):
        if pkg not in package_closures:
            package_closures[pkg] = [p for p in run("nix", "path-info", "--recursive", pkg).splitlines() if p]
        return package_closures[pkg]

    root_closures = {r: closure_of(r) for r in roots}
    tool_closures = {
        tool: sorted({p for r in r_list for p in root_closures.get(r, [])})
        for tool, r_list in identity["toolRoots"].items()
    }
    # Each role enumerates its exact allowed tools and roots without inheriting tool_closures.
    role_closures = {
        role: {
            tool: sorted({p for package in packages for p in closure_of(package)})
            for tool, packages in role_tools.items()
        }
        for role, role_tools in identity["roleToolRoots"].items()
    }
    role_local_tools = {
        role: sorted(set(tools))
        for role, tools in identity["roleLocalTools"].items()
    }
    role_compilers = {role: sorted({realpath(p) for p in paths})
                      for role, paths in identity["roleCompilers"].items()}
    role_linkers = {
        role: {tool: sorted({realpath(str(Path(root) / "bin" / alias))
                            for root in roots for alias in RESOLVED_NAMES[tool]
                            if (Path(root) / "bin" / alias).is_file()})
               for tool in ("ar", "ld", "ld64", "ld.lld", "as")}
        for role, roots in identity["roleLinkers"].items()
    }
    for tool in ("lean", "lake"):
        actual = realpath(shutil.which(tool) or "")
        expected = realpath(lean + "/bin/" + tool)
        if actual != expected: raise ValueError(f"consumed {tool}: {display(actual)}, expected {display(expected)}")
    pin = Path("lean-toolchain").read_text().strip().split(":")[-1].removeprefix("v")
    if not re.match(rf"^Lean \(version {re.escape(pin)}, ", run(lean + "/bin/lean", "--version")):
        raise ValueError("Lean version differs from lean-toolchain")
    return {"closure": closure, "tool_closures": tool_closures, "role_closures": role_closures,
            "role_local_tools": role_local_tools,
            "role_compilers": role_compilers, "role_linkers": role_linkers, "lean": lean,
            "inputs": {p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
                       for p in ("flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json")}}


def prepare(output):
    evidence = store_evidence()
    verify_packages()
    lcb = Path(".lake/packages/lean-compiler-backend/.lake/build/bin/lcb").resolve()
    evidence.update(lcb=str(lcb), lcb_sha256=hashlib.sha256(lcb.read_bytes()).hexdigest())
    Path(output).write_text(json.dumps(evidence, sort_keys=True) + "\n")


def check(logs, allowed, evidence, lcb, role):
    if isinstance(logs, (str, Path)):
        logs = [logs]
    allowed = set(Path(allowed).read_text().splitlines())
    evidence = json.loads(Path(evidence).read_text())
    for p, digest in evidence["inputs"].items():
        if hashlib.sha256(Path(p).read_bytes()).hexdigest() != digest:
            raise ValueError(f"identity input changed during audit: {p}")
    failed = False
    paths = parse_many(logs, paths=True)
    if not paths: raise ValueError("no successful exec records")
    lcb_real = realpath(lcb)
    if role not in evidence["role_closures"]:
        raise ValueError(f"unknown pipeline role: {role}")
    tool_closures = evidence["role_closures"][role]
    allowed_local = set(evidence.get("role_local_tools", {}).get(role, ["lcb"] if role == "native" else []))
    for path in paths:
        norm_path = os.path.normpath(path)
        name = os.path.basename(norm_path)
        if name not in allowed:
            print(f"FAIL tools: unlisted program '{display(name)}'")
            failed = True
        real = realpath(path)
        real_name = os.path.basename(real)
        path_root = "/".join(norm_path.split("/")[:4]) if norm_path.startswith("/nix/store/") else ""
        root = "/".join(real.split("/")[:4]) if real.startswith("/nix/store/") else ""
        local = (name == "lcb" and "lcb" in allowed_local and real_name == "lcb" and real == lcb_real)
        if local and (real != evidence.get("lcb") or
                hashlib.sha256(Path(real).read_bytes()).hexdigest() != evidence.get("lcb_sha256")):
            print(f"FAIL tools: changed local LCB executable: {display(real)}")
            failed = True
        allowed_roots = set(tool_closures.get(name, []))
        valid_store = (
            not local
            and name != "lcb"
            and real != lcb_real
            and path_root in allowed_roots
            and root in allowed_roots
            and root in evidence["closure"]
            and real_name in RESOLVED_NAMES.get(name, {name})
        )
        if name in {"clang", "clang-21"} and real not in evidence["role_compilers"][role]:
            print(f"FAIL tools: identity-mismatch role '{display(role)}' program '{display(name)}': {display(real)}")
            failed = True
            continue
        if name in {"ar", "ld", "ld64", "ld.lld", "as"} and real not in evidence["role_linkers"][role][name]:
            print(f"FAIL tools: identity-mismatch role '{display(role)}' program '{display(name)}': {display(real)}")
            failed = True
            continue
        if not local and not valid_store:
            print(f"FAIL tools: identity-mismatch role '{display(role)}' program '{display(name)}': {display(real)}")
            failed = True
        else:
            info = evidence["closure"].get(root, {})
            print("tool-identity " + json.dumps({"executed": path, "resolved": real,
                  "narHash": info.get("narHash"), "local": local, "role": role}, sort_keys=True))
    if failed:
        return 1
    mappings.check(logs, evidence, role)
    return 0


if __name__ == "__main__":
    try:
        if sys.argv[1] == "prepare": prepare(sys.argv[2])
        elif sys.argv[1] == "check":
            if len(sys.argv) < 7:
                raise ValueError("usage: trusted_identity.py check ROLE LOG [LOG ...] ALLOWED EVIDENCE LCB")
            sys.exit(check(sys.argv[3:-3], sys.argv[-3], sys.argv[-2], sys.argv[-1], sys.argv[2]))
        else: raise ValueError("unknown identity operation")
    except (ValueError, OSError, subprocess.SubprocessError) as e:
        sys.exit(f"FAIL identity: {e}")
