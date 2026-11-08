#!/usr/bin/env bash
# Canonical build identity, with no timestamps, hostnames or checkout paths.
# The compiler sources are identified by commit AND by a digest of the working-tree content of
# every tracked input (lib/slice0_sources.sh), with `source_tree=dirty` when they differ from the
# commit, so a local edit never reports a committed identity. Every dependency checkout must be
# at the revision lake-manifest.json pins with no modified or untracked file (its ignored `.lake`
# build directory aside); otherwise this script fails. slice0_repro.sh copies only a dependency's
# tracked files and its `.git`, so ignored files never reach the compared builds.
set -euo pipefail
cd "$(dirname "$0")/../.."
: "${CONCRETE_SLICE0_CLANG:?run inside nix develop}"
# shellcheck source=scripts/checks/lib/slice0_sources.sh
source scripts/checks/lib/slice0_sources.sh
printf 'compiler_commit=%s\n' "$(git rev-parse HEAD)"
if [ -z "$(git status --porcelain --untracked-files=no -- "${slice0_sources[@]}")" ]; then
  echo 'source_tree=clean'
else
  echo 'source_tree=dirty'
fi
git ls-files -z -- "${slice0_sources[@]}" | python3 -c '
import hashlib,sys
paths=sorted(p for p in sys.stdin.buffer.read().split(b"\0") if p)
h=hashlib.sha256()
for p in paths:
    h.update(hashlib.sha256(open(p,"rb").read()).hexdigest().encode()+b"  "+p+b"\n")
print("source_sha256="+h.hexdigest())'
printf 'target=%s\n' "$(bash scripts/checks/lib/slice0_native.sh target)"
printf 'lean_pin=%s\n' "$(cat lean-toolchain)"
printf 'lean_version=%s\n' "$(lean --version)"
printf 'clang_version=%s\n' "$("$CONCRETE_SLICE0_CLANG" --version | head -1)"
python3 - <<'PY'
import hashlib,json,subprocess,sys
from pathlib import Path
lock=json.loads(Path('flake.lock').read_text())
print('nixpkgs_rev='+lock['nodes']['nixpkgs']['locked']['rev'])
for name in ('flake.nix','flake.lock','lake-manifest.json'):
    print(name+'_sha256='+hashlib.sha256(Path(name).read_bytes()).hexdigest())
for p in json.loads(Path('lake-manifest.json').read_text())['packages']:
    name=p['name'].removeprefix('«').removesuffix('»')  # Lake writes escaped names, «a-b»
    d=Path('.lake/packages')/name
    git=lambda *a: subprocess.run(['git','-C',str(d),*a],check=True,capture_output=True,text=True).stdout
    head=git('rev-parse','HEAD').strip()
    if head != p['rev'] or git('status','--porcelain','--untracked-files=all').strip():
        sys.exit(f"build_identity: {name}: checkout {head} is not the clean pinned {p['rev']}")
    print(name+'_rev='+p['rev'])
PY
