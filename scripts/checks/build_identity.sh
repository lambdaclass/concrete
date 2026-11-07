#!/usr/bin/env bash
# Canonical build identity, with no timestamps, hostnames or checkout paths.
set -euo pipefail
cd "$(dirname "$0")/../.."
: "${CONCRETE_SLICE0_CLANG:?run inside nix develop}"
printf 'compiler_commit=%s\n' "$(git rev-parse HEAD)"
printf 'target=%s\n' "$(bash scripts/checks/slice0_native.sh target)"
printf 'lean_pin=%s\n' "$(cat lean-toolchain)"
printf 'lean_version=%s\n' "$(lean --version)"
printf 'clang_version=%s\n' "$("$CONCRETE_SLICE0_CLANG" --version | head -1)"
python3 - <<'PY'
import hashlib,json
from pathlib import Path
lock=json.loads(Path('flake.lock').read_text())
print('nixpkgs_rev='+lock['nodes']['nixpkgs']['locked']['rev'])
for name in ('flake.nix','flake.lock','lake-manifest.json'):
    print(name+'_sha256='+hashlib.sha256(Path(name).read_bytes()).hexdigest())
for p in json.loads(Path('lake-manifest.json').read_text())['packages']:
    print(p['name']+'_rev='+p['rev'])
PY
