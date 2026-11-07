#!/usr/bin/env bash
# Two fresh source copies, different cwd/TMPDIR, every stage compared without normalization.
# timeout is supplied by the pinned Nix coreutils on both supported runners.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
work=$(mktemp -d)
trap 'rm -rf "${work:?}"' EXIT
manifest=${1:-}
[ -z "$manifest" ] || manifest=$(realpath -m "$manifest")
timeout 1800 lake build Concrete lean-compiler-backend/lcb
bash scripts/checks/build_identity.sh > "$work/identity"
target=$(bash scripts/checks/slice0_native.sh target)
# Include working source edits during local verification; do not copy build products.
for side in first second; do
  d="$work/$side/checkout-with-$side-path"
  mkdir -p "$d" "$work/$side/tmp"
  git ls-files -z Concrete Concrete.lean ConcreteSpec scripts/checks lakefile.toml lean-toolchain lake-manifest.json |
    tar --null -T - -cf - | tar -xf - -C "$d"
  mkdir -p "$d/.lake"
  # Read-only dependencies and oleans are the same pinned compiler in both builds.
  ln -s "$root/.lake/packages" "$d/.lake/packages"
  ln -s "$root/.lake/build" "$d/.lake/build"
  (
    cd "$d"
    export TMPDIR="$work/$side/tmp" LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1
    timeout 120 lake env lean --run scripts/checks/slice0_bir.lean list > names
    [ -s names ] || { echo 'slice0_repro: no examples' >&2; exit 1; }
    while read -r name; do
      timeout 120 lake env lean --run scripts/checks/slice0_bir.lean emit "$name" "$d/out/$name" "$target"
      timeout 120 bash "$root/scripts/checks/slice0_native.sh" "$d/out/$name" \
        "$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
    done < names
  )
done
python3 - "$work" "$manifest" <<'PY'
import hashlib,sys
from pathlib import Path
work=Path(sys.argv[1])
a=work/'first/checkout-with-first-path'
b=work/'second/checkout-with-second-path'
if (a/'names').read_bytes() != (b/'names').read_bytes():
    sys.exit('FAIL slice0_repro: example inventory differs')
stages=['bir.txt','llvm.ll','lcb_job.txt','lcb_source.txt','lcb_body.ssa','lcb_body.s','lcb_rt.ssa',
        'lcb_rt.s','llvm.o','lcb_body.o','lcb_rt.o','llvm.exe','lcb.exe']
lines=(work/'identity').read_text().splitlines()
for name in (a/'names').read_text().splitlines():
    for stage in stages:
        hashes=[hashlib.sha256((d/'out'/name/stage).read_bytes()).hexdigest() for d in (a,b)]
        if hashes[0] != hashes[1]:
            sys.exit(f'FAIL slice0_repro: {name}: {stage}: {hashes[0]} != {hashes[1]}')
        lines.append(f'{hashes[0]}  {name}/{stage}')
    print(f'ok slice0_repro: {name}: all stages byte-identical')
if sys.argv[2]:
    Path(sys.argv[2]).write_text('\n'.join(lines)+'\n')
print('PASS slice0_repro')
PY
