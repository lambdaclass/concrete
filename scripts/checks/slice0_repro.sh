#!/usr/bin/env bash
# Slice-0 reproducibility gate (Concrete/README.md, gate 8). Two independent builds of the compiler
# and of LCB, from two source copies with different paths and TMPDIRs, emit every hand-written BIR
# example and build it natively; every stage is compared byte for byte, with no normalization.
# The two sides share only the pinned Lean toolchain and the dependency SOURCES (each side compiles
# them itself). The comparator is checked by negative controls: a flipped byte, a missing artifact
# and a differing inventory must each be refused.
#
#   slice0_repro.sh [MANIFEST]   (inside nix develop, from the repository root)
#
# MANIFEST gets the build identity and one hash per artifact; CI compares the manifests of two
# independent runners of the same target.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
# shellcheck source=scripts/checks/lib/slice0_sources.sh
source scripts/checks/lib/slice0_sources.sh
gate_limits 64 40000
work=$(mktemp -d)
trap 'rm -rf "${work:?}"' EXIT
manifest=${1:-}
[ -z "$manifest" ] || manifest=$(realpath -m "$manifest")
compare=(python3 "$root/scripts/checks/lib/repro_compare.py")
# The dependency checkouts must be the manifest's revisions; build_identity.sh refuses otherwise.
timeout -k 30 1800 lake build Concrete lean-compiler-backend/lcb >/dev/null
bash scripts/checks/build_identity.sh > "$work/identity"
target=$(bash scripts/checks/slice0_native.sh target)
# Working source edits are included (the identity records their digest); no build product is.
for side in first second; do
  d="$work/$side/checkout-with-$side-path"
  mkdir -p "$d/.lake/packages" "$work/$side/tmp"
  git ls-files -z -- "${slice0_sources[@]}" |
    tar --null -T - -cf - | tar -xf - -C "$d"
  # Dependency checkouts (with .git, which Lake checks against the manifest), without their builds.
  for pkg in .lake/packages/*/; do
    pkg=$(basename "$pkg")
    mkdir -p "$d/.lake/packages/$pkg"
    tar -C ".lake/packages/$pkg" --exclude=./.lake -cf - . | tar -xf - -C "$d/.lake/packages/$pkg"
  done
  (
    cd "$d"
    export TMPDIR="$work/$side/tmp" LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1
    timeout -k 30 3000 lake build Concrete lean-compiler-backend/lcb >"$work/$side/build.log" 2>&1 ||
      { tail -50 "$work/$side/build.log" >&2; echo "slice0_repro: $side: build failed" >&2; exit 1; }
    lcb="$d/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
    timeout -k 10 120 lake env lean --run scripts/checks/slice0_bir.lean list > names
    [ -s names ] || { echo 'slice0_repro: no examples' >&2; exit 1; }
    while read -r name; do
      timeout -k 10 120 lake env lean --run scripts/checks/slice0_bir.lean emit "$name" "$d/out/$name" "$target"
      timeout -k 10 120 bash scripts/checks/slice0_native.sh "$d/out/$name" "$lcb"
    done < names
  )
done
a="$work/first/checkout-with-first-path"
b="$work/second/checkout-with-second-path"
"${compare[@]}" "$a" "$b" "$work/identity" ${manifest:+"$manifest"}

# Negative controls: the comparator must refuse each perturbation of an identical pair.
control() { # control LABEL EXPECTED-MESSAGE SETUP...
  local label=$1 expect=$2 c="$work/ctl/$1" rc=0; shift 2
  mkdir -p "$c"
  cp -R "$a" "$c/a"; cp -R "$a" "$c/b"
  (cd "$c/b" && "$@")
  "${compare[@]}" "$c/a" "$c/b" "$work/identity" >"$c/log" 2>&1 || rc=$?
  if [ "$rc" = 0 ] || ! grep -qF -- "$expect" "$c/log"; then
    echo "FAIL slice0_repro control $label: not refused as '$expect'" >&2; cat "$c/log" >&2; exit 1
  fi
  echo "ok slice0_repro control $label: refused"
}
first=$(head -1 "$a/names")
flip() { python3 -c 'import sys;p=sys.argv[1];d=bytearray(open(p,"rb").read());d[len(d)//2]^=1;open(p,"wb").write(d)' "$1"; }
control flipped-byte "$first: lcb.exe:" flip "out/$first/lcb.exe"
control missing-artifact "$first: lcb_rt.o: missing" rm "out/$first/lcb_rt.o"
control inventory 'example inventory differs' sh -c 'echo extra >> names'
echo 'PASS slice0_repro'
