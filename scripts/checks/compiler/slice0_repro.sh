#!/usr/bin/env bash
# Slice-0 reproducibility gate (Concrete/README.md, gate 8). Two independent builds of the compiler
# and of LCB, from two source copies with different paths and TMPDIRs, emit every bir: row in the
# test manifest and build it natively; every stage is compared byte for byte, with no normalization.
# The two sides share only the pinned Lean toolchain and the dependency SOURCES (each side compiles
# them itself, with no Lake artifact cache). The comparator is checked by negative controls: a flipped byte, a missing artifact
# and a differing inventory must each be refused.
#
#   slice0_repro.sh [MANIFEST]   (inside nix develop, from the repository root)
#
# MANIFEST gets the build identity and one hash per artifact; CI compares the manifests of two
# independent runners of the same target.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
# shellcheck source=scripts/checks/lib/slice0_sources.sh
source scripts/checks/lib/slice0_sources.sh
gate_limits 64 40000
work=$(mktemp -d)
# On every exit, the bounded-run summary (recovered and INCOMPLETE runs) goes to stdout before the
# scratch directory is removed, and a gate that would pass with an INCOMPLETE run exits 3 instead.
trap 'rc=$?; bounded_summary || true
  if [ "$rc" = 0 ] && grep -Eqs "^(incomplete|runner-failed)" "${BOUNDED_LOG:-/nonexistent}"; then
    echo "INCOMPLETE: a bounded run timed out twice; not a pass"; rc=3
  fi
  rm -rf "${work:?}"; exit "$rc"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "slice0_repro: python3 is required for bounded runs" >&2; exit 2; }
# bounded SECS CMD...: CMD through bounded_run (stdin /dev/null, one re-run on a timeout; a second
# timeout returns 124 with no output and is reported INCOMPLETE, never a pass).
bounded() {
  local secs=$1 rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "slice0_repro: INCOMPLETE (timed out twice): $*" >&2
  fi
  return "$rc"
}
manifest=${1:-}
[ -z "$manifest" ] || manifest=$(realpath -m "$manifest")
compare=(python3 -I "$root/scripts/checks/lib/repro_compare.py")
# Materialize the dependency checkouts (no build): they must be the manifest's clean revisions,
# which build_identity.sh checks before anything is built.
bounded 600 lake env true
bash scripts/checks/compiler/build_identity.sh > "$work/identity"
target=$(bash scripts/checks/lib/slice0_native.sh target)
# Working source edits are included (the identity records their digest); no build product is.
for side in first second; do
  d="$work/$side/checkout-with-$side-path"
  mkdir -p "$d/.lake/packages" "$work/$side/tmp"
  git ls-files -z -- "${slice0_sources[@]}" |
    tar --null -T - -cf - | tar -xf - -C "$d"
  # Dependency checkouts: tracked files and .git (which Lake checks against the manifest) only.
  for pkg in .lake/packages/*/; do
    pkg=$(basename "$pkg")
    mkdir -p "$d/.lake/packages/$pkg"
    (cd ".lake/packages/$pkg" && { git ls-files -z; printf '.git\0'; } | tar --null -T - -cf -) |
      tar -xf - -C "$d/.lake/packages/$pkg"
  done
  (
    cd "$d"
    export TMPDIR="$work/$side/tmp" LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1
    # No Lake artifact cache: each side compiles everything it uses.
    export LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$work/$side/lake-cache"
    bounded 3000 lake build Concrete lean-compiler-backend/lcb >"$work/$side/build.log" 2>&1 ||
      { tail -50 "$work/$side/build.log" >&2; echo "slice0_repro: $side: build failed" >&2; exit 1; }
    lcb="$d/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
    bounded 30 python3 -I scripts/checks/lib/test_manifest.py rows tests/manifest.tsv > test-rows
    awk -F '\t' '{print $1}' test-rows > names
    awk -F '\t' '$2 ~ /^bir:/ {sub(/^bir:/, "", $2); print $2}' test-rows | LC_ALL=C sort > manifest-programs
    [ -s names ] || { echo 'slice0_repro: no examples' >&2; exit 1; }
    bounded 120 lake env lean --run scripts/checks/compiler/slice0_bir.lean list | LC_ALL=C sort > emitter-programs
    if ! cmp -s manifest-programs emitter-programs; then
      echo 'slice0_repro: test manifest inventory differs from emitter' >&2; exit 1
    fi
    while IFS=$'\t' read -r name program expect; do
      case "$program" in
        bir:*)
          read -r -a expectation <<<"$expect"
          bounded 120 lake env lean --run scripts/checks/compiler/slice0_bir.lean emit "${program#bir:}" "$d/out/$name" "$target" "${expectation[@]}"
          bounded 120 bash scripts/checks/lib/slice0_native.sh "$d/out/$name" "$lcb"
          ;;
        *)
          echo "slice0_repro: $name: unsupported program $program" >&2; exit 1
          ;;
      esac
    done < test-rows
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
