#!/usr/bin/env bash
# Daily scheduled leg for gate 9 (R-0676 / R-0509): a private checkout with no
# compiler or dependency build products. PRs always run the real small fresh Lake
# compile/link control; this leg builds the entire Concrete/LCB pipeline from
# sources with its FIRST build traced and no network access during tracing.
set -euo pipefail
export LC_ALL=C LAKE_ARTIFACT_CACHE=false
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
source scripts/checks/lib/slice0_environment.sh
slice0_trusted_fresh_main() {
root=$PWD
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
# shellcheck source=scripts/checks/lib/slice0_sources.sh
source scripts/checks/lib/slice0_sources.sh
gate_limits 64 40000
work=$(mktemp -d)
trap 'rc=$?; bounded_summary || true
  if [ "$rc" = 0 ] && grep -Eqs "^(incomplete|runner-failed)" "${BOUNDED_LOG:-/nonexistent}"; then
    echo "INCOMPLETE: a bounded run timed out twice; not a pass"; rc=3
  fi
  rm -rf "${work:?}"; exit "$rc"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "slice0_trusted_fresh: python3 is required for bounded runs" >&2; exit 2; }
bounded() {
  local secs=$1 rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "slice0_trusted_fresh: INCOMPLETE (timed out twice): $*" >&2
  fi
  return "$rc"
}
# Hooks export repository-local Git variables; clear them before materializing or cloning.
# shellcheck source=scripts/checks/lib/git_sandbox.sh
source scripts/checks/lib/git_sandbox.sh
git_sandbox "$work/gitconfig"
# Require existing clean pins before Lake can load the project; never fetch dependencies.
bounded 30 python3 -I -c 'import runpy; runpy.run_path("scripts/checks/lib/trusted_identity.py")["verify_packages"]()'
bounded 600 lake env true
bash scripts/checks/compiler/build_identity.sh >/dev/null
bounded 120 git clone --shared --no-hardlinks "$root" "$work/source"
git ls-files -z -- "${slice0_sources[@]}" |
  tar --null -T - -cf - | tar -xf - -C "$work/source"
mkdir -p "$work/source/.lake/packages"
# Copy only tracked dependency sources and .git (which Lake checks against the
# manifest); no .lake/build artifacts are copied so the traced build compiles
# every dependency and Concrete module from scratch without network fetches.
for pkg in .lake/packages/*/; do
  pkg=$(basename "$pkg")
  mkdir -p "$work/source/.lake/packages/$pkg"
  (cd ".lake/packages/$pkg" && { git ls-files -z; printf '.git\0'; } | tar --null -T - -cf -) |
    tar -xf - -C "$work/source/.lake/packages/$pkg"
done
cd "$work/source"
export LAKE_ARTIFACT_CACHE=false LAKE_CACHE_DIR="$work/cache"
bounded 7200 bash scripts/checks/compiler/slice0_trusted_base.sh
echo "PASS slice0_trusted_fresh"

}
slice0_gate_environment slice0_trusted_fresh_main "$@"
