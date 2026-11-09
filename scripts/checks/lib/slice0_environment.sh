# shellcheck shell=bash
# One explicit environment policy for slice-0 native and trusted-base launches.
# Values are supplied by the owning launcher, never copied wholesale from env.
slice0_clean_environment() {
  clean=(env -i "PATH=$1" "HOME=$2" "TMPDIR=$3"
    LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1)
}

# Run a declared shell function with a lock-derived PATH and private configuration/cache roots.
# The parent shell and Nix evaluator are the bootstrap trust boundary, not audited compiler jobs.
slice0_gate_environment() (
  set -euo pipefail
  local entry=$1; shift
  source scripts/checks/lib/bounded.sh
  local sandbox system launch_path clang rc=0
  sandbox=$(mktemp -d)
  trap 'rm -rf -- "$sandbox"' EXIT
  mkdir -p "$sandbox/home" "$sandbox/tmp"
  slice0_clean_environment "$PATH" "$sandbox/home" "$sandbox/tmp"
  system=$(bounded_run 30 -- "${clean[@]}" nix eval --offline --impure --raw --expr builtins.currentSystem)
  launch_path=$(bounded_run 60 -- "${clean[@]}" nix eval --offline --raw ".#devShells.$system.default.trustedLaunchPath")
  clang=$(bounded_run 60 -- "${clean[@]}" nix eval --offline --raw ".#devShells.$system.default.trustedIdentity.nativeClang")
  slice0_clean_environment "$launch_path" "$sandbox/home" "$sandbox/tmp"
  bounded_run 86400 -- "${clean[@]}" "CONCRETE_SLICE0_CLANG=$clang" \
    LAKE_ARTIFACT_CACHE=false "LAKE_CACHE_DIR=$sandbox/lake-cache" LEAN_NUM_THREADS=2 \
    bash --noprofile --norc -c "$(declare -f "$entry"); $entry \"\$@\"" bash "$@" || rc=$?
  return "$rc"
)
