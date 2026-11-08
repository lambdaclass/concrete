#!/usr/bin/env bash
# Source after choosing target and changing to the object directory. This is the
# sole environment/driver construction, shared with the surrogate-sysroot control.
# Fixed epoch rather than wall clock or caller environment. No debug data is requested, and
# prefix maps also cover future debug additions. Separate objects avoid random linker temp names.
clean=(env -i "PATH=$CONCRETE_SLICE0_TOOL_PATH" NIX_STORE=/nix/store NIX_ENFORCE_PURITY=1 "NIX_BUILD_TOP=$PWD" "TMPDIR=${TMPDIR:-/tmp}"
  LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1)
cc=("${clean[@]}" "$CONCRETE_SLICE0_CLANG" --no-default-config
  -B"$CONCRETE_SLICE0_LINKER_BIN" --sysroot="$CONCRETE_SLICE0_SYSROOT"
  -O0 -g0 -w "-ffile-prefix-map=$PWD=." "-fdebug-prefix-map=$PWD=."
  -fdebug-compilation-dir=.)
# Do not derive the deployment target from the macOS version on the build host.
[ "$target" != aarch64-macos ] || cc+=(--target=arm64-apple-macosx15.0 -mmacosx-version-min=15.0)
# Every link is traced and validated before the executable is released to callers.
link_native() {
  local output=$1; shift
  rm -f "$output"
  if ! "${cc[@]}" "${link[@]}" -Wl,-t "$@" -o "$output" >"$output.trace" 2>&1; then
    cat "$output.trace" >&2; return 1
  fi
  if ! python3 "$policy_root/link_purity.py" "$output" "$output.trace" "$@"; then
    rm -f "$output"; return 1
  fi
}
