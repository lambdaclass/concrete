#!/usr/bin/env bash
# Native policy controls; called through bounded_run by slice0_repro.sh.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/host/lib" "$work/a" "$work/different/b" "$work/tmp-a" "$work/tmp-b"
# A small IR program exercises both ordinary control and sanitizer links, without Lake.
cat > "$work/a/llvm.ll" <<'IR'
define i32 @main() {
  ret i32 42
}
IR
"$CONCRETE_SLICE0_CLANG" -w -S "$work/a/llvm.ll" -o "$work/a/control.s"
cp "$work/a/llvm.ll" "$work/different/b/llvm.ll"
cp "$work/a/control.s" "$work/different/b/control.s"
for side in a different/b; do
  if [ "$side" = a ]; then
    export TMPDIR="$work/tmp-a"
  else
    export TMPDIR="$work/tmp-b"
    if [ "$(uname -s)" = Linux ]; then
      libc=$(find "$(cat "${CONCRETE_SLICE0_CLANG%/bin/clang}/nix-support/orig-libc")/lib" -maxdepth 1 -name libc.so | head -1)
      ln -s "$libc" "$work/host/lib/libc.so"
    fi
  fi
  # Ambient search/config variables must never enter the constructed toolchain environment.
  export LIBRARY_PATH="$work/host/lib" LD_RUN_PATH="$work/host/lib" NIX_LDFLAGS="-rpath $work/host/lib"
  bash "$root/scripts/checks/lib/slice0_native.sh" control "$work/$side"
  bash "$root/scripts/checks/lib/slice0_native.sh" "$work/$side" - sanitize
done
# An overridden sysroot is forbidden even when no host library is consumed.
if [ "$(uname -s)" = Linux ]; then
  (
    cd "$work/a"
    policy_root="$root/scripts/checks/lib"
    source "$policy_root/native_toolchain.sh"
    cc+=(--sysroot="$work/host")
    if link_native probe.exe control.o >refusal 2>&1; then
      echo 'FAIL link controls: surrogate sysroot accepted' >&2; exit 1
    fi
    grep -F "link command: driver differs from flake policy" refusal
  )
fi
for exe in control.exe llvm.san.exe; do
  cmp "$work/a/$exe" "$work/different/b/$exe"
  echo "ok link controls: $exe unchanged by host layout, cwd and TMPDIR"
done
# Deliberately bypass policy construction to create an impure executable. The validator
# must reject its metadata even though the consumed objects and trace are declared.
unset NIX_LDFLAGS LIBRARY_PATH LD_RUN_PATH
cd "$work/a"
mkdir -p "$work/host space/lib"
if [ "$(uname -s)" = Linux ]; then
  "$CONCRETE_SLICE0_CLANG" -Wl,-t -Wl,--build-id=none -Wl,-rpath,"$work/host space/lib" control.o -o impure.exe >impure.trace 2>&1
else
  "$CONCRETE_SLICE0_CLANG" -Wl,-t -Wl,-no_uuid -Wl,-rpath,"$work/host space/lib" control.o -o impure.exe >impure.trace 2>&1
fi
if python3 "$root/scripts/checks/lib/link_purity.py" impure.exe impure.trace control.o >refusal 2>&1; then
  echo 'FAIL link controls: host RUNPATH accepted' >&2; exit 1
fi
grep -F "link purity: undeclared path: $work/host space/lib" refusal
if [ "$(uname -s)" = Linux ]; then
  # A host alias into the store is still an undeclared filesystem input.
  ln -s "$(dirname "$libc")" "$work/host/alias"
  "$CONCRETE_SLICE0_CLANG" -w -Wl,-t -Wl,-rpath,"$work/host/alias" control.o -o alias.exe >alias.trace 2>&1
  if python3 "$root/scripts/checks/lib/link_purity.py" alias.exe alias.trace control.o >refusal 2>&1; then
    echo 'FAIL link controls: host alias RUNPATH accepted' >&2; exit 1
  fi
  grep -F "link purity: undeclared path: $work/host/alias" refusal
  # Independently test a host input alias in the trace of an otherwise pure binary.
  sed "s|$libc|$work/host/lib/libc.so|g" control.exe.trace >alias-input.trace
  if python3 "$root/scripts/checks/lib/link_purity.py" control.exe alias-input.trace control.o >refusal 2>&1; then
    echo 'FAIL link controls: host alias input accepted' >&2; exit 1
  fi
  grep -F "link purity: undeclared path: $work/host/lib/libc.so" refusal
fi
if CONCRETE_SLICE0_SYSROOT="$work/host" python3 "$root/scripts/checks/lib/link_policy.py" >refusal 2>&1; then
  echo 'FAIL link controls: effective policy mismatch accepted' >&2; exit 1
fi
grep -F 'link policy: effective setting differs: CONCRETE_SLICE0_SYSROOT' refusal
echo 'PASS native link controls'
