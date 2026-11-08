#!/usr/bin/env bash
# Shared native build policy. Input/output names and link order are fixed, relative to cwd.
# Use the pinned Nix compiler; discard ambient compiler/linker flags and wrapper injections.
set -euo pipefail
: "${CONCRETE_SLICE0_CLANG:?run inside nix develop}"
policy_root=$(cd "$(dirname "$0")" && pwd)
: "${CONCRETE_SLICE0_LINK_POLICY:?run inside nix develop}"
python3 "$policy_root/link_policy.py"
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) target=x86_64-linux; link=(-Wl,--build-id=none) ;;
  Linux-aarch64) target=aarch64-linux; link=(-Wl,--build-id=none) ;;
  Darwin-arm64) target=aarch64-macos; link=(-Wl,-no_uuid) ;;
  *) echo 'slice0_native: unsupported target' >&2; exit 2 ;;
esac
if [ "${1:-}" = target ]; then echo "$target"; exit; fi
# `control DIRECTORY` links DIRECTORY/control.s alone into control.exe, under the same policy: the run
# checks' controls (slice0_artifacts.sh) are hand-written assembly built exactly like the examples.
control=0
if [ "${1:-}" = control ] && [ "$#" = 2 ]; then control=1; set -- "$2" -; fi
# `sanitize` builds only llvm.san.exe: the LLVM example with ASan (README gate 10; no UBSan, see there).
[ "$#" = 2 ] || { [ "$#" = 3 ] && [ "$3" = sanitize ]; } ||
  { echo 'usage: slice0_native.sh DIRECTORY LCB [sanitize] | control DIRECTORY | target' >&2; exit 2; }
lcb=$2
cd "$1"
# shellcheck source=scripts/checks/lib/native_toolchain.sh
source "$policy_root/native_toolchain.sh"
if [ "${3:-}" = sanitize ]; then
  # ASan instruments only functions carrying `sanitize_address`, which clang's C front end adds and
  # the printed IR does not; the sanitizer build adds it to every definition (llvm.san.ll).
  sed -E 's/^(define .*\)) \{$/\1 sanitize_address {/' llvm.ll >llvm.san.ll
  [ "$(grep -c '^define ' llvm.san.ll)" = "$(grep -c '^define .* sanitize_address {$' llvm.san.ll)" ] ||
    { echo 'slice0_native: a definition lacks sanitize_address' >&2; exit 1; }
  san=(-fsanitize=address -fno-sanitize-recover=all)
  "${cc[@]}" "${san[@]}" -c llvm.san.ll -o llvm.san.o
  sanlink=("${san[@]}")
  if [ "$target" = aarch64-macos ]; then
    # Clang's implicit ASan link adds @executable_path, an undeclared runtime
    # search directory. Name the pinned runtime and its search path explicitly.
    runtime="${CONCRETE_SLICE0_CLANG%/bin/clang}/resource-root/lib/darwin"
    sanlink+=(-fno-sanitize-link-runtime "$runtime/libclang_rt.asan_osx_dynamic.dylib"
      "-Wl,-rpath,$runtime")
  fi
  link_native llvm.san.exe "${sanlink[@]}" llvm.san.o
  exit
fi
if [ "$control" = 1 ]; then
  "${cc[@]}" -c control.s -o control.o
  link_native control.exe control.o
  exit
fi
"${cc[@]}" -c llvm.ll -o llvm.o
"${cc[@]}" -c lcb_body.s -o lcb_body.o
"${clean[@]}" "$lcb" -t "$target" lcb_rt.ssa -o lcb_rt.s
"${cc[@]}" -c lcb_rt.s -o lcb_rt.o
link_native llvm.exe llvm.o
link_native lcb.exe lcb_body.o lcb_rt.o
