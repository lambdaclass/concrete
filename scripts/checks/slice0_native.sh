#!/usr/bin/env bash
# Shared native build policy. Input/output names and link order are fixed, relative to cwd.
# Use the pinned Nix compiler; discard ambient compiler/linker flags and wrapper injections.
set -euo pipefail
: "${CONCRETE_SLICE0_CLANG:?run inside nix develop}"
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) target=x86_64-linux; link=(-Wl,--build-id=none) ;;
  Linux-aarch64) target=aarch64-linux; link=(-Wl,--build-id=none) ;;
  Darwin-arm64) target=aarch64-macos; link=(-Wl,-no_uuid) ;;
  *) echo 'slice0_native: unsupported target' >&2; exit 2 ;;
esac
if [ "${1:-}" = target ]; then echo "$target"; exit; fi
# `sanitize` builds only llvm.san.exe: the LLVM example with ASan and UBSan (README gate 10).
[ "$#" = 2 ] || { [ "$#" = 3 ] && [ "$3" = sanitize ]; } ||
  { echo 'usage: slice0_native.sh DIRECTORY LCB [sanitize]' >&2; exit 2; }
lcb=$2
cd "$1"
# Fixed epoch rather than wall clock or caller environment. No debug data is requested, and
# prefix maps also cover future debug additions. Separate objects avoid random linker temp names.
clean=(env -i "PATH=$PATH" "HOME=$HOME" "TMPDIR=${TMPDIR:-/tmp}"
  LC_ALL=C TZ=UTC SOURCE_DATE_EPOCH=1 ZERO_AR_DATE=1)
cc=("${clean[@]}" "$CONCRETE_SLICE0_CLANG" --no-default-config
  -O0 -g0 -w "-ffile-prefix-map=$PWD=." "-fdebug-prefix-map=$PWD=."
  -fdebug-compilation-dir=.)
# Do not derive the deployment target from the macOS version on the build host.
[ "$target" != aarch64-macos ] || cc+=(--target=arm64-apple-macosx15.0 -mmacosx-version-min=15.0)
if [ "${3:-}" = sanitize ]; then
  # ASan instruments only functions carrying `sanitize_address`, which clang's C front end adds and
  # the printed IR does not; the sanitizer build adds it to every definition (llvm.san.ll). UBSan's
  # checks are inserted by the C front end, so on IR input only its runtime is linked.
  sed -E 's/^(define .*\)) \{$/\1 sanitize_address {/' llvm.ll >llvm.san.ll
  [ "$(grep -c '^define ' llvm.san.ll)" = "$(grep -c '^define .* sanitize_address {$' llvm.san.ll)" ] ||
    { echo 'slice0_native: a definition lacks sanitize_address' >&2; exit 1; }
  san=(-fsanitize=address,undefined -fno-sanitize-recover=all)
  "${cc[@]}" "${san[@]}" -c llvm.san.ll -o llvm.san.o
  "${cc[@]}" "${san[@]}" "${link[@]}" llvm.san.o -o llvm.san.exe
  exit
fi
"${cc[@]}" -c llvm.ll -o llvm.o
"${cc[@]}" -c lcb_body.s -o lcb_body.o
"${clean[@]}" "$lcb" -t "$target" lcb_rt.ssa -o lcb_rt.s
"${cc[@]}" -c lcb_rt.s -o lcb_rt.o
"${cc[@]}" "${link[@]}" llvm.o -o llvm.exe
"${cc[@]}" "${link[@]}" lcb_body.o lcb_rt.o -o lcb.exe
