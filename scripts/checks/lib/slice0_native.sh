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
# `control DIRECTORY` links DIRECTORY/control.s alone into control.exe, under the same policy: the run
# checks' controls (slice0_artifacts.sh) are hand-written assembly built exactly like the examples.
control=0
if [ "${1:-}" = control ] && [ "$#" = 2 ]; then control=1; set -- "$2" -; fi
# The public driver invokes bound mode only inside its private publication staging directory.
bound=0
verify=0
case "${1:-}" in
  bound) bound=1; shift ;;
  verify) verify=1; shift ;;
esac
# `sanitize` builds only llvm.san.exe: the LLVM example with ASan (README gate 10; no UBSan, see there).
[ "$#" = 2 ] || { [ "$#" = 3 ] && [[ "$3" = llvm || "$3" = lcb || "$3" = sanitize ]]; } ||
  { echo 'usage: slice0_native.sh DIRECTORY LCB [llvm|lcb|sanitize] | control DIRECTORY | {bound|verify} STAGING-DIRECTORY LCB [llvm|lcb] | target' >&2; exit 2; }
if [ "${3:-}" = sanitize ] && { [ "$bound" = 1 ] || [ "$verify" = 1 ]; }; then
  echo 'slice0_native: sanitize requires monolithic LLVM input' >&2
  exit 2
fi
lcb=$2
if [ "$verify" = 1 ]; then
  # Replay the complete retained build with the independently configured, pinned compiler.
  # Compare fixed inventories and their contents. A successful exit from the candidate
  # linker confers no authority to add an image to the publication.
  : "${CONCRETE_SLICE0_REFERENCE_CLANG:?run inside nix develop}"
  [[ "$CONCRETE_SLICE0_REFERENCE_CLANG" = /* ]] || exit 2
  stage=$(cd -- "$1" && pwd -P)
  replay=$(mktemp -d "$(dirname -- "$stage")/.concrete-replay.XXXXXXXX")
  trap 'rm -rf -- "$replay"' EXIT
  backends=()
  if [ "${3:-}" != lcb ]; then
    cp -- "$stage/component.ll" "$stage/launcher.ll" "$replay/"
    backends+=(llvm)
  fi
  if [ "${3:-}" != llvm ]; then
    cp -- "$stage/lcb_body.s" "$stage/lcb_rt.ssa" "$replay/"
    backends+=(lcb)
  fi
  CONCRETE_SLICE0_CLANG="$CONCRETE_SLICE0_REFERENCE_CLANG" \
    bash "${BASH_SOURCE[0]}" bound "$replay" "$lcb" "${@:3}"
  cd -- "$stage"
  for backend in "${backends[@]}"; do
    inventory="$backend.link.sha256"
    if ! cmp -s -- "$replay/$inventory" "$inventory"; then
      echo "slice0_native: $backend retained build differs from pinned replay" >&2
      exit 1
    fi
    sha256sum --quiet -c "$replay/$inventory"
    if [ ! -x "$backend.exe" ]; then
      echo "slice0_native: $backend image is not executable" >&2
      exit 1
    fi
  done
  # Enforce the retained file set as well as the digests, including rejecting symlinks.
  diff -qr --no-dereference -- "$replay" "$stage" >&2
  exit
fi
writer="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/slice0_write.py"
cd -- "$1"
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
  # the printed IR does not; the sanitizer build adds it to every definition (llvm.san.ll).
  sed -E 's/^(define .*) \{$/\1 sanitize_address {/' llvm.ll >llvm.san.ll
  [ "$(grep -c '^define ' llvm.san.ll)" = "$(grep -c '^define .* sanitize_address {$' llvm.san.ll)" ] ||
    { echo 'slice0_native: a definition lacks sanitize_address' >&2; exit 1; }
  san=(-fsanitize=address -fno-sanitize-recover=all)
  "${cc[@]}" "${san[@]}" -c llvm.san.ll -o llvm.san.o
  "${cc[@]}" "${san[@]}" "${link[@]}" llvm.san.o -o llvm.san.exe
  exit
fi
if [ "$control" = 1 ]; then
  "${cc[@]}" -c control.s -o control.o
  "${cc[@]}" "${link[@]}" control.o -o control.exe
  exit
fi
if [ "${3:-}" != lcb ]; then
  if [ "$bound" = 1 ]; then
    sha256sum component.ll launcher.ll >llvm.link.sha256
    "${cc[@]}" -c component.ll -o component.o
    sha256sum component.o >>llvm.link.sha256
    "${cc[@]}" -c launcher.ll -o launcher.o
    sha256sum launcher.o >>llvm.link.sha256
    sha256sum --quiet -c llvm.link.sha256
    "${cc[@]}" "${link[@]}" component.o launcher.o -o llvm.exe
    sha256sum --quiet -c llvm.link.sha256
    sha256sum llvm.exe >>llvm.link.sha256
  else
    "${cc[@]}" -c llvm.ll -o llvm.o
    "${cc[@]}" "${link[@]}" llvm.o -o llvm.exe
  fi
fi
if [ "${3:-}" != llvm ]; then
  if [ "$bound" = 1 ]; then sha256sum lcb_body.s lcb_rt.ssa >lcb.link.sha256; fi
  "${cc[@]}" -c lcb_body.s -o lcb_body.o
  if [ "$bound" = 1 ]; then sha256sum lcb_body.o >>lcb.link.sha256; fi
  rm -f -- lcb_rt.s
  "${clean[@]}" "$lcb" -t "$target" lcb_rt.ssa -o - | python3 "$writer" lcb_rt.s
  if [ "$bound" = 1 ]; then sha256sum lcb_rt.s >>lcb.link.sha256; fi
  "${cc[@]}" -c lcb_rt.s -o lcb_rt.o
  if [ "$bound" = 1 ]; then
    sha256sum lcb_rt.o >>lcb.link.sha256
    sha256sum --quiet -c lcb.link.sha256
  fi
  "${cc[@]}" "${link[@]}" lcb_body.o lcb_rt.o -o lcb.exe
  if [ "$bound" = 1 ]; then
    sha256sum --quiet -c lcb.link.sha256
    sha256sum lcb.exe >>lcb.link.sha256
  fi
fi
if [ "$bound" = 1 ]; then
  printf '%s\n' 'Runtime loader and system libraries are unbound environment premises.' >runtime-premises.txt
fi
