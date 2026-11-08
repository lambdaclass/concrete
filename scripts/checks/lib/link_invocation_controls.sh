#!/usr/bin/env bash
# Refusal properties of the production constructor. Optional historical helper
# extracts its unchanged cc/environment construction for differential testing.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"
policy_root="$root/scripts/checks/lib"
source "$policy_root/native_toolchain.sh"
printf 'define i32 @main() { ret i32 42 }\n' >control.ll
"${cc[@]}" -c control.ll -o control.o
mkdir -p host/lib
cp control.o host/external.o
ln -s "$work/host/external.o" external.o
printf 'SECTIONS {} INSERT AFTER .text;\n' >host/layout.ld
if [ "$#" = 1 ]; then
  # The old helper has no link_native function. Extract only its constructor,
  # then use its exact control-link command. No checker is retrofitted to it.
  python3 - "$1" <<'PY' >historical.sh
import pathlib, sys
s = pathlib.Path(sys.argv[1]).read_text()
print(s[s.index('clean=('):s.index('if [ "${3:-}" = sanitize ]; then')])
PY
  target=$(bash "$root/scripts/checks/lib/slice0_native.sh" target)
  source historical.sh
  link=(-Wl,--build-id=none)
  link_native() { local output=$1; shift; "${cc[@]}" "${link[@]}" "$@" -o "$output"; }
fi
failures=0
refuse() {
  local label=$1; shift
  if "$@" >"$label.log" 2>&1; then
    echo "FAIL invocation control: $label accepted"
    failures=$((failures + 1))
  elif grep -Eq '^link (command|purity):' "$label.log" && [ ! -e out.exe ]; then
    echo "ok invocation control: $label refused"
  else
    echo "FAIL invocation control: $label failed without policy refusal"
    cat "$label.log" >&2
    failures=$((failures + 1))
  fi
}
refuse unused-library-search link_native out.exe "-L$work/host/lib" control.o
refuse unused-framework-search link_native out.exe "-F$work/host/lib" control.o
refuse unused-tool-search link_native out.exe "-B$work/host/lib" control.o
refuse sysroot-argument link_native out.exe "--sysroot=$work/host" control.o
refuse runtime-search link_native out.exe "-Wl,-rpath,$work/host/lib" control.o
refuse linker-script link_native out.exe "-Wl,-T,$work/host/layout.ld" control.o
refuse symlinked-object link_native out.exe external.o
cc+=(--sysroot="$work/host")
refuse driver-sysroot-override link_native out.exe control.o
[ "$failures" = 0 ]
