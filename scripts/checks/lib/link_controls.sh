#!/usr/bin/env bash
# Native policy controls; called through bounded_run by slice0_repro.sh.
set -euo pipefail
native_helper=${CONCRETE_LINK_CONTROL_NATIVE_HELPER:-}
root=$(cd "$(dirname "$0")/../../.." && pwd)
native_helper=${native_helper:-$root/scripts/checks/lib/slice0_native.sh}
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
product_failures=0
for side in a different/b; do
  if [ "$side" = a ]; then
    export TMPDIR="$work/tmp-a"
  else
    export TMPDIR="$work/tmp-b"
  fi
  # Ambient search/config variables must never enter the constructed toolchain environment.
  export LIBRARY_PATH="$work/host/lib" LD_RUN_PATH="$work/host/lib" NIX_LDFLAGS="-rpath $work/host/lib"
  bash "$native_helper" control "$work/$side"
  bash "$native_helper" "$work/$side" - sanitize
  # Equality alone passed on 688e10a86. Check actual runtime metadata on BOTH
  # products too; historical helpers have no trace, so supply their exact local
  # object solely to exercise metadata validation (not to attest their inputs).
  for product in control llvm.san; do
    trace="$work/$side/$product.exe.trace"
    if [ ! -f "$trace" ]; then
      [ -n "${CONCRETE_LINK_CONTROL_NATIVE_HELPER:-}" ] || { echo "FAIL link controls: missing $trace" >&2; exit 1; }
      printf '%s\n' "$product.o" > "$trace"
    fi
    purity_mode=()
    if [ "$product" = llvm.san ]; then purity_mode=(--sanitize); fi
    if (cd "$work/$side"; python3 "$root/scripts/checks/lib/link_purity.py" "$product.exe" "$trace" "${purity_mode[@]}" "$product.o"); then
      echo "ok link controls: $side/$product executable purity"
    else
      echo "FAIL link controls: $side/$product executable purity" >&2
      product_failures=$((product_failures + 1))
    fi
  done
done
[ "$product_failures" = 0 ] || exit 1
if [ "$(uname -s)" = Linux ]; then
  for product in control llvm.san; do
    "$CONCRETE_SLICE0_LINKER_BIN/readelf" -W -l -d "$work/a/$product.exe" > "$work/hardening"
    grep -F GNU_RELRO "$work/hardening"
    grep -E 'BIND_NOW|FLAGS_1.*NOW' "$work/hardening"
  done
fi
for exe in control.exe llvm.san.exe; do
  cmp "$work/a/$exe" "$work/different/b/$exe"
  echo "ok link controls: $exe unchanged by host layout, cwd and TMPDIR"
done
# Expand the production vector, then append only the metadata perturbation.
unset NIX_LDFLAGS LIBRARY_PATH LD_RUN_PATH
cd "$work/a"
control_link() {
  local output=$1; shift
  CONCRETE_NATIVE_TMP="$work/tmp-a" PYTHONPATH="$root/scripts/checks/lib" python3 - "$output" "$@" <<'LINK'
import os, sys
from link_command import command, linker_environment
from link_policy import verify_policy
actual = command(verify_policy(), sys.argv[1], ['control.o']) + sys.argv[2:]
os.execve(actual[0], actual, linker_environment())
LINK
}
mkdir -p "$work/host space/lib"
control_link impure.exe -rpath "$work/host space/lib" >impure.trace 2>&1
if python3 "$root/scripts/checks/lib/link_purity.py" impure.exe impure.trace control.o >refusal 2>&1; then
  echo 'FAIL link controls: host RUNPATH accepted' >&2; exit 1
fi
grep -F "link purity: undeclared path: $work/host space/lib" refusal
# A host alias into the store remains an undeclared filesystem input on both targets.
libc=$(python3 - <<'INPUT'
from pathlib import Path
paths = Path('control.exe.trace').read_text().splitlines()
print(next(p for p in paths if p.startswith('/') and Path(p).is_file()))
INPUT
)
ln -s "$(dirname "$libc")" "$work/host/alias"
control_link alias.exe -rpath "$work/host/alias" >alias.trace 2>&1
if python3 "$root/scripts/checks/lib/link_purity.py" alias.exe alias.trace control.o >refusal 2>&1; then
  echo 'FAIL link controls: host alias RUNPATH accepted' >&2; exit 1
fi
grep -F "link purity: undeclared path: $work/host/alias" refusal
# Add a host alias input independently of the binary's runtime metadata.
ln -s "$libc" "$work/host/lib/$(basename "$libc")"
cp control.exe.trace alias-input.trace
printf '%s\n' "$work/host/lib/$(basename "$libc")" >>alias-input.trace
if python3 "$root/scripts/checks/lib/link_purity.py" control.exe alias-input.trace control.o >refusal 2>&1; then
  echo 'FAIL link controls: host alias input accepted' >&2; exit 1
fi
grep -F "link purity: undeclared path: $work/host/lib/$(basename "$libc")" refusal
if CONCRETE_SLICE0_SYSROOT="$work/host" python3 "$root/scripts/checks/lib/link_policy.py" >refusal 2>&1; then
  echo 'FAIL link controls: effective policy mismatch accepted' >&2; exit 1
fi
grep -F 'link policy: effective setting differs: CONCRETE_SLICE0_SYSROOT' refusal
# Unknown trace text (including the macOS warning from CI) must fail diagnostically,
# with no filesystem probe or traceback, even for an overlong line.
python3 - <<'WARN' >warning.trace
print('Warning: supplying the --target arm64-apple-macosx15.0 != arm64-apple-darwin ' + 'x' * 10000)
WARN
if python3 "$root/scripts/checks/lib/link_purity.py" control.exe warning.trace control.o >refusal 2>&1; then
  echo 'FAIL link controls: warning accepted as trace' >&2; exit 1
fi
grep -F 'link purity: unrecognized linker trace:' refusal
if grep -q 'Traceback' refusal; then cat refusal >&2; exit 1; fi
if [ "$(uname -s)" = Linux ]; then
  # Construct both runtime tag kinds using the production real-linker template. The executable metadata checker must reject them.
  (
    policy_root="$root/scripts/checks/lib"
    source "$policy_root/native_toolchain.sh"
    for mode in --enable-new-dtags --disable-new-dtags; do
      control_link empty.exe "$mode" >empty.trace 2>&1
      # Empty the declared runtime path string after linking to test the actual
      # dynamic string after linking to test both tag kinds independently.
      python3 - "$CONCRETE_SLICE0_LINKER_BIN/readelf" empty.exe <<'EMPTY'
import pathlib, re, subprocess, sys
p = pathlib.Path(sys.argv[2])
headers = subprocess.check_output([sys.argv[1], '-W', '-d', str(p)], text=True)
paths = re.findall(r'\((?:RUNPATH|RPATH)\).*?\[([^\]]*)\]', headers)
assert len(paths) == 1 and paths[0], headers
data = p.read_bytes()
needle = paths[0].encode() + b'\0'
assert data.count(needle) == 1
p.write_bytes(data.replace(needle, b'\0' + needle[1:]))
EMPTY
      if python3 "$policy_root/link_purity.py" empty.exe empty.trace control.o >refusal 2>&1; then
        echo 'FAIL link controls: empty runtime tag accepted' >&2; exit 1
      fi
      grep -F 'relative, empty or unresolved runtime path' refusal || { cat refusal >&2; exit 1; }
    done
  )
fi
bash "$root/scripts/checks/lib/link_invocation_controls.sh"
python3 "$root/scripts/checks/lib/link_parser_controls.py"
echo 'PASS native link controls'
