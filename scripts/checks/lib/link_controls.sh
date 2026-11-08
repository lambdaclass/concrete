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
    if (cd "$work/$side"; python3 "$root/scripts/checks/lib/link_purity.py" "$product.exe" "$trace" "$product.o"); then
      echo "ok link controls: $side/$product executable purity"
    else
      echo "FAIL link controls: $side/$product executable purity" >&2
      product_failures=$((product_failures + 1))
    fi
  done
done
[ "$product_failures" = 0 ] || exit 1
if [ "$(uname -s)" = Linux ]; then
  libc="$(cat "${CONCRETE_SLICE0_CLANG%/bin/clang}/nix-support/orig-libc")/lib/libc.so"
  ln -s "$libc" "$work/host/lib/libc.so"
fi
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
# Policy mutation controls: reject unused searches too, before the wrapper can
# silently discard them. Test caller args AND driver overrides separately.
(
  policy_root="$root/scripts/checks/lib"
  source "$policy_root/native_toolchain.sh"
  for option in "-L$work/host/lib" "-F$work/host/lib" "-B$work/host/lib" \
      "--sysroot=$work/host" "-Wl,-rpath,$work/host/lib" "-Wl,-T,$work/host/script"; do
    if link_native rejected.exe "$option" control.o >refusal 2>&1; then
      echo "FAIL link controls: accepted $option" >&2; exit 1
    fi
    grep -F 'link command: unapproved caller argument:' refusal
  done
  cp control.o "$work/host/outside.o"
  ln -s "$work/host/outside.o" outside.o
  if link_native rejected.exe outside.o >refusal 2>&1; then
    echo 'FAIL link controls: symlinked object accepted' >&2; exit 1
  fi
  grep -F 'link purity: undeclared path:' refusal
)
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
  # Construct empty runtime tags using the declared driver, then bypass only the
  # caller-argument check. The executable metadata checker must reject them.
  (
    policy_root="$root/scripts/checks/lib"
    source "$policy_root/native_toolchain.sh"
    for mode in --enable-new-dtags --disable-new-dtags; do
      "${cc[@]}" "${link[@]}" -Wl,-t "-Wl,$mode" control.o -o empty.exe >empty.trace 2>&1
      # The Nix wrapper appends its own nonempty RPATH. Empty the actual
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
echo 'PASS native link controls'
