#!/usr/bin/env bash
# Slice-0 backend gate (PROVISIONAL, Concrete/README.md gates 4 and 6, hand-written BIR leg):
# every hand-written BIR example goes through the LLVM path (clang) and the LCB path (lcb + cc);
# each executable's observation (exit status, plus the trap line on fd 2) must equal the one the
# BIR semantics gives. Classification uses status AND stderr: 134 with the trap line is a trap,
# 134 without it is an exit. Run inside the nix devshell, from the repository root.
set -euo pipefail
root=$(pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
lake build Concrete lean-compiler-backend/lcb >/dev/null
lcb="$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
case "$(uname -m)" in
  x86_64) target=amd64_sysv ;;
  aarch64|arm64) target=arm64 ;;
  *) echo "slice0_backends: unsupported host $(uname -m)" >&2; exit 2 ;;
esac
emitter=(lake env lean --run scripts/checks/slice0_bir.lean)

observe() { # observe EXE -> prints the observation
  local exe=$1 status err
  set +e
  err=$("$exe" 2>&1 >/dev/null)
  status=$?
  set -e
  if [ "$status" -eq 134 ] && [ -n "$err" ]; then
    printf 'trap %s\n' "$err"
  else
    printf 'exit %s\n' "$status"
    [ -z "$err" ] || printf 'unexpected stderr: %s\n' "$err"
  fi
}

fail=0
count=0
for name in $("${emitter[@]}" list); do
  d="$work/$name"
  "${emitter[@]}" emit "$name" "$d"
  expected=$(cat "$d/expected")
  build() { "$@" >"$d/build.log" 2>&1 || { cat "$d/build.log" >&2; echo "slice0_backends: $name: build failed: $*" >&2; exit 1; }; }
  build clang -O0 -w "$d/llvm.ll" -o "$d/llvm.exe"
  build "$lcb" -t "$target" "$d/lcb.ssa" -o "$d/lcb.s"
  build cc "$d/lcb.s" -o "$d/lcb.exe"
  for backend in llvm lcb; do
    got=$(observe "$d/$backend.exe")
    count=$((count + 1))
    if [ "$got" = "$expected" ]; then
      echo "ok   $name [$backend]: $got"
    else
      echo "FAIL $name [$backend]: got '$got', expected '$expected'"
      fail=1
    fi
  done
done
[ "$count" -gt 0 ] || { echo "slice0_backends: no examples ran" >&2; exit 1; }
exit "$fail"
