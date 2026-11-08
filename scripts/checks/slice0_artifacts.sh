#!/usr/bin/env bash
# Slice-0 artifact gates (PROVISIONAL, Concrete/README.md gates 10, 11 and 12), over every
# hand-written BIR example, on the executables slice0_native.sh links:
#
# 10. Sanitizers: the LLVM example also built with ASan and UBSan (slice0_native.sh sanitize: ASan
#     instruments the printed IR once every definition carries sanitize_address; UBSan's checks
#     come from clang's C front end, so on IR only its runtime is linked) must
#     give the same observation as the BIR semantics (status, exact fd-2 bytes, empty fd 1), so no
#     sanitizer report. Its memory is bounded by ASan's hard_rss_limit_mb (the shadow reservation
#     rules out an address-space cap). Control: a body with an out-of-bounds load must be reported.
# 11. Binary size (report-only, never fails on a size): bytes and text size of each executable,
#     per backend and target, in REPORT/sizes.tsv.
# 12. Stack bound: lib/stack_bound.py derives, from the linked executable's disassembly, a bound on
#     the Concrete-owned stack below main's entry, covering every byte addressed relative to the
#     stack or frame pointer (so the red zone), with external libc calls listed with their entry
#     depth (their own use, and that _exit and abort do not return, are premises). On x86-64
#     Linux, a run under gdb (lib/stack_measure.py) must stay within it (the run measures the
#     stack pointer only). REPORT/stack.tsv. Controls: the analysis must refuse an unmodelled
#     stack-pointer write, an indirect call, recursion, a call to an unknown function, two depths
#     meeting, a stack pointer used as a value and an indexed stack address, and give the
#     expected bound on a known function and on a red-zone store; the comparison must reject a
#     measurement above the bound.
#
# usage: slice0_artifacts.sh [REPORT_DIR]. Run inside the nix devshell, from the repository root.
set -euo pipefail
report=${1:-}
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
: "${CONCRETE_SLICE0_LLVM:?run inside nix develop}"
objdump=("$CONCRETE_SLICE0_LLVM/llvm-objdump" -d --no-show-raw-insn)
case "$target" in
  x86_64-linux) arch=x86_64; objdump+=(--x86-asm-syntax=intel) ;;
  aarch64-*) arch=aarch64 ;;
esac
gdb=
if [ "$target" = x86_64-linux ]; then gdb=${CONCRETE_SLICE0_GDB:?the x86-64 Linux shell pins gdb}; fi
[ -n "$report" ] || report="$work/report"
mkdir -p "$report"
sizes="$report/sizes.tsv"
stack="$report/stack.tsv"
printf 'example\tbackend\ttarget\tbytes\ttext\n' >"$sizes"
printf 'example\tbackend\ttarget\tbound\tmeasured\texternals\n' >"$stack"
# hard_rss_limit_mb and LeakSanitizer are Linux-only (ASan on Darwin refuses detect_leaks); on
# macOS RLIMIT_AS is not enforced either, so there the job's memory is the bound, as for all runs.
asan=abort_on_error=0:symbolize=0
[ "$(uname -s)" != Linux ] || asan="$asan:hard_rss_limit_mb=1024:detect_leaks=1"
san_env=("ASAN_OPTIONS=$asan" UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=0:symbolize=0)
analyse=(timeout -k 10 60 python3 -I "$root/scripts/checks/lib/stack_bound.py" "$arch")
fail=0
examples=0

# within MEASURED BOUND: the run stayed within the static bound (and actually ran).
within() { [ "$1" -gt 0 ] && [ "$1" -le "$2" ]; }

# stack_check NAME BACKEND EXE: derive the bound, measure a run where supported, record both.
stack_check() {
  local name=$1 backend=$2 exe=$3 dis="$3.dis" out bound ext measured=not-run
  if ! timeout -k 10 60 "${objdump[@]}" "$exe" >"$dis" 2>"$dis.err"; then
    echo "FAIL $name [$backend] stack: disassembly failed: $(head -c 300 "$dis.err")"; fail=1; return
  fi
  if ! out=$("${analyse[@]}" "$dis" --ranges); then
    echo "FAIL $name [$backend] stack: $out"; fail=1; return
  fi
  bound=$(awk '$1 == "bound" {print $2}' <<<"$out")
  ext=$(awk '$1 == "external" {printf "%s%s@%s", sep, $2, $3; sep=","}' <<<"$out")
  if [ -n "$gdb" ]; then
    local ranges main m
    ranges=$(awk '$1 == "range" {printf "%s%s:%s", sep, $3, $4; sep=","}' <<<"$out")
    main=$(awk '$1 == "range" && $2 == "main" {print $3}' <<<"$out")
    m=$(STACK_RANGES=$ranges STACK_MAIN=$main timeout -k 10 300 \
      "$gdb" -batch -nx -x "$root/scripts/checks/lib/stack_measure.py" "$exe" 2>"$exe.gdb.err" |
      awk '$1 == "measured" || $1 == "error" {print}') || true
    measured=$(awk '$1 == "measured" {print $2}' <<<"$m")
    if [ -z "$measured" ] || ! within "$measured" "$bound"; then
      echo "FAIL $name [$backend] stack: bound $bound, run: ${m:-no measurement} $(head -c 300 "$exe.gdb.err")"
      fail=1
    fi
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$backend" "$target" "$bound" "$measured" "$ext" >>"$stack"
  echo "ok   $name [$backend] stack: bound $bound, measured $measured, externals ${ext:-none}"
}

size_report() { # size_report NAME BACKEND EXE
  local bytes text
  bytes=$(wc -c <"$3" | tr -d ' ')
  text=$(timeout -k 10 60 "$CONCRETE_SLICE0_LLVM/llvm-size" "$3" 2>/dev/null | awk 'NR == 2 {print $1}') || true
  printf '%s\t%s\t%s\t%s\t%s\n' "$1" "$2" "$target" "$bytes" "${text:-unavailable}" >>"$sizes"
}

names=$(emit_examples)
mkdir -p "$work/ctl"
for name in $names; do
  d="$work/ex/$name"
  "${emitter[@]}" emit "$name" "$d" "$target"
  build "$name" "$d/build.log" timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$d" "$lcb"
  build "$name" "$d/build-san.log" \
    timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$d" "$lcb" sanitize
  observe "$d/llvm.san.exe" "$d/san" 30 30 0 "${san_env[@]}"
  if judge "$d" "$d/san"; then
    echo "ok   $name [llvm+asan+ubsan]: status $(cat "$d/san.status")"
  else
    echo "FAIL $name [llvm+asan+ubsan]: status $(cat "$d/san.status") (expected $(cat "$d/status")), stderr" \
      "$(head -c 600 "$d/san.stderr")"
    fail=1
  fi
  for backend in llvm lcb; do
    size_report "$name" "$backend" "$d/$backend.exe"
    stack_check "$name" "$backend" "$d/$backend.exe"
  done
  examples=$((examples + 1))
done

# Control (gate 10): an out-of-bounds load in the body must be reported by ASan.
c="$work/ctl/asan"
"${emitter[@]}" emit "${names%%$'\n'*}" "$c" "$target"
sed -i.orig 's/^define i32 @concrete_body() {$/define i32 @concrete_body_orig() {/' "$c/llvm.ll"
cat >>"$c/llvm.ll" <<'LL'

@concrete_ctl = global [4 x i8] zeroinitializer
define i32 @concrete_body() {
entry:
  %p = getelementptr i8, ptr @concrete_ctl, i64 4
  %v = load volatile i8, ptr %p
  %r = call i32 @concrete_body_orig()
  ret i32 %r
}
LL
build control-asan "$c/build.log" timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$c" "$lcb"
build control-asan "$c/build-san.log" \
  timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$c" "$lcb" sanitize
observe "$c/llvm.san.exe" "$c/san" 30 30 0 "${san_env[@]}"
if ! judge "$c" "$c/san" && grep -q 'AddressSanitizer' "$c/san.stderr"; then
  echo "ok   control: ASan reports an out-of-bounds load"
else
  echo "FAIL control: ASan did not report an out-of-bounds load (status $(cat "$c/san.status"))"; fail=1
fi

# Controls (gate 12): the analysis on synthetic disassembly.
ctl_dis() { # ctl_dis NAME: write a synthetic x86-64 disassembly from stdin
  { echo "Disassembly of section .text:"; cat; } >"$work/ctl/$1.dis"
  echo "$work/ctl/$1.dis"
}
known=$(ctl_dis known <<'D'

0000000000001000 <concrete_body>:
    1000:	push	rbp
    1001:	mov	rbp, rsp
    1004:	sub	rsp, 0x10
    1008:	add	rsp, 0x10
    100c:	pop	rbp
    100d:	ret

0000000000001010 <main>:
    1010:	push	rbp
    1011:	mov	rbp, rsp
    1014:	sub	rsp, 0x20
    1018:	call	0x1000 <concrete_body>
    101d:	je	0x1024 <main+0x14>
    101f:	call	0x2000 <write@plt>
    1024:	leave
    1025:	ret
D
)
# main: 8 + 0x20 = 40 deep; the call pushes 8; the body goes 8 + 0x10 = 24 deeper: 72.
if [ "$(python3 -I scripts/checks/lib/stack_bound.py x86_64 "$known")" = "$(printf 'bound 72\nexternal write 48')" ]; then
  echo "ok   control: stack bound of a known function"
else
  echo "FAIL control: stack bound of a known function: $(python3 -I scripts/checks/lib/stack_bound.py x86_64 "$known")"
  fail=1
fi
refuse() { # refuse NAME: stdin is the main function's body after its header
  local f
  f=$(ctl_dis "$1" <<<"$(printf '\n0000000000001010 <main>:\n'; cat)")
  if python3 -I scripts/checks/lib/stack_bound.py x86_64 "$f" >"$f.out"; then
    echo "FAIL control: stack analysis accepts $1: $(cat "$f.out")"; fail=1
  else
    echo "ok   control: stack analysis refuses $1"
  fi
}
printf '    1010:\tsub\trsp, rax\n    1013:\tret\n' | refuse dynamic-sp
printf '    1010:\tcall\trax\n    1012:\tret\n' | refuse indirect-call
printf '    1010:\tpush\trbp\n    1011:\tcall\t0x1010 <main>\n    1016:\tpop\trbp\n    1017:\tret\n' | refuse recursion
printf '    1010:\tcall\t0x3000 <helper>\n    1015:\tret\n' | refuse unknown-call
printf '    1010:\tje\t0x1014 <main+0x4>\n    1012:\tpush\trax\n    1013:\tnop\n    1014:\tret\n' | refuse depths-meet
printf '    1010:\tmov\trax, rsp\n    1013:\tret\n' | refuse stack-pointer-escape
printf '    1010:\tmov\teax, dword ptr [rsp + 4*rcx - 0x8]\n    1014:\tret\n' | refuse indexed-stack-address
# A leaf's red-zone store, below the stack pointer, is inside the bound: 8 + 0x30.
redzone=$(ctl_dis redzone <<'D'

0000000000001010 <main>:
    1010:	push	rbp
    1011:	mov	rbp, rsp
    1014:	mov	dword ptr [rbp - 0x30], eax
    1017:	pop	rbp
    1018:	ret
D
)
if [ "$(python3 -I scripts/checks/lib/stack_bound.py x86_64 "$redzone")" = "bound 56" ]; then
  echo "ok   control: a red-zone store is inside the bound"
else
  echo "FAIL control: red-zone bound: $(python3 -I scripts/checks/lib/stack_bound.py x86_64 "$redzone")"
  fail=1
fi
if within 73 72; then echo "FAIL control: a measurement above the bound is accepted"; fail=1
else echo "ok   control: a measurement above the bound is rejected"; fi

[ "$examples" -gt 0 ] || { echo "slice0_artifacts: no examples ran" >&2; exit 1; }
echo "slice0_artifacts: $examples examples; sizes in $sizes, stack bounds in $stack"
column -t -s "$(printf '\t')" "$sizes" 2>/dev/null || cat "$sizes"
exit "$fail"
