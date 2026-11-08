#!/usr/bin/env bash
# Slice-0 artifact gates (PROVISIONAL, Concrete/README.md gates 10, 11 and 12), over every
# hand-written BIR example, on the executables slice0_native.sh links:
#
# 10. Sanitizers: the LLVM example also built with ASan and UBSan (slice0_native.sh sanitize: ASan
#     instruments the printed IR once every definition carries sanitize_address; UBSan's checks
#     come from clang's C front end, so on IR only its runtime is linked) must
#     give the same observation as the BIR semantics (status, exact fd-2 bytes, empty fd 1), so no
#     sanitizer report. Its memory is bounded by ASan's hard_rss_limit_mb (the shadow reservation
#     rules out an address-space cap; every other command is capped, see SLICE0_GATE_MEM_GB). Control: a body with an out-of-bounds load must be reported.
# 11. Binary size (report-only, never fails on a size): bytes and text size of each executable,
#     per backend and target, in REPORT/sizes.tsv.
# 12. Stack bound: lib/stack_bound.py derives, from the linked executable's disassembly, a bound on
#     the Concrete-owned stack below main's entry, covering every byte addressed relative to the
#     stack or frame pointer (so the red zone), with external libc calls listed with their entry
#     depth (their own use, and that _exit and abort do not return, are premises). On x86-64
#     Linux, a run under gdb (lib/stack_measure.py) must stay within it (the run measures the
#     stack pointer only). REPORT/stack.tsv. Controls: the analysis must refuse an unmodelled
#     stack-pointer write, an indirect call, recursion, a call to an unknown function, two depths
#     meeting, a stack pointer used as a value or written through a partial register, a partial
#     frame-pointer write, an indexed stack address, a saved frame pointer popped into another
#     register, a stack address taken, an overwritten return address, an AArch64 frame pointer
#     stored, a register-directed return, a return after a call without the link register
#     restored, a freed or restored saved slot reread, a callee that does not restore the frame pointer, a
#     frame record away from sp, an unmodelled
#     instruction (x86 `loop`) and frame-pointer writeback (AArch64), and give the
#     expected bound on known functions (x86 frame, red-zone store, two AArch64 frame-record
#     shapes); the comparison must reject a
#     measurement above the bound.
#
# usage: slice0_artifacts.sh [REPORT_DIR]. Run inside the nix devshell, from the repository root.
set -euo pipefail
report=${1:-}
# The sanitizer executions need an unbounded address-space reservation, so the gate sets no cap of
# its own and caps every other command it starts (`capped`, and limited_exec.sh for executables).
SLICE0_GATE_MEM_GB=0
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
: "${CONCRETE_SLICE0_LLVM:?run inside nix develop}"
objdump=("${capped[@]}" "$CONCRETE_SLICE0_LLVM/llvm-objdump" -d --no-show-raw-insn)
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
# stack_bound ARCH DISASM [--ranges]: the analysis, capped and bounded in time.
stack_bound() { "${capped[@]}" timeout -k 10 60 python3 -I "$root/scripts/checks/lib/stack_bound.py" "$@"; }
fail=0
examples=0
last_exe= last_analysis=

# within MEASURED BOUND: the run stayed within the static bound (and actually ran).
within() { [ "$1" -gt 0 ] && [ "$1" -le "$2" ]; }

# measure EXE ANALYSIS EXPECT: the gdb run check's output (`measured N` or `error ...`), given the
# analysis output (its `range` lines) and the expected exit status.
measure() {
  local ranges main
  ranges=$(awk '$1 == "range" {printf "%s%s:%s", sep, $3, $4; sep=","}' <<<"$2")
  main=$(awk '$1 == "range" && $2 == "main" {print $3}' <<<"$2")
  STACK_RANGES=$ranges STACK_MAIN=$main STACK_EXPECT=$3 \
    "${capped[@]}" timeout -k 10 300 "$gdb" -batch -nx -x "$root/scripts/checks/lib/stack_measure.py" \
    "$1" 2>"$1.gdb.err" | awk '$1 == "measured" || $1 == "error" {print}' | cap_out || true
}

# stack_check NAME BACKEND EXE: derive the bound, measure a run where supported, record both.
stack_check() {
  local name=$1 backend=$2 exe=$3 dis="$3.dis" out bound ext m measured=not-run
  if ! timeout -k 10 60 "${objdump[@]}" "$exe" >"$dis" 2>"$dis.err"; then
    echo "FAIL $name [$backend] stack: disassembly failed: $(head -c 300 "$dis.err")"; fail=1; return
  fi
  if ! out=$(stack_bound "$arch" "$dis" --ranges | cap_out); then
    echo "FAIL $name [$backend] stack: $out"; fail=1; return
  fi
  bound=$(awk '$1 == "bound" {print $2}' <<<"$out")
  ext=$(awk '$1 == "external" {printf "%s%s@%s", sep, $2, $3; sep=","}' <<<"$out")
  if [ -n "$gdb" ]; then
    m=$(measure "$exe" "$out" "$(head -c 16 "$(dirname "$exe")/status")")
    measured=$(awk '$1 == "measured" {print $2}' <<<"$m")
    if [ -z "$measured" ] || ! within "$measured" "$bound"; then
      echo "FAIL $name [$backend] stack: bound $bound, run: ${m:-no measurement} $(head -c 300 "$exe.gdb.err")"
      fail=1; return
    fi
    last_exe=$exe last_analysis=$out
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$backend" "$target" "$bound" "$measured" "$ext" >>"$stack"
  echo "ok   $name [$backend] stack: bound $bound, measured $measured, externals ${ext:-none}"
}

size_report() { # size_report NAME BACKEND EXE
  local bytes text
  bytes=$(wc -c <"$3" | tr -d ' ')
  text=$("${capped[@]}" timeout -k 10 60 "$CONCRETE_SLICE0_LLVM/llvm-size" "$3" 2>/dev/null |
    awk 'NR == 2 {print $1}' | cap_out) || true
  printf '%s\t%s\t%s\t%s\t%s\n' "$1" "$2" "$target" "$bytes" "${text:-unavailable}" >>"$sizes"
}

names=$(emit_examples)
mkdir -p "$work/ctl"
for name in $names; do
  d="$work/ex/$name"
  "${emitter[@]}" emit "$name" "$d" "$target"
  build "$name" "$d/build.log" "${capped[@]}" timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$d" "$lcb"
  build "$name" "$d/build-san.log" \
    "${capped[@]}" timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$d" "$lcb" sanitize
  observe "$d/llvm.san.exe" "$d/san" 30 30 0 "${san_env[@]}"
  if judge "$d" "$d/san"; then
    echo "ok   $name [llvm+asan+ubsan]: status $(cat "$d/san.status")"
  else
    echo "FAIL $name [llvm+asan+ubsan]: status $(head -c 16 "$d/san.status") (expected $(head -c 16 "$d/status")), stderr" \
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
build control-asan "$c/build.log" "${capped[@]}" timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$c" "$lcb"
build control-asan "$c/build-san.log" \
  "${capped[@]}" timeout -k 10 300 bash "$root/scripts/checks/slice0_native.sh" "$c" "$lcb" sanitize
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
if [ "$(stack_bound x86_64 "$known")" = "$(printf 'bound 72\nexternal write 48')" ]; then
  echo "ok   control: stack bound of a known function"
else
  echo "FAIL control: stack bound of a known function: $(stack_bound x86_64 "$known")"
  fail=1
fi
refuse() { # refuse NAME BODY [ARCH [MAIN]]: BODY (printf format) follows the main header
  local f
  f=$(ctl_dis "$1" <<<"$(printf "\n0000000000001010 <${4:-main}>:\n$2")")
  if stack_bound "${3:-x86_64}" "$f" >"$f.out"; then
    echo "FAIL control: stack analysis accepts $1: $(head -c 2000 "$f.out")"; fail=1
  else
    echo "ok   control: stack analysis refuses $1"
  fi
}
refuse dynamic-sp '    1010:\tsub\trsp, rax\n    1013:\tret\n'
refuse indirect-call '    1010:\tcall\trax\n    1012:\tret\n'
refuse recursion '    1010:\tpush\trbp\n    1011:\tcall\t0x1010 <main>\n    1016:\tpop\trbp\n    1017:\tret\n'
refuse unknown-call '    1010:\tcall\t0x3000 <helper>\n    1015:\tret\n'
refuse depths-meet '    1010:\tje\t0x1014 <main+0x4>\n    1012:\tpush\trax\n    1013:\tnop\n    1014:\tret\n'
refuse stack-pointer-escape '    1010:\tmov\trax, rsp\n    1013:\tret\n'
refuse indexed-stack-address '    1010:\tmov\teax, dword ptr [rsp + 4*rcx - 0x8]\n    1014:\tret\n'
refuse partial-stack-pointer '    1010:\tpush\trax\n    1011:\tpop\tsp\n    1013:\tret\n'
refuse partial-frame-pointer '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tmov\tbp, 0x0\n    1018:\tmov\teax, dword ptr [rbp - 0x100]\n    101e:\tpop\trbp\n    101f:\tret\n'
refuse loop-instruction '    1010:\tmov\tecx, 0x2\n    1015:\tloop\t0x1018 <main+0x8>\n    1017:\tret\n    1018:\tsub\trsp, 0x100\n    101f:\tadd\trsp, 0x100\n    1026:\tret\n'
refuse saved-frame-pointer-popped '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tpush\trbp\n    1015:\tpop\trax\n    1016:\tmov\tdword ptr [rax - 0x100], ecx\n    101c:\tpop\trbp\n    101d:\tret\n'
refuse stack-address-taken '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tlea\trax, [rbp - 0x10]\n    1018:\tpop\trbp\n    1019:\tret\n'
refuse return-address-overwrite '    1010:\tmov\tqword ptr [rsp], rax\n    1014:\tret\n'
refuse a64-frame-pointer-stored '    1010:\tmov\tx29, sp\n    1014:\tstr\tx29, [x0]\n    1018:\tret\n' aarch64 _main
refuse a64-ret-register '    1010:\tret\tx0\n' aarch64 _main
refuse a64-link-register-lost '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tbl\t0x1020 <_concrete_body>\n    1018:\tadd\tsp, sp, #0x10\n    101c:\tret\n\n0000000000001020 <_concrete_body>:\n    1020:\tret\n' aarch64 _main
refuse freed-slot-reread '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tpush\trbp\n    1015:\tadd\trsp, 0x8\n    1019:\tmov\trax, qword ptr [rsp - 0x8]\n    101e:\tmov\tdword ptr [rax - 0x100], ecx\n    1024:\tpop\trbp\n    1025:\tret\n'
refuse callee-clobbers-frame-pointer '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tcall\t0x1040 <concrete_body>\n    1019:\tmov\tdword ptr [rbp - 0x100], ecx\n    101f:\tpop\trbp\n    1020:\tret\n\n0000000000001040 <concrete_body>:\n    1040:\tsub\trsp, 0x1000\n    1047:\tmov\trbp, rsp\n    104a:\tadd\trsp, 0x1000\n    1051:\tret\n'
refuse a64-frame-record-elsewhere '    1010:\tldp\tx29, x30, [x0]\n    1014:\tret\n' aarch64 _main
refuse restored-slot-reread '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tpush\trbp\n    1015:\tpop\trbp\n    1016:\tmov\trax, qword ptr [rsp - 0x8]\n    101b:\tmov\tdword ptr [rax - 0x1000], ecx\n    1021:\tpop\trbp\n    1022:\tret\n'
refuse a64-restored-record-reread '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tmov\tx29, sp\n    1018:\tstp\tx29, x30, [sp, #-0x10]!\n    101c:\tldp\tx29, x30, [sp], #0x10\n    1020:\tldr\tx0, [sp, #-0x10]\n    1024:\tstr\tx1, [x0, #-0x1000]\n    1028:\tldp\tx29, x30, [sp], #0x10\n    102c:\tret\n' aarch64 _main
refuse a64-w29-writeback-load '    1010:\tsub\tsp, sp, #0x10\n    1014:\tmov\tx29, sp\n    1018:\tldr\tw29, [sp], #0x10\n    101c:\tstr\tx0, [x29, #-0x100]\n    1020:\tret\n' aarch64 _main
refuse a64-frame-writeback '    1010:\tmov\tx29, sp\n    1014:\tstr\tx0, [x29], #-0x100\n    1018:\tstr\tx0, [x29]\n    101c:\tadd\tx29, x29, #0x100\n    1020:\tret\n' aarch64 _main
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
if [ "$(stack_bound x86_64 "$redzone")" = "bound 56" ]; then
  echo "ok   control: a red-zone store is inside the bound"
else
  echo "FAIL control: red-zone bound: $(stack_bound x86_64 "$redzone")"
  fail=1
fi
# bound_is NAME ARCH EXPECTED BODY: the analysis of the synthetic function(s) BODY (printf format,
# headers included) gives exactly EXPECTED.
bound_is() {
  local f got
  f=$(ctl_dis "$1" <<<"$(printf "\n$4")")
  got=$(stack_bound "$2" "$f" | cap_out | tr '\n' ' ') || true
  if [ "$got" = "$3 " ]; then echo "ok   control: stack bound of $1"
  else echo "FAIL control: stack bound of $1: ${got:-refused}, expected $3"; fail=1; fi
}
bound_is a64-framed-main-and-leaf aarch64 'bound 32' '0000000000001000 <_main>:\n    1000:\tstp\tx29, x30, [sp, #-0x10]!\n    1004:\tmov\tx29, sp\n    1008:\tbl\t0x1020 <_concrete_body>\n    100c:\tldp\tx29, x30, [sp], #0x10\n    1010:\tret\n\n0000000000001020 <_concrete_body>:\n    1020:\tsub\tsp, sp, #0x10\n    1024:\tstr\tw0, [sp, #0xc]\n    1028:\tadd\tsp, sp, #0x10\n    102c:\tret\n'
bound_is a64-offset-frame-record aarch64 'bound 32' '0000000000001000 <_main>:\n    1000:\tsub\tsp, sp, #0x20\n    1004:\tstp\tx29, x30, [sp, #0x10]\n    1008:\tadd\tx29, sp, #0x10\n    100c:\tstur\twzr, [x29, #-0x4]\n    1010:\tldp\tx29, x30, [sp, #0x10]\n    1014:\tadd\tsp, sp, #0x20\n    1018:\tret\n'
if [ -n "$gdb" ]; then
  # A run that does not end as expected gives no measurement.
  wrong=$(( ($(head -c 16 "$(dirname "$last_exe")/status") + 1) % 256 ))
  if grep -q '^measured' <<<"$(measure "$last_exe" "$last_analysis" "$wrong")"; then
    echo "FAIL control: a run with an unexpected exit status is measured"; fail=1
  else
    echo "ok   control: a run with an unexpected exit status gives no measurement"
  fi
fi
if within 73 72; then echo "FAIL control: a measurement above the bound is accepted"; fail=1
else echo "ok   control: a measurement above the bound is rejected"; fi

[ "$examples" -gt 0 ] || { echo "slice0_artifacts: no examples ran" >&2; exit 1; }
echo "slice0_artifacts: $examples examples; sizes in $sizes, stack bounds in $stack"
column -t -s "$(printf '\t')" "$sizes" 2>/dev/null || cat "$sizes"
exit "$fail"
