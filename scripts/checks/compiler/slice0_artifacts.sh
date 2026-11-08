#!/usr/bin/env bash
# Slice-0 artifact gates (PROVISIONAL, Concrete/README.md gates 10, 11 and 12), over every
# hand-written BIR example, on the executables slice0_native.sh links:
#
# 10. Sanitizers: the LLVM example also built with ASan (slice0_native.sh sanitize: ASan
#     instruments the printed IR once every definition carries sanitize_address; no UBSan: its
#     checks are inserted by clang's C front end, so on IR input it checks nothing) must
#     give the same observation as the BIR semantics (status, exact fd-2 bytes, empty fd 1), so no
#     sanitizer report. Its memory is bounded by ASan's hard_rss_limit_mb (the shadow reservation
#     rules out an address-space cap; every other command is capped, see SLICE0_GATE_MEM_GB). Control: a body with an out-of-bounds load must be reported.
# 11. Binary size (report-only, never fails on a size): bytes and text size of each executable,
#     per backend and target, in REPORT/sizes.tsv. Compile time (report-only, owner 2026-11-08):
#     wall-clock milliseconds of each example's emit step (the Lean emitter: checks, LLVM print,
#     LCB adaptation and in-process assembly, Lean start-up included) and native step (clang and
#     LCB's command line, linking both executables), next to its budget (the step's time bound), in
#     REPORT/compile_times.tsv.
# 12. Stack bound: lib/stack_bound.py derives, from the linked executable's disassembly and (x86)
#     PT_TLS program header, a bound on the Concrete-owned stack below main's entry, covering every
#     byte addressed relative to the stack or frame pointer (so the red zone), with external libc
#     calls listed with their entry depth (their own use, and that _exit and abort do not return,
#     are premises). A run under gdb (x86-64 Linux, lib/stack_measure.py) or LLDB (aarch64-macos,
#     lib/stack_measure_lldb.py) must stay within it (the run counts the stack pointer and every
#     stack byte a stepped instruction addresses, lib/stack_access.py, so the red zone). Both
#     analyses share the fail-closed instruction and operand grammar in lib/stack_access.py. Each
#     analysed disassembly is kept in REPORT/disassembly/. REPORT/stack.tsv. Controls: the analysis
#     must refuse an unmodelled stack-pointer write, an indirect call, recursion, a call to an
#     unknown function, two depths meeting, a stack pointer used as a value or written through a
#     partial register, a partial frame-pointer write, an indexed or partial-register stack address,
#     a saved frame pointer popped into another register, a stack address taken, an overwritten
#     return address, an AArch64 frame pointer stored, a register-directed return, a return after a
#     call without the link register restored, a freed or restored saved slot reread, a callee's
#     leftover saved slot reread by its caller (both architectures), a read of bytes written on one
#     path only or never written, a store through main's argv, a PC-relative base kept across a
#     call, walked by repeated adds or beyond the reach premise, fs intervals outside the TCB or
#     Variant II aligned PT_TLS bounds (every width, both signs, wrap-around, unbracketed forms and
#     movabs), every gs or register-relative override (including segment-before-size), segment
#     register reads, compares and writes, multi-memory-operand instructions, unclosed brackets,
#     duplicate-sign expressions, and an absolute address; also missing, duplicate, non-power-of-two
#     or PT_LOAD-less PT_TLS metadata, a callee that does not restore the frame pointer, a frame
#     record away from sp, an unmodelled instruction (x86 `loop`, AArch64 `mrs` TPIDR_EL0, string
#     ops, `xlat`, `enter`), AArch64 operand and addressing-mode violations (`nop garbage`,
#     unbracketed memory-like operands, `sp` post-index/pre+post/unscaled writeback, unsupported
#     addressing modes, unextended `w` index or mismatched index extend, malformed arity, shifts,
#     extends, conditions, targets, and `cbz`/`tbz`/`brk` operands), and frame- or link-register
#     writeback (AArch64), with a mutation check
#     proving each guard is load-bearing. Known bounds cover x86 frame, red-zone store, two AArch64
#     frame-record shapes, a read written on both paths, an adrp-based load, an fs:0x28 canary load,
#     aligned PT_TLS allocations, and LCB's single link-register save with a Mach-O stub call in a
#     loop; also refuse the link register loaded from another slot, a stub call with no
#     indirect-symbol entry, into a stub entry's middle or to the bare stub section. Real GDB must
#     count unbracketed fs forms, fail closed without fs_base, and derive a real aligned PT_TLS;
#     the comparison must reject a measurement
#     above the bound; refuse an access beyond the target's red zone (x86-64, Apple arm64) and any
#     below the stack pointer on aarch64-linux; and a linked main storing at the red zone's edge must
#     be measured at 128 (bound 128), and with a 120-byte red zone not at all.
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
objdump=(bounded 60 "$CONCRETE_SLICE0_LLVM/llvm-objdump" -d --no-show-raw-insn)
indirect=(bounded 60 "$CONCRETE_SLICE0_LLVM/llvm-objdump" --macho --indirect-symbols)
readobj=(bounded 60 "$CONCRETE_SLICE0_LLVM/llvm-readobj" --program-headers --elf-output-style=JSON)
case "$target" in
  x86_64-linux) arch=x86_64; objdump+=(--x86-asm-syntax=intel) ;;
  aarch64-*) arch=aarch64 ;;
esac
# The run check's debugger: gdb on x86-64 Linux, LLDB on aarch64-macos (none elsewhere).
gdb= lldb=
case "$target" in
  x86_64-linux) gdb=${CONCRETE_SLICE0_GDB:?the x86-64 Linux shell pins gdb} ;;
  aarch64-macos)
    lldb=${CONCRETE_SLICE0_LLDB:?the macOS shell names lldb}
    [ -x "$lldb" ] || { echo "slice0_artifacts: no LLDB at $lldb" >&2; exit 2; }
    echo "lldb: $("$lldb" --version 2>&1 | head -c 200 | head -1)" ;;
esac
measuring=${gdb:-$lldb}
[ -n "$report" ] || report="$work/report"
mkdir -p "$report"
sizes="$report/sizes.tsv"
stack="$report/stack.tsv"
printf 'example\tbackend\ttarget\tbytes\ttext\n' >"$sizes"
times="$report/compile_times.tsv"
printf 'example\ttarget\temit_ms\temit_budget_ms\tnative_ms\tnative_budget_ms\n' >"$times"
# now_ms: a monotonic clock in milliseconds (portable: BSD date has no %N).
now_ms() { python3 -I -c 'import time; print(time.monotonic_ns() // 1000000)'; }
printf 'example\tbackend\ttarget\tbound\tmeasured\texternals\n' >"$stack"
# hard_rss_limit_mb and LeakSanitizer are Linux-only (ASan on Darwin refuses detect_leaks); on
# macOS RLIMIT_AS is not enforced either, so there the job's memory is the bound, as for all runs.
asan=abort_on_error=0:symbolize=0
[ "$(uname -s)" != Linux ] || asan="$asan:hard_rss_limit_mb=1024:detect_leaks=1"
san_env=("ASAN_OPTIONS=$asan")
# stack_bound TARGET DISASM [--ranges]: the analysis, capped and bounded in time.
stack_bound() { bounded 60 python3 -I "$root/scripts/checks/lib/stack_bound.py" "$@"; }
fail=0
examples=0
last_exe= last_analysis=

# within MEASURED BOUND: the run stayed within the static bound (and actually ran).
within() { [ "$1" -gt 0 ] && [ "$1" -le "$2" ]; }

# measure EXE ANALYSIS EXPECT: the run check's output (`measured N` or `error ...`), given the
# analysis output (its `range` lines) and the expected exit status: gdb (lib/stack_measure.py) or
# LLDB (lib/stack_measure_lldb.py, the same contract; Apple's LLDB runs its own Python, so the
# shell's Python variables are removed).
measure() {
  local ranges main redzone
  ranges=$(awk '$1 == "range" {printf "%s%s:%s", sep, $3, $4; sep=","}' <<<"$2")
  main=$(awk '$1 == "range" && $2 == "main" {print $3}' <<<"$2")
  redzone=$(awk '$1 == "redzone" {print $2}' <<<"$2")
  [ -n "$redzone" ] || { echo "error the analysis names no red zone"; return; }
  if [ -n "$gdb" ]; then
    STACK_RANGES=$ranges STACK_MAIN=$main STACK_EXPECT=$3 STACK_RED_ZONE=$redzone \
      STACK_LIB="$root/scripts/checks/lib" \
      bounded 300 "$gdb" -batch -nx -x "$root/scripts/checks/lib/stack_measure.py" \
      "$1" 2>"$1.gdb.err" | awk '$1 == "measured" || $1 == "error" {print}' | cap_out || true
  else
    STACK_EXE=$1 STACK_RANGES=$ranges STACK_MAIN=$main STACK_EXPECT=$3 STACK_RED_ZONE=$redzone \
      STACK_LIB="$root/scripts/checks/lib" \
      bounded 300 env -u PYTHONPATH -u PYTHONHOME "$lldb" --batch --no-lldbinit \
      -o "command script import $root/scripts/checks/lib/stack_measure_lldb.py" \
      2>"$1.gdb.err" | awk '$1 == "measured" || $1 == "error" {print}' | cap_out || true
  fi
}

# append_tls EXE DIS: append the linked ELF's Variant II static-TLS reservation M (zero without
# PT_TLS or when p_memsz = 0), derived from structured llvm-readobj JSON. Requires at least one
# PT_LOAD segment and at most one PT_TLS segment; unreadable, malformed or repeated metadata fails
# closed.
append_tls() {
  local headers="$1.program-headers"
  "${readobj[@]}" "$1" >"$headers" 2>"$headers.err" || return 1
  stack_bound --elf-tls "$headers" >>"$2" 2>>"$headers.err"
}

# stack_check NAME BACKEND EXE: derive the bound, measure a run where supported, record both.
stack_check() {
  local name=$1 backend=$2 exe=$3 dis="$3.dis" out bound ext m measured=not-run
  if ! "${objdump[@]}" "$exe" >"$dis" 2>"$dis.err"; then
    echo "FAIL $name [$backend] stack: disassembly failed: $(head -c 300 "$dis.err")"; fail=1; return
  fi
  # Mach-O names only the whole __stubs section; each stub entry's imported symbol comes from the
  # indirect symbol table, appended as `stub ADDR SYMBOL` lines (lib/stack_bound.py).
  if [ "$target" = aarch64-macos ]; then
    if ! "${indirect[@]}" "$exe" 2>"$dis.err" | awk '
         /^Indirect symbols for / {s = ($0 ~ /\(__TEXT,__stubs\)/); next}
         s && $1 ~ /^0x[0-9a-f]+$/ && NF == 3 {print "stub", $1, $3}' >>"$dis"; then
      echo "FAIL $name [$backend] stack: indirect symbols failed: $(head -c 300 "$dis.err")"; fail=1; return
    fi
  fi
  if [ "$target" = x86_64-linux ] && ! append_tls "$exe" "$dis"; then
    echo "FAIL $name [$backend] stack: PT_TLS metadata failed: $(head -c 300 "$exe.program-headers.err")"
    fail=1; return
  fi
  # The disassembly the bound is derived from is kept in the report (audit evidence).
  mkdir -p "$report/disassembly"
  cp "$dis" "$report/disassembly/$name.$backend.$target.dis"
  if ! out=$(stack_bound "$target" "$dis" --ranges | cap_out); then
    echo "FAIL $name [$backend] stack: $out"; fail=1; return
  fi
  bound=$(awk '$1 == "bound" {print $2}' <<<"$out")
  ext=$(awk '$1 == "external" {printf "%s%s@%s", sep, $2, $3; sep=","}' <<<"$out")
  if [ -n "$measuring" ]; then
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
  text=$(bounded 60 "$CONCRETE_SLICE0_LLVM/llvm-size" "$3" 2>/dev/null |
    awk 'NR == 2 {print $1}' | cap_out) || true
  printf '%s\t%s\t%s\t%s\t%s\n' "$1" "$2" "$target" "$bytes" "${text:-unavailable}" >>"$sizes"
}

names=$(emit_examples)
mkdir -p "$work/ctl"
for name in $names; do
  d="$work/ex/$name"
  t0=$(now_ms)
  "${emitter[@]}" emit "$name" "$d" "$target"
  t1=$(now_ms)
  build "$name" "$d/build.log" bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb"
  t2=$(now_ms)
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$target" $((t1 - t0)) $((emit_secs * 1000)) $((t2 - t1)) $((native_secs * 1000)) >>"$times"
  build "$name" "$d/build-san.log" \
    bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb" sanitize
  observe "$d/llvm.san.exe" "$d/san" 30 30 0 "${san_env[@]}"
  if judge "$d" "$d/san"; then
    echo "ok   $name [llvm+asan]: status $(cat "$d/san.status")"
  else
    echo "FAIL $name [llvm+asan]: status $(head -c 16 "$d/san.status") (expected $(head -c 16 "$d/status")), stderr" \
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
build control-asan "$c/build.log" bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$c" "$lcb"
build control-asan "$c/build-san.log" \
  bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$c" "$lcb" sanitize
observe "$c/llvm.san.exe" "$c/san" 30 30 0 "${san_env[@]}"
if ! judge "$c" "$c/san" && grep -q 'AddressSanitizer' "$c/san.stderr"; then
  echo "ok   control: ASan reports an out-of-bounds load"
else
  echo "FAIL control: ASan did not report an out-of-bounds load (status $(cat "$c/san.status"))"; fail=1
fi

# Controls (gate 12): the analysis on synthetic disassembly. PT_TLS is explicit even in controls:
# absence of the line is a fail-closed error, not an implicit zero-sized TLS segment.
ctl_dis() { # ctl_dis NAME [PT_TLS_MEMSZ]: write a synthetic disassembly from stdin
  { echo "Disassembly of section .text:"; cat; echo "tls_memsz ${2:-0x0}"; } >"$work/ctl/$1.dis"
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
if [ "$(stack_bound x86_64-linux "$known")" = "$(printf 'bound 72\nexternal write 48')" ]; then
  echo "ok   control: stack bound of a known function"
else
  echo "FAIL control: stack bound of a known function: $(stack_bound x86_64-linux "$known")"
  fail=1
fi
sed '/^tls_memsz /d' "$known" >"$work/ctl/no-tls-metadata.dis"
if ! stack_bound x86_64-linux "$work/ctl/no-tls-metadata.dis" >"$work/ctl/no-tls-metadata.out" &&
   grep -q '^refused: x86 analysis without the linked executable.s PT_TLS' "$work/ctl/no-tls-metadata.out"; then
  echo "ok   control: x86 analysis without PT_TLS metadata is refused"
else
  echo "FAIL control: x86 analysis without PT_TLS metadata: $(cat "$work/ctl/no-tls-metadata.out")"; fail=1
fi
{ cat "$known"; echo 'tls_memsz 0x0'; } >"$work/ctl/duplicate-tls-metadata.dis"
if ! stack_bound x86_64-linux "$work/ctl/duplicate-tls-metadata.dis" >"$work/ctl/duplicate-tls-metadata.out" &&
   grep -q '^refused: malformed or duplicate PT_TLS metadata' "$work/ctl/duplicate-tls-metadata.out"; then
  echo "ok   control: duplicate PT_TLS metadata is refused"
else
  echo "FAIL control: duplicate PT_TLS metadata: $(cat "$work/ctl/duplicate-tls-metadata.out")"; fail=1
fi
: >"$work/ctl/refuse_manifest.tsv"
refuse() { # refuse NAME DIAG BODY [TARGET [MAIN [PT_TLS_MEMSZ]]]: BODY follows the main header
  local f rc=0
  f=$(ctl_dis "$1" "${6:-0x0}" <<<"$(printf "\n0000000000001010 <${5:-main}>:\n$3")")
  printf '%s\t%s\t%s\t%s\n' "$1" "${4:-x86_64-linux}" "$f" "$2" >>"$work/ctl/refuse_manifest.tsv"
  stack_bound "${4:-x86_64-linux}" "$f" >"$f.out" || rc=$?
  if [ "$rc" -eq 1 ] && grep -Eq "^refused: .*$2" "$f.out"; then
    echo "ok   control: stack analysis refuses $1"
  else
    echo "FAIL control: stack analysis $1 (expected '$2'): status $rc, output $(head -c 2000 "$f.out")"; fail=1
  fi
}
refuse_tp() { # refuse_tp NAME BODY [PT_TLS_MEMSZ]: an fs operand gets the reach refusal
  refuse "$1" "thread-pointer-relative access beyond the premise's reach" "$2" x86_64-linux main "${3:-0x0}"
}
refuse non-power-of-two-tls-align 'PT_TLS p_align 0x3 is not a power of two' '    1010:\tret\n' x86_64-linux main '0x8 0x3 0x0'
refuse dynamic-sp 'unmodelled stack-pointer write' '    1010:\tsub\trsp, rax\n    1013:\tret\n'
refuse indirect-call 'indirect call' '    1010:\tcall\trax\n    1012:\tret\n'
refuse recursion 'recursion through main' '    1010:\tpush\trbp\n    1011:\tcall\t0x1010 <main>\n    1016:\tpop\trbp\n    1017:\tret\n'
refuse unknown-call 'neither owned nor a stub' '    1010:\tcall\t0x3000 <helper>\n    1015:\tret\n'
refuse depths-meet 'states .* meet' '    1010:\tje\t0x1014 <main+0x4>\n    1012:\tpush\trax\n    1013:\tnop\n    1014:\tret\n'
refuse stack-pointer-escape 'stack or frame pointer used as a value' '    1010:\tmov\trax, rsp\n    1013:\tret\n'
refuse indexed-stack-address 'indexed stack address' '    1010:\tmov\tdword ptr [rsp + 4*rcx - 0x8], eax\n    1014:\tret\n'
refuse partial-stack-address 'stack address through a partial register' '    1010:\tmov\tdword ptr [esp - 0x8], eax\n    1015:\tret\n'
refuse partial-stack-pointer 'pop of other than a 64-bit general register' '    1010:\tpush\trax\n    1011:\tpop\tsp\n    1013:\tret\n'
refuse partial-frame-pointer 'frame-relative use of a frame pointer that is not this frame' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tmov\tbp, 0x0\n    1018:\tmov\tdword ptr [rbp - 0x10], eax\n    101e:\tpop\trbp\n    101f:\tret\n'
refuse loop-instruction 'unmodelled instruction: loop' '    1010:\tmov\tecx, 0x2\n    1015:\tloop\t0x1018 <main+0x8>\n    1017:\tret\n    1018:\tsub\trsp, 0x100\n    101f:\tadd\trsp, 0x100\n    1026:\tret\n'
refuse a64-tpidr-el0 'unmodelled instruction: mrs' '    1010:\tmrs\tx8, tpidr_el0\n    1014:\tret\n' aarch64-macos _main
refuse saved-frame-pointer-popped 'a saved frame pointer popped into rax' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tpush\trbp\n    1015:\tpop\trax\n    1016:\tpop\trbp\n    1017:\tret\n'
refuse stack-address-taken 'stack address taken as a value' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tlea\trax, [rbp - 0x10]\n    1018:\tpop\trbp\n    1019:\tret\n'
refuse x86-lea-non-rip 'LEA through a register that may hold a stack address, or absolutely' '    1010:\tlea\trax, [rsi + 0x10]\n    1014:\tret\n'
refuse x86-lea-segment-override 'segment override on LEA' '    1010:\tlea\trax, fs:[rip + 0x10]\n    1017:\tret\n'
refuse return-address-overwrite 'touches the return address or the caller' '    1010:\tmov\tqword ptr [rsp], rax\n    1014:\tret\n'
refuse a64-frame-pointer-stored 'stack or frame pointer stored as a value' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tmov\tx29, sp\n    1018:\tadrp\tx8, 0x1000 <_main>\n    101c:\tstr\tx29, [x8]\n    1020:\tldp\tx29, x30, [sp], #0x10\n    1024:\tret\n' aarch64-macos _main
refuse a64-ret-register 'indirect control transfer ret x0' '    1010:\tret\tx0\n' aarch64-macos _main
refuse a64-link-register-lost 'return through a link register not restored' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tbl\t0x1020 <_concrete_body>\n    1018:\tadd\tsp, sp, #0x10\n    101c:\tret\n\n0000000000001020 <_concrete_body>:\n    1020:\tret\n' aarch64-macos _main
refuse freed-slot-reread 'touches a saved frame pointer or link register' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tpush\trbp\n    1015:\tadd\trsp, 0x8\n    1019:\tmov\trax, qword ptr [rsp - 0x8]\n    101e:\tpop\trbp\n    101f:\tret\n'
refuse callee-clobbers-frame-pointer 'return without the caller.s frame pointer restored' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tcall\t0x1040 <concrete_body>\n    1019:\tpop\trbp\n    101a:\tret\n\n0000000000001040 <concrete_body>:\n    1040:\tsub\trsp, 0x10\n    1044:\tmov\trbp, rsp\n    1047:\tadd\trsp, 0x10\n    104b:\tret\n'
refuse a64-frame-record-elsewhere 'link register overwritten' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tadrp\tx8, 0x1000 <_main>\n    1018:\tldp\tx29, x30, [x8]\n    101c:\tldp\tx29, x30, [sp], #0x10\n    1020:\tret\n' aarch64-macos _main
refuse restored-slot-reread 'touches a saved frame pointer or link register' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tpush\trbp\n    1015:\tpop\trbp\n    1016:\tmov\trax, qword ptr [rsp - 0x8]\n    101b:\tpop\trbp\n    101c:\tret\n'
refuse a64-restored-record-reread 'touches a saved frame pointer or link register' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tmov\tx29, sp\n    1018:\tstp\tx29, x30, [sp, #-0x10]!\n    101c:\tldp\tx29, x30, [sp], #0x10\n    1020:\tldr\tx0, [sp, #-0x10]\n    1024:\tldp\tx29, x30, [sp], #0x10\n    1028:\tret\n' aarch64-macos _main
refuse a64-w29-writeback-load 'frame-relative use of a frame pointer that is not this frame' '    1010:\tstp\tx29, x30, [sp, #-0x20]!\n    1014:\tmov\tx29, sp\n    1018:\tstr\tx0, [sp, #0x10]\n    101c:\tldr\tw29, [sp, #0x10]\n    1020:\tstr\tx0, [x29, #-0x8]\n    1024:\tldp\tx29, x30, [sp], #0x20\n    1028:\tret\n' aarch64-macos _main
refuse a64-frame-writeback 'writeback on the frame pointer' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tmov\tx29, sp\n    1018:\tstr\tx0, [x29, #-0x8]!\n    101c:\tldp\tx29, x30, [sp], #0x10\n    1020:\tret\n' aarch64-macos _main
# Bytes below the caller's stack pointer are clobbered by a call and cannot be reread after it.
refuse callee-residual-reread 'reads stack bytes this function has not written' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tmov\tqword ptr [rsp - 0x10], rax\n    1019:\tcall\t0x1040 <concrete_body>\n    101e:\tmov\trax, qword ptr [rsp - 0x10]\n    1023:\tpop\trbp\n    1024:\tret\n\n0000000000001040 <concrete_body>:\n    1040:\tpush\trbp\n    1041:\tpop\trbp\n    1042:\tret\n'
# Writeback through the link register changes the return address.
refuse a64-link-register-writeback 'writeback on the link register' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tbl\t0x1040 <_concrete_body>\n    1018:\tldp\tx29, x30, [sp], #0x10\n    101c:\tret\n\n0000000000001040 <_concrete_body>:\n    1040:\tldr\tx0, [x30], #-0x8\n    1044:\tret\n' aarch64-macos _main
refuse a64-link-register-store-writeback 'writeback on the link register' '    1010:\tstr\tx0, [x30, #0x8]!\n    1014:\tret\n' aarch64-macos _main
# main's argv (rsi, x1) points into the stack: memory is not addressed through it.
refuse argv-store 'memory addressed through a register that may hold a stack address, or absolutely' '    1010:\tmov\tbyte ptr [rsi - 0x1000], 0x0\n    1017:\txor\teax, eax\n    1019:\tret\n'
refuse a64-argv-store 'memory addressed through a register that may hold a stack address' '    1010:\tstrb\twzr, [x1, #-0x100]\n    1014:\tret\n' aarch64-macos _main
refuse a64-pc-relative-after-call 'memory addressed through a register that may hold a stack address' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tadrp\tx8, 0x2000 <_main+0xff0>\n    1018:\tbl\t0x1040 <_concrete_body>\n    101c:\tldr\tw0, [x8, #0x10]\n    1020:\tldp\tx29, x30, [sp], #0x10\n    1024:\tret\n\n0000000000001040 <_concrete_body>:\n    1040:\tret\n' aarch64-macos _main
# PC-relative provenance through repeated adds (a loop walking toward the stack).
refuse a64-pc-relative-walk 'memory addressed through a register that may hold a stack address' '    1010:\tadr\tx8, 0x1010 <_main>\n    1014:\tsub\tx2, x1, #0xfff\n    1018:\tadd\tx8, x8, #0x8\n    101c:\tcmp\tx8, x2\n    1020:\tb.lo\t0x1018 <_main+0x8>\n    1024:\tstrb\twzr, [x8]\n    1028:\tret\n' aarch64-macos _main
refuse a64-pc-relative-far 'memory addressed through a register that may hold a stack address' '    1010:\tadrp\tx8, 0x7fff0000\n    1014:\tstrb\twzr, [x8]\n    1018:\tret\n' aarch64-macos _main
refuse absolute-address 'memory addressed through a register that may hold a stack address, or absolutely' '    1010:\tmov\tbyte ptr [0x7fffffd0], 0x0\n    1018:\tret\n'
refuse_tp fs-beyond-reach '    1010:\tmov\tbyte ptr fs:[0x100000], 0x0\n    1018:\tret\n'
refuse_tp fs-unbracketed-beyond-reach '    1010:\tmov\tbyte ptr fs:0x100000, 0x0\n    1018:\tret\n'
refuse_tp fs-movabs-beyond-reach '    1010:\tmovabs\trax, fs:0x100000\n    101a:\tret\n'
refuse gs-absolute 'gs segment override on x86_64-linux' '    1010:\tmov\tbyte ptr gs:[0x0], 0x0\n    1018:\tret\n'
refuse gs-unbracketed 'gs segment override on x86_64-linux' '    1010:\tmov\tbyte ptr gs:0x0, 0x0\n    1018:\tret\n'
refuse gs-before-size-ptr 'gs segment override on x86_64-linux' '    1010:\tmov\trax, gs:qword ptr [rip + 0x10]\n    1018:\tret\n'
refuse fs-before-size-ptr 'thread-pointer-relative access through a register' '    1010:\tmov\tfs:qword ptr [rsp - 0x8], rax\n    1018:\tret\n'
for segment in fs gs; do
  if [ "$segment" = fs ]; then
    seg_diag='thread-pointer-relative access through a register'
  else
    seg_diag='gs segment override on x86_64-linux'
  fi
  refuse "x86-$segment-through-rsp" "$seg_diag" "    1010:\tmov\tbyte ptr $segment:[rsp - 0x8], 0x0\n    1018:\tret\n"
  refuse "x86-$segment-through-rax" "$seg_diag" "    1010:\tmov\tbyte ptr $segment:[rax + 0x8], 0x0\n    1018:\tret\n"
  refuse "x86-$segment-through-rbp" "$seg_diag" "    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tmov\tbyte ptr $segment:[rbp - 0x8], 0x0\n    101c:\tpop\trbp\n    101d:\tret\n"
  refuse "x86-$segment-through-rip" "$seg_diag" "    1010:\tmov\tbyte ptr $segment:[rip + 0x10], 0x0\n    1018:\tret\n"
done
for segment in cs ds es ss; do
  refuse "x86-$segment-stack-override" "unmodelled $segment segment override" "    1010:\tmov\tbyte ptr $segment:[rsp - 0x8], 0x0\n    1018:\tret\n"
done
for segment in cs ds es fs gs ss; do
  refuse "x86-segment-write-$segment" 'write to a segment register' "    1010:\tmov\t$segment, eax\n    1013:\tret\n"
  refuse "x86-segment-read-$segment" 'segment register used as a value' "    1010:\tmov\teax, $segment\n    1013:\tret\n"
done
for segment in fs gs; do
  refuse "x86-segment-cmp-$segment" 'segment register used as a value' "    1010:\tcmp\t$segment, eax\n    1013:\tret\n"
done
refuse x86-two-memory-operands 'instruction with more than one memory operand' '    1010:\tmov\tqword ptr [rsp - 0x8], qword ptr fs:[0x28]\n    1019:\tret\n'
refuse x86-two-memory-operands-unbracketed 'instruction with more than one memory operand' '    1010:\tmov\tqword ptr [rsp - 0x8], qword ptr fs:0x28\n    1019:\tret\n'
refuse x86-empty-fs-address 'empty memory address' '    1010:\tmov\trax, qword ptr fs:[]\n    1015:\tret\n'
refuse x86-unclosed-bracket 'unmodelled operand' '    1010:\tmov\trax, qword ptr [0x100000\n    1015:\tret\n'
refuse x86-duplicate-sign-address 'malformed memory address' '    1010:\tmov\trax, byte ptr fs:[- -0x100000]\n    1019:\tret\n' x86_64-linux main 0x100000
refuse x86-signless-second-term 'malformed memory address' '    1010:\tmov\trax, byte ptr fs:[-0x100000 0x100000]\n    1019:\tret\n'
refuse x86-pc-relative-wide-beyond 'PC-relative access beyond the premise' '    1010:\tmov\trax, qword ptr [rip + 0xfffffa]\n    1015:\tret\n'
refuse x86-lea-pc-relative-beyond 'PC-relative LEA beyond the premise' '    1010:\tlea\trdi, [rip + 0x1000001]\n    1015:\tret\n'
# The link register loaded from a slot that does not hold it, and a stub call with no stub entry.
refuse a64-lr-from-other-slot 'x30 loaded from other than its saved slot' '    1010:\tsub\tsp, sp, #0x20\n    1014:\tstr\tx30, [sp, #0x10]\n    1018:\tstr\tx0, [sp, #0x8]\n    101c:\tldr\tx30, [sp, #0x8]\n    1020:\tadd\tsp, sp, #0x20\n    1024:\tret\n' aarch64-macos _main
refuse a64-unmapped-stub 'neither owned nor a stub' '    1010:\tstr\tx30, [sp, #-0x10]!\n    1014:\tbl\t0x1040 <_write+0x1040>\n    1018:\tldr\tx30, [sp], #0x10\n    101c:\tret\n' aarch64-macos _main
# A call into the middle of a Mach-O stub entry, and to the stub section with no entry mapped.
refuse a64-mid-stub-call 'neither owned nor a stub' '    1010:\tstr\tx30, [sp, #-0x10]!\n    1014:\tbl\t0x1044 <__stubs+0x4>\n    1018:\tldr\tx30, [sp], #0x10\n    101c:\tret\n\nDisassembly of section __TEXT,__stubs:\n\n0000000000001040 <__stubs>:\n    1040:\tadrp\tx16, 0x2000\n    1044:\tldr\tx16, [x16]\n    1048:\tbr\tx16\nstub 0x0000000000001040 _write\n' aarch64-macos _main
refuse a64-unmapped-stub-section 'neither owned nor a stub' '    1010:\tstr\tx30, [sp, #-0x10]!\n    1014:\tbl\t0x1040 <__stubs>\n    1018:\tldr\tx30, [sp], #0x10\n    101c:\tret\n\nDisassembly of section __TEXT,__stubs:\n\n0000000000001040 <__stubs>:\n    1040:\tadrp\tx16, 0x2000\n    1044:\tldr\tx16, [x16]\n    1048:\tbr\tx16\n' aarch64-macos _main
refuse a64-callee-residual-reread 'reads stack bytes this function has not written' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tmov\tx29, sp\n    1018:\tstur\tx0, [sp, #-0x10]\n    101c:\tbl\t0x1040 <_concrete_body>\n    1020:\tldr\tx0, [sp, #-0x10]\n    1024:\tldp\tx29, x30, [sp], #0x10\n    1028:\tret\n\n0000000000001040 <_concrete_body>:\n    1040:\tstp\tx29, x30, [sp, #-0x10]!\n    1044:\tldp\tx29, x30, [sp], #0x10\n    1048:\tret\n' aarch64-macos _main
refuse a64-nop-garbage 'unexpected operand: nop garbage' '    1010:\tnop\tgarbage\n    1014:\tret\n' aarch64-macos _main
refuse a64-malformed-operand-list 'malformed operand list: mov x0, , x1' '    1010:\tmov\tx0, , x1\n    1014:\tret\n' aarch64-macos _main
refuse a64-ldst-wrong-arity 'wrong operand count: ldr x0' '    1010:\tldr\tx0\n    1014:\tret\n' aarch64-macos _main
refuse a64-unmodelled-transfer-reg 'unmodelled transfer register: str foo, \[sp, #-0x8\]' '    1010:\tstr\tfoo, [sp, #-0x8]\n    1014:\tret\n' aarch64-macos _main
refuse a64-sp-loaded 'stack pointer loaded: ldr sp, \[sp, #-0x8\]' '    1010:\tstr\tx0, [sp, #-0x8]\n    1014:\tldr\tsp, [sp, #-0x8]\n    1018:\tret\n' aarch64-macos _main
refuse a64-unbracketed-mem 'unbracketed or malformed memory operand: ldr x0, 0x1000' '    1010:\tldr\tx0, 0x1000\n    1014:\tret\n' aarch64-macos _main
refuse a64-unbracketed-mem-sp 'unbracketed or malformed memory operand: str x0, sp' '    1010:\tstr\tx0, sp\n    1014:\tret\n' aarch64-macos _main
refuse a64-empty-mem-address 'malformed memory address: str x0, \[\]' '    1010:\tstr\tx0, []\n    1014:\tret\n' aarch64-macos _main
refuse a64-wsp-base 'unmodelled stack address: str x0, \[wsp, #-0x8\]' '    1010:\tstr\tx0, [wsp, #-0x8]\n    1014:\tret\n' aarch64-macos _main
refuse a64-unmodelled-base-reg "unmodelled base register 'w8': str x0, \[w8, #-0x8\]" '    1010:\tstr\tx0, [w8, #-0x8]\n    1014:\tret\n' aarch64-macos _main
refuse a64-sp-writeback-reg-post 'unsupported addressing mode: str x0, \[sp\], x8' '    1010:\tsub\tsp, sp, #0x10\n    1014:\tstr\tx0, [sp], x8\n    1018:\tadd\tsp, sp, #0x10\n    101c:\tret\n' aarch64-macos _main
refuse a64-sp-writeback-pre-and-post 'unsupported addressing mode: str x0, \[sp, #-0x8\]!, #0x8' '    1010:\tstr\tx0, [sp, #-0x8]!, #0x8\n    1014:\tadd\tsp, sp, #0x8\n    1018:\tret\n' aarch64-macos _main
refuse a64-sp-writeback-unscaled 'unsupported addressing mode: stur x0, \[sp, #-0x10\]!' '    1010:\tstur\tx0, [sp, #-0x10]!\n    1014:\tadd\tsp, sp, #0x10\n    1018:\tret\n' aarch64-macos _main
refuse a64-unsupported-addressing-mode 'unsupported addressing mode: str x0, \[sp, x8, ror #1\]' '    1010:\tstr\tx0, [sp, x8, ror #1]\n    1014:\tret\n' aarch64-macos _main
refuse a64-pair-indexed-addressing-mode 'unsupported addressing mode: stp x0, x1, \[sp, x8\]' '    1010:\tstp\tx0, x1, [sp, x8]\n    1014:\tret\n' aarch64-macos _main
refuse a64-unextended-w-index 'unsupported addressing mode: str x0, \[sp, w8\]' '    1010:\tstr\tx0, [sp, w8]\n    1014:\tret\n' aarch64-macos _main
refuse a64-mismatched-index-extend 'unsupported addressing mode: str x0, \[sp, x8, sxtw #2\]' '    1010:\tstr\tx0, [sp, x8, sxtw #2]\n    1014:\tret\n' aarch64-macos _main
refuse a64-indexed-stack-address 'indexed stack address: str x0, \[sp, x8\]' '    1010:\tstr\tx0, [sp, x8]\n    1014:\tret\n' aarch64-macos _main
refuse a64-pc-relative-offset-beyond "PC-relative access beyond the premise's reach" '    1010:\tadrp\tx8, 0x1000 <_main>\n    1014:\tldr\tx0, [x8, #0x8000]\n    1018:\tret\n' aarch64-macos _main
refuse a64-pc-relative-writeback "PC-relative access beyond the premise's reach" '    1010:\tadrp\tx8, 0x1000 <_main>\n    1014:\tldr\tx0, [x8, #0x8]!\n    1018:\tret\n' aarch64-macos _main
refuse a64-pc-relative-indexed "PC-relative access beyond the premise's reach" '    1010:\tadrp\tx8, 0x1000 <_main>\n    1014:\tldr\tx0, [x8, x9]\n    1018:\tret\n' aarch64-macos _main
refuse a64-plain-wrong-arity 'wrong operand count: mov x0' '    1010:\tmov\tx0\n    1014:\tret\n' aarch64-macos _main
refuse a64-bad-adrp-target "unmodelled address target '\[sp\]': adrp x8, \[sp\]" '    1010:\tadrp\tx8, [sp]\n    1014:\tret\n' aarch64-macos _main
refuse a64-bad-condition "unmodelled condition 'bogus': cset w0, bogus" '    1010:\tcset\tw0, bogus\n    1014:\tret\n' aarch64-macos _main
refuse a64-bad-shift-extend "unmodelled shift or extend 'ror #1': add x0, x1, x2, ror #1" '    1010:\tadd\tx0, x1, x2, ror #1\n    1014:\tret\n' aarch64-macos _main
refuse a64-bad-imm-extend "unmodelled shift or extend 'uxtw': add x0, x1, #0x1, uxtw" '    1010:\tadd\tx0, x1, #0x1, uxtw\n    1014:\tret\n' aarch64-macos _main
refuse a64-bad-movz-shift "unmodelled shift or extend 'lsl #8': movz x0, #0x1, lsl #8" '    1010:\tmovz\tx0, #0x1, lsl #8\n    1014:\tret\n' aarch64-macos _main
refuse a64-invalid-imm 'invalid immediate operand: mul x0, x1, #0x2' '    1010:\tmul\tx0, x1, #0x2\n    1014:\tret\n' aarch64-macos _main
refuse a64-unmodelled-operand "unmodelled operand '\[sp\]': mov x0, \[sp\]" '    1010:\tmov\tx0, [sp]\n    1014:\tret\n' aarch64-macos _main
refuse a64-missing-required-imm "unmodelled operand 'x1': movz x0, x1" '    1010:\tmovz\tx0, x1\n    1014:\tret\n' aarch64-macos _main
refuse a64-dynamic-sp 'unmodelled stack-pointer write: add sp, sp, x8' '    1010:\tadd\tsp, sp, x8\n    1014:\tret\n' aarch64-macos _main
refuse a64-fp-from-sp-sub 'stack or frame pointer used as a value: sub x29, sp, #0x8' '    1010:\tstp\tx29, x30, [sp, #-0x10]!\n    1014:\tsub\tx29, sp, #0x8\n    1018:\tldp\tx29, x30, [sp], #0x10\n    101c:\tret\n' aarch64-macos _main
refuse a64-lr-overwritten 'link register overwritten: mov x30, x0' '    1010:\tmov\tx30, x0\n    1014:\tret\n' aarch64-macos _main
refuse a64-sp-escape 'stack or frame pointer used as a value: mov x0, sp' '    1010:\tmov\tx0, sp\n    1014:\tret\n' aarch64-macos _main
refuse a64-cbz-bad-reg 'indirect control transfer cbz sp, 0x1010 <_main>' '    1010:\tcbz\tsp, 0x1010 <_main>\n    1014:\tret\n' aarch64-macos _main
refuse a64-tbz-bad-bit 'indirect control transfer tbz x0, #64, 0x1010 <_main>' '    1010:\ttbz\tx0, #64, 0x1010 <_main>\n    1014:\tret\n' aarch64-macos _main
refuse a64-brk-bad-imm 'indirect control transfer brk garbage' '    1010:\tbrk\tgarbage\n    1014:\tret\n' aarch64-macos _main
# A read of a byte written on only one path into it.
refuse one-path-write 'reads stack bytes this function has not written' '    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tje\t0x101e <main+0xe>\n    1016:\tmov\tqword ptr [rbp - 0x8], rax\n    101e:\tmov\trax, qword ptr [rbp - 0x8]\n    1022:\tpop\trbp\n    1023:\tret\n'
# A read never written (stale bytes of whatever ran before).
refuse unwritten-read 'reads stack bytes this function has not written' '    1010:\tmov\trax, qword ptr [rsp - 0x40]\n    1015:\tret\n'
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
if [ "$(stack_bound x86_64-linux "$redzone")" = "bound 56" ]; then
  echo "ok   control: a red-zone store is inside the bound"
else
  echo "FAIL control: red-zone bound: $(stack_bound x86_64-linux "$redzone")"
  fail=1
fi
# bound_is NAME ARCH EXPECTED BODY [PT_TLS_MEMSZ]: the synthetic function(s) BODY (printf format,
# headers included) give exactly EXPECTED.
bound_is() {
  local f got
  f=$(ctl_dis "$1" "${5:-0x0}" <<<"$(printf "\n$4")")
  got=$(stack_bound "$2" "$f" | cap_out | tr '\n' ' ') || true
  if [ "$got" = "$3 " ]; then echo "ok   control: stack bound of $1"
  else echo "FAIL control: stack bound of $1: ${got:-refused}, expected $3"; fail=1; fi
}
bound_is a64-framed-main-and-leaf aarch64-macos 'bound 32' '0000000000001000 <_main>:\n    1000:\tstp\tx29, x30, [sp, #-0x10]!\n    1004:\tmov\tx29, sp\n    1008:\tbl\t0x1020 <_concrete_body>\n    100c:\tldp\tx29, x30, [sp], #0x10\n    1010:\tret\n\n0000000000001020 <_concrete_body>:\n    1020:\tsub\tsp, sp, #0x10\n    1024:\tstr\tw0, [sp, #0xc]\n    1028:\tadd\tsp, sp, #0x10\n    102c:\tret\n'
bound_is a64-offset-frame-record aarch64-macos 'bound 32' '0000000000001000 <_main>:\n    1000:\tsub\tsp, sp, #0x20\n    1004:\tstp\tx29, x30, [sp, #0x10]\n    1008:\tadd\tx29, sp, #0x10\n    100c:\tstur\twzr, [x29, #-0x4]\n    1010:\tldp\tx29, x30, [sp, #0x10]\n    1014:\tadd\tsp, sp, #0x20\n    1018:\tret\n'
# A load PC-relative through adrp + add is not a stack access.
bound_is a64-pc-relative-load aarch64-macos 'bound 0' '0000000000001000 <_main>:\n    1000:\tadrp\tx8, 0x2000 <_main+0x1000>\n    1004:\tadd\tx8, x8, #0x10\n    1008:\tldr\tw0, [x8, #0x4]\n    100c:\tret\n'
# LCB's frame: the link register saved and restored by a single str/ldr; a Mach-O stub entry named
# by the indirect symbol table (a `stub` line); a call made in a loop (the link register meets).
bound_is a64-single-lr-save-and-stub aarch64-macos 'bound 48 external write 48' '0000000000001000 <_main>:\n    1000:\tsub\tsp, sp, #0x30\n    1004:\tstr\tx30, [sp, #0x20]\n    1008:\tmov\tw0, #0x2\n    100c:\tbl\t0x1040 <_write+0x1040>\n    1010:\tcbnz\tw0, 0x1008 <_main+0x8>\n    1014:\tldr\tx30, [sp, #0x20]\n    1018:\tadd\tsp, sp, #0x30\n    101c:\tret\n\nDisassembly of section __TEXT,__stubs:\n\n0000000000001040 <__stubs>:\n    1040:\tadrp\tx16, 0x2000\n    1044:\tldr\tx16, [x16]\n    1048:\tbr\tx16\nstub 0x0000000000001040 _write\n'
# A read of bytes the function wrote on both paths into it is accepted.
bound_is both-path-write x86_64-linux 'bound 16' '0000000000001010 <main>:\n    1010:\tpush\trbp\n    1011:\tmov\trbp, rsp\n    1014:\tje\t0x101c <main+0xc>\n    1016:\tmov\tqword ptr [rbp - 0x8], rax\n    101a:\tjmp\t0x1020 <main+0x10>\n    101c:\tmov\tqword ptr [rbp - 0x8], rcx\n    1020:\tmov\trax, qword ptr [rbp - 0x8]\n    1024:\tpop\trbp\n    1025:\tret\n'
bound_is x86-thread-pointer-canary x86_64-linux 'bound 0' '0000000000001010 <main>:\n    1010:\tmov\trax, qword ptr fs:[0x28]\n    1019:\tret\n'
bound_is x86-pc-relative-wide-edge x86_64-linux 'bound 0' '0000000000001010 <main>:\n    1010:\tmov\trax, qword ptr [rip + 0xfffff9]\n    1015:\tret\n'
bound_is x86-lea-pc-relative x86_64-linux 'bound 0' '0000000000001010 <main>:\n    1010:\tlea\trdi, [rip + 0xdf5]\n    1015:\tret\n'
# Every supported width is checked at both ends of both fs intervals. The last accepted positive
# offset ends at 0x6f; the negative interval is [-0x80, 0). One byte farther is refused, as is
# every negative access when PT_TLS is absent. Width one at offset zero is TCB, not negative TLS.
for spec in byte:1 word:2 dword:4 qword:8 tbyte:10 xmmword:16 ymmword:32 zmmword:64; do
  width_name=${spec%%:*} width=${spec##*:}
  printf -v offset '0x%x' "$((0x70 - width))"
  printf -v body '0000000000001010 <main>:\n    1010:\tmov\trax, %s ptr fs:[%s]\n    1019:\tret\n' "$width_name" "$offset"
  bound_is "x86-fs-tcb-$width_name-edge" x86_64-linux 'bound 0' "$body"
  printf -v offset '0x%x' "$((0x70 - width + 1))"
  refuse_tp "x86-fs-tcb-$width_name-overflow" "    1010:\tmov\trax, $width_name ptr fs:[$offset]\n    1019:\tret\n"
  printf -v offset -- '-0x%x' "$width"
  printf -v body '0000000000001010 <main>:\n    1010:\tmov\trax, %s ptr fs:[%s]\n    1019:\tret\n' "$width_name" "$offset"
  bound_is "x86-fs-tls-$width_name-top-edge" x86_64-linux 'bound 0' "$body" 0x80
  refuse_tp "x86-fs-tls-$width_name-no-segment" "    1010:\tmov\trax, $width_name ptr fs:[$offset]\n    1019:\tret\n"
  if [ "$width" -gt 1 ]; then
    printf -v offset -- '-0x%x' "$((width - 1))"
    refuse_tp "x86-fs-tls-$width_name-crosses-tcb" "    1010:\tmov\trax, $width_name ptr fs:[$offset]\n    1019:\tret\n" 0x80
  fi
  printf -v body '0000000000001010 <main>:\n    1010:\tmov\trax, %s ptr fs:[-0x80]\n    1019:\tret\n' "$width_name"
  bound_is "x86-fs-tls-$width_name-bottom-edge" x86_64-linux 'bound 0' "$body" 0x80
  refuse_tp "x86-fs-tls-$width_name-underflow" "    1010:\tmov\trax, $width_name ptr fs:[-0x81]\n    1019:\tret\n" 0x80
done
bound_is x86-fs-wrapped-negative x86_64-linux 'bound 0' '0000000000001010 <main>:\n    1010:\tmov\trax, byte ptr fs:[0xffffffffffffffff]\n    1019:\tret\n' 0x1
refuse_tp x86-fs-wrapped-interval-crosses-tcb '    1010:\tmov\trax, word ptr fs:[0xffffffffffffffff]\n    1019:\tret\n' 0x1
# Variant II aligned PT_TLS reservations: p_memsz=1, p_align=64, p_vaddr=0x3dc0 reserves 0x40 bytes
# ([-0x40, 0)); p_memsz=8, p_align=16, p_vaddr=0x3dc0 reserves 0x10 bytes ([-0x10, 0)).
bound_is x86-fs-tls-align64-memsz1-edge x86_64-linux 'bound 0' '0000000000001010 <main>:\n    1010:\tmov\trax, byte ptr fs:[-0x40]\n    1019:\tret\n' '0x1 0x40 0x3dc0'
refuse_tp x86-fs-tls-align64-memsz1-underflow '    1010:\tmov\trax, byte ptr fs:[-0x41]\n    1019:\tret\n' '0x1 0x40 0x3dc0'
bound_is x86-fs-tls-align16-memsz8-edge x86_64-linux 'bound 0' '0000000000001010 <main>:\n    1010:\tmov\trax, qword ptr fs:[-0x10]\n    1019:\tret\n' '0x8 0x10 0x3dc0'
refuse_tp x86-fs-tls-align16-memsz8-underflow '    1010:\tmov\trax, byte ptr fs:[-0x11]\n    1019:\tret\n' '0x8 0x10 0x3dc0'
# Parser controls use GDB's actual spellings, including its unsigned rendering of negative offsets,
# and check fail-closed rejection of string ops, xlat, enter, segment overrides, segment-register
# uses, malformed expressions, and invalid ELF program-header JSON.
if bounded 60 python3 -I -c '
import json, os, sys, tempfile
sys.path.insert(0, sys.argv[1])
import stack_access, stack_bound
base = 0x7fff00000000
def reg(name):
    if name == "fs_base":
        return base
    if name in ("rsp", "rbp", "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "sp", "x8", "x9", "x10"):
        return 0x7fffffffe000
    raise ValueError("unexpected register " + name)
cases = {
    "mov    rax,QWORD PTR fs:0x28": [base + 0x28],
    "mov    BYTE PTR fs:0x100000,0x0": [base + 0x100000],
    "mov    rax,QWORD PTR fs:0xffffffffffffffd8": [(base - 0x28) & stack_access.MASK],
    "movabs rax,fs:0x100": [base + 0x100],
    "mov    rax,fs:QWORD PTR [0x28]": [base + 0x28],
    "syscall": [],
}
for asm, expected in cases.items():
    addresses = stack_access.x86_addresses(asm, reg, 0)
    assert addresses == expected, (asm, addresses, expected)
addresses = stack_access.x86_addresses("mov BYTE PTR fs:0x100000,0x0", reg, 0)
sp0 = base + 0x100000 + 128
assert stack_access.lowest_stack(addresses, sp0, sp0, 128, 8 << 20) == base + 0x100000
rejected_x86 = (
    "mov rax,QWORD PTR fs:bogus",
    "mov rax,QWORD PTR [rsp",
    "mov BYTE PTR ds:garbage,0x0",
    "mov BYTE PTR ds:0x100000,0x0",
    "mov BYTE PTR cs:[rsp-0x8],0x0",
    "mov BYTE PTR ds:[rsp-0x8],0x0",
    "mov BYTE PTR es:[rsp-0x8],0x0",
    "mov BYTE PTR gs:0x0,0x0",
    "mov BYTE PTR gs:[0x0],0x0",
    "mov BYTE PTR ss:[rsp-0x8],0x0",
    "mov rax,gs:QWORD PTR [rip+0x10]",
    "mov fs:QWORD PTR [rsp-0x8],rax",
    "mov rax,QWORD PTR fs:[rsp-0x8]",
    "mov eax,fs",
    "mov rax,gs",
    "mov fs,eax",
    "cmp fs,eax",
    "mov rax,BYTE PTR fs:[- -0x100000]",
    "mov rax,BYTE PTR fs:[-0x100000 0x100000]",
    "mov QWORD PTR [rsp-0x8],QWORD PTR fs:0x28",
    "rep movs QWORD PTR es:[rdi],QWORD PTR ds:[rsi]",
    "rep stos BYTE PTR es:[rdi],al",
    "movs QWORD PTR es:[rdi],QWORD PTR ds:[rsi]",
    "stos BYTE PTR es:[rdi],al",
    "lods BYTE PTR ds:[rsi]",
    "cmps BYTE PTR ds:[rsi],BYTE PTR es:[rdi]",
    "scas BYTE PTR es:[rdi]",
    "xlat BYTE PTR ds:[rbx]",
    "enter 0x10,0x0",
    "loop 0x1010",
    "call rax",
    "ret 0x8",
)
for asm in rejected_x86:
    try:
        stack_access.x86_addresses(asm, reg, 0)
    except ValueError:
        pass
    else:
        raise AssertionError("accepted unmodelled x86 instruction or operand: " + asm)
a64_cases = (
    ("stp", "x29, x30, [sp, #-0x10]!", [0x7fffffffdff0], 0x7fffffffdff0),
    ("ldp", "x29, x30, [sp], #0x10", [0x7fffffffe000], 0x7fffffffe000),
    ("ldr", "w8, [x9, x10, lsl #2]", [(0x7fffffffe000 + (0x7fffffffe000 << 2)) & stack_access.MASK], 0x7fffffffe000),
    ("ldr", "w8, [x9, w10, sxtw #2]", [(0x7fffffffe000 + (-0x2000 << 2)) & stack_access.MASK], 0x7fffffffe000),
    ("ldr", "w8, [x9, w10, uxtw #2]", [(0x7fffffffe000 + (0xffffe000 << 2)) & stack_access.MASK], 0x7fffffffe000),
    ("ldr", "q0, [sp, #0x10]", [0x7fffffffe010], 0x7fffffffe000),
    ("mov", "w8, #0x64 ; =100", [], 0x7fffffffe000),
    ("adds", "w8, w9, w9, uxtb", [], 0x7fffffffe000),
    ("cbnz", "w8, 0x1008 <_main+0x8>", [], 0x7fffffffe000),
    ("nop", "", [], 0x7fffffffe000),
)
for mn, ops, expected_addrs, expected_sp in a64_cases:
    got_addrs = stack_access.a64_addresses(mn, ops, reg)
    got_sp = stack_access.a64_new_sp(mn, ops, reg, 0x7fffffffe000)
    assert got_addrs == expected_addrs, (mn, ops, got_addrs, expected_addrs)
    assert got_sp == expected_sp, (mn, ops, got_sp, expected_sp)
rejected_a64 = (
    ("mrs", "x8, tpidr_el0"),
    ("nop", "garbage"),
    ("ldr", "x0, 0x1000"),
    ("ldr", "x0, sp"),
    ("str", "x0, [sp], x8"),
    ("ldr", "x0, [sp, #-0x8]!, #0x8"),
    ("stur", "x0, [sp, #-0x8]!"),
    ("ldr", "x0, [sp, x8]!"),
    ("ldp", "x0, x1, [sp, x8]"),
    ("ldr", "x0, [sp, w8]"),
    ("ldr", "x0, [sp, x8, sxtw #2]"),
    ("ldr", "x0, [sp, x8, ror #1]"),
    ("ldr", "x0, [sp] [sp]"),
    ("ldr", "x0, [sp"),
    ("ldr", "sp, [sp, #0x8]"),
    ("ldr", "x0, [wsp, #-0x8]"),
    ("ldr", "x0, [w8, #-0x8]"),
    ("mov", "x0, [sp]"),
    ("mov", "x0"),
    ("mov", "x0, , x1"),
    ("adrp", "x8, [sp]"),
    ("cset", "w0, bogus"),
    ("add", "x0, x1, x2, ror #1"),
    ("add", "x0, x1, #0x1, uxtw"),
    ("movz", "x0, #0x1, lsl #8"),
    ("mul", "x0, x1, #0x2"),
    ("movz", "x0, x1"),
    ("ret", "x0"),
    ("blr", "x8"),
    ("cbz", "sp, 0x1010 <_main>"),
    ("tbz", "x0, #64, 0x1010 <_main>"),
    ("brk", "garbage"),
)
for mn, ops in rejected_a64:
    try:
        stack_access.a64_addresses(mn, ops, reg)
    except ValueError:
        pass
    else:
        raise AssertionError(f"accepted unmodelled a64 instruction: {mn} {ops}")
assert stack_access.tls_reservation(1, 64, 0x3dc0) == 0x40
assert stack_access.tls_reservation(8, 16, 0x3dc0) == 0x10
assert stack_access.tls_reservation(0, 64, 0x3dc0) == 0
with tempfile.TemporaryDirectory() as td:
    for name, payload in (
        ("empty", []),
        ("no-load", [{"ProgramHeaders": []}]),
        ("two-tls", [{"ProgramHeaders": [
            {"ProgramHeader": {"Type": {"Name": "PT_LOAD"}}},
            {"ProgramHeader": {"Type": {"Name": "PT_TLS"}, "MemSize": 8, "FileSize": 8, "Alignment": 8, "VirtualAddress": 0}},
            {"ProgramHeader": {"Type": {"Name": "PT_TLS"}, "MemSize": 8, "FileSize": 8, "Alignment": 8, "VirtualAddress": 0}},
        ]}]),
        ("bad-align", [{"ProgramHeaders": [
            {"ProgramHeader": {"Type": {"Name": "PT_LOAD"}}},
            {"ProgramHeader": {"Type": {"Name": "PT_TLS"}, "MemSize": 8, "FileSize": 8, "Alignment": 3, "VirtualAddress": 0}},
        ]}]),
        ("filesz-gt-memsz", [{"ProgramHeaders": [
            {"ProgramHeader": {"Type": {"Name": "PT_LOAD"}}},
            {"ProgramHeader": {"Type": {"Name": "PT_TLS"}, "MemSize": 4, "FileSize": 8, "Alignment": 8, "VirtualAddress": 0}},
        ]}]),
    ):
        p = os.path.join(td, name + ".json")
        with open(p, "w", encoding="utf-8") as f:
            json.dump(payload, f)
        try:
            stack_bound.elf_tls_line(p)
        except stack_bound.Refuse:
            pass
        else:
            raise AssertionError("accepted invalid program-header JSON: " + name)
' "$root/scripts/checks/lib"; then
  echo "ok   control: shared parser handles GDB/objdump forms and fails closed on unmodelled operands/headers"
else
  echo "FAIL control: shared parser GDB/objdump forms or fail-closed handling"; fail=1
fi
if [ -n "$gdb" ]; then
  # Real GDB disassembly prints these absolute operands without brackets. Set FS so fs:0x100000 is
  # exactly main_entry_sp - 128, then restore it before returning to libc. After restoring FS,
  # read the 1-byte, 64-byte-aligned .tdata variable (`tls_var@TPOFF`, relocated by the linker to
  # fs:[-0x40]) and the qword canary at fs:0x28. No synthetic reg callback or analysis is involved.
  tp="$work/ctl/thread-pointer"; mkdir -p "$tp"
  cat >"$tp/control.s" <<'ASM'
	.intel_syntax noprefix
	.text
	.globl	main
	.type	main,@function
main:
	mov	eax, 158
	mov	edi, 0x1003
	lea	rsi, [rsp - 0x8]
	syscall
	mov	r10, QWORD PTR [rsp - 0x8]
	mov	rsi, rsp
	sub	rsi, 0x100080
	mov	eax, 158
	mov	edi, 0x1002
	syscall
	mov	BYTE PTR fs:[0x100000], 0
	mov	rsi, r10
	mov	eax, 158
	mov	edi, 0x1002
	syscall
	mov	al, BYTE PTR fs:[tls_var@TPOFF]
	mov	rax, QWORD PTR fs:[0x28]
	mov	eax, 7
	ret
	.size	main, .-main
	.section	.tdata,"awT",@progbits
	.p2align 6
tls_var:
	.byte	1
	.section	.note.GNU-stack,"",@progbits
ASM
  if ! bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" control "$tp" >"$tp/build.log" 2>&1; then
    echo "FAIL control: the thread-pointer executable does not build: $(head -c 1000 "$tp/build.log")"; fail=1
  else
    "${objdump[@]}" "$tp/control.exe" >"$tp/control.exe.dis"
    if ! tp_range=$(awk '
      $2 == "<main>:" { inside = 1; lo = "0x" $1; next }
      inside && /^[0-9a-f]+ </ { exit }
      inside && $1 ~ /^[0-9a-f]+:$/ { hi = "0x" substr($1, 1, length($1) - 1) }
      END { if (!lo || !hi) exit 1; print lo, hi }' "$tp/control.exe.dis"); then
      echo "FAIL control: thread-pointer main has no disassembly range"; fail=1
    else
      tp_analysis=$(printf 'redzone 128\nrange main %s\n' "$tp_range")
      tpm=$(measure "$tp/control.exe" "$tp_analysis" 7)
      if [ "$tpm" = "measured 128" ]; then
        echo "ok   control: real GDB counts BYTE PTR fs:0x100000, fs:[-0x40] and QWORD PTR fs:0x28"
      else
        echo "FAIL control: real GDB thread-pointer run: ${tpm:-none} $(head -c 300 "$tp/control.exe.gdb.err")"; fail=1
      fi
      tpm=$(STACK_UNAVAILABLE_REGS=fs_base measure "$tp/control.exe" "$tp_analysis" 7)
      if grep -q '^error .*fs_base is unavailable' <<<"$tpm"; then
        echo "ok   control: real GDB fails closed when fs_base is unavailable"
      else
        echo "FAIL control: unavailable fs_base gave a measurement or wrong error: ${tpm:-none}"; fail=1
      fi
      if append_tls "$tp/control.o" "$work/ctl/relocatable-obj.dis" 2>/dev/null; then
        echo "FAIL control: append_tls accepted a relocatable object with no PT_LOAD"; fail=1
      else
        echo "ok   control: append_tls refuses a relocatable object with no PT_LOAD"
      fi
      tls_dis="$work/ctl/real-pt-tls.dis"
      tls_under="$work/ctl/real-pt-tls-underflow.dis"
      printf '\n0000000000001010 <main>:\n    1010:\tmov\trax, byte ptr fs:[-0x40]\n    1019:\tret\n' >"$tls_dis"
      printf '\n0000000000001010 <main>:\n    1010:\tmov\trax, byte ptr fs:[-0x41]\n    1019:\tret\n' >"$tls_under"
      tls_memsz=0x0
      if append_tls "$tp/control.exe" "$tls_dis" &&
         append_tls "$tp/control.exe" "$tls_under" &&
         tls_memsz=$(awk '$1 == "tls_memsz" {print $2}' "$tls_dis") &&
         [ "$tls_memsz" = 0x40 ] &&
         [ "$(stack_bound x86_64-linux "$tls_dis")" = "bound 0" ] &&
         ! stack_bound x86_64-linux "$tls_under" >/dev/null 2>&1; then
        echo "ok   control: linked PT_TLS (p_memsz=1, p_align=64 -> $tls_memsz) accepts fs:[-0x40] and refuses fs:[-0x41]"
      else
        echo "FAIL control: linked PT_TLS (p_memsz=1, p_align=64) gave $tls_memsz"; fail=1
      fi
      tp16="$work/ctl/thread-pointer-align16"; mkdir -p "$tp16"
      cat >"$tp16/control.s" <<'ASM16'
	.intel_syntax noprefix
	.text
	.globl	main
	.type	main,@function
main:
	mov	rax, QWORD PTR fs:[tls_qword@TPOFF]
	xor	eax, eax
	ret
	.size	main, .-main
	.section	.tdata,"awT",@progbits
	.p2align 4
tls_qword:
	.quad	0
	.section	.note.GNU-stack,"",@progbits
ASM16
      tls16_dis="$work/ctl/real-pt-tls-16.dis"
      tls16_under="$work/ctl/real-pt-tls-16-underflow.dis"
      printf '\n0000000000001010 <main>:\n    1010:\tmov\trax, qword ptr fs:[-0x10]\n    1019:\tret\n' >"$tls16_dis"
      printf '\n0000000000001010 <main>:\n    1010:\tmov\trax, byte ptr fs:[-0x11]\n    1019:\tret\n' >"$tls16_under"
      tls16_memsz=0x0
      if bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" control "$tp16" >"$tp16/build.log" 2>&1 &&
         append_tls "$tp16/control.exe" "$tls16_dis" &&
         append_tls "$tp16/control.exe" "$tls16_under" &&
         tls16_memsz=$(awk '$1 == "tls_memsz" {print $2}' "$tls16_dis") &&
         [ "$tls16_memsz" = 0x10 ] &&
         [ "$(stack_bound x86_64-linux "$tls16_dis")" = "bound 0" ] &&
         ! stack_bound x86_64-linux "$tls16_under" >/dev/null 2>&1; then
        echo "ok   control: linked PT_TLS (p_memsz=8, p_align=16 -> $tls16_memsz) accepts fs:[-0x10] and refuses fs:[-0x11]"
      else
        echo "FAIL control: linked PT_TLS (p_memsz=8, p_align=16) gave $tls16_memsz"; fail=1
      fi
    fi
  fi
fi
if [ -n "$measuring" ]; then
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

# The red zone (stack_bound.py TARGETS: 128 bytes on x86-64 System V and Apple arm64, none under
# AAPCS64 on Linux): a store at its edge is in the bound, one past it is refused.
refuse x86-beyond-red-zone 'beyond the 128-byte red zone' '    1010:\tmov\tqword ptr [rsp - 0x88], 0x0\n    101c:\tret\n'
bound_is x86-red-zone-edge x86_64-linux 'bound 128' '0000000000001010 <main>:\n    1010:\tmov\tqword ptr [rsp - 0x80], 0x0\n    1019:\tret\n'
refuse a64-beyond-red-zone 'beyond the 128-byte red zone' '    1010:\tstur\txzr, [sp, #-0x88]\n    1014:\tret\n' aarch64-macos _main
bound_is a64-red-zone-edge aarch64-macos 'bound 128' '0000000000001010 <_main>:\n    1010:\tstur\txzr, [sp, #-0x80]\n    1014:\tret\n'
refuse a64-linux-no-red-zone 'beyond the 0-byte red zone' '    1010:\tstur\txzr, [sp, #-0x8]\n    1014:\tret\n' aarch64-linux main
# Mutation check: every guard in stack_access.py and stack_bound.py must be load-bearing. Deleting
# or neutralizing a guard must cause its named control in refuse_manifest.tsv to fail.
if bounded 60 python3 -I -c '
import os, re, sys, types
lib_dir, ctl_dir = sys.argv[1], sys.argv[2]
with open(os.path.join(lib_dir, "stack_access.py"), encoding="utf-8") as f:
    sa_src = f.read()
with open(os.path.join(lib_dir, "stack_bound.py"), encoding="utf-8") as f:
    sb_src = f.read()
controls = {
    "no-tls-metadata": ("x86_64-linux", os.path.join(ctl_dir, "no-tls-metadata.dis"), "x86 analysis without the linked executable.s PT_TLS"),
    "duplicate-tls-metadata": ("x86_64-linux", os.path.join(ctl_dir, "duplicate-tls-metadata.dis"), "malformed or duplicate PT_TLS metadata"),
}
with open(os.path.join(ctl_dir, "refuse_manifest.tsv"), encoding="utf-8") as f:
    for line in f:
        name, tgt, dis, diag = line.rstrip("\n").split("\t", 3)
        controls[name] = (tgt, dis, diag)

def run_mutated(which, old, new, target, dis_path):
    assert (sa_src if which == "stack_access.py" else sb_src).count(old) == 1, f"guard not unique in {which}: {old!r}"
    m_sa = sa_src.replace(old, new, 1) if which == "stack_access.py" else sa_src
    m_sb = sb_src.replace(old, new, 1) if which == "stack_bound.py" else sb_src
    sa_mod = types.ModuleType("stack_access")
    exec(compile(m_sa, "stack_access.py", "exec"), sa_mod.__dict__)
    old_sa = sys.modules.get("stack_access")
    sys.modules["stack_access"] = sa_mod
    try:
        sb_mod = types.ModuleType("stack_bound")
        sb_mod.__file__ = os.path.join(lib_dir, "stack_bound.py")
        exec(compile(m_sb, "stack_bound.py", "exec"), sb_mod.__dict__)
        sb_mod.TARGET = target
        sb_mod.ARCH, sb_mod.RED_ZONE = sb_mod.TARGETS[target]
        try:
            funcs, stubs, sb_mod.TLS_MEMSZ = sb_mod.parse(dis_path)
            total, _, _ = sb_mod.analyse(funcs, stubs)
            return f"bound {total}"
        except sb_mod.Refuse as e:
            return f"refused: {e}"
    finally:
        if old_sa is not None:
            sys.modules["stack_access"] = old_sa

mutations = [
    ("stack_access.py", "if a & (a - 1):\n        raise ValueError(f\"PT_TLS p_align {align:#x} is not a power of two\")", "pass", "non-power-of-two-tls-align"),
    ("stack_access.py", "if not inner or not inner.strip():\n        raise ValueError(f\"empty memory address: {what}\")", "if not inner or not inner.strip():\n        return [(\"imm\", None, 0)], [], False, 0", "x86-empty-fs-address"),
    ("stack_access.py", "if not tm or (idx > 0 and tm.group(1) not in (\"+\", \"-\")):\n            raise ValueError(f\"malformed memory address: {what}\")", "if not tm:\n            break", "x86-duplicate-sign-address"),
    ("stack_access.py", "if not tm or (idx > 0 and tm.group(1) not in (\"+\", \"-\")):\n            raise ValueError(f\"malformed memory address: {what}\")", "if not tm:\n            raise ValueError(f\"malformed memory address: {what}\")", "x86-signless-second-term"),
    ("stack_access.py", "if not X86_PLAIN.match(mn):\n        raise ValueError(f\"unmodelled instruction: {what}\")", "if not X86_PLAIN.match(mn):\n        return [], []", "loop-instruction"),
    ("stack_access.py", "if op in X86_SEGMENTS:\n            if i == 0 and mn not in (\"cmp\", \"test\"):\n                raise ValueError(f\"write to a segment register: {what}\")\n            raise ValueError(f\"segment register used as a value: {what}\")", "if op in X86_SEGMENTS:\n            parsed_ops.append((\"reg\", op))\n            continue", "x86-segment-write-fs"),
    ("stack_access.py", "if i == 0 and mn not in (\"cmp\", \"test\"):\n                raise ValueError(f\"write to a segment register: {what}\")\n            raise ValueError(f\"segment register used as a value: {what}\")", "if i == 0 and mn not in (\"cmp\", \"test\"):\n                raise ValueError(f\"write to a segment register: {what}\")\n            parsed_ops.append((\"reg\", op))\n            continue", "x86-segment-read-fs"),
    ("stack_access.py", "if i == 0 and mn not in (\"cmp\", \"test\"):\n                raise ValueError(f\"write to a segment register: {what}\")\n            raise ValueError(f\"segment register used as a value: {what}\")", "if i == 0 and mn not in (\"cmp\", \"test\"):\n                raise ValueError(f\"write to a segment register: {what}\")\n            parsed_ops.append((\"reg\", op))\n            continue", "x86-segment-cmp-fs"),
    ("stack_access.py", "if bm is None and (am is None or not (am.group(\"seg1\") or am.group(\"seg2\"))):\n            raise ValueError(f\"unmodelled operand {op!r}: {what}\")", "if bm is None and (am is None or not (am.group(\"seg1\") or am.group(\"seg2\"))):\n            continue", "x86-unclosed-bracket"),
    ("stack_access.py", "am = None if bm is not None else _ABS_SEG_MEM.fullmatch(op)", "am = None if bm is not None else None\n        if bm is None:\n            continue", "fs-unbracketed-beyond-reach"),
    ("stack_access.py", "segment = seg1 or seg2", "segment = seg2", "gs-before-size-ptr"),
    ("stack_access.py", "segment = seg1 or seg2", "segment = seg2", "fs-before-size-ptr"),
    ("stack_access.py", "elif mn == \"movabs\" and not bracketed and len(raw_ops) == 2 and raw_ops[1 - i] in X86_ACC_SIZE:\n                width = X86_ACC_SIZE[raw_ops[1 - i]]", "elif mn == \"movabs\":\n                continue", "fs-movabs-beyond-reach"),
    ("stack_access.py", "if segment:\n                raise ValueError(f\"segment override on LEA: {what}\")", "pass", "x86-lea-segment-override"),
    ("stack_access.py", "if segment == \"gs\":\n                raise ValueError(f\"gs segment override on x86_64-linux: {what}\")", "if segment == \"gs\":\n                segment = \"fs\"", "gs-absolute"),
    ("stack_access.py", "if segment == \"gs\":\n                raise ValueError(f\"gs segment override on x86_64-linux: {what}\")", "if segment == \"gs\":\n                segment = None", "x86-gs-through-rbp"),
    ("stack_access.py", "if segment and segment != \"fs\":\n                raise ValueError(f\"unmodelled {segment} segment override: {what}\")", "pass", "x86-cs-stack-override"),
    ("stack_access.py", "if segment == \"fs\" and regs:\n                raise ValueError(f\"thread-pointer-relative access through a register: {what}\")", "pass", "x86-fs-through-rsp"),
    ("stack_access.py", "if segment == \"fs\" and regs:\n                raise ValueError(f\"thread-pointer-relative access through a register: {what}\")", "pass", "x86-fs-through-rbp"),
    ("stack_access.py", "if segment == \"fs\" and regs:\n                raise ValueError(f\"thread-pointer-relative access through a register: {what}\")", "pass", "x86-fs-through-rip"),
    ("stack_access.py", "if segment == \"fs\" and regs:\n                raise ValueError(f\"thread-pointer-relative access through a register: {what}\")", "pass", "x86-fs-through-rax"),
    ("stack_access.py", "if len(memory) > 1:\n        raise ValueError(f\"instruction with more than one memory operand: {what}\")", "pass", "x86-two-memory-operands"),
    ("stack_access.py", "if len(memory) > 1:\n        raise ValueError(f\"instruction with more than one memory operand: {what}\")", "pass", "x86-two-memory-operands-unbracketed"),
    ("stack_bound.py", "if ARCH == \"x86_64\" and TLS_MEMSZ is None:\n        raise Refuse(\"x86 analysis without the linked executable'\''s PT_TLS\")", "pass", "no-tls-metadata"),
    ("stack_bound.py", "if not m or tls_memsz is not None:\n                raise Refuse(f\"malformed or duplicate PT_TLS metadata: {line}\")", "if not m:\n                raise Refuse(f\"malformed or duplicate PT_TLS metadata: {line}\")", "duplicate-tls-metadata"),
    ("stack_bound.py", "if not isinstance(fp, int):\n        raise Refuse(f\"frame-relative use of a frame pointer that is not this frame'\''s: {what}\")", "if not isinstance(fp, int):\n        return 8", "partial-frame-pointer"),
    ("stack_bound.py", "if lo < 1:\n        raise Refuse(f\"touches the return address or the caller'\''s frame: {what}\")", "pass", "return-address-overwrite"),
    ("stack_bound.py", "if \"r\" in mode or not exact:\n                raise Refuse(f\"touches a saved frame pointer or link register: {what}\")", "pass", "freed-slot-reread"),
    ("stack_bound.py", "if \"r\" in mode or not exact:\n                raise Refuse(f\"touches a saved frame pointer or link register: {what}\")", "pass", "restored-slot-reread"),
    ("stack_bound.py", "if \"r\" in mode or not exact:\n                raise Refuse(f\"touches a saved frame pointer or link register: {what}\")", "pass", "a64-restored-record-reread"),
    ("stack_bound.py", "if \"r\" in mode and not all(b in st.w for b in range(lo, hi + 1)):\n        raise Refuse(f\"reads stack bytes this function has not written: {what}\")", "pass", "unwritten-read"),
    ("stack_bound.py", "if any(r in X86_STACK for r in regs):\n                raise Refuse(f\"stack address taken as a value: {what}\")", "if any(r in X86_STACK for r in regs):\n                continue", "stack-address-taken"),
    ("stack_bound.py", "elif regs != [\"rip\"] or scaled:\n                raise Refuse(f\"LEA through a register that may hold a stack address, or absolutely: {what}\")", "elif regs != [\"rip\"] or scaled:\n                continue", "x86-lea-non-rip"),
    ("stack_bound.py", "if abs(lo) > PC_REACH:\n                raise Refuse(f\"PC-relative LEA beyond the premise'\''s reach: {what}\")", "pass", "x86-lea-pc-relative-beyond"),
    ("stack_bound.py", "if not (in_tcb or in_tls):\n                    raise Refuse(f\"thread-pointer-relative access beyond the premise'\''s reach: {what}\")", "pass", "fs-beyond-reach"),
    ("stack_bound.py", "in_tcb = 0 <= lo and hi < TCB_SIZE", "in_tcb = 0 <= lo and lo < TCB_SIZE", "x86-fs-tcb-qword-overflow"),
    ("stack_bound.py", "in_tls = -TLS_MEMSZ <= lo and hi < 0", "in_tls = -TLS_MEMSZ <= lo and lo < 0", "x86-fs-tls-qword-crosses-tcb"),
    ("stack_bound.py", "in_tls = -TLS_MEMSZ <= lo and hi < 0", "in_tls = -TLS_MEMSZ - 1 <= lo and hi < 0", "x86-fs-tls-align64-memsz1-underflow"),
    ("stack_bound.py", "if lo < -PC_REACH or hi > PC_REACH:\n                    raise Refuse(f\"PC-relative access beyond the premise'\''s reach: {what}\")", "pass", "x86-pc-relative-wide-beyond"),
    ("stack_bound.py", "else:\n                raise Refuse(f\"memory addressed through a register that may hold a stack address, \"\n                             f\"or absolutely: {what}\")", "else:\n                continue", "argv-store"),
    ("stack_bound.py", "else:\n                raise Refuse(f\"memory addressed through a register that may hold a stack address, \"\n                             f\"or absolutely: {what}\")", "else:\n                continue", "absolute-address"),
    ("stack_bound.py", "if base[0] not in (\"rsp\", \"rbp\"):\n            raise Refuse(f\"stack address through a partial register: {what}\")", "if base[0] not in (\"rsp\", \"rbp\", \"esp\"):\n            raise Refuse(f\"stack address through a partial register: {what}\")", "partial-stack-address"),
    ("stack_bound.py", "if len(regs) != 1 or scaled:\n            raise Refuse(f\"indexed stack address: {what}\")", "pass", "indexed-stack-address"),
    ("stack_bound.py", "if not X86_GPR64.match(dst):\n            raise Refuse(f\"pop of other than a 64-bit general register: {what}\")", "pass", "partial-stack-pointer"),
    ("stack_bound.py", "if any(r in X86_STACK for r in srcs):\n        raise Refuse(f\"stack or frame pointer used as a value: {what}\")", "pass", "stack-pointer-escape"),
    ("stack_bound.py", "if dst in X86_SP:\n        raise Refuse(f\"unmodelled stack-pointer write: {what}\")", "pass", "dynamic-sp"),
    ("stack_bound.py", "if dst in X86_FP:\n        return st.but(fp=None), deepest", "pass", "partial-frame-pointer"),
    ("stack_bound.py", "if slot and dst != \"rbp\":\n        raise Refuse(f\"a saved frame pointer popped into {dst}: {what}\")", "pass", "saved-frame-pointer-popped"),
    ("stack_bound.py", "if base in A64_FP and writeback:\n            raise Refuse(f\"writeback on the frame pointer: {what}\")", "pass", "a64-frame-writeback"),
    ("stack_bound.py", "if base in A64_LR and writeback:\n            raise Refuse(f\"writeback on the link register: {what}\")", "pass", "a64-link-register-writeback"),
    ("stack_bound.py", "if base in A64_LR and writeback:\n            raise Refuse(f\"writeback on the link register: {what}\")", "pass", "a64-link-register-store-writeback"),
    ("stack_bound.py", "if base not in {x[0] for x in st.g}:\n                raise Refuse(f\"memory addressed through a register that may hold a stack address: {what}\")", "pass", "a64-argv-store"),
    ("stack_bound.py", "if writeback or mode == \"index\" or abs(k) > A64_OFF_REACH:\n                raise Refuse(f\"PC-relative access beyond the premise'\''s reach: {what}\")", "pass", "a64-pc-relative-offset-beyond"),
    ("stack_bound.py", "if writeback or mode == \"index\" or abs(k) > A64_OFF_REACH:\n                raise Refuse(f\"PC-relative access beyond the premise'\''s reach: {what}\")", "pass", "a64-pc-relative-writeback"),
    ("stack_bound.py", "if writeback or mode == \"index\" or abs(k) > A64_OFF_REACH:\n                raise Refuse(f\"PC-relative access beyond the premise'\''s reach: {what}\")", "pass", "a64-pc-relative-indexed"),
    ("stack_bound.py", "if mode == \"index\":\n                raise Refuse(f\"indexed stack address: {what}\")", "if mode == \"index\":\n                return st, None", "a64-indexed-stack-address"),
    ("stack_bound.py", "if tgt is not None and abs(tgt - addr) <= PC_REACH:", "if tgt is not None:", "a64-pc-relative-far"),
    ("stack_bound.py", "        g.add((dst, 1))", "        g.add((dst, 0))", "a64-pc-relative-walk"),
    ("stack_bound.py", "if not slot or slot[2][0] != tag:\n                        raise Refuse(f\"{data[0]} loaded from other than its saved slot: {what}\")", "if not slot or slot[2][0] != tag:\n                        slot = (lo, top, (tag, True))", "a64-lr-from-other-slot"),
    ("stack_bound.py", "if not (record or single) and any(a in A64_SP + A64_FP for a in data):\n                raise Refuse(f\"stack or frame pointer stored as a value: {what}\")", "pass", "a64-frame-pointer-stored"),
    ("stack_bound.py", "if any(a in A64_LR for a in data) and not (record or restored):\n            raise Refuse(f\"link register overwritten: {what}\")", "pass", "a64-frame-record-elsewhere"),
    ("stack_bound.py", "if any(a in A64_FP for a in data) and not restored:\n            st = st.but(fp=None)", "pass", "a64-w29-writeback-load"),
    ("stack_bound.py", "            return st.but(d=f + n if mn == \"sub\" else f - n), None\n        raise Refuse(f\"unmodelled stack-pointer write: {what}\")", "            return st.but(d=f + n if mn == \"sub\" else f - n), None\n        return st, None", "a64-dynamic-sp"),
    ("stack_bound.py", "if any(a in A64_SP + A64_FP for a in srcs):\n            raise Refuse(f\"stack or frame pointer used as a value: {what}\")", "pass", "a64-fp-from-sp-sub"),
    ("stack_bound.py", "if dst in A64_LR:\n        raise Refuse(f\"link register overwritten: {what}\")", "pass", "a64-lr-overwritten"),
    ("stack_bound.py", "if any(a in A64_SP + A64_FP for a in srcs):\n        raise Refuse(f\"stack or frame pointer used as a value: {what}\")", "pass", "a64-sp-escape"),
    ("stack_bound.py", "if key in active:\n            raise Refuse(f\"recursion through {key}\")", "if key in active:\n            return 0, {}", "recursion"),
    ("stack_bound.py", "if old.key() != st.key():\n                raise Refuse(f\"{key}+{addr:#x}: states {old.key()} and {st.key()} meet\")", "pass", "depths-meet"),
    ("stack_bound.py", "if not (old.w <= st.w and old.g <= st.g and (st.lr or not old.lr)):\n                states[j] = old.but(w=old.w & st.w,", "if not (st.w <= old.w and old.g <= st.g and (st.lr or not old.lr)):\n                states[j] = old.but(w=old.w | st.w,", "one-path-write"),
    ("stack_access.py", "if not A64_PLAIN.match(mn):\n        raise ValueError(f\"unmodelled instruction: {what}\")", "if not A64_PLAIN.match(mn):\n        return [], None", "a64-tpidr-el0"),
    ("stack_access.py", "if any(not a for a in raw_ops):\n        raise ValueError(f\"malformed operand list: {what}\")", "if any(not a for a in raw_ops):\n        return [], None", "a64-malformed-operand-list"),
    ("stack_access.py", "if mn in A64_ZERO_OP:\n        if raw_ops:\n            raise ValueError(f\"unexpected operand: {what}\")", "if mn in A64_ZERO_OP:\n        return [], None", "a64-nop-garbage"),
    ("stack_access.py", "if len(raw_ops) not in (n_data + 1, n_data + 2):\n            raise ValueError(f\"wrong operand count: {what}\")", "if len(raw_ops) not in (n_data + 1, n_data + 2):\n            return [], None", "a64-ldst-wrong-arity"),
    ("stack_access.py", "if not all(A64_DATA_REG.fullmatch(r) for r in data):\n            raise ValueError(f\"unmodelled transfer register: {what}\")", "if not all(A64_DATA_REG.fullmatch(r) for r in data):\n            return [], None", "a64-unmodelled-transfer-reg"),
    ("stack_access.py", "if mn.startswith(\"ld\") and any(r in A64_SP for r in data):\n            raise ValueError(f\"stack pointer loaded: {what}\")", "pass", "a64-sp-loaded"),
    ("stack_access.py", "if not m:\n            raise ValueError(f\"unbracketed or malformed memory operand: {what}\")", "if not m:\n            return [], None", "a64-unbracketed-mem"),
    ("stack_access.py", "if not m:\n            raise ValueError(f\"unbracketed or malformed memory operand: {what}\")", "if not m:\n            return [], None", "a64-unbracketed-mem-sp"),
    ("stack_access.py", "if not inner or any(not p for p in inner):\n            raise ValueError(f\"malformed memory address: {what}\")", "if not inner or any(not p for p in inner):\n            return [], None", "a64-empty-mem-address"),
    ("stack_access.py", "if base in (\"wsp\", \"w29\"):\n            raise ValueError(f\"unmodelled stack address: {what}\")", "if base in (\"wsp\", \"w29\"):\n            return [], None", "a64-wsp-base"),
    ("stack_access.py", "if not A64_ADDR_BASE.fullmatch(base):\n            raise ValueError(f\"unmodelled base register {base!r}: {what}\")", "if not A64_ADDR_BASE.fullmatch(base):\n            return [], None", "a64-unmodelled-base-reg"),
    ("stack_access.py", "if post_op is not None and not pre and mn not in A64_UNSCALED_LDST and len(inner) == 1 and _A64_IMM.fullmatch(post_op):\n            mem = (\"post\", data, base, _num(post_op[1:]), None, True)", "if post_op is not None and not pre and mn not in A64_UNSCALED_LDST and len(inner) == 1:\n            mem = (\"offset\", data, base, 0, None, False)", "a64-sp-writeback-reg-post"),
    ("stack_access.py", "if post_op is not None and not pre and mn not in A64_UNSCALED_LDST and len(inner) == 1 and _A64_IMM.fullmatch(post_op):\n            mem = (\"post\", data, base, _num(post_op[1:]), None, True)", "if post_op is not None:\n            mem = (\"pre\", data, base, -8, None, True)", "a64-sp-writeback-pre-and-post"),
    ("stack_access.py", "elif post_op is None and len(inner) == 2 and _A64_IMM.fullmatch(inner[1]) and not (pre and mn in A64_UNSCALED_LDST):", "elif post_op is None and len(inner) == 2 and _A64_IMM.fullmatch(inner[1]):", "a64-sp-writeback-unscaled"),
    ("stack_access.py", "else:\n            raise ValueError(f\"unsupported addressing mode: {what}\")", "else:\n            return [], None", "a64-unsupported-addressing-mode"),
    ("stack_access.py", "elif post_op is None and not pre and mn not in A64_PAIR_LDST and mn not in A64_UNSCALED_LDST and _parse_a64_index(inner[1:]) is not None:", "elif post_op is None and not pre and mn not in A64_UNSCALED_LDST and _parse_a64_index(inner[1:]) is not None:\n            return [], None\n        elif False:", "a64-pair-indexed-addressing-mode"),
    ("stack_access.py", "return None if idx_reg.startswith(\"w\") else (idx_reg, \"lsl\", 0)", "return (idx_reg, \"lsl\", 0)", "a64-unextended-w-index"),
    ("stack_access.py", "if not em or (em.group(1) in (\"uxtw\", \"sxtw\")) != idx_reg.startswith(\"w\"):\n        return None", "if not em:\n        return None", "a64-mismatched-index-extend"),
    ("stack_access.py", "if len(raw_ops) not in A64_ARITY[mn]:\n        raise ValueError(f\"wrong operand count: {what}\")", "if len(raw_ops) not in A64_ARITY[mn]:\n        return [], None", "a64-plain-wrong-arity"),
    ("stack_access.py", "if not _A64_TARGET.fullmatch(op):\n                raise ValueError(f\"unmodelled address target {op!r}: {what}\")", "if not _A64_TARGET.fullmatch(op):\n                return [], None", "a64-bad-adrp-target"),
    ("stack_access.py", "if not _A64_CC.fullmatch(op):\n                raise ValueError(f\"unmodelled condition {op!r}: {what}\")", "if not _A64_CC.fullmatch(op):\n                return [], None", "a64-bad-condition"),
    ("stack_access.py", "if not _valid_a64_mod(mn, parsed_ops[-1][0], op):\n                raise ValueError(f\"unmodelled shift or extend {op!r}: {what}\")", "if not _valid_a64_mod(mn, parsed_ops[-1][0], op):\n                return [], None", "a64-bad-shift-extend"),
    ("stack_access.py", "if prev_kind == \"imm\":\n        return mn in (\"add\", \"adds\", \"sub\", \"subs\", \"cmp\", \"cmn\") and bool(re.fullmatch(r\"lsl\\s+#(0|12)\", op))", "if prev_kind == \"imm\":\n        return True", "a64-bad-imm-extend"),
    ("stack_access.py", "if mn in (\"movz\", \"movk\", \"movn\"):\n        return bool(re.fullmatch(r\"lsl\\s+#(0|16|32|48)\", op))", "if mn in (\"movz\", \"movk\", \"movn\"):\n        return True", "a64-bad-movz-shift"),
    ("stack_access.py", "if (mn, i) not in A64_IMM_OK:\n                raise ValueError(f\"invalid immediate operand: {what}\")", "pass", "a64-invalid-imm"),
    ("stack_access.py", "if (mn, i) not in A64_IMM_REQ and _valid_a64_reg(mn, op):", "if _valid_a64_reg(mn, op):", "a64-missing-required-imm"),
    ("stack_access.py", "        if (mn, i) not in A64_IMM_REQ and _valid_a64_reg(mn, op):\n            parsed_ops.append((\"reg\", op))\n            continue\n        raise ValueError(f\"unmodelled operand {op!r}: {what}\")", "        if (mn, i) not in A64_IMM_REQ and _valid_a64_reg(mn, op):\n            parsed_ops.append((\"reg\", op))\n            continue\n        return [], None", "a64-unmodelled-operand"),
    ("stack_access.py", "if len(parts) != 2 or not A64_GPR_NO_SP.fullmatch(parts[0]):\n            return \"indirect\", ops", "pass", "a64-cbz-bad-reg"),
    ("stack_access.py", "if (len(parts) != 3 or not A64_GPR_NO_SP.fullmatch(parts[0])\n                or not _A64_UIMM.fullmatch(parts[1]) or _num(parts[1][1:]) > 63):\n            return \"indirect\", ops", "pass", "a64-tbz-bad-bit"),
    ("stack_access.py", "if ops and not _A64_UIMM.fullmatch(ops):\n            return \"indirect\", ops", "pass", "a64-brk-bad-imm"),
    ("stack_bound.py", "if touched is not None and touched > max(before, st.d) + RED_ZONE:\n                    raise Refuse(f\"{key}+{addr:#x}: an access {touched - max(before, st.d)} bytes below the \"\n                                 f\"stack pointer, beyond the {RED_ZONE}-byte red zone of {TARGET}: {mn} {ops}\")", "pass", "x86-beyond-red-zone"),
    ("stack_bound.py", "if kind == \"indirect\":\n                raise Refuse(f\"{key}+{addr:#x}: indirect control transfer {mn} {ops}\")", "if kind == \"indirect\":\n                continue", "a64-ret-register"),
    ("stack_bound.py", "if not st.lr:\n                    raise Refuse(f\"{key}+{addr:#x}: return through a link register not restored\")", "pass", "a64-link-register-lost"),
    ("stack_bound.py", "if st.fp != ENTRY:\n                    raise Refuse(f\"{key}+{addr:#x}: return without the caller'\''s frame pointer restored\")", "pass", "callee-clobbers-frame-pointer"),
    ("stack_bound.py", "if tgt is None:\n                raise Refuse(f\"{key}+{addr:#x}: indirect {mn} {ops}\")", "if tgt is None:\n                flow(i + 1, st, addr); continue", "indirect-call"),
    ("stack_bound.py", "raise Refuse(f\"{key}+{addr:#x}: call to {sym}, neither owned nor a stub\")", "pass", "unknown-call"),
    ("stack_bound.py", "w=frozenset(b for b in st.w if b <= d)", "w=st.w", "callee-residual-reread"),
    ("stack_bound.py", "w=frozenset(b for b in st.w if b <= d)", "w=st.w", "a64-callee-residual-reread"),
    ("stack_bound.py", "g=frozenset(),", "g=st.g,", "a64-pc-relative-after-call"),
]
for which, old, new, ctl_name in mutations:
    assert ctl_name in controls, f"unknown control {ctl_name}"
    tgt, dis_path, diag_re = controls[ctl_name]
    out = run_mutated(which, old, new, tgt, dis_path)
    if re.search(r"^refused: .*" + diag_re, out):
        raise AssertionError(f"control {ctl_name} still passed when its guard in {which} was deleted: {out}")
    if ctl_name in ("a64-link-register-writeback", "a64-link-register-store-writeback"):
        assert "memory addressed through a register that may hold a stack address" in out, out
    elif ctl_name in ("a64-unextended-w-index", "a64-mismatched-index-extend"):
        assert "indexed stack address:" in out, out
    else:
        assert out.startswith("bound "), f"control {ctl_name} was shadowed by another guard when its guard in {which} was deleted: {out}"
' "$root/scripts/checks/lib" "$work/ctl"; then
  echo "ok   control: guard-mutation check verifies all 86 guards are load-bearing and unshadowed"
else
  echo "FAIL control: guard-mutation check found a shadowed or non-load-bearing guard"; fail=1
fi
if [ -n "$measuring" ]; then
  # The run check counts the bytes an instruction addresses below the stack pointer, not only the
  # stack pointer: a main that stores at the red zone's edge without moving the stack pointer
  # (built like the examples, slice0_native.sh control) is measured at 128 bytes, not 0. With a
  # smaller red zone the same run is an error, not a measurement. On AArch64 it first allocates and
  # frees a 144-byte frame with a pre-indexed store (judged against the new stack pointer, so not
  # beyond the red zone), so the expected depth there is 144.
  rz="$work/ctl/redzone"; mkdir -p "$rz"
  case "$target" in
    x86_64-linux) rzdepth=128; printf '\t.intel_syntax noprefix\n\t.text\n\t.globl\tmain\n\t.type\tmain,@function\nmain:\n\tmov\tqword ptr [rsp - 0x80], 0\n\tmov\teax, 7\n\tret\n\t.size\tmain, .-main\n\t.section\t.note.GNU-stack,"",@progbits\n' ;;
    aarch64-macos) rzdepth=144; printf '\t.text\n\t.globl\t_main\n\t.p2align\t2\n_main:\n\tstp\tx29, x30, [sp, #-0x90]!\n\tldp\tx29, x30, [sp], #0x90\n\tstur\txzr, [sp, #-0x80]\n\tmov\tw0, #7\n\tret\n' ;;
  esac >"$rz/control.s"
  if ! bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" control "$rz" >"$rz/build.log" 2>&1; then
    echo "FAIL control: the red-zone executable does not build: $(head -c 1000 "$rz/build.log")"; fail=1
  else
    "${objdump[@]}" "$rz/control.exe" >"$rz/control.exe.dis"
    if [ "$target" = x86_64-linux ] && ! append_tls "$rz/control.exe" "$rz/control.exe.dis"; then
      rza="refused: PT_TLS metadata failed"
      echo "FAIL control: red-zone PT_TLS metadata could not be derived"; fail=1
    else
      rza=$(stack_bound "$target" "$rz/control.exe.dis" --ranges | cap_out) || true
    fi
    rzm=$(measure "$rz/control.exe" "$rza" 7)
    if [ "$(awk '$1 == "bound" {print $2}' <<<"$rza")" = "$rzdepth" ] && [ "$rzm" = "measured $rzdepth" ]; then
      echo "ok   control: a red-zone store is measured (bound $rzdepth, measured $rzdepth)"
    else
      echo "FAIL control: red-zone run: analysis ${rza:-none}; run ${rzm:-none} $(head -c 300 "$rz/control.exe.gdb.err")"; fail=1
    fi
    rzm=$(measure "$rz/control.exe" "$(sed 's/^redzone .*/redzone 120/' <<<"$rza")" 7)
    if grep -q '^error .*beyond the 120-byte red zone' <<<"$rzm"; then
      echo "ok   control: a run beyond the red zone gives no measurement"
    else
      echo "FAIL control: a run beyond the red zone: ${rzm:-none}"; fail=1
    fi
  fi
fi

[ "$examples" -gt 0 ] || { echo "slice0_artifacts: no examples ran" >&2; exit 1; }
echo "slice0_artifacts: $examples examples; sizes in $sizes, compile times in $times, stack bounds in $stack"
column -t -s "$(printf '\t')" "$sizes" 2>/dev/null || cat "$sizes"
column -t -s "$(printf '\t')" "$times" 2>/dev/null || cat "$times"
exit "$fail"
