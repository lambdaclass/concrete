- ISS-0028: slice-0 gate 12 now parses instructions and operands through one shared, fail-closed
  grammar in `scripts/checks/lib/stack_access.py` used by both `stack_bound.py` and the debugger
  run checks, consuming every byte of every operand against an explicit allowlist of mnemonics and
  operand shapes. String instructions (`rep`/`movs`/`stos`/`lods`/`cmps`/`scas`), `xlat`, `enter`,
  AArch64 `mrs` (`TPIDR_EL0`), unclosed brackets, malformed displacement terms, `gs:`, every
  register-relative or non-`fs` segment override (before `<size> ptr` or before the address),
  segment-register writes, reads and compares, and instructions with multiple memory operands are
  refused. Absolute `fs:` accesses (bracketed, unbracketed or `movabs`) must fit their complete
  byte interval in the Linux TCB header `[0, 0x70)` or the linked ELF's Variant II static-TLS
  reservation `[-M, 0)`, where `M = roundup(p_memsz + ((p_vaddr - p_memsz) & (a - 1)), a)` is
  derived from `llvm-readobj --program-headers --elf-output-style=JSON` (requiring `PT_LOAD`).
  Controls isolate each rule, assert its diagnostic, pass a guard-mutation check, and exercise
  linked aligned `PT_TLS` allocations (`p_memsz = 1, p_align = 64` at `fs:[-0x40]` and
  `p_memsz = 8, p_align = 16` at `fs:[-0x10]`) and a real GDB run whose `fs:0x100000` store is
  measured 128 bytes below `main`'s entry stack pointer.
