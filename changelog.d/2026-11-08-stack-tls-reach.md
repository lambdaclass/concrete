- ISS-0028: slice-0 gate 12 now parses both x86-64 and AArch64 instructions and operands through
  shared, fail-closed grammars in `scripts/checks/lib/stack_access.py` (`parse_x86_plain`,
  `parse_a64_plain`, `control`) used by both `stack_bound.py` and the debugger run checks,
  consuming every byte of every operand against explicit per-architecture allowlists of mnemonics
  and operand shapes:
  - On x86-64 (`llvm-objdump` Intel syntax and GDB), string instructions
    (`rep`/`movs`/`stos`/`lods`/`cmps`/`scas`), `xlat`, `enter`, `loop`, unclosed brackets,
    malformed displacement terms, `gs:`, every register-relative or non-`fs` segment override
    (before `<size> ptr` or before the address), segment-register writes, reads and compares, and
    instructions with multiple memory operands are refused. Absolute `fs:` accesses (bracketed,
    unbracketed or `movabs`) must fit their complete byte interval in the Linux TCB header
    `[0, 0x70)` or the linked ELF's Variant II static-TLS reservation `[-M, 0)`, where
    `M = roundup(p_memsz + ((p_vaddr - p_memsz) & (a - 1)), a)` is derived from
    `llvm-readobj --program-headers --elf-output-style=JSON` (requiring `PT_LOAD`).
  - On AArch64 (`llvm-objdump` and LLDB, stripping both `//` and Mach-O `;` comments), every
    operand of control and non-control instructions is validated against explicit allowlists of
    general and SIMD/FP scalar registers, immediates, shift/extend modifiers, condition codes,
    `adr`/`adrp` targets, single and pair loads/stores, and bracketed addressing modes (immediate
    offset, pre-index `[base, #simm]!`, post-index `[base], #simm`, and register offset with
    optional `lsl`/`uxtw`/`sxtw`/`uxtx`/`sxtx` shift/extend). `mrs`/`msr` (`TPIDR_EL0`), trailing
    operands on zero-operand instructions (`nop garbage`), wrong operand counts, unbracketed or
    malformed memory operands (`ldr x0, 0x1000`, `str x0, sp`), bracketed operands on non-memory
    instructions, loading `sp`, 32-bit base registers (`[wsp]`, `[w8]`), unsupported addressing
    modes (register post-index writeback `[sp], x8`, simultaneous pre- and post-index writeback
    `[sp, #-0x8]!, #0x8`, writeback on unscaled `ldur*`/`stur*`, indexed pair loads/stores,
    unextended 32-bit index registers, or invalid index extends/shifts), invalid modifiers or
    conditions, and malformed `cbz`/`tbz`/`brk` operands are refused by both `stack_bound.py` and
    `stack_access.py`, while `stack_bound.py` additionally refuses indexed stack addresses
    (`[sp, x8]`), frame/link writeback, unmodelled `sp` writes (`add sp, sp, x8`), and PC-relative
    accesses with writeback, index registers, or offsets beyond `A64_OFF_REACH`.
  - Controls isolate each x86-64 and AArch64 rule, assert its diagnostic, pass an 86-guard
    mutation check across `stack_access.py` and `stack_bound.py`, and exercise linked aligned
    `PT_TLS` allocations (`p_memsz = 1, p_align = 64` at `fs:[-0x40]` and
    `p_memsz = 8, p_align = 16` at `fs:[-0x10]`) and a real GDB run whose `fs:0x100000` store is
    measured 128 bytes below `main`'s entry stack pointer.
