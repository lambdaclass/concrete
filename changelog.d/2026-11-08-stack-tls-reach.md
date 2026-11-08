- ISS-0028: slice-0 gate 12 now models x86 thread-pointer accesses structurally. `gs:` and every
  register-relative segment override are refused, as are segment-register writes and instructions
  with multiple memory operands. Absolute `fs:` accesses must fit their complete byte interval in
  the Linux TCB header or the linked ELF's `PT_TLS` allocation, whose `p_memsz` is derived with
  `llvm-readelf` (zero when absent); the premise states modular-address separation from the stack.
  The run check parses GDB's actual unbracketed absolute operands as well as bracketed forms and
  fails closed on unparsed memory or an unavailable base. Controls cover every operand width,
  positive and negative boundaries, wrap-around, real `PT_TLS` metadata, and a real GDB run whose
  `fs:0x100000` store is measured 128 bytes below `main`'s entry stack pointer.
