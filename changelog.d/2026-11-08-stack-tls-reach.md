- ISS-0028: slice-0 gate 12 now fails closed on x86 `fs:`/`gs:` absolute displacements beyond
  `TP_REACH = 0x100`, instead of exempting every thread-pointer-relative operand from stack
  accounting. None of the 64 real owned-code disassemblies contained such an operand; the bound
  covers the canary-style `fs:0x28` control with 216 bytes of margin. The premise now requires the
  stack to be farther than `TP_REACH` from each segment base in either direction. The gdb run check
  computes `fs_base`/`gs_base`-relative effective addresses and counts any that reach the stack.
