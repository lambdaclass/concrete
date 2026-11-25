### Slice 0 retires the LLVM syntactic inventory before B0a

The LLVM assembly reader, inventory proofs and their theorem entries are retired.
The printer's scalar width and round-trip laws remain. Full-module golden fixtures
pin the existing emitted bytes, including runtime and site data. Isolated printer
mutations exercise those comparisons and native differential controls for return
status and trap stderr against the Lean BIR oracle. The native test and ASan gates continue to qualify the LLVM path,
with LCB parity on its existing schedule. The exact LLVM text's meaning remains
under `P_print`.
