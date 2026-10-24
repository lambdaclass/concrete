### Phase 7 Workload 10: stackvm (2026-10-24)

`examples/stackvm` is a bytecode stack VM with its assembler, bytecode codec and load-time
verifier, and disassembler (`asm`, `dis`, `run`, `exec`, `check`; 13t exit codes), written over a
23-variant instruction enum, a generic bounded `Stack<T>`, the `Encode` and `Render` traits and
`defer` cleanup. `scripts/tests/check_stackvm_differential.sh` compares every output (assembled
bytes, listing, program output, stderr diagnostic, exit code) byte for byte with an independent
Python implementation (`scripts/tests/lib/stackvm_ref.py`) on the test programs, 120 generated
programs, a mutation of each, 120 token-edge programs and 120 corrupted bytecode files; ten `@run` cases agree natively and
under `--interp`. The workload found bugs 222, 223 (open), 224, 225 and 228 and no miscompile.
`examples/stackvm/FRICTION.md` maps its friction to the open language decisions.

### An Imported Enum Brings Its Impls (bug 222, 2026-10-24)

The struct and newtype import paths each selected the imported type's impl blocks and trait
impls; the enum paths (by name and through the transitive closure) did not, so every trait bound
on an imported enum was refused (E0241) outside its module. One helper now selects what travels
with an imported type, for every type kind. Pinned by `regress_222_imported_enum_impls.con`.

### Diagnostics: Recovered `let mut` And Refused Bounds (bugs 224 and 225, 2026-10-24)

A `let mut` whose initializer failed to check was recovered as immutable, so later assignments to
it got a false E0217; it keeps its mutability now. A refused `Copy` bound named every builtin
type `<type>`, and no E0241 carried a position; every refused bound now names its type and points
at the call. Pinned by `error_224_failed_let_keeps_mutability.con` and
`error_225_bound_names_builtin_type.con`.

### Interpreter Loops Are Linear In Their Iteration Count (bug 228, 2026-10-24)

`--interp` carried each loop iteration's bindings into the next, so every lookup of an outer
variable walked all earlier iterations' bindings and a loop was quadratic (stackvm's dispatch loop:
2000 steps in 222 s, now 2.5 s). Each iteration's bindings are dropped before the next. Pinned by
`regress_228_interp_loop_env.con` (`--interp` within 20 s).
