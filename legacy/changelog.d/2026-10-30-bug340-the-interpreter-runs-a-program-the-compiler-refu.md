### Bug 340: The interpreter runs a program the compiler refuses as a proven runtime-safety violation (E0900) (2026-10-30)

Filed, not fixed: `--interp` runs a program the compiler refuses with E0900 (an index proven out
of bounds), and exits 0 when the index is on a path that does not run (silent; MEM-6@1). The native
refusal is pinned by check_example_outputs.sh's open-bug reproducers leg; the structural owner is
R-0497 ([bug 340](docs/bugs/340_the_interpreter_runs_a_program_the_compiler_refu.md)).
