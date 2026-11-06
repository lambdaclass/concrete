### Programs With Many Functions or Types Are Emitted in Linear Time (bug 288, 2026-10-26)

The LLVM emitter kept the program's function names, type names and string literals in lists and
searched them for every function, type or use, so emitting 100000 functions took 220 s (the rest of
the compiler took under 6 s). The tables are now hash sets and maps; the same program emits in
7.5 s and the output is unchanged ([bug 288](docs/bugs/288_emitssa_name_tables_lists.md)).
