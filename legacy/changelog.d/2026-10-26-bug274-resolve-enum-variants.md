### Resolving a Module of Many Enums Takes Linear Time (bug 274, 2026-10-26)

Resolve built a module's global scope by appending each enum's variant symbols to the list built
so far, so a module of n enums took time n² to resolve (20000 enums: 6.8 s, now 75 ms). The variant
symbols are built in one walk, and each module's scope is built once where it was built twice. The
compiler's output is unchanged ([bug 274](docs/bugs/274_resolve_enum_variant_symbols_fold.md)).
