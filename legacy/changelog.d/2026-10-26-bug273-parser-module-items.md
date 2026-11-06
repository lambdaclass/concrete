### A Module of Many Structs or Enums Parses in Linear Time (bug 273, 2026-10-26)

The parser appended each struct, enum, constant, newtype, import and submodule of a module to the
list built so far, so a module of n such declarations took time n² to parse (20000 structs: 3.0 s,
now 50 ms). Every declaration list is now consed and reversed once, as functions already were. The
compiler's output is unchanged ([bug 273](docs/bugs/273_parser_module_items_append.md)).
