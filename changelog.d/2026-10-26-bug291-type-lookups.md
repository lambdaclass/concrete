### Type Declarations Are Found by Name in a Table, in Every Stage (bug 291, 2026-10-26)

Resolve, Check, Elab, CoreCheck and Lower each found a struct, enum or newtype declaration by
searching a list of all of them at every use, so a program with many types took time that grew
with the square of their number (8000 structs: checking 0.55 s, now 0.05 s). Each stage now
builds a table of its declarations once and looks names up in it. Output is unchanged
([bug 291](docs/bugs/291_type_lookups_by_list_search.md)).
