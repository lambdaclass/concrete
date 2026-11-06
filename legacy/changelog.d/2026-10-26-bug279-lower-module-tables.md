### Lowering Many Declarations No Longer Searches Lists per Function, Type or Call (bug 279, 2026-10-26)

Lower appended each lowered function of a module to the list built so far, filtered the program's
types against the module's own and deduplicated them by list searches, checked each call's callee
against a list of defined functions and numbered string literals by a list search, so a module of
n declarations took time n² to lower (a chain of 20000 functions: 3.9 s, now 134 ms). Results are
consed and reversed once and the lookups are hash sets and maps. Output and reports are unchanged
([bug 279](docs/bugs/279_lower_module_function_list_append.md)).
