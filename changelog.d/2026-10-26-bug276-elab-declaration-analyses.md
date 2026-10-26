### Elaborating Many Declarations Takes Linear Time (bug 276, 2026-10-26)

Elab's program-level declaration analyses (top-level function collisions, ambiguous type names,
import spellings, the first binding of each type spelling) deduplicated or scanned lists per
declaration, its per-function results were appended one at a time, and a call looked its callee up
in lists, so a program of n declarations took time n² to elaborate (20000 functions: 14 s, now
122 ms; 20000 structs: 8.2 s, now 29 ms). Deduplication, first-binding and callee tables are
one-pass hash sets and maps. Output, diagnostics and reports are unchanged
([bug 276](docs/bugs/276_elab_declaration_analyses_list_scans.md)).
