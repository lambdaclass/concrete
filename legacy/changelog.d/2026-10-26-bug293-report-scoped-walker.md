### Deeply Nested Functions Compile Without Re-walking Each Level for Safety Obligations (bug 293, 2026-10-26)

Every compile checks bounds, division and overflow obligations, and the walk that collects them
looked again through everything nested below each statement and copied the list of known facts at
every level, so a function nested 10000 deep took 9.5 s to compile when the rest of the compiler
took a quarter of a second. The walk now collects what it needs on its single pass and shares the
facts between levels; reports and diagnostics are unchanged
([bug 293](docs/bugs/293_report_scoped_walker_nested_rewalks.md)).
