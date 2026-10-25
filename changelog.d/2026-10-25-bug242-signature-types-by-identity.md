### A Type Crossing an Import Keeps Its Declaration (bug 242, 2026-10-25)

A type named in an imported signature, field, payload, alias or constant was read in the importer's
environment by its defining module's spelling, so an importer declaring its own type of that
spelling (or importing another module's) could not use it: `make(40).v` beside the importer's own
`struct P` was E0252. Import resolution now respells such a type to its declaration's identity
(`m::shapes::P`) and binds that spelling to the declaration, in every pass. Seven bug 096 fixtures
change accordingly: three refused by Check (E0220) rather than by Core, four now valid. Pinned by
`regress_242_*` (native and `--interp`) and `error_242_two_imported_methods_one_core_name.con`.

### The Interpreter Constructs and Unwraps a Newtype Over a Struct (bug 254, 2026-10-25)

`--interp` refused `W(p)` and `w.0` for a newtype over a struct or enum ("unsupported cast"), which
the binary runs. Pinned by `regress_254_newtype_over_struct.con` and `regress_254_newtype_over_enum.con`.
