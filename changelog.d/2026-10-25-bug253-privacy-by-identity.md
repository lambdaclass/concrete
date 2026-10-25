### Representation Privacy Is Judged by the Declaration (bug 253, 2026-10-25)

A module importing another module's newtype or struct could construct the newtype, or build, read
and write the struct's private fields, whenever one of its children declared a type of the same
spelling (with an import alias too): Check asked whether a type of that name is defined here
rather than whether the declaration the name resolved to is. Construction rights and field privacy
are now judged by the declaration's identity (E0296 / E0297 / E0298). Pinned by the `error_253_*`
fixtures and `regress_253_local_rights_beside_imports.con`.
