### A Module May Write Only the Types Its Environment Holds (bug 230, 2026-10-25)

Resolve checked a top-level module's written types against every module's type names, and its
children's aliases, while Check gives the module its own declarations and its imports. A cast to
another module's alias, or a child's, that the module never imported passed Resolve and made the
backend panic (`unknown named type`). Every module's type namespace now comes from one definition,
`Resolve.moduleTypeEnvironment`, plus what its imports transport (R-0492 slice 6), and the cast is
refused with E0108. The same change lets a module use the builtin `Result` when another top-level
module declares its own `enum Result` (the top-level half of bug 227). Pinned by
`error_230_toplevel_foreign_alias_cast.con`, `error_230_parent_names_child_alias.con`,
`regress_230_toplevel_type_environment.con` and `regress_227_toplevel_builtin_result.con`.
