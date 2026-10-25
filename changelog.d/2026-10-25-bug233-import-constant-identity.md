### A Public Constant Can Be Imported (bug 233, 2026-10-25)

MOD-2@1 lets a `pub` declaration cross a module boundary by import, but `import cfg.{BASE}` of a
`pub const` was refused (contained since 2026-10-24 with the reason): Core named a constant by its
spelling, and Lower inlined it from the importing module's own tree. A constant is now imported by
its identity (R-0492 step 4): the import binds its `ConstId`, Core references it by its defining
module (`cfg::BASE`), and Lower and the interpreter inline its initializer with the defining
module's constants in scope. A trait import is still refused with its reason (traits are
module-local), and a constant imported beside a local constant of one spelling is E0299. Pinned by
`regress_233_import_constant.con`, `regress_233_nested_import_constant.con`,
`regress_233_import_constant_scope.con`, `regress_233_nested_alias_constant.con`,
`error_233_import_constant_collides_local.con` and the project fixture, native and `--interp`.
