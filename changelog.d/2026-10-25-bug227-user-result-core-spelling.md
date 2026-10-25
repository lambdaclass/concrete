### A User `enum Result` No Longer Captures the Builtin in Another Module (bug 227, 2026-10-25)

A user `enum Result` in one module and the builtin `Result<T, E>` in another were one name in Core,
so the native build of a nested module using the builtin failed in Lower (E0602: variant `Ok` not
found) while `--interp` ran it, and between top-level modules Resolve answered the builtin with the
user's enum (E0113). A top-level module's type namespace is now its own environment (with bug 230),
and Core spells a user `Result` by its module (`outer_Result`, its methods with it) whenever another
module can see the builtin, which keeps its name (R-0492 slice 6). Pinned by
`regress_227_nested_builtin_result.con`, `regress_227_toplevel_builtin_result.con` and
`regress_227_user_result_methods_beside_builtin.con`, natively and under `--interp`.
