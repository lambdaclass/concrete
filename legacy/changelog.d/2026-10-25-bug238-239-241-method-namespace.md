### A Free Function No Longer Answers a Method Call (bugs 238, 239, 241, 2026-10-25)

Free functions and impl methods shared one spelled namespace, so a free function named like a
method's definition name (`fn P_get` beside `p.get()`) or like a method call's Core spelling
(`fn outer_a_P_get` for a respelled type's method) was called instead of the method: 99 for 42,
natively and under `--interp` alike. Check and Elab now look a method up in a method table alone, a
Core call carries the namespace it was resolved in, every Elab renaming renames each namespace by
its own table, and a method call whose spelling a free function defines is spelled by its method. A
free function spelled like its own module's method definition name, which failed to build, is
E0299 (MOD-3), and a parent's call of its child's respelled type's method no longer names an
undefined function. Pinned by `regress_238_free_fn_spelled_like_method_call.con`,
`regress_239_free_fn_beside_imported_method.con`, `error_239_free_fn_named_like_own_method.con`
and `regress_241_parent_calls_child_respelled_method.con`.
