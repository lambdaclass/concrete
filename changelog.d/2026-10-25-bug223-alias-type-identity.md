### An Import Alias Names the Imported Type (bug 223, 2026-10-25)

`import m.{P as Q}` gave the importer a type `Q` with none of `P`'s methods, which a `P` returned by
`m` did not match: Check compared named types, and every pass looked up methods, by spelling
(`Q_mk` where the impl registered `P_mk`). Types are now compared, and their methods and trait impls
looked up, by declaration identity (R-0492 step 3): `buildFileSummary` stamps each impl's target
and each impl method with a `MethodKey` (the target's `TypeId` and the method name), and Resolve,
Check and Elab select a method by that key before its spelling. Core for an aliased import is the
Core of the same program without the alias. Pinned by `regress_223_aliased_struct_methods.con`,
`regress_223_aliased_enum.con`, `regress_223_aliased_generic_and_bound.con` and
`regress_223_aliased_destroy_impl.con`, native and `--interp`.
