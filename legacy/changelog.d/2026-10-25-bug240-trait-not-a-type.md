### A Trait Is Not a Type (bug 240, 2026-10-25)

A trait name in a type position (`x as Marker`, a parameter, a field, a type argument) passed
Resolve, and the native build panicked in EmitSSA ("unknown named type") while `--interp` ran the
program. There are no trait-object types (TYPE-2), so Resolve now refuses it, E0108 with the
reason. Bounds and impls name traits as before. Pinned by `error_240_trait_cast_target.con`,
`error_240_trait_struct_field.con`, `error_240_trait_parameter.con` and `regress_240_trait_bounds_and_type_param_shadow.con`.
