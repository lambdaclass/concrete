### Generic Arithmetic Is Checked On Each Instance (bug 211, 2026-10-23)

A generic `+` instantiated at a struct or enum reached `llvm-as` or built a binary, because CoreCheck
defers a type parameter's operator to the instantiation and only comparisons had a post-Mono
re-check. `verifyOperatorTypes` now holds every monomorphized arithmetic, wrapping, saturating,
bitwise and shift operator to TYPE-6 (E0501, post-mono). Pinned by `error_211_generic_add_struct.con`,
`error_211_generic_mul_enum.con` and `regress_211_generic_add_numeric.con`.
