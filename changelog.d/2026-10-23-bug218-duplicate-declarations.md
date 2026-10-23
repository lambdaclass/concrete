### A Module Declares Each Name Once (bug 218, 2026-10-23)

MOD-3's check compared a module's imports with its functions but never its declarations with each
other, so two structs of one name compiled with the first silently winning, a struct and an enum,
two aliases or a function and a constant of one name were accepted, and two functions of one name
failed in `llvm-as`. `declarationCollisions` now refuses the later declaration with E0299; types
and values are separate namespaces. Pinned by four `error_218_*.con` fixtures and
`regress_218_type_and_value_namespaces.con`.
