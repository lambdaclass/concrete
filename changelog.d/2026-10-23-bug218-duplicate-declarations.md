### A Module Declares Each Name Once (bug 218, 2026-10-23)

MOD-3's check compared a module's imports with its functions but never its declarations with each
other, so two structs of one name compiled with the first silently winning, a struct and an enum,
or two aliases of one name were accepted, two constants failed at `lower`, and two functions of one name
failed in `llvm-as`. `declarationCollisions` now refuses the later declaration with E0299; callables,
constants and types are separate namespaces. Pinned by five `error_218_*.con` fixtures and
`regress_218_type_and_value_namespaces.con`.
