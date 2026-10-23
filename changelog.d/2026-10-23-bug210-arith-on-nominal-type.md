### Arithmetic On A Struct, Enum Or Newtype Is Refused (bug 210, 2026-10-23)

CoreCheck's operator rules read every named type as a type parameter and skipped TYPE-6, so `p + p`
on a struct and `e * e` on an enum compiled and ran natively while `--interp` stopped, and a newtype
sum ran under `--interp` while the binary failed in `llvm-as`. One predicate, `isOpaqueOperandTy`,
now decides which types an operator or cast rule defers to the instantiation, and a declared
struct, enum or newtype is refused with E0501. Found by the fuzz farm's mutation generator. Pinned by
four `error_210_*.con` fixtures.
