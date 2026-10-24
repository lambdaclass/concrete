### An Unknown Type In A Cast Or Type-Argument List Is Refused (bug 215, 2026-10-23)

Resolve checked written types in annotations, signatures and fields but not inside expressions, so
`x as Nope` reached the backend (a panic natively; `--interp` ran it) and an unknown type in a
type-argument list could compile. Cast targets and the type-argument lists of calls, method calls,
static method calls, struct literals and enum literals now go through the same check (E0108). Found
by the fuzz farm's mutation generator. Pinned by four `error_215_*.con` fixtures.
