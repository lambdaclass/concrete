### Language specification: MEMORY_SEMANTICS absorbed (2026-10-30)

`docs/language/MEMORY_SEMANTICS.md` is now a design record in `docs/archive/design-records/`, and
`docs/language/README.md`, an index that still called that directory normative, is deleted. New
entries: MEM-D1 (the ownership states), REF-D3 (the borrow block, its binder type, freeze and exit),
TRUST-11 (a raw store neither reads nor destroys the old contents) and TRUST-12 (trusted code keeps the move, cleanup and freeze rules). REF-5, REF-6 and REF-8 gained examples for refusals main already enforces
under their tags: a non-place base of a field access, a borrow in a constant initializer, a generic
instantiated at a reference type in erased code and a read of a frozen owner. Whether `as` may be
applied to a reference is a new open question (OPEN-103).
