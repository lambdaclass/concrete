### Bug 307: a call argument &mut x is x's place, no longer a copy written back after the call (2026-10-27, reclassified 2026-10-29)

A call argument `&mut x` is now `x`'s own place, as `&mut x` is everywhere else (OD2 part 3),
instead of a temporary copy loaded back into `x` after the call. No program defined under OD15's
G19 (a pointer derived from a loan is valid only while the loan lives) told the two apart, so this
is not a wrong-code fix: the copy agreed with `x`'s slot only because the checker refuses
overlapping arguments, the exclusivity guarantee OD15(b) forbids a pass to rely on until it is
proven. Fixtures: `regress_307_call_arg_mut_is_place.con` (differential, binary and `--interp`,
plus an IR leg that fails if a `mutref.` copy slot comes back) and
`regress_307_call_arg_owner_types.con` (raw-pointer, `Heap` and `&mut`-parameter owners). Structural owner: OD2 part 3 and OD15(b), then R-0501
([bug 307](docs/bugs/307_a_call_argument_mut_x_passed_a_temporary_copy_of.md)).
