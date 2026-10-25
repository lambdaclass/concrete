### Backend And Interpreter Repairs: Bugs 108, 109, 110, 116, 119, 120, 130 And 133 (2026-10-21)

The examples-health pass of 2026-10-20 found six of them; the semantics probes found bug 130 and
the Codex review of bug 116's fix found bug 133, both on 2026-10-21. All are fixed and pinned:

- [bug 108](docs/bugs/108_bool_cast_trunc.md): `bool as <int>` was emitted as `trunc i1`, which
  LLVM rejects. A `bool` now widens with `zext`, and `<int> as bool`, which neither backend gave a
  meaning, is refused (E0553). That refusal also closes
  [bug 130](docs/bugs/130_int_as_bool_invalid_ir.md) (`x as bool` with `x: Int` failed in
  `llvm-as`).
- [bug 109](docs/bugs/109_f32_literal_invalid_ir.md): float constants were printed to LLVM with six
  decimals, so an inexact `f32` was invalid IR and an `f64` silently lost precision. Constants are
  now the hex of their IEEE bits.
- [bug 110](docs/bugs/110_array_value_store.md): an array value held as a pointer to its storage was
  stored as the aggregate (invalid IR); the element type now hints the value and the backend's casts
  use the storage address.
- [bug 116](docs/bugs/116_vec_push_first_field_through_ref.md): `vec_push(&mut p.v, x)` with `v` the
  first field of a `&mut P` called an undefined `@vec_push`, because GEP-zero elimination gave the
  call the wrong pointer type. A pointer use keeps its own type.
- [bug 133](docs/bugs/133_deref_field_borrow_copies.md), silent: `&mut (*p).f` borrowed the field of
  a copy of `*p`, so a write through it was lost in the binary while `--interp` performed it.
  `*r` as a place is now the storage `r` points at.
- [bug 119](docs/bugs/119_interp_generic_trait_method.md) and
  [bug 120](docs/bugs/120_interp_field_borrow_through_ref.md): the interpreter could not call a
  trait method on a type parameter, nor borrow a field through a reference parameter. Each has an
  interpreter-against-native leg.
