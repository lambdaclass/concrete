# runtime_checked  — PLANNED

**Class:** a runtime-error obligation (e.g. bounds / overflow / div-by-zero)
discharged so the operation cannot fault under a named profile.

**Status: planned.** The registry already has a `runtime_error` coverage kind,
but a clean worked example needs a runtime-error obligation discharged by a Lean
theorem (there is no auto runtime-check mode that would let this corpus entry be
non-trivial without a hand proof). Not faked here. When the runtime-error proof
path has a small reusable shape, this subexample will get a `src/main.con` + a
discharged bounds/overflow obligation + a snapshot.
