# C/ABI Glue Generation — UX Decision

Status: decided, implementation deferred — ROADMAP Phase 6 #9.

How will Concrete interoperate with C at the tooling level — generate C headers
from Concrete, import declarations from C headers, generate host stubs, or expose
a narrow command? This records the decision; it is **not** built yet.

## Decision

The eventual shape is a **narrow `concrete ffi` / `concrete bindgen` path** that
emits and/or imports **auditable glue**, not an open-ended FFI generator. Any
generated glue must carry, as first-class recorded facts:

- **ABI / layout assumptions** (struct layout, alignment, `repr`, integer widths);
- **ownership boundary rules** (who allocates/frees, move vs borrow across the
  boundary);
- **capability / trust labels** (the glue is a `trusted`/`Unsafe` boundary; what
  authority crosses it is explicit);
- **source spans** (glue points back at the Concrete and C declarations it bridges);
- **reproducible output** (same inputs → byte-identical glue; no timestamps/paths).

This matches Concrete's stance everywhere else: a foreign boundary is an
*explicit, audited assumption*, never silent textual glue.

## Deferred — and why

Implementation is **blocked on the FFI language surface itself (ROADMAP #8)**:
`extern` syntax, layout restrictions, and the trust/capability rules for foreign
calls. Designing glue-generation UX before the surface it generates against is
settled would bake in premature commitments. So:

- No `concrete ffi`/`bindgen` command is built now.
- When #8 lands, this UX is implemented against it, with the recorded-facts list
  above as the acceptance bar and an `examples/ffi_glue/` + gate.

Until then, FFI is whatever #8 defines directly in-language; there is no
glue-generation tooling. See [FFI.md](FFI.md) and [SAFETY.md](SAFETY.md) for the
current trust-boundary model.
