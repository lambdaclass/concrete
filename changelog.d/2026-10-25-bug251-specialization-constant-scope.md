### A Generic's Specialization Reads Its Own Module's Constants (bug 251, 2026-10-25)

A generic function that read a private module constant, instantiated from another module, read
that other module's constant of the same spelling in the binary (a wrong value; `--interp` was
right), or failed to build when that module had none (E0704): Mono emits a specialization into the
instantiating module's tree and Lower resolved constants there. A specialization now keeps its
generic's defining tree (`CFnDef.constTree`), whose constants Lower uses. `--report model-diff` now
samples each function in its own tree's constants, as `--interp` runs it (bug 245 review). Pinned by
`regress_251_*`, native and `--interp`.
