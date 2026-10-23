### The Backends Key Locals By Binding Identity (R-0492 step 4, 2026-10-23)

The interpreter, Lower, Mono, CoreCheck, the post-mono reference-binding verifier and the proof
model's extraction now find the binding a Core local denotes by its identity (`CLocal.key`: the
`LocalId` the local walk minted, or the Core name of a compiler temporary), one backend per slice.
`Callee.indirect` carries the binding. The `CLocal -> String` coercion is removed, and so are
`CLocal`'s equality, hash and order on the spelling, so a consumer says `.key` or `.name`.
ElabVerify also runs after Mono, since Lower reads post-Mono Core. Loop-boundedness matches the
stepped variable by identity. Core, reports and interpreter output are byte-identical on the
corpus; SSA and LLVM differ only in register numbering on seven programs (contract 5 in
`docs/compiler/VERIFIED_PIPELINE.md`). `check_local_identity.sh` gains compile-fail legs F9 – F13.
