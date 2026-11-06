### Stringly dispatch, slice 1: identities instead of spellings in EmitSSA, Elab and Resolve (2026-10-29)

The Vec operations in EmitSSA, the variadic `print`/`println`/`append` in Elab and the `free`/`abort`
exemption in Resolve dispatch on `IntrinsicId` rather than on string literals (new
`IntrinsicId.ofCanonical?` accepts only the canonical spelling elaboration emits). The wildcard
binder `_` and the implicit root module name `main` each have one definition (`isWildcardBinder`,
`rootModuleName`) used by every compiler stage (the proof model keeps its literal). Emitted Core, SSA, LLVM, check and interpreter outputs and
every report kind are byte-identical before and after. All 458 literal comparisons are classified in
`research/compiler/stringly-dispatch-2026-10.md`.
