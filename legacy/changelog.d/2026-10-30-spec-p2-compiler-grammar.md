### Language specification: docs/compiler and the grammar comments cite the spec (2026-10-30)

The language statements in `docs/compiler/` (ARCHITECTURE, BINDING_IDENTITY_DESIGN,
CAPABILITY_FACTS, COMPILER_BOUNDARY, COMPILER_PIPELINE, DECLARATION_IDENTITY, FORMATTING_OUTPUT,
PASSES, PIPELINE_ARCHITECTURE, PIPELINE_CONTRACTS, VERIFIED_PIPELINE) now cite rule IDs. Stale
statements are corrected. A monomorphized `Int`/`Uint` pair is refused (E0501, TYPE-6). The
interpreter is not the language's definition (PASS-13). Discards are OWN-7. An unsupported `print`
argument is refused (E0421), with no `<unprintable>` placeholder. Canonical arm and field order is
internal, so PAT-9 and STMT-4 still hold. SYNTAX_FREEZE_REVIEW moves to
`docs/archive/design-records/`. The comments of `grammar/concrete.ebnf` cite the syntax entries and
the open resync (OPEN-77 to OPEN-85); no production changed. New entries: SYN-6 (tokens), SYN-7
(`>>` closes two type-argument lists), SYN-8 (a `match` that begins a statement), SYN-9 (a value
`if` has an `else`) and SYN-10 (the separators `.` and `::`). They come with SYN-1 refusal examples,
a MOD-2 example for re-export, and rationale for builtin `Option`/`Result` (MOD-1), `Type_method`
collisions (MOD-3) and the output builtins (TYPE-10). Open placeholders OPEN-128 to OPEN-133:
`->` in match arms, enum-literal type arguments from the expected type, positional pattern binders,
`union`, polymorphic recursion (main does not terminate) and user functions named `print`,
`println` or `append`.
