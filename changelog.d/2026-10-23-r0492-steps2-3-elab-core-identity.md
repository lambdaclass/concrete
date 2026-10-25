### Elab And Core Name Locals By Binding Identity (R-0492 steps 2 and 3, 2026-10-23)

Step 2: Elab no longer keeps its own name-keyed scope. It resolves every local through the table
of the one lexical walk the checker already used (R-0492 slice 1), so no front-end pass decides
by spelling which binding a name denotes. The Core it produces is unchanged: `--emit-core` is
identical on all 1474 corpus files, and fingerprint and subject-fact reports are identical on 111
example entry points. One intended difference: a parameter spelled like Elab's destructure
temporary is no longer captured by it.

Step 3: Core's local binders and references are `CLocal`s, a Core name plus the walk's `LocalId`,
and a function records its parameters' identities. `ElabVerify`, which runs inside
`Pipeline.elaborate`, refuses Core whose identities and names disagree: a binder identity already
visible where it is introduced, a reference whose identity and spelling name different bindings or
that a newer binder shadows, a reference without an identity that spells a visible source binding,
or missing parameter identities. Elab's renaming (the bug 045 / 087 class) is therefore checked on
every compile, not only by tests. A name does not coerce to a `CLocal`, so a string-literal pattern
on a Core binder is a type error. Step 3 also closed bug 208.

`check_local_identity.sh` gains legs E (the exact Core-name profile of the edge cases) and V (Core
identities and ElabVerify forgeries). Mutation families 99 (Elab ignores the walk's shadow fact)
and 100 (Elab drops a reference's identity) join the inventory. The slice
contracts are contracts 3 and 4 in [VERIFIED_PIPELINE.md](docs/compiler/VERIFIED_PIPELINE.md).
