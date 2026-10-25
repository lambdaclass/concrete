### Module-Level Types And Constants Are Found By Declaration Identity (R-0492 slice 6, 2026-10-25)

Four bugs open when the slice started had one cause: a module-level declaration was found again by
its spelling, in tables whose scope differed from pass to pass. The design note
[DECLARATION_IDENTITY.md](docs/compiler/DECLARATION_IDENTITY.md) gives the fix: the `TypeId` and
`ConstId` the front end already stamped on every declaration (defining module plus declared name,
never changed by an import alias) are compared instead of spellings, and a module's type
environment says which declaration a spelling denotes there. Its four steps have landed: one type environment per module (bug 230 and the top-level half of bug 227),
Core spelling a user `Result` apart from the builtin (the rest of bug 227), Check comparing types
and finding methods and trait impls by identity (bug 223), and an imported constant referred to by its `ConstId`
(bug 233). Resolve now computes one type environment per module, but Check and Elab still build
their own; constants within one module tree are still named by spelling (bug 132's refusal remains
there); and a constant whose initializer calls or names a function is not inlined across modules.
The design note records these limits. Each bug has its own entry. Bugs 242, 243 and 245, found while reviewing these fixes,
stay open under R-0492/R-0514.
