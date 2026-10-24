### Resolve Admits What Check Admits: Namespaces, Builtin Option, Child Newtypes (bug 229, 2026-10-24)

Resolve refused three kinds of valid program: a function declared before a same-named enum made
`E::A` "not an enum" (E0104), a user `enum Option` hid the builtin `Option<T>` (E0113) although Check
and Elab never withhold it, and a child module's newtype was an unknown type (E0108). Type positions
now search the type namespace only and consult aliases and enums as Check does (an enum named
`Option` is always the builtin), and children's newtypes are known
types. Found by the Codex reviews of bug 217's fix. Pinned by five `regress_229_*.con` fixtures.
