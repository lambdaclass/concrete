### A Child's Newtype Constructor Is Refused at Check (bug 262, 2026-10-25)

A parent module calling its child's newtype constructor without importing it (`K(30)` beside
`mod kids { pub newtype K = i64; }`) passed Check and was refused only by Elab, as a call to an
undeclared function with the internal code E0402. Check took the children's newtypes, which a
module may write as types, for constructors bound in it. Check and Elab now bind newtype
constructors from one definition, the module's own newtypes and its imports', so the program is
refused at Check (E0263, MOD-2); a call of an imported function that shares a child newtype's
spelling, which Check had refused as that newtype's constructor, now compiles. Pinned by the `error_262_*` fixtures and
`regress_262_own_rights_beside_child.con`, natively and under `--interp`.
