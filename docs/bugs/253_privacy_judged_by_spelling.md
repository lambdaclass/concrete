# Bug 253 — construction and field rights are judged by spelling, so a child's type grants an import's

<!-- bug-ledger
summary: newtype construction rights and struct field privacy were judged by spelling (is a type of that name defined here?), so a module importing another module's newtype or struct and having a child that declares a type of the same spelling could construct the imported newtype and build, read and write the imported struct's private fields (E0296 / E0297 / E0298 bypassed; with an import alias too). Judged by the declaration's identity. Found by the Codex review of bug 243's fix, 2026-10-25. Fixtures `error_253_*`, `regress_253_local_rights_beside_imports.con`. Class 1.
fixtures: error_253_child_newtype_decoy.con error_253_aliased_newtype_decoy.con error_253_child_struct_decoy_literal.con error_253_child_struct_decoy_field_read.con error_253_child_struct_decoy_field_write.con regress_253_local_rights_beside_imports.con
class: 1 — construction and field rights asked whether a type of the spelling is defined here, not whether the declaration the spelling resolved to is; fixed 2026-10-25: judged by identity
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED (2026-10-25, branch `wip/ident-followups`)
**Found:** 2026-10-25, by the Codex review of bug 243's fix (an aliased newtype import beside a
child newtype of the alias's spelling); the plain-import and struct forms by the probes of that
finding.
**Severity:** silent. A program that breaks another module's representation privacy (OWN 0b:
direct newtype construction and private fields are the defining module's) is accepted; both
backends then run it.

Confirmed real by Codex review, 2026-10-25: TYPE-11 (docs/spec/types.md): a same-named child declaration granted access to an unrelated imported type’s private representation; OD21 cannot authorize that.

## Reproducer

`tests/programs/error_253_child_newtype_decoy.con`: `mod m { pub newtype N = i64; }` and a module
`user` with `mod decoy { newtype N = i64; }` and `import m.{N};`, whose `N(42)` builds `m.N`.
Observed on `wip/r0492-steps34` (main `abc6318a` alike): compiled, the binary returns 42.
Expected: `(E0296) newtype 'N' cannot be constructed here`. The struct forms
(`error_253_child_struct_decoy_literal.con`, `..._field_read.con`, `..._field_write.con`, with
`m.P`'s private field `v`) were admitted the same way, E0297 / E0298 expected; and since bug 243's
fix so is the alias form (`error_253_aliased_newtype_decoy.con`, `import m.{N as M}` beside a
child's `newtype M`).

## Cause

Check decides construction and field rights with `localNewtypeNames` / `localStructNames`: the
names of the newtypes and structs declared by the module and its children. The declaration a
spelling resolves to comes from the module's environment, where an import precedes a child's
declaration (`allNewtypes := m.newtypes ++ imports.newtypes ++` the children's). So the spelling
`N` named `m.N` while the rights check found the child's `N` among the local names.

## Fix

The rights are asked of the declaration the spelling resolved to, by identity:
`TypeEnv.newtypeIsLocal` and `TypeEnv.structIsLocal` compare its `TypeId` with
`localNewtypeIds` / `localStructIds`, the identities of the module's and its children's
declarations as their summaries stamped them. A declaration with no identity (a parsed one: the
module's own or a child's AST) keeps the name rule. All four sites (newtype construction, struct
literal, field read, field write) use them.

Regression legs in `run_tests.sh`: the five `error_253_*` fixtures (`run_err`, E0296 / E0297 /
E0298) and the positive control `regress_253_local_rights_beside_imports.con` (a module beside
aliased and plain imports constructs its own types and reads their private fields: 42, native and
`--interp`).

## Structural cause

Class 1 ("declaration identity by spelled name"): "is this declaration mine?" was answered by "is
a declaration of this spelling mine?", two different questions once a spelling can denote an
imported declaration while a child declares another.

## Structural owner

R-0492 / R-0514: the module's type environment binds each spelling to one identity, and every
question about the declaration (rights, methods, equality) is asked of that identity. The rights
now are; the name lists remain only for declarations without an identity.
