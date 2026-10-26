# Bug 291 — every stage found a type declaration by name with a list search per use

<!-- bug-ledger
summary: Resolve (`isKnownType`), Check (`lookupStruct`/`lookupEnum`/`lookupNewtype`, the Copy judgment's callback, `typeIdOf`, `structIsLocal`), Elab (`lookupStruct`, `nominalTypeId?`, `typeIdOfSpelling`), CoreCheck (`lookupStruct`, `isOpaqueOperandTy`, `findNewtype`) and Lower (`Layout.lookupStruct` through `fieldOffset`, enum lookups) found a struct, enum or newtype declaration by searching a list per use, so a program of n types each used once took time n² in five stages (8000 types: check 548 ms, elaborate 343 ms, lower 504 ms). Class 17.
fixtures: regress_291_type_lookups_by_name.con
class: 17 — type declarations found by a list search per use
silent: no
-->

**Status:** FIXED (branch `wip/bug-291`)
**Found:** 2026-10-26, probing the front end with declaration shapes beyond the R-0550 gate's
(many struct types, each constructed and read once).
**Severity:** loud (performance). Compile time only: the emitted IR and every diagnostic are
unchanged.

## Reproducer

n declarations `struct Copy S_i { a: i64 }`; `main` builds each once and reads its field
(`let s_i: S_i = S_i { a: 1 }; t = t + s_i.a;`). Stage times of `--emit-trace-json`, ms, fastest
of 3, dev host (fede-256gb), main cc7a4084:

| n | resolve | check | elaborate | coreCheck | lower |
|---|---|---|---|---|---|
| 2000 | 16 | 29 | 18 | 5 | 32 |
| 8000 | 237 | 548 | 343 | 166 | 504 |
| 10000 | 342 | 909 | 562 | 305 | 1026 |

Heartbeats grow fourfold from 2000 to 8000 in every stage: searches, which allocate nothing.
Callgrind, n = 1000 to 2000: 0.99 G to 2.10 G instructions, with `List.find?` over the type lists
and `memcmp` the growing part.

## Cause

Codex review 2026-10-26 located the searches (correcting some of the profile's attributions,
which named shared specializations):
- Resolve: `isKnownType` asks `ctx.knownTypes.contains`, a list of every type name, for every
  struct literal and written type; `resolveFnBody` also copied that list per function to add the
  type parameters.
- Check: `lookupStruct`, `lookupEnum`, `lookupNewtype`, the Copy judgment's callback in
  `isCopyType` and `TypeEnv.typeIdOf` search `env.structs`/`enums`/`newtypes`; `structIsLocal`
  searches the lists of local struct identities and names.
- Elab: `lookupStruct`, `lookupEnum`, `nominalTypeId?`, the newtype lookups and `typeIdOfSpelling`
  search the environment's lists.
- CoreCheck: its own `lookupStruct`, `isOpaqueOperandTy` (`any` over three lists and
  `programTypeNames.contains`) and `findNewtype`.
- Lower: `Layout.lookupStruct` (from `fieldOffset`, twice per struct use), `lookupStructFields`,
  `variantIndex`, `variantFields` and the enum payload lookups search the lists.

## Fix

Each environment holds its declarations by name, built once when the environment is: Check's
and Elab's `typeIndex` (`TypeNameIndex.ofLists`), CoreCheck's `typeIndex`, `programTypeNames` and
`programNewtypeIndex`, Lower's `layoutIndex` (`Layout.Index.ofLists`, once per module and handed to
each function, read through `getLayoutCtx`), Resolve's `knownTypes` as a set with the function's
type parameters beside it (`extraKnownTypes`), and Check's local identity and name sets. Each
index keeps the FIRST declaration of a name, as `List.find?` returned it, and
`TypeNameIndex.typeIdOf` answers as `typeIdOfSpelling` did. `Layout.Ctx.index` is optional: a
producer that has not built it (EmitSSA, the reports, the interpreter) still searches the lists.

`--emit-ssa` and `--emit-llvm` (output, diagnostics and exit status) of every `.con` under
`tests/programs` and `examples` (4122 compiles) are byte-identical with main's compiler.

## Measurements

Same program, ms, fastest of 3, dev host, after the fix:

| n | resolve | check | elaborate | coreCheck | lower |
|---|---|---|---|---|---|
| 2000 | 5 | 9 | 8 | 1 | 19 |
| 8000 | 24 | 48 | 50 | 7 | 96 |
| 10000 | 33 | 63 | 78 | 11 | 140 |

Callgrind with the fix, n = 2000 to 4000: 1.18 G to 2.23 G instructions (x1.9 for x2).

## Regression gate and leg

`run_tests.sh`: `regress_291_type_lookups_by_name.con` (layout offsets, a Copy struct, an enum
payload, a newtype) returns 42, and a program of 8000 Copy structs must check and elaborate within
150 ms per stage, retried once at 300 ms (main's compiler: 548 and 343 ms).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a program-wide table
kept as a list and searched per use, the shape of bugs 275 – 279. Five stages kept their own copy
of the same search (class 12's shape), so fixing one left the others.

## Structural owner

R-0492/R-0514 (one resolver: declarations found by identity from one table built once); R-0422
holds the throughput envelope meanwhile.

Codex review 2026-10-26: PARTLY CONFIRMED — Repeated linear type and locality searches cause quadratic work in all five stages, but several claimed source-level call relationships and the EmitSSA timing attribution are incorrect.
