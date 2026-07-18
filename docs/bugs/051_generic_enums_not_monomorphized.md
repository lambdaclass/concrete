# Bug 051: user-defined generic enums are never monomorphized — mixed instantiations corrupt memory

**Status:** Contained (R-0001 slice 1, 2026-07-18) — user generic enums are
rejected fail-closed at Mono with **E0808** rather than emitting corrupt code
(`Concrete/IR/Mono.lean` monoProgram; gate
`scripts/tests/check_generic_enum_containment.sh`; fixture
`tests/programs/error_generic_enum_051.con`). Builtin Option/Result are
unaffected (canonical union). REMAINING: real per-instantiation enum
monomorphization (mirror the struct pass / union treatment) to lift the
restriction — that is the rest of R-0001.
**Discovered:** 2026-07-18, middle-end audit; independently re-verified
(compiled prints garbage `-1640`, interp prints the correct values).

## Symptom

```con
enum Wrap<T> { W { v: T }, N }
let a: Wrap<[i64; 3]> = ...   -- payload 24 bytes at offset 8
let b: Wrap<[i32; 4]> = ...   -- payload 16 bytes at offset 4
```

EmitSSA declares ONE `%enum.Wrap = { i32, [16 x i8] }` (sized from the
smaller/first instantiation), while Lower writes the `[i64; 3]` payload at
offset 8 through it — 24+8 bytes into a 20-byte alloca. Silent stack
corruption / garbage reads. Builtin `Option`/`Result` are NOT affected
(whole-program alignment-aware union, audit 3/3) — only user generic enums.

## Root cause

`monoStructsInProgram` (`Concrete/IR/Mono.lean:838-909`) monomorphizes only
`m.structs`; there is no enum pass, so post-Mono Core keeps
`.generic "Wrap" [args]` types. EmitSSA then emits one LLVM type per enum
name from the first instantiation found (EmitSSA.lean:1389-1401) while
Lower computes per-instantiation payload offsets — the same class as the
builtin-enum canonical bug, minus the builtin's program-wide canonical
union.

## Root fix (R-0001 slice 2) — DIRECTION FIXED: per-instantiation monomorphization

Per-instantiation enum monomorphization (NOT a program-wide oversized union):
each canonical instantiation gets its own injective mangled name and exact
layout, mirroring the struct-mono pass — no permanent oversized
representations, and it fixes the name/layout-identity failure directly.

Reuse: `monoStructName` (mangling), `collectGenericTyInstances` /
`collectFnInstances` (instance collection), `rewriteTy` / `rewriteFnTys`
(type rewriting). New: a `substEnumTypeArgs` (parallel to
`substStructTypeArgs`) + `monoEnumsInProgram` (parallel to
`monoStructsInProgram`). KEY subtlety: the struct pass rewrites function types
and adds struct defs but does NOT rewrite enum-def payloads, so struct+enum
mono must share one instance mapping and run in an order that resolves nested
generics (generic struct in an enum payload, generic enum in a struct/array).

### Acceptance criteria (binding, per review 2026-07-18)

1. Two instantiations differing in BOTH size and alignment.
2. Nested generic enums and enum-in-array / enum-in-struct cases.
3. Cross-module and renamed-import use.
4. Canonical, INJECTIVE generated identities (no two distinct instantiations
   collide to one mangled name; no accidental sharing).
5. Interpreter/LLVM differential agreement on every case.
6. Linear payloads destroyed exactly once (drop-glue through the mangled enum).
7. Verifier check: every payload write fits its emitted aggregate
   (SSAVerify/layout-consistency; the class gate the bug's own note asked for).
8. Remove E0808 ONLY for newly-supported cases.
9. E0808 RETAINED fail-closed for any genuinely unsupported residual
   (e.g. instantiations the pass cannot resolve).

## Candidate fix

Either monomorphize generic enums per instantiation (mirroring the struct
pass, mangled names per type args), or emit the builtin-union treatment for
user generic enums (program-wide alignment-aware footprint scan + per-
instantiation offsets, extending the audit-3/3 mechanism). Regression: the
repro above prints 3 and 40 compiled AND interpreted; a two-instantiation
fixture in tests/programs pins it. Class gate: SSAVerify/layout consistency
check that every enum instantiation's writes fit the declared type.
