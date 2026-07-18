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

## Candidate fix

Either monomorphize generic enums per instantiation (mirroring the struct
pass, mangled names per type args), or emit the builtin-union treatment for
user generic enums (program-wide alignment-aware footprint scan + per-
instantiation offsets, extending the audit-3/3 mechanism). Regression: the
repro above prints 3 and 40 compiled AND interpreted; a two-instantiation
fixture in tests/programs pins it. Class gate: SSAVerify/layout consistency
check that every enum instantiation's writes fit the declared type.
