# Bug 051: user-defined generic enums are never monomorphized — mixed instantiations corrupt memory

**Status:** FIXED (R-0001, 2026-07-24) — user generic enums are monomorphized per
canonical type arguments, so each instantiation has its own declaration and its
own payload footprint (`Concrete/IR/Mono.lean` `monoTypesInProgram`). Gate:
`scripts/tests/check_generic_enum_mono.sh` (20 checks: values with
interpreter agreement, layout, linear payloads, builtin non-interference,
forged-name rejection). Fixtures: `tests/programs/regress_generic_enum_051.con`
(43) and `tests/programs/adversarial_mono_generic_enum.con` (42). Mutation
`test_mutation.sh` #19 disables the specialization and is KILLED.
**Discovered:** 2026-07-18, middle-end audit; independently re-verified
(compiled printed garbage `-1640`, interp printed the correct values).
**Contained:** 2026-07-18 (slice 1) — every user generic enum rejected with
E0808. **Root fix:** 2026-07-24 (below).

## Symptom

```con
enum Wrap<T> { W { v: T }, N }
let a: Wrap<[i64; 3]> = ...   -- payload 24 bytes at offset 8
let b: Wrap<[i32; 4]> = ...   -- payload 16 bytes at offset 4
```

EmitSSA declared ONE `%enum.Wrap = { i32, [16 x i8] }` (sized from the
smaller/first instantiation), while Lower wrote the `[i64; 3]` payload at
offset 8 through it — 24+8 bytes into a 20-byte alloca. Silent stack
corruption / garbage reads. Builtin `Option`/`Result` were NOT affected
(whole-program alignment-aware union, audit 3/3) — only user generic enums.

## Root cause

`monoStructsInProgram` monomorphized only `m.structs`; there was no enum pass, so
post-Mono Core kept `.generic "Wrap" [args]` types. EmitSSA then emitted one LLVM
type per enum name from the first instantiation found while Lower computed
per-instantiation payload offsets — the same class as the builtin-enum canonical
bug, minus the builtin's program-wide canonical union.

## The fix

`monoStructsInProgram` became `monoTypesInProgram`: structs and enums are
specialized by ONE instance set and ONE mangled-name mapping, because they nest
in each other (`Wrap<Point>`, `Holder { w: Wrap<i32> }`). Beyond mirroring the
struct pass, three things were needed:

- **Enum identity at use sites.** Lower resolves variant indices and payload
  layout by NAME, so `enumLit` and `enumArm` are rewritten to the specialization
  name. Arms carry no type of their own, so the instantiation is read from the
  scrutinee's type — the same place Lower reads it (`monoTypeNameFor`).
- **Every type-bearing declaration is rewritten**, not just function signatures
  and bodies. A non-generic `struct Holder { w: Wrap<i32> }` kept a `Ty.generic`
  field, and Layout then sized that field from the unsubstituted declaration —
  the same wrong-layout path reached through a field instead of a local.
- **The instance set is a transitive closure** over definition bodies
  (`closeInstances`), since `Wrap<Box<i32>>` names `Box<i32>` only inside Wrap's
  variant payload.

E0808 is retained as a residual backstop: a `Ty.generic` reference to a user enum
surviving mono fails closed instead of being laid out from an unsubstituted
declaration.

## Acceptance criteria (from the 2026-07-18 review)

1. **Size and alignment** — met. `mixed_sizes` (i64/i32), `mixed_align`
   (i8/i64), `array_payloads` ([i64;3]/[i32;4]).
2. **Nested enums, enum-in-array, enum-in-struct** — met. `nested_enum`
   (`Wrap<Wrap<i32>>`), `enum_in_array` (`[Wrap<i32>;3]` and `[Wrap<Int>;2]`),
   `struct_payload` (`Wrap<Point>` and `Holder { w: Wrap<i32> }`).
3. **Cross-module and renamed-import** — cross-module met (`cross_module`).
   Renamed import is NOT reachable: importing a TYPE under a new name is
   unsupported language-wide (`import Types.{ Color as Hue }` fails at Check with
   E0261 for a non-generic enum too), so there is nothing for mono to get wrong.
   If type aliasing on import ever lands, it must add a generic-enum case here.
4. **Injective generated identities** — partially met, deliberately.
   Distinct instantiations of one base are already injective (`tyToSuffix` keys
   on the full bracketed type; `check_mono_name_collision.sh`). FORGED names
   could still collide — a declaration spelled `Wrap_Int` occupies the name
   `Wrap<Int>` mangles to, and with the user declaration narrower the
   specialization writes past its end. That path was unreachable while E0808
   refused every user generic enum, so this fix closes it by FAILING CLOSED with
   **E0809** rather than by inheriting it. A mangling users cannot forge is
   R-0007's subject.
5. **Interpreter/LLVM differential agreement** — met on every case whose
   intrinsics the interpreter implements; the two exceptions (`vec_pop`, the
   raw-pointer counter cast) are compiled-only and say so.
6. **Linear payloads destroyed exactly once** — met. `linear_payload_once`
   counts destroys through a specialized enum (exactly 1);
   `linear_payload_leak` pins that specialization does not launder linearity.
7. **Verifier: every payload write fits its emitted aggregate** — NOT met.
   The gate asserts this structurally for its own programs (distinct
   declarations, differing footprints) and the fix makes it true by
   construction, but there is no pass that checks it for ALL programs. Tracked
   as the remaining slice of R-0001.
8. **E0808 removed only for newly-supported cases** — met; all user generic
   enums are now supported.
9. **E0808 retained fail-closed for residuals** — met (see above).

## What the mutation showed

Disabling specialization (`test_mutation.sh` #19) makes the gate red with 8
failures, three of them SIGABRT stack-smashing. Two lessons worth keeping:

- A **value check alone is not sufficient**. Under the mutation the shared
  declaration is sized from whichever instantiation is emitted first, so cases
  where the WIDER one comes first still returned correct results. The layout
  assertion is the reliable detector.
- **Guard locals do not detect the clobber.** Scalar locals live in SSA
  registers, not adjacent stack slots, so a source-level neighbour probe cannot
  be credited as a corruption detector.
