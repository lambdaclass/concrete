# Bug 037: repr(align(N>8)) changes Layout but not the declared LLVM type

**Status:** Open
**Discovered:** 2026-07-16, during the audit-3/3 enum-union fix
(`Concrete/Backend/EmitSSA.lean` canonical-union work).

## Symptom

`#[repr(align(16))] struct Aligned { x: i32 }` makes `Layout.tySize` = 16 and
`Layout.tyAlign` = 16, and `sizeof::<Aligned>()` correctly evaluates to 16
(`tests/programs/repr_align.con`, run_ok 16) — but the declared LLVM type is
still `%struct.Aligned = type { i32 }` (size 4, align 4): `structTypeDef`
(`Concrete/Check/Layout.lean:455`) ignores `reprAlign` entirely.

Consequences whenever the type is *stored*, not just `sizeof`'d:

- an alloca reserves 4 bytes where Concrete believes 16 are available;
- a containing struct's fields are placed by Concrete at align-16 offsets
  that the LLVM layout disagrees with;
- enum payloads / arrays of the type inherit both errors.

Only `repr(align(N))` with N greater than the struct's natural alignment is
affected; smaller or equal values are no-ops and safe. The one existing test
exercises only the `sizeof` constant, which is why nothing caught it.

## Context

Found while fixing the enum canonical-union alignment (audit 2026-07-16):
the union work assumed "all current Concrete types have alignment ≤ 8"
(EmitSSA.lean note at the builtin-enum emission). repr(align(16)) is the one
shipped feature that falsifies that assumption at the *declaration* level,
not only inside enum unions.

## Candidate fix (not yet applied)

Fail closed: reject `repr(align(N))` with N greater than the struct's
natural field alignment with a structured diagnostic ("over-alignment is not
yet supported in codegen"), until struct declarations carry explicit
alignment (padding + aligned members or alloca align attributes, the same
mechanism the enum-union fix used). The power-of-two check
(`reprAlignNotPowerOfTwo`, E0577) is the adjacent validation point.
`tests/programs/repr_align.con` would flip to an error fixture or move to
`align(8)`.

## Regression coverage

None yet — filed from review, not from a failing program. The gate when
fixed: the E-code diagnostic fixture plus a `check_struct_layout.sh`-style
assertion that over-aligned structs are rejected.
