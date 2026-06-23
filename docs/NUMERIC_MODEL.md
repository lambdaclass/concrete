# Numeric Model (literals, casts, overflow)

Status: CORE-COMPLETE — ROADMAP Phase 6 #6. The de-facto numeric rules are
documented and gated here; the soundness footgun (silent out-of-range literal
truncation) is fixed; the remaining sub-parts are routed to their natural homes
(overflow profiles → #10 build profiles; lossy-cast and signed/unsigned lints →
#21 lint/vet; literal suffixes → workload-gated). Gated by
`scripts/tests/check_numeric_literals.sh`.
Date: 2026-06-23

## Integer types

`Int`/`i64`, `Uint`/`u64`, and the fixed widths `i8 i16 i32` / `u8 u16 u32`.
There is no separate `i64`/`u64` constructor — `Int` is the 64-bit signed type,
`Uint` the 64-bit unsigned.

## Literals

- **Default type.** An un-annotated integer literal is `Int` (i64): `let x = 5`
  gives `x : Int`. With a hint (annotation, parameter, field, arm), the literal
  takes the hinted integer type.
- **Bases.** Decimal, hex (`0xFF`), and binary (`0b1010`) literal bases are
  supported.
- **Out-of-range literals are rejected, not truncated** (E0227). `let a: u8 =
  300` is a hard error (`integer literal 300 is out of range for type 'u8'
  (0..=255)`), not a silent `44`. This holds for every fixed width and for
  negatives via unary minus: `let a: u8 = -1` and `let a: i8 = -129` are
  rejected, while the exact signed minimum `let a: i8 = -128` compiles (the
  negated value `-N` is range-checked, not the positive inner literal). This is
  the one numeric *soundness* rule — a literal must never silently become a
  different value.
- **Suffixes** (`7u8`, `5i32`) are NOT supported (parse error). Use an annotation
  or `as`. (Workload-gated: annotations already cover the need; a suffix is pure
  convenience. Revisit if a real workload shows annotations are too noisy.)

## Casts (`as`)

- `as` converts between numeric types in every direction (widen, narrow,
  signed↔unsigned, int↔float). Narrowing **truncates** (`300 as u8` → 44,
  `-1 as u32` → 4294967295, `3.9 as i32` → 3).
- **`as` is the explicit, opt-in lossy path.** There is **no implicit numeric
  conversion** — even widening requires `as` (`want_u64(my_u8)` is a type error,
  E0220; write `my_u8 as u64`). This keeps every conversion visible at the call
  site.
- A diagnostic/lint for *provably* lossy casts (a narrowing `as` on a constant
  that loses information) is a **lint**, not a type error — routed to #21
  (`concrete lint`/`vet`), since blanket-warning the explicit escape hatch would
  be noise.

## Comparisons

- Comparisons follow the operand type's signedness: a `u8`/`u16`/… comparison is
  unsigned (`(200 : u8) < (100 : u8)` is false), a signed-type comparison is
  signed. This is enforced in codegen (EmitSSA picks `icmpU*` vs `icmpS*` from
  the operand type).
- A **mixed-signedness** comparison (`i32 < u32`) is currently accepted without a
  diagnostic. Whether to reject it or require an explicit cast is a **lint**
  decision routed to #21; the model does not silently reinterpret — both sides
  keep their declared types.

## Overflow

- Arithmetic generates runtime-safety **overflow obligations** (alongside
  bounds/division obligations).
- **Proven overflow can be a hard error** (E0900, `proven runtime-safety
  violation … always overflows`) via the same enforcement path as proven
  out-of-bounds / division-by-zero (`provenViolationDiagnostics`). NOTE: the
  constant-folding that *proves* overflow is currently conservative — not every
  statically-overflowing expression is recognized as proven (e.g. `let c: u8 =
  200 + 100` is not caught today), so this is a partial guarantee, not a complete
  one.
- **Unproven overflow wraps** at runtime in ordinary builds (two's-complement),
  exactly as an unproven array index compiles without a runtime trap today. This
  is consistent across all runtime-safety obligations.
- **Deferred to #10 (build profiles):** a runtime **checked** overflow profile
  (trap on overflow), explicit **wrapping**/**saturating** arithmetic helpers
  (`wrapping_add`, …), completing the proven-overflow folder, and the
  strict/predictable-profile policy for unproven overflow. Overflow *policy* is a
  build-profile concern (debug-checked vs release-wrapping), not a literal/cast
  rule, so it lives with #10 rather than here.

## Summary of what is enforced today vs. routed

| Concern | Today | Where the rest lives |
|---|---|---|
| out-of-range literal | **rejected (E0227)** | — (done) |
| implicit conversion | **rejected (E0220)** — explicit `as` only | — (done) |
| comparison signedness | type-driven | — (done) |
| lossy `as` cast | silent (explicit opt-in) | lint → #21 |
| mixed-signedness compare | accepted | lint → #21 |
| literal suffixes | unsupported | workload-gated |
| proven overflow | E0900 (folder partial) | folder completion → #10 |
| unproven overflow | wraps | checked/wrapping profiles → #10 |
