# Numeric Model (literals, casts, overflow) (design record)

**Superseded as the normative text (2026-10-23).** See [docs/spec/arithmetic.md](../spec/arithmetic.md)
(ARITH-2 to ARITH-6) and TYPE-4 / TYPE-6 of [docs/spec/types.md](../spec/types.md).

Status: CORE-COMPLETE — ROADMAP old Phase 6 #6. The literal rules are
documented and gated here; the soundness footgun (silent out-of-range literal
truncation) is fixed. Overflow and casts are owned by
[ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md) (overflow: §1 and §5 – §7, checked in every
profile since ROADMAP #10 Stage 2.3; casts: §8, decided 2026-10-21); the sections below
summarize them. The signed/unsigned comparison lint stays routed to #21 lint/vet;
literal suffixes are workload-gated. Gated by `scripts/tests/check_numeric_literals.sh`.
Date: 2026-06-23 (overflow and cast sections updated 2026-10-21)

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

The rule is [ARITHMETIC_POLICY.md §8](ARITHMETIC_POLICY.md#8-numeric-conversions-as)
(decided 2026-10-21, staged): `as` performs only conversions that are lossless for
every value of the source type, and every lossy numeric conversion is a compile error
that uses a named operation (checked conversion returning an error, or explicit
truncation / reinterpretation / rounding). Until that lands, `as` converts in every
direction and narrowing **truncates** silently (`300 as u8` → 44, `-1 as u32` →
4294967295, `3.9 as i32` → 3); a float → integer conversion whose value is out of the
target's range **traps**.

There is **no implicit numeric conversion** — even widening requires `as`
(`want_u64(my_u8)` is a type error, E0220; write `my_u8 as u64`). This keeps every
conversion visible at the call site.

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

The policy is [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md) §1 and §5 – §7; this is a summary.

- Ordinary `+ - *`, negation, division and remainder **trap (abort) on overflow and on a
  zero divisor** (`MIN / -1` included). A shift traps on a count `>= width` or negative
  and otherwise discards the shifted-out bits without a trap. All of this holds **in every
  profile**: codegen is profile-independent (`--profile` changes only `--report profile`),
  and the interpreter traps to match. True on main since ROADMAP #10 Stage 2.3.
- `wrapping_*` and `saturating_*` are the named alternatives for modular and clamping
  arithmetic.
- **Proven overflow can be a hard error** (E0900, `proven runtime-safety
  violation … always overflows`) via the same enforcement path as proven
  out-of-bounds / division-by-zero (`provenViolationDiagnostics`). NOTE: the
  constant-folding that *proves* overflow is conservative — not every
  statically-overflowing expression is recognized as proven (`let c: u8 = 200 + 100`
  compiles and traps at run time), so this is a partial guarantee, not a complete one.

## Summary of what is enforced today vs. routed

| Concern | Today | Where the rest lives |
|---|---|---|
| out-of-range literal | **rejected (E0227)** | — (done) |
| implicit conversion | **rejected (E0220)** — explicit `as` only | — (done) |
| comparison signedness | type-driven | — (done) |
| lossy `as` cast | silent truncation (float → integer out of range traps) | compile error, decided 2026-10-21, staged (ARITHMETIC_POLICY §8; exit 2k) |
| mixed-signedness compare | accepted | lint → #21 |
| literal suffixes | unsupported | workload-gated |
| proven overflow | E0900 (folder partial) | folder completion → #10 |
| unproven overflow | traps (abort), every profile | — (done; ARITHMETIC_POLICY §1) |
