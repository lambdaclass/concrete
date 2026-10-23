# Proof extraction gaps found by workload proofs

Status: findings record (2026-10-21)

Adding `#[proof_by]` claims to pure functions of the Phase 7 workloads (`toml`, `hexdump`,
`png_chunks`) ran into extractor refusals that the flagship examples never hit, because the
flagships were written for the extractor and the workloads were not. Each gap below gives the exact
refusal from `concrete <file> --report extraction`, what the workload did about it, and the compiler
capability that would close it. [PROVABLE_V1.md](PROVABLE_V1.md) remains the allowlist; this file
records demand against it.

## 1. Boolean `&&` / `||` are not extracted

```
toml_parser.is_bare_key_char(ch)
  status: eligible (extraction failed)
  unsupported: unsupported operator: Concrete.BinOp.and_ at Concrete.Ty.bool
toml_parser.is_ws(ch)
  status: eligible (extraction failed)
  unsupported: unsupported operator: Concrete.BinOp.or_ at Concrete.Ty.bool
```

Every character-class predicate in `examples/toml` (`is_ws`, `is_newline`, `is_digit`,
`is_bare_key_char`) and the signature test in `examples/png_chunks` were written with `&&` / `||`,
the natural shape for a validator, and none of them extracted.

**What the workloads did:** rewrote each predicate as an early-return chain
(`if ch < 48 { return false; } return ch <= 57;`), which extracts to a linear `ifThenElse` chain
without duplicating continuations. The rewrite is behaviour-preserving (the example output gates are
unchanged), but it is a rewrite done for the prover, not for the reader.

**Capability that closes it:** extract `a && b` as `ifThenElse a b false` and `a || b` as
`ifThenElse a true b` at `Ty.bool`. That is exactly the short-circuit semantics Concrete's `&&`/`||`
have, so the extraction would be faithful.

## 2. `u64` (`Ty.uint`) division, remainder and bitwise operators are not extracted

```
probe.crc_step64(c)   -- a scratch probe (not in the repo): one step of std.checksum.crc32, as std writes it
  status: eligible (extraction failed)
  unsupported: unsupported operator: Concrete.BinOp.mod at Concrete.Ty.uint,
               unsupported operator: Concrete.BinOp.bitand at Concrete.Ty.uint,
               unsupported operator: Concrete.BinOp.bitxor at Concrete.Ty.uint,
               unsupported operator: Concrete.BinOp.div at Concrete.Ty.uint
```

The same step written at `u32` (`if c % 2 == 1 { return (c / 2) ^ 3988292384; } return c / 2;`)
extracts, as does an eight-iteration byte update at `u32` (as a `while_step` loop). std's `crc32`
does its arithmetic in `u64` under an explicit 32-bit mask, so its `%`, `/`, `^` and `&` do not
extract (`u64` `+`, `-` and `*` do: `binOpToPBinOp` maps them width-agnostically). And the function
as a whole is refused before operators are considered:

```
std.checksum.checksum_crc32(b, start, len)
  status: excluded
  reasons: unbounded loops
```

because its outer loop runs to a runtime `len` over a `Bytes` buffer.

**What the workloads did:** no claim on the CRC32 path. `png_chunks` carries a claim on its
signature check only. Rewriting std's `crc32` in `u32`, only to fit the extractor, would touch a
shared std module and the many programs that use it.

**Capability that closes it:** `mod`, `div`, `bitand`, `bitxor` (and the shifts) at `Ty.uint`
(64-bit unsigned), in the same append-only way `u32` gained them for HMAC-SHA256. That makes the
byte-update step provable. The whole `crc32` additionally needs a `Bytes` model in the proof
`FnTable` (see 3) and a loop bounded by a parameter rather than a literal.

## 3. `Bytes` reads extract to calls nothing models

```
png_chunks.is_png_sig(b)
  status: extracted
  ProofCore: if (Bytes_len(b) < 8) then false else png_sig_ok(Bytes_get_unchecked(b, 0), ...)
```

`is_png_sig` reports `extracted`, but its body is calls to `Bytes_len` and `Bytes_get_unchecked`,
and no proof `FnTable` gives those a body. A theorem about `is_png_sig` would evaluate to `none` at
the first call. So the workloads prove the pure decision (`png_sig_ok`) and leave the buffer read
that feeds it, a length check followed by eight in-bounds reads, unproved. The same boundary is why
`hexdump.dump_line` (String building, `Bytes` reads, `Console`) is out of reach while its two
helpers are proved.

**Capability that closes it:** a `Bytes` value model in ProofCore (length plus a byte list) with
`Bytes_len` / `Bytes_get_unchecked` / `Bytes_get` given model bodies in a shared std proof table,
the way fixed arrays already are.

## Demand from the language specification's examples (2026-10-23)

The specification's accepted examples also run under the model (`--report model-diff`, the third
leg of check_spec S4). Of 117 accepted single-file examples, 16 were compared at first: `main` was
excluded from extraction as the entry point, so a program whose behaviour is all in `main` had no
model. Two changes raised that to 48, every compared input agreeing:

- the comparison also extracts an entry point whose only exclusion is being `main` (proof
  eligibility is unchanged; the comparison alone uses it): 16 to 42;
- the functional state model gained assignments outside loops and `match` statements followed by
  more code ([PROVABLE_V1.md](PROVABLE_V1.md), "Statements And State"): 42 to 49.

What blocks the remaining 69, by examples blocked (one example can have several blockers):

| blocker (`--report extraction`) | examples | status in PROVABLE_V1 |
|---|---|---|
| `borrow` / `mutable borrow` / `deref` / `borrow region` | 30 | excluded: references and borrow semantics |
| unmodelled statement (`discard(e)`, expression statements, destructuring) | 6 | not admitted |
| `main` has capabilities, a non-scalar result or sits in a module | 7 | outside the comparison |
| unbounded loops | 5 | excluded |
| unsupported operator (`div`/`shl`/`bitand` at `Int`/`u8`, wrapping/saturating at `u8`/`i8`, `&&`) | 7 | width follow-ups |
| `unary operator` | 3 | not admitted |
| guards, range patterns, labelled `break` | 4 | not admitted |

References are the largest: admitting shared borrows of values (a `&x` argument read through
`*r`) would unlock about twenty, and is a change to the proof surface, not to this comparison.
