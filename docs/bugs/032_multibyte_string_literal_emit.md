# Bug 032: multibyte UTF-8 string literals miscompile in emitted LLVM globals

**Status:** Fixed (2026-07-14)
**Fixed in:** EmitSSA.lean — `ssaEscapeCharForLLVM` now emits each non-ASCII
char's UTF-8 BYTES as `\XX` hex escapes (it hex-escaped the CODEPOINT, garbage
for anything >0xFF); the string-literal global emission sizes the array and
`stringLengths` entry by `utf8ByteSize` (was `String.length` = char count).
**Regression test:** `tests/programs/regress_032_multibyte_str_literal.con`
(run_ok 42) — any compiled program with a non-ASCII literal failed `llvm-as`
before the fix.
**Battery:** std 284/0, fast suite, golden, examples, ssa-verify — green.
**Discovered:** 2026-07-14
**Discovered in:** Phase 7 pull 2 — `test_bytes_from_string` used a `"hi✓"`
literal; the full-std `--test` build then failed `llvm-as` validation:
`constant expression type mismatch: got type '[7 x i8]' but expected '[4 x i8]'`.

## Symptom

Any COMPILED program containing a non-ASCII string literal failed LLVM IR
validation (`llvm-as` reject), because the emitted global declared
`[chars+1 x i8]` while the escaped content was `bytes+1` long. Additionally
the escape text itself was mangled — `hexDigit (0x271)` produced non-hex
characters. Interp was unaffected (never hit the emitter), making this a
build-level differential. Nothing caught it earlier because the entire fixture
corpus, std, and all examples used ASCII-only literals; the workload-driven
UTF-8 test was the first non-ASCII literal to reach the backend.

## Fix

Byte-based, both halves: escapes iterate `(String.singleton c).toUTF8`, sizes
use `val.utf8ByteSize`. `stringLengths` feeds the materialized `String.len`
field, so the runtime length now also agrees with std's byte-length `len()`
(UNICODE_POLICY: String.len is bytes, not chars).
