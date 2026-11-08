### Lexer literal spellings and raw-scalar refusals (2026-11-08)

Implements owner items 3, 4 and 5, with the owner's follow-up spellings of 2026-11-08, in the
slice-0 declarative surface judgment and executable lexer:

- ASCII `u8` byte literals admit raw U+0020–U+007E except single quote and backslash, and exactly
  the escapes `\n`, `\t`, `\r`, `\0`, `\\` and `\'`. Closing-quote matching distinguishes them from
  labels; non-ASCII content, `\"` and `\xHH` are refused.
- Strings refuse raw Unicode Cc, U+2028/U+2029 and the decided bidi controls. Comments permit raw
  TAB alone among Cc, refuse the other forbidden scalars, and end before LF or CR. These refusals
  propagate through trivia scanning as internal error constructors.
- Integers support the lowercase `0x`, `0b` and `0o` prefixes, lowercase hexadecimal digits and the
  suffixes `i8 i16 i32 i64 u8 u16 u32 u64` on every base. Decimal leading zeroes remain refused.
  Adjacent malformed identifier-continuation text, including `_`, invalid base digits and float-like
  or repeated suffixes, is one refused token rather than an integer followed by an identifier.
- Executable scanners have soundness and completeness theorems against the independent judgments,
  plus kernel-checked conformance and range-boundary examples.
- checker obligation: complementary suffix typing per owner 2026-11-08; Q6.2's example becomes S { a: 200 }

Stacking dependency: this branch is based on `da687c9ec` (`wip/slice0-lexer-tokens`), which is not
yet on `main`.
