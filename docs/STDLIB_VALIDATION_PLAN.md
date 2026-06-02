# Stdlib Validation Plan: Canonical Examples Against Stdlib APIs

Status: Phase 3 support document (items 66-68)

This document maps each canonical example and pressure test to the stdlib APIs it should use, identifies which programs can be rewritten to use stdlib types instead of ad hoc copies, and defines what "validated" means.

Status note: the `parse_validate` and `service_errors` rewrites are landed. Sections 2.1 and 2.2 are marked DONE. The remaining sections are kept as a template for future rewrites.

For the stdlib module inventory, see [STDLIB_TARGET.md](STDLIB_TARGET.md).
For error handling ergonomics, see [ERROR_HANDLING_DESIGN.md](ERROR_HANDLING_DESIGN.md).
For byte cursor APIs, see [BYTE_CURSOR_API.md](BYTE_CURSOR_API.md).
For the string/text/bytes split, see [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md).
For the core/hosted layer split, see [HOSTED_STDLIB_SPLIT.md](HOSTED_STDLIB_SPLIT.md).
For Phase 3 exit criteria, see [PHASE_EXIT_CHECKLISTS.md](PHASE_EXIT_CHECKLISTS.md) lines 72-94.

---

## 1. Validation Criteria

An example program is "stdlib-validated" when all of the following are true:

### 1.1 Uses stdlib Result/Option instead of custom Copy enums

The program uses `Result<T, E>` and `Option<T>` for fallible operations instead of defining its own structurally identical enums (`ParseResult`, `ValidateResult`, `AuthResult`, `ServiceResult`, etc.). The `?` operator is used where the error type matches the enclosing function's return type. Where error types differ, `map_err` + `?` is used instead of manual match-and-convert blocks.

### 1.2 Uses stdlib String/Bytes instead of raw arrays where appropriate

Programs that process text use `std.string.String` and `&String` for owned and borrowed text. Programs that process binary data use `std.bytes.Bytes` for owned buffers and byte array views for fixed-size data. Raw `[u8; N]` arrays are acceptable for fixed-size protocol headers and stack-only predictable code. Raw `[i32; N]` arrays used as surrogate strings (e.g., storing characters as integers) are replaced.

### 1.3 Uses stdlib byte cursor for binary parsing instead of hand-rolled shifts

Programs that parse binary protocols (DNS, ELF, packet formats) use `ByteCursor` from `std.numeric` instead of hand-rolling `read_u16_be`, `read_u32_be`, and manual offset tracking. Standalone endian functions (`read_u16_be_at`) are acceptable for single-read patterns.

### 1.4 Uses stdlib error helpers instead of verbose match cascades

Multi-stage pipelines that convert between error types use `Result.map_err` + `?` instead of 9-line match-and-convert blocks per stage. Single-type error propagation uses `?` instead of explicit match on every fallible call.

### 1.5 Uses stdlib collections where they exist

Programs that hand-roll ring buffers, ordered maps, or hash maps use `std.deque`, `std.ordered_map`, or `std.map` instead of manual fixed-array implementations, unless the program intentionally demonstrates fixed-capacity patterns (e.g., `examples/fixed_capacity/`).

### 1.6 Uses stdlib character classification

Programs that hand-roll `is_digit`, `is_whitespace`, `is_alpha` with magic constants use `std.ascii` functions instead.

### 1.7 Compiles and produces correct output

The rewritten program compiles without errors, produces the same observable behavior (exit code, stdout output) as the original, and passes any existing test assertions.

### 1.8 No regressions in trust-gate

Programs that pass `--check predictable` continue to pass after rewriting. Programs with proof-backed functions retain their proofs. The trust boundary (trusted vs safe code) does not expand.

---

## 2. Example-by-Example Plan

### 2.1 `examples/parse_validate/` --- Phase 3 exit criterion (DONE)

**Status.** Rewritten to use `std.result.Result<Header, ParseError>` with `?` error propagation. Custom `ParseResult` enum removed. Validators return `Result<(), ParseError>`. Landed and verified (e29389e, 053472b).

### 2.2 `examples/service_errors/` --- Phase 3 exit criterion (DONE)

**Status.** Rewritten to use `std.result.Result<T, E>` with explicit match conversion at stage boundaries (`map_err` not yet available, so conversion functions used with manual match). Custom result enums removed. Landed and verified (053472b, 6d56806).

### 2.3 `examples/grep/` --- Phase 3 exit criterion (REQUIRED)

**Current state.** Already uses several stdlib types: `std.fs.read_to_string`, `Result`, `std.args`. However, it has significant hand-rolled code:
- `to_lower` reimplements ASCII case conversion with magic constants (65, 90, 32).
- `contains_pattern` calls `string_contains` but wraps it in manual case conversion.
- Flag parsing uses magic character codes (45 for '-', 110 for 'n', 99 for 'c', etc.).
- Line splitting uses manual `string_char_at` scanning for newlines (character code 10).
- Output construction uses `string_push_char(&mut out, 58)` instead of `out.append(&":")`.

**Stdlib types currently used.** `std.fs.read_to_string`, `Result`, `std.args.count`, `std.args.get`, `std.fs.FsError`. Also uses `String` (implicit via stdlib).

**What it should use after stdlib is complete.**
- `std.ascii.to_lower` for case conversion instead of hand-rolling the 65/90/32 range check.
- `std.string.to_lower` for full-string case conversion instead of the character-by-character loop.
- Explicit character constants or `std.ascii` classification instead of magic integers.
- `String.append` with string literals for `":"` instead of `string_push_char(&mut out, 58)`.
- `String.contains` method (already exists) used consistently.
- `String.starts_with` for flag detection instead of manual `string_char_at` checks.

**Changes needed.**
1. Replace `to_lower` function body with `std.string.to_lower` or `std.ascii` byte-level conversion.
2. Replace magic character codes with named constants or string literal comparisons where possible.
3. Replace `string_push_char(&mut out, 58)` with `out.append(&":")` and similar for other character pushes.
4. Replace flag parsing (`has_flag` function) with `String.starts_with` checks or `std.ascii` helpers.
5. The line-splitting pattern (scanning for newline bytes) remains manual -- there is no stdlib line iterator. This is acceptable.

**Priority.** REQUIRED for Phase 3 exit.

### 2.4 `examples/elf_header/` --- nice-to-have

**Current state.** Uses raw FFI for file I/O (`fopen`, `fread`, `fclose` as trusted extern). Extracts bytes one at a time via a trusted `read_byte` function using raw pointer dereference. The pure validator core (`check_magic`, `check_class`, `check_data`, `check_version`, `validate_header`) operates on `Int` values, not byte types. Has Lean-backed proofs for the pure core.

**Stdlib types currently used.** None for the core logic. Uses only raw pointers and `trusted extern fn`.

**What it should use after stdlib is complete.**
- `std.fs.read_file` or `std.fs.read_to_string` instead of raw `fopen`/`fread`/`fclose`.
- `ByteCursor` from `std.numeric` for byte extraction instead of the trusted `read_byte` function.
- `Result<ElfIdent, CursorError>` for the parsing return type instead of `i32` return codes.
- `std.args` for command-line argument parsing instead of raw `__concrete_get_argc`/`__concrete_get_argv`.

**Changes needed.**
1. Replace `trusted extern fn fopen/fread/fclose` with `std.fs.read_file`.
2. Replace `trusted fn read_byte` with `ByteCursor.read_u8`.
3. Adopt `std.args.count`/`std.args.get` for CLI parsing.
4. The pure validator core (`check_magic` etc.) can remain unchanged to preserve proofs. The boundary between trusted I/O and pure core narrows (ByteCursor is safe; `read_byte` was trusted).
5. Optionally adopt `Result<T, E>` for the validator return types (this would invalidate existing Lean proofs and require re-proving -- separate decision).

**Proof concern.** The pure core functions have Lean-backed proofs tied to their fingerprints. Changing their signatures or bodies invalidates the proofs. The stdlib rewrite should focus on the I/O shell, not the proved core. If the core is rewritten to use `Result`, the proofs must be regenerated.

**Priority.** Nice-to-have. Demonstrates ByteCursor value but proof preservation is a constraint.

### 2.5 `examples/json/` --- nice-to-have

**Current state.** A full recursive-descent JSON parser using `Vec<Val>`, `Vec<String>`, `Vec<ArrayDesc>`, etc. Defines its own `ParseResult` struct (`{ val: Val, pos: i32 }`). Hand-rolls `is_ws`, `is_digit`, `skip_ws`, keyword matching (`match_true`, `match_false`, `match_null`). Uses `string_char_at` with integer character codes throughout.

**Stdlib types currently used.** `Vec<T>` (via `vec_new`, `vec_push`, `vec_get`, `vec_free`), `String` (implicit).

**What it should use after stdlib is complete.**
- `std.ascii.is_digit`, `std.ascii.is_whitespace` instead of hand-rolled character classification.
- `std.parse.Cursor` (the text cursor) for position tracking instead of manual `pos: i32` threading.
- `Result<Val, ParseError>` instead of the ad hoc `ParseResult` with error-as-tag-6 pattern.
- `String.eq` or `string_eq` for keyword matching instead of byte-by-byte comparison loops.

**Changes needed.**
1. Import and use `std.ascii` for character classification functions.
2. Consider replacing the `(Val, pos)` pattern with a text Cursor that tracks position automatically.
3. Define a proper `ParseError` enum instead of using `Val { tag: 6 }` as the error signal.
4. Use `Result<Val, ParseError>` for parse functions.
5. Use `?` for error propagation in the recursive parse calls.

**Priority.** Nice-to-have. The parser is large and the rewrite is significant. The example already works and demonstrates capability separation well. Stdlib validation here is about polish, not exit criteria.

### 2.6 `examples/http/` --- nice-to-have

**Current state.** A minimal HTTP/1.1 server. Already uses `std.net`, `std.fs`, `Result`, `std.args`, `std.parse`, `Option`. Hand-rolls: `find_space` for request parsing, `is_get` method check using character codes, `is_path_safe` for path traversal detection, `buf_to_string` for byte-to-string conversion, `send_string` for string-to-network conversion.

**Stdlib types currently used.** `TcpListener`, `TcpStream`, `NetError`, `read_to_string`, `file_exists`, `FsError`, `Result`, `Option`, `parse_uint`, `count`, `get`.

**What it should use after stdlib is complete.**
- `String.starts_with` or `String.contains` instead of manual `find_space` parsing.
- `String.eq` for method comparison (`is_get`) instead of byte-by-byte character code checks.
- `String.contains` for path traversal detection.
- `Bytes.to_string` for byte buffer to string conversion instead of the trusted `buf_to_string`.
- A stdlib way to write strings to network streams (currently missing -- gap).

**Changes needed.** Moderate refactoring of string helper functions. The network I/O gap (no `stream.write_string`) means `send_string` stays as trusted code.

**Priority.** Nice-to-have. Already demonstrates good stdlib usage. Polish only.

### 2.7 `examples/fixed_capacity/` --- Phase 3 exit criterion (checked indexing/slice)

**Current state.** A bounded message processor with no allocation. Uses `MsgBuf` (Copy struct with `[u8; 256]`), `RingBuf` (Copy struct with `[i32; 16]`), and `ValidateResult` (ad hoc result struct with `error: i32`). Hand-rolls `read_u8`, `read_u16_be`, ring buffer operations, and tag computation. All stack-only, all bounded.

**Stdlib types currently used.** None.

**What it should use after stdlib is complete.**
- The `ValidateResult` struct with `error: i32` should use `Result<ValidateOk, ValidateError>` where `ValidateOk` holds the parsed fields and `ValidateError` is an enum.
- `read_u8` and `read_u16_be` could use standalone `std.numeric` functions, but this changes the Copy struct pattern (the current functions take `MsgBuf` by value and index with `buf.data[offset]`).
- The ring buffer is intentionally a fixed-capacity demonstration and should NOT be replaced with `std.deque`.

**Changes needed.**
1. Replace `ValidateResult { error: i32, ... }` with `Result<ValidateOk, ValidateError>` where `ValidateError` is a Copy enum.
2. Optionally use `std.numeric` standalone endian functions for `read_u16_be`.
3. Keep ring buffer operations as-is (they demonstrate the pattern the stdlib `Deque` is based on).

**Priority.** Required per Phase 3 exit ("one fixed-capacity example uses the checked indexing/slice surface"). The validation focus here is on the Result type adoption, not on replacing the ring buffer.

### 2.8 `examples/packet/` --- nice-to-have

**Current state.** Binary protocol decoder using trusted pointer operations. Defines its own `read_u8`, `read_u16_be`, `read_u32_be`, `write_u8`, `write_u16_be`, `write_u32_be`, `compute_checksum`, and `decode_header`. All byte operations are trusted (raw pointer dereference). Uses `DecodeResult` with `error: i32` field.

**Stdlib types currently used.** None.

**What it should use after stdlib is complete.**
- `ByteCursor` for reading instead of trusted `read_u8`/`read_u16_be`/`read_u32_be`.
- `ByteWriter` for test packet construction instead of trusted `write_u8`/`write_u16_be`/`write_u32_be`.
- `Result<PacketHeader, DecodeError>` instead of `DecodeResult` with `error: i32`.
- `CursorError` as the base error type, wrapped in a domain `DecodeError` enum.

**Changes needed.**
1. Replace all hand-rolled byte read/write functions with `ByteCursor`/`ByteWriter` methods.
2. Replace `DecodeResult` with `Result<PacketHeader, DecodeError>`.
3. The `decode_header` function goes from `trusted fn` to a safe function (ByteCursor is safe once constructed).
4. Test packet construction (`build_packet`) uses `ByteWriter` instead of raw pointer writes.

**Priority.** Nice-to-have. Excellent ByteCursor validation candidate -- the most natural rewrite target for the byte cursor API.

### 2.9 `examples/kvstore/` --- nice-to-have

**Current state.** A persistent key-value store using `HashMap<String, String>`, `Vec<String>`, file I/O. Already uses significant stdlib surface: `std.fs`, `Result`, `Option`, `std.args`, `std.bytes.Bytes`, `std.map.HashMap`, `std.hash`, `std.vec.Vec`.

Hand-rolls: `str_eq` for string comparison (has a local implementation despite `String.eq` existing), `clone_string`, `string_to_bytes`, `find_space`, line-by-line log parsing.

**Stdlib types currently used.** `HashMap`, `Vec`, `Result`, `Option`, `Bytes`, `read_to_string`, `write_file`, `append_file`, `file_exists`, `hash_string`, `eq_string`, `count`, `get`.

**What it should use after stdlib is complete.**
- `String.eq` instead of hand-rolled `str_eq`.
- `String.clone` instead of hand-rolled `clone_string`.
- `Bytes::from_string` or equivalent instead of hand-rolled `string_to_bytes`.
- `String.contains` for substring search in log parsing.

**Changes needed.** Minor -- mostly replacing hand-rolled string helpers with stdlib equivalents that already exist or nearly exist.

**Priority.** Nice-to-have. Already demonstrates good stdlib usage.

### 2.10 `examples/lox/` --- nice-to-have (interpreter workload)

**Current state.** A Lox tree-walk interpreter. Uses a pool-based architecture to avoid String ownership issues (stores characters as `Vec<i32>` with `Vec<StrMeta>` metadata). Uses `Vec`, `Result`, `std.fs`, `std.args`.

**Stdlib types currently used.** `Vec<T>`, `Result`, `read_to_string`, `FsError`, `count`, `get`.

**What it should use after stdlib is complete.**
- The string pool pattern (char-as-i32 pool) could potentially use `Bytes` or a more direct string storage model, but the pool design is an intentional architectural choice to avoid double-free.
- `std.ascii` for character classification in the lexer.
- `std.parse` cursor could replace manual position tracking in the lexer.

**Changes needed.** Moderate. The pool architecture is valid and may be kept. The primary stdlib feedback is: does `Vec<String>` work reliably enough that the pool workaround is unnecessary?

**Priority.** Nice-to-have. Validates runtime/interpreter workload per Phase 3 exit criteria.

### 2.11 `examples/mal/` --- nice-to-have (interpreter workload)

**Current state.** A MAL (Make A Lisp) interpreter. Uses tag-based value representation (`Val { tag, data }`), cons-cell pool, symbol table, environment chains. Uses `Vec` for pools.

**Stdlib types currently used.** `Vec<T>` (implicitly through `vec_new`/`vec_push`/`vec_get`).

**What it should use.** Similar to Lox: `std.ascii` for classification, `std.parse` for input, `Result` for error handling. The value representation is intentionally flat (Copy structs) and should remain so.

**Priority.** Nice-to-have.

### 2.12 `examples/policy_engine/` --- nice-to-have

**Current state.** A capability-aware policy engine. Uses `Vec<Rule>` for rule storage. The `evaluate` function is pure (no capabilities). Rule matching uses manual loop + comparison.

**Stdlib types currently used.** `Vec<T>` (implicitly).

**What it should use.** Minimal changes needed. The example already demonstrates the capability story well. Could use `Result` for the evaluate return if desired, but the current `i32` return (1 = allow, 0 = deny) is clean for the predictable subset.

**Priority.** Nice-to-have.

### 2.13 `examples/vm/` --- nice-to-have

**Current state.** A stack-based bytecode VM. Uses fixed-size arrays for code, stack, and call stack. All execution is a bounded dispatch loop.

**Stdlib types currently used.** None.

**What it should use.** Minimal stdlib relevance. The VM intentionally uses fixed arrays for the predictable subset. No stdlib types needed beyond what exists.

**Priority.** Not a stdlib validation target. The VM validates language features, not stdlib surface.

### 2.14 `examples/toml/` --- nice-to-have

**Current state.** A TOML parser using the same architecture as the JSON parser: string pool, tag-based values, `Vec`-based pools. Hand-rolls character classification and position tracking.

**Stdlib types currently used.** `Vec<T>`, `Result`, `read_to_string`, `FsError`, `count`, `get`.

**What it should use.** Same as JSON: `std.ascii` for classification, `std.parse.Cursor` for text parsing, `Result<Val, ParseError>` for error handling.

**Priority.** Nice-to-have.

### 2.15 Other examples

| Example | Stdlib relevance | Priority |
|---------|-----------------|----------|
| `examples/integrity/` | Already uses `std.fs`, `std.result`, `std.bytes`, `std.map`, `std.sha256`, `std.hex` | Already well-validated |
| `examples/verify/` | Already uses `std.fs`, `std.result`, `std.bytes`, `std.sha256`, `std.hex` | Already well-validated |
| `examples/crypto_verify/` | Pure arithmetic, no stdlib needed | Not a validation target |
| `examples/proof_pressure/` | Pure proof-eligible code, no stdlib needed | Not a validation target |
| `examples/thesis_demo/` | Demonstration wrapper, minimal code | Not a validation target |
| `examples/project/` | Module system demo, no stdlib beyond print | Not a validation target |
| `examples/snippets/` | Language feature demos, not stdlib validation targets | Not a validation target |

---

## 3. Pressure Test Validation

### 3.1 Parser pressure tests --> byte cursor APIs

#### `pressure_parse_json_subset.con`

**Current.** Defines its own `Cursor { data: [u8; 256], len: i32, pos: i32 }`, `is_digit`, `is_whitespace`, `Token`, `TokenList`, `KVPair`, `ParsedObject`. All byte-level with integer character codes.

**Rewrite potential.** HIGH.
- Replace `is_digit`/`is_whitespace` with `std.ascii` functions.
- Replace the manual `Cursor` struct with `std.parse.Cursor` (text cursor) since JSON is text, not binary.
- Replace ad hoc `IntResult { ok: i32, value: i32 }` with `Result<IntValue, ParseError>`.
- Use `?` for error propagation.

**What stays manual.** Token representation, object parsing logic, array storage. These are domain-specific.

#### `pressure_parse_dns_header.con`

**Current.** Hand-rolls `read_u16_be` for `[u8; 64]`, `bit_at`, `bits_range` for flag extraction. Returns `DnsHeader { ..., error: i32 }`.

**Rewrite potential.** HIGH.
- Replace `read_u16_be` with `ByteCursor.read_u16_be`.
- Replace the ad hoc `DnsHeader { error: i32 }` with `Result<DnsHeader, CursorError>`.
- Bitfield extraction (`bit_at`, `bits_range`) stays manual -- no stdlib bitfield support.

**What stays manual.** Bitfield extraction, flag interpretation. This is domain-specific and explicitly out of stdlib scope.

#### `pressure_parse_dns_packet.con` (referenced in STDLIB_TARGET.md validation criteria)

**Rewrite potential.** HIGH. Same as DNS header: ByteCursor for endian reads, Result for error handling.

#### `pressure_parse_http_request.con`

**Current.** Hand-rolls a complete `Cursor { data: [u8; 256], pos: i32, len: i32 }` with `peek`, `advance`, `advance_n`, `remaining` -- 70 lines of cursor infrastructure. Hand-rolls `is_alpha`, `is_digit`, `is_whitespace`.

**Rewrite potential.** HIGH.
- The text cursor (`std.parse.Cursor`) directly replaces the hand-rolled cursor (both operate on text, not binary data).
- `std.ascii` replaces all character classification.

#### `pressure_parse_binary_endian.con`

**Current.** Defines four endian read functions and four endian write functions. Tests round-trip encoding/decoding.

**Rewrite potential.** COMPLETE REPLACEMENT.
- This pressure test is exactly what `std.numeric` ByteCursor and ByteWriter are designed to replace.
- The entire test could be rewritten as a ByteCursor/ByteWriter round-trip validation.

### 3.2 Fixed-capacity pressure tests --> stdlib fixed-capacity helpers

#### `pressure_fixcap_ring_buffer.con`

**Current.** Defines `IntRing` (16 slots) and `MsgRing` (8 slots) with push, pop, peek, full/empty checks. Uses `IntPopResult { value, ring, ok }` for pop results.

**Rewrite potential.** MEDIUM.
- `std.deque.Deque` replaces the hand-rolled ring buffer.
- `Option<PopValue>` replaces `IntPopResult { ok: i32 }`.
- The struct-of-array ring buffer pattern is intentional for the pressure test. A rewrite to Deque validates the stdlib but loses the pressure-test purpose.

**Recommendation.** Keep the original as a pressure test. Write a parallel version using `std.deque` to demonstrate the stdlib alternative. The STDLIB_TARGET.md validation criterion says the ring buffer "can be replaced or simplified using std.deque" -- a parallel version satisfies this.

#### `pressure_fixcap_controller.con`

**Current.** PID controller with fixed-point arithmetic, clamp operations.

**Rewrite potential.** LOW. Uses `std.math.clamp` if it exists; otherwise manual clamping is fine. The pressure is on fixed-point arithmetic, not stdlib gaps.

### 3.3 Ownership pressure tests --> stdlib collection types

#### `pressure_own_ordered_map.con`

**Current.** Sorted array-backed map with binary search, insert-with-shift, delete-with-shift. Uses `[Entry; 16]` fixed array.

**Rewrite potential.** MEDIUM.
- `std.ordered_map` replaces the hand-rolled sorted array.
- The pressure test intentionally demonstrates manual shift operations. A rewrite validates the stdlib but loses the ownership pressure.

**Recommendation.** Similar to ring buffer: keep original, write a parallel stdlib version.

#### `pressure_own_tree.con`

**Current.** Arena-allocated binary tree.

**Rewrite potential.** LOW. Trees are not a stdlib type. The pressure is on arena allocation and ownership patterns.

#### `pressure_own_intrusive_list.con`

**Rewrite potential.** LOW. Intrusive data structures are not stdlib types.

### 3.4 Cleanup pressure tests --> stdlib destroy helpers

#### `pressure_defer_nested.con`

**Current.** Tests defer ordering with two linear resources using `impl Destroy` pattern.

**Rewrite potential.** NONE. This test validates the language's defer semantics, not stdlib types. The `destroy` pattern is the intended pattern.

#### Other defer pressure tests

All defer pressure tests validate language semantics (LIFO ordering, interaction with early return, interaction with `?`). None should be rewritten to use stdlib types -- they are testing the mechanism that stdlib types depend on.

### 3.5 FFI pressure tests

#### `pressure_ffi_libc_wrapper.con`

**Current.** Tests `trusted extern fn` declarations for `abs`, `strlen`, `memcmp`.

**Rewrite potential.** LOW. The pressure is on FFI boundary crossing, not stdlib types. `std.math.abs` could replace the `abs` call, but the point is testing FFI linking.

#### `pressure_ffi_cabi.con`, `pressure_ffi_os_facade.con`

**Rewrite potential.** LOW. FFI boundary tests, not stdlib validation targets.

### 3.6 Summary Table

| Pressure test | Stdlib APIs to use | Rewrite potential | Priority |
|--------------|-------------------|-------------------|----------|
| `pressure_parse_json_subset` | `std.ascii`, `std.parse.Cursor`, `Result` | HIGH | Medium |
| `pressure_parse_dns_header` | `ByteCursor`, `Result` | HIGH | High (STDLIB_TARGET validation) |
| `pressure_parse_dns_packet` | `ByteCursor`, `Result` | HIGH | High (STDLIB_TARGET validation) |
| `pressure_parse_http_request` | `std.parse.Cursor`, `std.ascii` | HIGH | Medium |
| `pressure_parse_binary_endian` | `ByteCursor`, `ByteWriter` | COMPLETE | High |
| `pressure_fixcap_ring_buffer` | `std.deque`, `Option` | MEDIUM | Medium |
| `pressure_fixcap_controller` | `std.math.clamp` | LOW | Low |
| `pressure_own_ordered_map` | `std.ordered_map` | MEDIUM | Low |
| `pressure_own_tree` | None | LOW | Not a target |
| `pressure_own_intrusive_list` | None | LOW | Not a target |
| `pressure_defer_nested` | None | NONE | Not a target |
| `pressure_ffi_libc_wrapper` | `std.math.abs` | LOW | Not a target |

---

## 4. Gap Detection

If an example CANNOT be rewritten with stdlib, that is a stdlib gap. Each gap is documented below.

### Gap 1: No line-splitting / line-iteration API

**Affected programs.** `examples/grep/`, `examples/kvstore/`, `examples/toml/`, `examples/json/`.

**Current workaround.** Every line-oriented program hand-rolls the same pattern: scan `string_char_at` for newline (code 10), extract substring with `string_slice(content, line_start, pos)`, advance `line_start`.

**Stdlib gap.** No `String.lines()` iterator, no `String.split(delimiter)`, no `String.split_lines()` function. This requires either closures (excluded) or a callback/function-pointer-based API.

**Recommendation.** Add a `split_lines` function that returns a `Vec<Text>` or provides a cursor-based line reader. Alternatively, document the manual scan pattern as the idiomatic approach and defer `split` to a future release.

**Severity.** Medium. The manual pattern works but adds 10-15 lines of boilerplate to every line-oriented program.

### Gap 2: No stream-write-string for network I/O

**Affected programs.** `examples/http/`.

**Current workaround.** `send_string` copies string bytes into a malloc'd buffer character-by-character, then calls `stream.write_all`.

**Stdlib gap.** `TcpStream.write_all` takes `*const u8` and length, but there is no `TcpStream.write_string(&String)` convenience method.

**Recommendation.** Add `TcpStream.write_string(s: &String)` to `std.net`. Internally it accesses `s.ptr` and `s.len` in a trusted wrapper.

**Severity.** Low. Only affects network I/O programs. The workaround is functional.

### Gap 3: No Bytes-to-String or String-to-Bytes zero-copy conversion

**Affected programs.** `examples/kvstore/` (hand-rolls `string_to_bytes`), `examples/http/` (hand-rolls `buf_to_string`).

**Current workaround.** Character-by-character copy between String and Bytes.

**Stdlib gap.** `Bytes.to_string` exists but `String.to_bytes` does not. Zero-copy conversion in both directions is missing.

**Recommendation.** Add `String.to_bytes() -> Bytes` (ownership transfer, zero-copy). Documented in STRING_TEXT_CONTRACT.md section 3 as a known gap.

**Severity.** Medium. Affects any program that needs to write string content to files or network as bytes.

### Gap 4: No string equality via == operator

**Affected programs.** `examples/kvstore/` (hand-rolls `str_eq`), `examples/http/` (hand-rolls `is_get` with byte comparison).

**Current workaround.** Manual character-by-character comparison or using `String.eq` method.

**Stdlib gap.** `String.eq` method exists but is not always discoverable. The operator `==` does not work on strings (no operator overloading). Several programs reimplement `str_eq` despite `String.eq` existing.

**Recommendation.** This is a documentation/discoverability gap, not a missing API. Ensure `String.eq` is prominently documented and imported examples use it consistently.

**Severity.** Low. The API exists; programs just do not use it yet.

### Gap 5: No stdlib bitfield extraction

**Affected programs.** `pressure_parse_dns_header.con`, any protocol parser with flags packed into bytes.

**Current workaround.** Hand-rolled `bit_at` and `bits_range` functions using division and modulo.

**Stdlib gap.** No `std.numeric.bit_at`, `std.numeric.bits_range`, or bitfield extraction helpers. This is explicitly documented as out of scope in BYTE_CURSOR_API.md section 12.

**Recommendation.** Accept as a permanent manual pattern. Bitfield extraction via shift-and-mask is explicit and portable. Do not add stdlib helpers for this.

**Severity.** None (intentional exclusion).

### Gap 6: `std.numeric` module does not exist yet

**Affected programs.** All binary parser examples and pressure tests.

**Current workaround.** Hand-rolled endian read/write functions in every program.

**Stdlib gap.** Resolved. `std.numeric` with `ByteCursor`, `ByteWriter`, and standalone endian functions is implemented (891d561, 9 tests pass). Checked narrowing deferred to Tier 2.

**Status.** DONE. `examples/packet/` rewrite onto ByteCursor is the first validation target.

### Gap 7: `Result.map_err` not yet implemented

**Affected programs.** `examples/service_errors/` (Phase 3 exit criterion).

**Current workaround.** 9-line match-and-convert blocks per pipeline stage.

**Stdlib gap.** `Result.map_err` is Tier 2 in ERROR_HANDLING_DESIGN.md, requiring function-pointer-in-generic validation.

**Recommendation.** Validate function-pointer-in-generic methods and implement `map_err`. If this is not achievable before Phase 3 exit, the fallback is: use `Result<T, E>` with `?` where error types match, and explicit match blocks where they differ. The exit criterion is satisfied if custom result enums are eliminated even without `map_err`.

**Severity.** HIGH for full ergonomic validation. MEDIUM for the Phase 3 exit criterion (which requires stdlib types, not necessarily `map_err`).

### Gap 8: `Result.unwrap_or` and `Option.unwrap_or` not yet implemented

**Affected programs.** All pressure tests using ad hoc `ok: i32` result structs.

**Stdlib gap.** `unwrap_or` is Tier 1 in ERROR_HANDLING_DESIGN.md (no function pointer needed) but not yet implemented.

**Recommendation.** Implement as the first Result/Option helper. Low compiler risk.

**Severity.** MEDIUM. Programs can use match as a workaround, but the ergonomic improvement is significant.

---

## 5. Validation Sequence

The order below maximizes feedback on the stdlib surface by tackling the most constrained exit criteria first, then expanding to broader validation.

### Wave 1: Phase 3 Exit Gate (MUST complete before Phase 3 closes)

**Step 1: `examples/parse_validate/` --> stdlib Result**

Difficulty: LOW. The rewrite is mechanical: replace `ParseResult` with `Result<Header, ParseError>`, adopt `?`. No new stdlib APIs needed beyond what exists (`Result`, `?`). Validates that `Result<T, E>` with Copy payloads works end-to-end.

Validates: Result generic instantiation, `?` operator with user error enums, match on `Result::Ok`/`Result::Err`.

**Step 2: `examples/service_errors/` --> stdlib Result + map_err (or fallback)**

Difficulty: MEDIUM. If `map_err` works: mechanical rewrite, three custom enums eliminated, pipeline collapses to one function. If `map_err` does not work: keep explicit match blocks but use `Result<T, E>` instead of custom result enums.

Validates: Result with multiple error types, `map_err` + `?` chaining (or confirms the function-pointer-in-generic gap), error type conversion patterns.

**Step 3: `examples/grep/` --> stdlib string/ascii APIs**

Difficulty: LOW. Replace magic character codes with `std.ascii` calls and `String` method calls. No architectural change.

Validates: `std.ascii` usability in real text processing, `String.to_lower`/`String.contains`/`String.starts_with` in a real program, discovery of missing string helpers.

**Step 4: `examples/fixed_capacity/` --> checked Result type**

Difficulty: LOW. Replace `ValidateResult { error: i32 }` with `Result<ValidateOk, ValidateError>`. Ring buffer stays as-is.

Validates: Result in the predictable subset (no allocation), checked indexing surface with fixed-size arrays.

### Wave 2: Binary Parsing Validation (validates std.numeric)

**Step 5: Implement `std.numeric` core (ByteCursor, endian reads)**

Prerequisite for all binary parser rewrites. Steps 1-3 of the BYTE_CURSOR_API.md implementation sequence.

**Step 6: `pressure_parse_dns_header.con` --> ByteCursor**

Difficulty: MEDIUM. Replace `read_u16_be` with `ByteCursor.read_u16_be`. Replace `DnsHeader { error }` with `Result<DnsHeader, CursorError>`.

Validates: ByteCursor with fixed-size stack buffer, endian read correctness, Result integration with cursor errors.

**Step 7: `examples/packet/` --> ByteCursor + ByteWriter**

Difficulty: MEDIUM. Replace all hand-rolled byte read/write with ByteCursor/ByteWriter. The trusted boundary shrinks (cursor construction is the only trust point).

Validates: ByteWriter for serialization, ByteCursor/ByteWriter round-trip, trust boundary reduction.

**Step 8: `pressure_parse_binary_endian.con` --> ByteCursor + ByteWriter**

Difficulty: LOW (after step 5). Complete replacement of hand-rolled endian functions.

Validates: Full endian API surface, round-trip encoding/decoding with both endiannesses.

### Wave 3: Collection and Ergonomic Validation

**Step 9: `pressure_fixcap_ring_buffer.con` --> parallel Deque version**

Write a parallel version using `std.deque.Deque`. Keep original for pressure-test purposes.

Validates: Deque API usability, comparison with hand-rolled ring buffer.

**Step 10: `examples/json/` --> std.ascii + Result**

Difficulty: HIGH (large program). Replace character classification, adopt Result for error handling.

Validates: Text parsing with stdlib APIs at scale, discovery of text cursor gaps.

**Step 11: `examples/kvstore/` --> replace hand-rolled string helpers**

Difficulty: LOW. Replace `str_eq`, `clone_string`, `string_to_bytes` with stdlib equivalents.

Validates: String/Bytes conversion, discoverability of existing stdlib APIs.

### Wave 4: Interpreter Workload Validation

**Step 12: `examples/lox/` or `examples/mal/` --> stdlib collection surface**

Evaluate whether `Vec<String>` is reliable enough to replace the string pool workaround. If yes, simplify the architecture. If not, document why the pool pattern is necessary.

Validates: Collection reliability for interpreter workloads, Phase 3 exit criterion for "one interpreter/runtime-heavy medium workload."

---

## Summary

### Phase 3 Exit Requirements

| Example | Required stdlib change | Blocking gaps | Status |
|---------|----------------------|---------------|--------|
| `examples/parse_validate/` | Replace `ParseResult` with `Result<Header, ParseError>`, use the canonical builtin surface | None | Done |
| `examples/service_errors/` | Replace 4 custom result enums with `Result<T, E>`, keep explicit conversion matches until helper coverage grows | `map_err` still pending as a preferred cleanup path | Done |
| `examples/grep/` | Use `std.ascii`, `String` methods instead of magic constants | None | Not started |
| `examples/fixed_capacity/` | Replace `ValidateResult` with `Result`, keep ring buffer | None | Not started |
| One string-heavy workload | `grep` or `policy_engine` validates formatting/text APIs | None | `grep` covers this |
| One interpreter workload | `lox` or `mal` validates collection/runtime surface | None | Not started |

### Critical Path

1. Implement `Result.unwrap_or` and `Option.unwrap_or` (Tier 1 helpers).
2. Validate function-pointer-in-generic for `Result.map_err` (Tier 2 helper).
3. Rewrite `parse_validate` and `service_errors` examples (Steps 1-2).
4. Rewrite `grep` with `std.ascii` and `String` methods (Step 3).
5. Implement `std.numeric` ByteCursor (enables Wave 2).
6. Rewrite at least one binary parser pressure test (Step 6 or 7).
7. Validate one interpreter workload (Step 12).
