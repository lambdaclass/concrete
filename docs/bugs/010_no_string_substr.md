# Bug 010: No String Substring Extraction Path

**Status:** Fixed
**Discovered:** 2026-03-15
**Discovered in:** `examples/mal/main.con`

## Symptom

Concrete has string inspection helpers such as `string_length`, `string_char_at`, and `string_concat`, but no substring extraction path such as:

```con
string_substr(s: &String, start: Int, len: Int) -> String
```

This became a real blocker while implementing MAL's reader/symbol handling. Parser code naturally wants to slice the source string into token substrings, intern them, and move on.

## Current Workaround

Avoid constructing substrings entirely:

- compute symbol hashes directly from `(start, end)` source positions
- intern by `(hash, length)` instead of by a real substring value

This works, but it distorts normal parser structure and makes otherwise straightforward code harder to read.

## Impact

- parser/reader code cannot express normal substring-oriented logic directly
- Phase H interpreter/runtime workloads become more contorted than they should be
- pushes programs toward custom slice-hash logic instead of ordinary string processing

## Fix

Two distinct operations now exist:

- `string_slice(s: &String, start: Int, end: Int) -> String` — extracts chars from index `start` to `end` (exclusive). Both indices are clamped to `[0, length]`.
- `string_substr(s: &String, start: Int, len: Int) -> String` — extracts `len` chars starting at index `start`. Implemented as `string_slice(s, start, start + len)`.

`string_substr` has its own intrinsic ID (`stringSubstr`) and its own LLVM function that computes `end = start + len` before delegating to `string_slice`. The two operations have genuinely different semantics — `string_substr(s, 3, 2)` extracts 2 characters starting at position 3, while `string_slice(s, 3, 2)` returns empty (end < start).
