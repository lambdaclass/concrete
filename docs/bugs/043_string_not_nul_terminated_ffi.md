# Bug 043: hosted std passed non-NUL-terminated String buffers to C string APIs

**Status:** Fixed (2026-07-18)
**Fixed in:** std — `String::to_cstr()` (trusted; allocates len+1, copies,
NUL-terminates; caller deallocs) + every C-string FFI site converted:
`std.env` get/set/unset, `std.fs` fopen ×7 (via `fopen_cstr`), `std.io`
TextFile open/create (via `fopen_cstr_io`), `std.process` spawn/execvp,
`std.net` inet_pton. Sites passing `ptr + len` pairs (write/send/memcmp)
are NOT affected — only NUL-delimited C string parameters.
**Regression test:** check_envcfg.sh env-override leg (the failing CI leg)
+ check_std_compiled_coverage.sh env/fs/io/net legs — all exercise
slice-built (exact-capacity) Strings through the FFI on Linux CI.
**Discovered:** 2026-07-18, envcfg gate red on ubuntu CI only
(`CACHE=on` override ignored) after envcfg switched key construction to
`Bytes::slice → to_string`.

## Mechanism

A Concrete `String` is `{ptr, len, cap}` with NO NUL terminator
guaranteed. `std.env.get` passed `name.ptr` directly to `getenv`, which
reads until a zero byte — beyond `len`. Whether the lookup works depends
on what happens to sit after the buffer:

- String literals: codegen emits a trailing `\00` → always worked.
- `push_char`-grown buffers: growth slack usually zeroed on macOS → worked
  by luck (this is why workload 5 passed everywhere at first).
- `Bytes::slice(..).to_string()`: EXACT-capacity buffer, no slack →
  `getenv("CACHE<garbage>")` on Linux glibc → miss; macOS malloc slack
  still happened to be zero → gate green locally, red on ubuntu.

Nondeterministic-by-allocator, cross-platform-divergent, and invisible to
the interpreter (interp env access doesn't go through the raw pointer).

## Signature fallout

`env.set`/`env.unset` gained `Alloc`; `TextFile::open` gained `Alloc`
(create already had it); `process.spawn` gained `Alloc`. These are frozen-
surface signature changes justified as bug fixes (the documented semantics
— "look up THIS name" — were not what the implementation did). Manifest
TSV regenerated.

## Class rule going forward

Any `&String` crossing to a C API that expects a NUL-terminated string
goes through `to_cstr` inside the trusted boundary. Passing `.ptr`
directly is only valid together with an explicit length parameter.
