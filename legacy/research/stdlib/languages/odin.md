# Odin Core Library Packet

Status: research

Source pointers: Odin overview and core/vendor package docs: collections,
strings, fmt, os, path/filepath, time, math, encoding, crypto/hash, random,
testing, runtime, and system-oriented helpers.

## What Odin Has

- Practical systems-programming core packages.
- Arrays/slices/dynamic arrays and maps.
- String and formatting helpers.
- OS/filesystem/process/time helpers.
- Math and numeric helpers.
- Encoding/decoding and data-format packages.
- Hashing/random/crypto-adjacent helpers.
- Testing and runtime utilities.

## What Concrete Should Copy

1. **Practical small modules.**
   Odin's stdlib is useful because common systems tasks do not require a large
   dependency ecosystem. Concrete should have enough bytes/path/fmt/parse/io
   coverage for small tools.

2. **Plain API naming.**
   Avoid trait-heavy or macro-heavy APIs. Concrete should use ordinary function
   names and modules.

3. **Low-level ergonomics.**
   Byte buffers, fixed buffers, endian helpers, simple parsing, and path tools
   should be smooth.

4. **Testing/runtime utilities.**
   Concrete should make test/oracle helpers a stdlib surface, not just scripts.

## What Concrete Should Not Copy

- Broad runtime convenience that hides allocation or authority.
- OS wrappers without capability facts.
- Concurrency/runtime surfaces before the model exists.

## Missing Concrete Items This Pressures

- More explicit `std.os`/hosted boundary classification, even if modules remain
  `std.fs`, `std.process`, `std.env`, `std.net`.
- `std.terminal` / console behavior.
- Encoding helpers beyond hex/base64: varint, possibly base32.
- Practical examples for parser/CLI/file tools.

## Concrete Classification

- Copy now: practical low-level helpers and plain APIs.
- Stdlib later: terminal, encoding extras, OS boundary helpers.
- Package later: broad data formats and platform-specific wrappers.

