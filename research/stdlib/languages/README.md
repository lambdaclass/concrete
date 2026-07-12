# Stdlib Language Packets

Status: research

This directory breaks the stdlib comparison down by language. Each packet asks:

1. What does this language/library family include?
2. What should Concrete copy?
3. What should Concrete explicitly defer or reject?
4. Which roadmap/research items does it pressure?

Use these files before expanding Phase 7 or the package-later stdlib surface.
The cross-language synthesis remains in
[../stdlib-comparative-inventory.md](../stdlib-comparative-inventory.md).

Packets:

- [zig.md](zig.md)
- [rust.md](rust.md)
- [austral.md](austral.md) — closest structural relative (linear + capabilities + no GC)
- [hare.md](hare.md) — minimalist systems + closed stdlib mandate
- [spark.md](spark.md) — proof-friendly pure-core discipline
- [cpp.md](cpp.md)
- [odin.md](odin.md)
- [clojure.md](clojure.md)
- [elixir.md](elixir.md)

The austral/hare/spark packets (added 2026-07-12) carry verified, cited
API-shape findings; zig/rust have a verified-findings section appended to the
existing curation. Other useful comparisons are summarized in the main
inventory: Go, Roc, Gleam, and OCaml. Add dedicated packets for them if a
concrete workload starts pulling that ecosystem's shape.

