# Hare Stdlib Packet

Status: research (API-shape claims verified 2026-07-12 against harelang.org docs/tutorials + the stdlib mandate)

Source pointers: harelang.org tutorials (introduction, stdlib) and
docs.harelang.org (`io`, `bufio`, `strings`); the Standard Library Mandate
(`docs/stdlib.md` in the Hare tree); DeVault's 2021 stdlib-development post.

Why this packet matters: Hare is the minimalist-systems reference — explicit
allocation, a small dependency-free stdlib, C-replacement energy, and (uniquely)
a **written, closed stdlib mandate** that doubles as an inclusion gate.

## What Hare Has

- ~39 flat, lowercase, single-word modules (`io`, `bufio`, `fmt`, `strings`,
  `os`, `fs`, `net`, `sort`, `strconv`, ...); `use` + fully-qualified `::`
  access, no wildcard imports. Shallow nesting only for genuine sub-domains
  (`crypto::sha256`, `hare::lex`/`hare::parse` — the compiler's own front end
  ships as stdlib modules).
- Explicit allocation: stack / static / dynamic (`alloc(x)!` / `free`); ownership
  is a per-symbol documented borrow-vs-own contract (`toutf8` borrows; `dup`,
  `concat` say "caller must free"). Pervasive `_buf` / non-`_buf` API pairs
  (caller-supplied slice vs internal allocation).
- Failure lives in the return type as a flat tagged union:
  `create(...) (io::file | fs::error)`, `read(...) (size | EOF | error)`; `?`
  propagates, `!` asserts-or-aborts, `match` is exhaustive; no `Ok`/`Err` wrapper.
- A **closed 5-item stdlib mandate** + per-symbol quality bar (concise interface,
  complete over a defined subset, fully documented, tested); everything outside
  it lives in external `hare-*` libraries. The language itself is declared
  feature-complete and then frozen.

## What Concrete Should Copy

1. **`_buf` / non-`_buf` API pairs.** Ship an allocating variant and a
   caller-supplied-buffer variant of the same operation so the caller picks
   static vs dynamic. Directly serves no-hidden-allocation.
2. **The closed, written stdlib mandate as an inclusion gate.** Sharper than
   Concrete's current "every API justifies its existence": the question becomes
   "is it one of the N?" Feeds `STDLIB_TARGET.md` and the surface-manifest gate
   (Phase 7 item 2a).
3. **The `io::handle` + `bufio` scanner stack.** One narrow 3-method interface
   (`read`/`write`/`close`) that files, sockets, and in-memory buffers all
   satisfy, with a buffered scanner (`scan_line`/`scan_rune`/`scan_bytes`)
   layered over a caller-controlled buffer. Parsers depend only on the handle.
   Concretely validates "build `std.parse`/cursor first," and Hare eats its own
   dog food (`hare::lex`/`parse` are built on it).
4. **Failure-in-return-type as a flat union.** Concrete's `Result`/`Option`
   already do this; Hare confirms the flat-union ergonomics — multiple error
   kinds coexist without nesting, `done`/`EOF` plays the `None` role.

## What Concrete Should Not Copy

- `alloc` / `free` as bare language keywords — Concrete routes allocation through
  the `with(Alloc)` capability, not a global keyword.
- Ownership as documentation-only contract — Concrete makes it a linear type rule.
- "Freeze the language, grow only the stdlib" as literal policy (Concrete is
  pre-1.0); the *spirit* (library growth over language growth) is already Concrete's.

## Missing Concrete Items This Pressures

- A single narrow `io::handle`-style stream interface unifying file/socket/memory
  before hosted IO grows (Phase 7 #3, Reader/Writer).
- A caller-buffered scanner as the canonical parse primitive (Phase 7 #6 / `std.parse`).
- A written closed stdlib mandate (Phase 7 #1 / `STDLIB_TARGET.md`).

## Concrete Classification

- Copy now: `_buf`/non-`_buf` pairs, closed-mandate inclusion gate, handle+scanner stack shape.
- Confirms decided: failure-in-return-type flat union.
- Reject: alloc/free keywords, documentation-only ownership.
