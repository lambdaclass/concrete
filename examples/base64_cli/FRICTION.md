# base64_cli — friction log (Phase 7, workload 1)

The first workload program: build first, record every friction point, implement
only what the workload actually pulls. Rules of engagement in ROADMAP Phase 7.

Verified against system `base64`: fixed vectors ("", "a", "ab", "abc", "abcd",
"hello world", multibyte UTF-8) + 40 random round-trips, encode and decode,
zero mismatches. Invalid input (`!!!!`, bad length) → recoverable error message
+ rc=1 per ERROR_CONVENTIONS.

## Friction found

1. **Standalone files cannot import std** (`concrete file.con` rejects
   `import std.args...`; project mode required). Fine once known, but the
   error does not say "use a project"; a one-line hint would save the next
   person the detour. → CLI diagnostics polish, small.

2. **`String.len` field is private cross-package; `len()` method required.**
   Error was clear enough (E-level, named the field). No change requested —
   this is the intended encapsulation — but noting the first-contact stumble.

3. **BUG 031 (compiler, fixed en route):** decode segfaulted on every input
   while encode worked. A non-Copy local borrowed in more than one branch
   (method-call autoborrow counts) read an UNINITIALIZED promotion alloca on
   the branch lowered second. Root-caused to lazy `addrOfLocal` promotion;
   fixed at all three sites (if/else, value-if, match) with dominating
   pre-promotion; 3 regression fixtures; full trust battery green.
   See `docs/bugs/031_branch_lazy_promotion_uninit.md`. **This is the workload
   paying rent**: the shape "first borrow of a local inside command-dispatch
   branches" is the CLI-program shape, and no fixture/fuzzer had it.

4. **`main`'s return value was echoed to stdout and the process exited 0.**
   A CLI tool could neither keep its stdout clean when piped nor signal
   failure to the shell. → pulled and SHIPPED (stage 1): compiled `main`'s
   return IS the process exit code (8-bit masked), stdout untouched; this
   tool now pipes cleanly and exits 1 on invalid input. Full decision record
   and the stage-2 end state (`fn main() -> u8 | Unit`) in
   `docs/MAIN_EXIT_MODEL.md`.

5. **base64 inline was ~70 lines and easy** — the alphabet/padding logic is
   self-contained. The friction was NOT the encoding math; it was everything
   around it (3, 4). A `std.base64` is justified mostly so every CLI/proto
   workload doesn't re-derive the padding edge cases (len%4, `=` handling,
   invalid-char rejection) — encode_len/decode paths want the overflow-guarded
   style parse.con already uses.

6. **No byte-level argv.** `args.get` returns validated String (right default);
   for a hypothetical `base64_cli decode-file <path>` taking raw OS bytes the
   planned `args.get_bytes` (already a ROADMAP note) is the answer. Not pulled
   by this workload — text argv sufficed.

7. **`String → Bytes` needed a hand loop** (`string_to_bytes` here, byte-wise
   push). Crossing from validated text down to bytes is policy-clean
   (UNICODE_POLICY: String→Bytes is always safe, it's the reverse that
   validates) but had no std helper. → pulled and SHIPPED as
   `Bytes::from_string(&String)` (alloc-carrying, infallible); this program
   now uses it.

## Pull list (ranked)

| # | Pull | Evidence | Size |
|---|------|----------|------|
| 1 | main return = exit code (not stdout echo) | friction 4 — blocks ALL CLI workloads | **SHIPPED stage 1** (docs/MAIN_EXIT_MODEL.md; stage 2 = `u8\|Unit` main, ROADMAP P7 #3) |
| 2 | `Bytes::from_string` / `String.to_bytes` | friction 7 — every text↔bytes program | std, small — **SHIPPED** (bytes.con, this workload now uses it) |
| 3 | `std.base64` (encode/decode, Option-failing decode) | friction 5 — proto/CLI recurrence | std, small-medium |
| 4 | standalone-import diagnostic hint | friction 1 — first-contact UX | CLI, tiny |

Not pulled: scanner/parse helpers (b64_val's compare-chain was fine), Writer
polish (console_writer + write + write_str covered it), error formatting
(static messages sufficed), args.get_bytes (no byte-argv need here).
