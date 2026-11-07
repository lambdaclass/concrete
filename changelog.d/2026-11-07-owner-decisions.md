### Owner decisions: bounds, showcases, final slices, tools and speed tracking (2026-11-07)

`docs/decisions.md` records the owner's verbatim answers and the approved recommendations, with the
schedule applied in `ROADMAP.md` and `docs/proposals/EXAMPLES.md`:

- **Step and time bounds:** the step-counting model starts with loops at slice 4 and extends through
  calls at 5; step-bound proofs arrive at 8. Bounds count abstract operations in input and pattern
  size; wall-clock claims need separate backend evidence.
- **Outside-reader order:** Codex, agy and Kimi read first, unclear points are resolved before the
  owner's formal read, real design questions still go to the owner, and Kimi's absence is recorded.
- **Struct invariants:** the spec definition still precedes the freeze, but source
  `#[invariant(...)]` moves from slice 9 to slice 17 with modules and field privacy.
- **All seven showcases:** grep at 16 → 18 → 22; QOI at 19 → 21 and PNG at 21–22; EVM at
  15 → 21 → 23; the key-value store at 23 with RocksDB comparison at 25; WebAssembly at
  21 → 23 → 26; crypto at 7 → 15 → 18, with the encrypted tunnel after 28; and the sequential web
  server at 27, with its concurrent version after 28. Each states functional, resource, authority
  and backend claims separately.
- **Final slice order:** slice 27 keeps httpget, tcpserve and the sequential web server; slice 28 is
  concurrency; ethlambda and the encrypted tunnel follow it, the tunnel after the randomness/nonce
  audit. The concurrency model itself remains undecided.
- **Developer tools:** the test runner lands at slice 8 and extends to modules at 17; the formatter
  lands at 17; the LSP comes later.
- **Speed tracking:** flagship runtime from slice 2 and compiler wall time, per-stage time and
  memory from slice 0, recorded by exact SHA, backend and target and report-only until noise is
  understood.
- **Cleanup:** the Codex/Kimi documentation and tooling cleanup list is approved and recorded in the
  decision row. This change only records that approval; it does not perform the cleanup.
