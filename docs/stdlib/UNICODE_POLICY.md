# Unicode Policy (Phase 7 items 5–6, v1 — NORMATIVE)

Status: normative for all stdlib text APIs. Companion: `docs/VALIDATED_WRAPPERS.md`,
`docs/ERROR_CONVENTIONS.md`, the Bytes/Text boundary gate
(`check_bytes_text_boundary.sh`) and the unicode-policy section within it.

## The v1 policy

1. **`String` / `Text` are valid UTF-8, always.** The only crossings from raw
   bytes are CHECKED (`Bytes::to_string -> Option`, `Text::try_from_raw ->
   Option`, `try_new`) or carry the `_unchecked` name (a caller-proved
   obligation per the 2a manifest rule: `to_string_unchecked`,
   `from_raw_unchecked`). `std.args.get` validates argv (OS bytes) before it
   becomes a `String`.
2. **`Bytes` is raw.** No text semantics on `Bytes` — `cmp` is byte-lexicographic,
   never collation.
3. **ASCII helpers are explicitly ASCII-only.** `std.ascii` predicates are false
   for non-ASCII; `to_lower`/`to_upper` transform ONLY `A–Z`/`a–z` and pass all
   other bytes through unchanged. They are byte-level tools, not Unicode case
   mapping.
4. **No normalization in v1.** No NFC/NFD/NFKC/NFKD anywhere; equality is
   byte-equality of the UTF-8 encoding.
5. **No case folding in v1.** No Unicode-aware `to_upper`/`to_lower`/casefold.
6. **No display-width semantics in v1.** No grapheme clusters, no wcwidth,
   no terminal-column arithmetic.
7. **Scalar iteration: deferred.** Not shipped in v1; lands only when a
   workload pulls it (a `chars`-style scalar walk is the first candidate).

Items 4–7 are deliberate NON-goals for v1, not gaps: each imports large tables
and locale questions that the evidence story would then have to carry. When a
workload pulls one, it arrives as its own module with its own manifest facts.

## Long-term note (recorded, not v1)

`std.args.get` returning `""` for non-UTF-8 argv is safe but lossy; the
complete API is a future `std.args.get_bytes` (raw OS-argument accessor
returning `Bytes`) for callers that must handle non-UTF-8 argv. Tracked in
ROADMAP Phase 7 #5.
