# Checked Indexing And Slice Views

**Status:** Open

This note covers one narrow but high-leverage problem: Concrete needs a sharper story for checked access and borrowed contiguous views before the stdlib and predictable-core claims freeze.

## Problem

Concrete currently has indexing syntax and some checked library access patterns, but the overall surface is still too blurry:

- array indexing is easy to write but still carries an out-of-bounds UB gap today
- parser and fixed-capacity code still has to choose between hand-rolled guards, ad hoc library helpers, and trusted/raw-pointer fallbacks
- owned buffers and borrowed views do not yet present one obvious checked/unchecked vocabulary

That is a bad fit for a language that wants predictable code and explicit trust boundaries.

## Design Goals

1. **The safe path must be the obvious path.**
   A reviewer should not have to guess whether `get`-style access is checked or unchecked.

2. **The unchecked path must stay available and honest.**
   Low-level code still needs fast paths, but they should be named as such.

3. **Arrays, slices, bytes, and similar buffer-like types should feel like one family.**
   The same mental model should apply across owned and borrowed contiguous storage.

4. **Borrowed views must stay allocation-free and ownership-honest.**
   Slicing should not hide allocation, copying, or lifetime tricks.

5. **Reports and proofs should be able to distinguish checked access from unchecked access.**
   Evidence claims are weaker if both look the same in the surface language.

## Direction

The likely first-release direction is:

- checked access uses explicit checked APIs such as `get`, `set`, `subslice`, or similarly direct names
- unchecked access uses explicit names such as `get_unchecked`, `set_unchecked`, or `subslice_unchecked`
- borrowed slice/view types are first-class stdlib types with pointer+length semantics and obvious ownership rules
- parser-facing APIs should compose around checked byte-cursor and slice operations rather than raw indexing

One surface question remains open: whether `arr[i]` survives as the checked surface, or whether the language should push users toward explicit checked methods and reserve direct indexing for a narrower role. The important point is not the exact syntax. The important point is that the checked path must be the default public story, and the unchecked path must be named.

## Non-Goals

- panic-on-OOB indexing as the default model
- hidden bounds checks that look identical to raw access but have materially different cost/error behavior
- implicit slicing copies
- Python-style rich slicing syntax if it complicates the grammar or borrow model

## Why This Fits Concrete

This is not convenience sugar. It reduces trusted pressure, lowers audit cost, and makes the predictable subset more honest:

- fewer tiny trusted wrappers for byte reads
- clearer failure and UB boundaries
- better parser and fixed-capacity examples
- a cleaner bridge into future proof and report surfaces

## Roadmap Fit

This belongs before the first stdlib/syntax freeze. It directly affects:

- predictable-core credibility
- parser/decoder ergonomics
- stdlib slice and byte-cursor design
- memory/UB reporting
