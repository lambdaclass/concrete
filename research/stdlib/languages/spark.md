# SPARK / Ada Stdlib Packet

Status: research (verified 2026-07-12 against SPARK UG/RM 27.0w + AdaCore/SPARKlib source)

Source pointers: SPARK User's Guide + Reference Manual
(docs.adacore.com/spark2014-docs), the `SPARK.Containers.Formal.*` source
(github.com/AdaCore/SPARKlib), learn.adacore.com.

Why this packet matters: SPARK is the mature high-assurance stdlib and the model
for Concrete's **proof-friendly pure core** (`option`/`result`/`bytes`/`numeric`).
It answers the one question the other packets don't: what makes a stdlib module
mechanically provable.

## What SPARK Has

- 12 **formal containers** (`SPARK.Containers.Formal.Vectors` / `Ordered_Maps` /
  ...) provable where the normal Ada containers are not, because: bounded storage
  (index into a fixed array, no hidden allocation), cursors carry no container
  reference (no aliasing), a narrowed API (no callback-taking `Update_Element`),
  and — the key — a **ghost mathematical model** (`Formal_Model`: sequence/map)
  that contracts are written against.
- Contracts as aspects: `Pre` / `Post` / `Contract_Cases` (proven disjoint +
  complete = totality) / `Global` / `Depends`. Real: `Append` has
  `Pre => Length < Capacity` (overflow is not even expressible), `Post` frames the
  new length and preservation of prior elements.
- Effect discipline: a side-effect-free function has `Global => null`, no
  out-parameters, no exceptions, and always terminates — a hard checked rule.
- `SPARK_Mode On/Off` partitions provable from trusted: a contracted (`On`) spec
  may have an unchecked (`Off`) body; the prover *assumes* the body honors the
  spec; the boundary is one-way.

## What Concrete Should Copy

1. **Model-not-representation contracts (the keystone).** Give each pure-core
   type an abstract mathematical model and write every contract against the
   model, never the layout — proofs become representation-independent and stable.
   For Concrete: `Option` = 0-or-1 model, `Result` = tagged-union model,
   `bytes` = finite `u8` sequence + length, `numeric` = mathematical integers +
   explicit bound predicates.
2. **Bound everything so allocation/overflow isn't expressible.**
   `Pre => Length < Capacity` beats a runtime check — the failure mode is proven
   absent. Feeds the fixed-capacity core.
3. **`Global => null` as the pure/no-effect marker** — the analogue of Concrete's
   "no capability ⇒ no IO." Any effect must surface as a declared global/capability.
4. **The trusted-boundary seam.** A contracted spec over an unchecked body
   (`SPARK_Mode Off`) = Concrete's `trusted`/`Unsafe`, where the proof-class
   degrades to `trusted-boundary` and the obligation becomes manual review. Feeds
   the surface-manifest proof-class column (Phase 7 item 2a) and the
   certificate-chain contract (Phase 6B item 14a).
5. **`Contract_Cases` (disjoint + complete)** as a totality-for-free pattern for
   pure-core partial functions.

## What Concrete Should Not Copy

- Ada surface syntax and aspect-heavy annotation density on every subprogram —
  Concrete's proof facts should derive from types + capabilities, not per-function
  aspect blocks.
- The full 12-container matrix now — take the *shape* (bounded, model-annotated)
  for the pure core, not the whole set.
- Cursors at all — Concrete already decided against them (see `iterators.md`).

## Missing Concrete Items This Pressures

- A mathematical-model convention for pure-core types (feeds Phase 7 #10
  `formal_vec`/`formal_map` and the manifest proof-class column).
- A bound-as-precondition idiom for fixed-capacity collections.
- Explicit degradation of proof-class at the trusted/Unsafe seam.

## Concrete Classification

- Copy now: model-not-representation contracts, `Global => null`-style purity
  marker, trusted-boundary seam, bound-as-precondition.
- Feeds later: `formal_vec`/`formal_map` (Phase 7 #10), certificate-chain (Phase 6B #14a).
- Reject: Ada surface, aspect density, cursors, full container matrix.
