# C/Rust Interop and ABI Glue — UX Decision

Status: direction decided; implementation pending. Prioritized 2026-10-17 under
[R-0285](../../ROADMAP.md#task-r-0285), with exports under
[R-0286](../../ROADMAP.md#task-r-0286), after the current soundness/identity work.

Concrete's intended foreign-package workflow covers C libraries and Rust libraries
through a C-compatible bridge: declare a dependency, import its generated module,
then use the normal build/test commands. This records the tooling direction; the
generator, package integration and proposed commands are **not built yet**.

## Decision

The eventual shape is a **narrow `concrete ffi` / `concrete bindgen` path** that
emits and/or imports **auditable glue**, not an open-ended FFI generator. Any
generated glue must carry, as first-class recorded facts:

- **ABI / layout assumptions** (struct layout, alignment, `repr`, integer widths);
- **ownership boundary rules** (who allocates/frees, move vs borrow across the
  boundary);
- **capability / trust labels** (the glue is a `trusted`/`Unsafe` boundary; what
  authority crosses it is explicit);
- **source spans** (glue points back at the Concrete and C declarations it bridges);
- **reproducible output** (same inputs → byte-identical glue; no timestamps/paths).

This matches Concrete's stance everywhere else: a foreign boundary is an
*explicit, audited assumption*, never silent textual glue.

## Package workflow

Normal project builds should invoke Cargo or the C compiler, generate bindings,
link the native artifacts and cache unchanged work. Pin dependency versions,
features, bridge inputs, toolchains, flags and targets, and record their artifact
identities. Start with local binding packages; publishing a registry is a separate
milestone. Failed generation/builds cannot silently retain an old linked artifact.

C header import supplies declarations and ABI facts. A reviewed binding description
supplies ownership, buffer lengths, pointer retention, cleanup, errors, threading and
capabilities that a header cannot establish. A typed Rust bridge generates Rust
C-ABI shims and Concrete declarations from one selected interface and checks the
shims against the Rust implementation. Arbitrary Rust ABI does not become a public
Concrete compatibility boundary.

Generated wrappers expose ordinary Concrete methods, scoped borrows, explicit
consuming close/drop, error values and capability-bearing handles. They retain the
library's allocation/free pairing. Missing ownership/effect facts stay unavailable
for safe generation; the generator cannot infer a safety proof from a C signature.
Existing rules for explicit cleanup and returned references remain in force.

Rust runtime adapters may expose bounded event queues, polling, cancellation and
shutdown with explicit lifetime/thread-affinity contracts. This is a bridge for a
specific dependency, not general Concrete async or a concurrency-safety theorem.

## Implementation checkpoint

The former deferral was waiting for foreign declarations and their trust/capability
rules. Those are now implemented, including R-0484's effect checks; generator and
build integration remain pending. R-0283/R-0287 still have to validate every ABI form
the bindings advertise, and R-0286 supplies the library/export subset.

R-0285's first acceptance cases are RocksDB's C API, a real operation from ethlambda's
Rust crypto stack, and a bounded libp2p adapter. A non-author builds and runs them
through the normal project workflow, with ABI, ownership, error and saturation/
cancellation controls. Proposed command names remain a tooling design choice;
current code uses the direct foreign declarations documented in [FFI.md](FFI.md).
See [SAFETY.md](../language/SAFETY.md) for the current trust-boundary model.
