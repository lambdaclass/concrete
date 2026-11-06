# QBE Backend For Concrete

Status: open

This note covers the smaller and more realistic of two QBE-related ideas:

- add a QBE backend for Concrete

This means:

- Concrete lowers to its own SSA
- the compiler emits QBE IL text
- the external `qbe` tool turns that into assembly or object code

It does **not** mean rewriting QBE in Concrete.
That is a separate and much larger idea.

## Why QBE Is Interesting

QBE is one of the few backend targets that fits Concrete's taste at all.

It is attractive because it is:

- much smaller than LLVM
- easier to inspect
- easier to explain
- closer to "small enough to understand"
- still capable of decent native code generation

That makes it a plausible alternate backend for a language that values:

- explicitness
- auditability
- smaller trusted surfaces
- backend plurality without giant framework adoption

## Why This Fits Better Than MLIR

For Concrete, MLIR is mostly attractive as infrastructure.
QBE is attractive as a backend that matches the language's philosophy better.

Compared to MLIR, QBE is:

- smaller
- less abstract
- less ecosystem-heavy
- easier to reason about as a backend experiment

Compared to LLVM, QBE is:

- much simpler
- likely easier to make deterministic and inspectable
- less powerful, but possibly good enough for many Concrete workloads

Compared to Cranelift, QBE is:

- smaller and arguably more philosophically aligned
- less featureful and less industrially broad

## What This Project Would Actually Be

The first serious experiment would be:

- `Concrete SSA -> QBE IL`

not:

- a new optimizer architecture
- a new high-level IR
- a rewrite of the compiler around QBE

The Concrete side would need:

- `EmitQBE.lean`
- QBE type mapping
- SSA instruction mapping
- control-flow lowering to QBE blocks
- call/global/data lowering
- driver integration for `--backend=qbe`

## Why This Is Worth Trying

This would answer several useful questions quickly:

1. Is Concrete's SSA close enough to QBE's model to lower cleanly?
2. Is QBE code quality good enough for real Phase H style programs?
3. Does a lighter backend improve auditability or reproducibility in practice?
4. Is backend plurality worth carrying operationally?

Those are high-value questions even if QBE never becomes the default backend.

## Good Evaluation Criteria

Concrete should judge a QBE backend by:

- correctness
- implementation simplicity
- output determinism
- compile-time/toolchain friction
- code quality on the existing Phase H workloads
- debug/inspection quality
- maintenance burden

The comparison should be against:

- current LLVM path
- maybe later Cranelift, if that ever becomes relevant

## Good First Scope

The first version should stay narrow:

- integers
- control flow
- calls
- aggregates only as far as QBE lowering stays honest
- existing examples and Phase H programs as validation targets

It should not start with:

- every exotic lowering case
- optimization parity with LLVM
- cross-platform ambitions beyond a small supported slice

## What This Is Not

This should not be sold as:

- immediate self-hosting
- a proof of backend superiority
- a replacement for the current LLVM path before evidence exists

It is an experiment in backend plurality and architectural fit.

## Philosophy Check

This idea is a good fit for Concrete only if it stays:

- small
- explicit
- backend-focused
- evidence-driven

It becomes a bad fit if it turns into:

- infrastructure sprawl
- optimizer ambition for its own sake
- a detour away from Concrete's current identity work

## Roadmap Fit

This belongs, if anywhere, as later backend-plurality work after the current compiler, package, and operational surfaces are more mature.

It is best treated as:

- research now
- implementation only if it earns its cost

## Bottom Line

A QBE backend is a serious and philosophy-compatible research direction.

The disciplined next step is:

- test whether `SSA -> QBE IL` is clean, useful, and maintainable

not:

- rewrite the world around QBE.
