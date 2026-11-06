# Persistent Equality And Rewrite State

Status: open

This note records a later compiler-architecture direction inspired by:

- Chris Fallin, “The acyclic e-graph” — <https://cfallin.org/blog/2026/04/09/aegraph/>
- “E-Graphs as a Persistent Compiler Abstraction” — <https://arxiv.org/pdf/2602.16707>

The key idea is not “use e-graphs because they are fashionable”.
The useful idea for Concrete is narrower:

- preserve useful equivalence and rewrite knowledge across phases instead of rediscovering it pass-by-pass

## Why This Matters

Concrete already has a compiler architecture with many explicit boundaries:

- Parse
- Resolve
- Check
- Elab
- CoreCheck
- ProofCore extraction and obligation generation
- Lower
- SSA / backend reporting

That explicitness is good for trust.
But it also means semantic knowledge can get repeatedly rederived or discarded across phase boundaries.

The sources above are useful because they attack a real long-term problem:

- phase ordering
- duplicate canonicalization work
- local rewrite opportunities that disappear between passes
- analyses that are rebuilt after every transformation

## What Concrete Could Learn

### 1. Preserve semantic equalities longer

Concrete should eventually ask:

- which facts discovered in Core or mid-end optimization are worth preserving across later passes?
- which equivalences matter for proofs, reports, or optimization extraction?
- which equalities should survive lowering rather than disappearing into one chosen representation too early?

This does not require full equality saturation at first.

### 2. Reuse existing analyses

The paper's strongest practical point is that a persistent equality abstraction becomes much more attractive if existing compiler analyses can run over it instead of requiring a separate analysis universe.

For Concrete, that suggests:

- any richer rewrite/equality layer should reuse the normal analysis/fact/report pipeline where possible
- avoid introducing a second isolated “optimizer-only truth world”

### 3. Make extraction decisions explicit

If multiple equivalent representations are kept alive, the final selected form should be:

- explicit
- costed
- inspectable
- ideally explainable in reports/debug dumps

This fits Concrete's audit-first direction much better than opaque optimizer behavior.

### 4. Treat this as a later mid-end/backend topic

Concrete should not prioritize this before:

- proof/evidence artifacts are stronger
- package/interface artifacts are stable
- wrong-code hunting and semantic diff workflows are more mature
- the backend contract is explicit

Otherwise the optimizer architecture will outrun the trust architecture.

## Questions To Revisit Later

1. Is a persistent equality structure needed at all, or do simpler rewrite/canonicalization fixpoints get most of the value?
2. Should the first use case be:
   - canonicalization
   - optimization
   - proof/extraction alignment
   - semantic diff support
3. Can existing Concrete analyses be reused directly over the structure?
4. How would extraction/selection be made visible in dumps and reports?
5. Does this help preserve semantic information for proof-facing workflows, or only optimization?

## What This Is Not

This note is not a commitment to:

- full equality saturation soon
- replacing the current pass pipeline
- adding a Rust-MIR-sized new IR layer by default

It is a reminder that later compiler architecture should consider:

- persistent semantic knowledge
- reusable analyses
- explicit extraction choices

instead of only adding more one-shot passes.
