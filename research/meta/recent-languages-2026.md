# Recent Languages And Verified Toolchains (April–October 2026)

Status: Research — survey and open questions; no adoption decision, schedule, or release requirement.

Date: 2026-10-30. Scope: GitHub repositories created between 2026-04-24 and
2026-10-24 that present a programming language, compiler, or verification
toolchain. Candidates were found with GitHub repository search (topics
`programming-language`, `compiler`, `language`, `lean4`; description searches for
systems, memory-safe, verified-compiler, effects, contracts, agent-oriented, and
self-hosted languages), ranked by stars, with spam and non-language projects
removed. Goose was surveyed separately in
[goose-design-survey.md](../compiler/goose-design-survey.md); Bend and Roc in
[bend-roc-lessons.md](bend-roc-lessons.md).

**Method limit.** Every entry below comes from README-level review at the pinned
commit. No code was read, nothing was built, and no benchmark or proof claim was
reproduced. A "creation date" is the repository's, so some projects are older
work in a new repository (Valen revives Vale). Each entry is a question to
answer by a later source review, not a finding about the project.

The open questions are tracked under
[Open Research Questions: Recent Languages](../../docs/project/ROADMAP_TASKS.md#open-research-questions-recent-languages).

## 1. Admission Filter

Concrete's [design policy](../../docs/project/PROJECT.md#design-policy),
[anti-features](../../docs/project/ANTI_FEATURES.md),
[value model](../../docs/language/VALUE_MODEL.md),
[evidence architecture](../../docs/verification/EVIDENCE_ARCHITECTURE.md), and the
owner's recorded language decisions are authoritative. A candidate idea is
considered only if it keeps:

- explicit control flow, allocation, authority, cleanup, FFI, and trust boundaries;
- linear ownership and second-class references (never stored, never returned);
- no implicit conversions, truthiness, inference at signatures, closures,
  overloading, macros, compile-time execution, or effect handlers;
- every claim backed by an obligation and an evidence class, with proof,
  compiler enforcement, testing, runtime checks, and trusted assumptions kept distinct.

Any language-surface consequence of an idea below goes to the owner as a
proposal with options; this note decides no syntax or semantics.

## 2. Highest Relevance

### Jacquard — effects in signatures for model-written, human-reviewed code

[jbwinters/jacquard-lang @ 3071c09](https://github.com/jbwinters/jacquard-lang/tree/3071c0974ebb4355ec6006c107fb81f0d159a6c8).
OCaml checker and interpreter, compiler emitting C. Every function signature
lists the outside-world effects it may perform (`->{net}`); the checker
guarantees the list is complete; the runtime refuses effects not granted on the
command line. Also claims canonical program identity and publishes a
`LIMITS.md` stating its boundary.

Questions:
- How does completeness of the effect list compare with Concrete's capability
  checking, particularly for FFI, indirect calls, and trusted code?
- Could the capability set of `main` be recorded in the built artifact so a
  deployer can compare two releases' authority without the source, extending
  `--report authority` rather than adding a runtime grant layer?
- How does its canonical program identity compare with `Concrete/BuildIdentity.lean`?

Not imported: a runtime that grants or refuses effects at launch.

### Solidus — one end-to-end theorem for a Lean-written compiler

[paradigmxyz/solidus @ 32c7cdb](https://github.com/paradigmxyz/solidus/tree/32c7cdbc0b466bdb13ce6eeabe5add8c49f356e2).
A Yul-to-EVM compiler written and verified in Lean 4. One theorem,
`compile_correct`, connects source semantics to emitted bytes on a pinned EVM
interpreter. The semantics live in a separate repository; the frontend is not yet built.

Questions:
- How does its single statement, and the way partial results feed it, compare
  with the S15 refinement and transport theorems in
  [VERIFIED_PIPELINE.md](../../docs/compiler/VERIFIED_PIPELINE.md)?
- What does keeping the target semantics in a separate pinned repository buy
  for review and trust, compared with semantics in the compiler tree?
- How do they state the observation relation and resource premises?

### LeanCIL and qedsvm — verifying the artifact that runs

[NethermindEth/LeanCIL @ 4229b54](https://github.com/NethermindEth/LeanCIL/tree/4229b5485e3773e33c2867d8aa29e3d61a6c7242)
extracts compiled .NET bytecode into Lean, binds it to the assembly by SHA-256,
regenerates and compares, audits axioms, and writes a `verified.json` report.
The extractor, CIL semantics, and Lean kernel are listed as trusted.

[QEDGen/qedsvm @ 8d2a13b](https://github.com/QEDGen/qedsvm/tree/8d2a13b50942df3a82af00ad3b95dd3a84f70b10)
uses one Lean model of the Solana VM both as a reference interpreter
(differentially tested against the production engines) and as the basis for
path-scoped proofs with compute-unit bounds. It states that execution is not verification.

Questions:
- Could Concrete's executable semantics (`Concrete.Semantics.TermIR`) join
  `scripts/tests/fuzz_differential.py` as a third leg beside the interpreter
  and the compiled binary, so disagreement between the proof model and emitted
  code is found by fuzzing? Owner: [emitted-code validation research](../compiler/sparkle-emitted-code-validation.md).
- Do Concrete receipts already require that regeneration from the recorded
  identity reproduce the same bytes? If not, what would that gate cost?
- Is their trusted-computing-base presentation clearer than ours for a reader
  of one report?

### Phil — architecture, authority, and obligations first

[abiggerhammer/phil @ 318b771](https://github.com/abiggerhammer/phil/tree/318b771106ab94329065f36f57ef4725d66f3b0b).
A systems language that starts from boundaries, protocols, authority, and proof
obligations, and separates source verification from artifact certification.

Questions:
- Does its source/artifact split map onto Concrete's evidence classes, and does
  it name a distinction we do not?
- Are its authority contracts on callables comparable with Concrete's
  capability rules for function-pointer values?

### Valen — group borrowing as the contrast case

[valen-lang/Valen @ b2aa19b](https://github.com/valen-lang/Valen/tree/b2aa19bdcd01570efba6684fac9a3d08a43c7d37).
Revives Vale with "group borrowing" (borrow checking that permits mutable
aliasing), planned generational references, and Rust interop through a patched rustc.

Question:
- Which workloads does group borrowing accept that Concrete's second-class
  references reject, and does Concrete have an explicit answer (scoped
  callbacks, owned views, operation APIs) for each? A contrast note only;
  its memory model conflicts with linear ownership.

## 3. Specific Techniques Worth Checking

| Project | Technique | Concrete owner or question |
|---|---|---|
| [vercel-labs/zerolang @ 7e1a64d](https://github.com/vercel-labs/zerolang/tree/7e1a64d27cc37671df31c6370890bce86f5135e1) | Agents query a semantic graph and submit checked edits; stale graph hashes and invalid shapes are rejected before anything is written | [semantic query interface](../compiler/semantic-query-interface.md): could query results carry a content hash that edit tooling must match? Text remains the source. |
| [dawnop/dawn-lang @ ea72ae9](https://github.com/dawnop/dawn-lang/tree/ea72ae9ecc8da23db60de3d8d98d96e3d99f7fc2) | Two peer backends held to the same answer by a gate; a fake GPU device runs the same host program without hardware | [QBE backend](../compiler/qbe-backend.md): a cross-backend equality gate once a second backend exists |
| [Krzysztof-Cieslak/fai @ 3e03dd1](https://github.com/Krzysztof-Cieslak/fai/tree/3e03dd1ce3222dab895a71f4556af5fbff1109a1), [kablorp/blorp @ 1d4f3c6](https://github.com/kablorp/blorp/tree/1d4f3c62662688f7442742f61fb0462e6bc793e6) | Languages designed for generation and review: explicit effects, contracts, structured diagnostics for edit-check-test loops | Which diagnostic and tooling choices help agents without the automatic memory management both rely on? |
| [hale-lang/hale @ bf02789](https://github.com/hale-lang/hale/tree/bf02789a646b8a9dad124bbeb1fbafcdf85b845f) | Single owner per piece of state; components communicate only over declared typed channels; architecture checked like types | Concurrency research (R-0399), only after its prerequisites |
| [rux-lang/Rux @ 1d9f6bd](https://github.com/rux-lang/Rux/tree/1d9f6bd5d0eb91081fec473ae7a000c48ae2ca02) | Self-contained backend with its own linkers; release optimization limited to explicit bounded passes; a written ownership contract | Backend contract and optimization-policy comparison |
| [siliconjungle/ink-lang @ f74c4ca](https://github.com/siliconjungle/ink-lang/tree/f74c4ca5f7fec6588c7a0eddae49978ba0b42630) | An external store of proved rewrite laws applied by a planner | Proof-guided optimization questions in the Bend/Roc record |
| [javimosch/machin @ 9e0fb93](https://github.com/javimosch/machin/tree/9e0fb93e17c43d7ed047df80aa881037286aaae4), [Lulzx/bendc @ 56edcc6](https://github.com/Lulzx/bendc/tree/56edcc65b3527a75838011682c231072d5b46097) | Byte-for-byte self-compilation fixpoints in CI; a 415-program codegen comparison suite | [Determinism](../../docs/compiler/DETERMINISM.md): is identical emitted output across hosts and runs already gated? |
| [ymm-oss/fsl @ f8e3726](https://github.com/ymm-oss/fsl/tree/f8e3726e2ead231c89cd4f52eef3dd5d652c774d) | Specification language for AI write-verify-repair loops; JSON-only verifier output; k-induction | Proof/agent authoring ergonomics |

## 4. Language-Level Candidates (proposals only)

The only language or stdlib ideas from this survey and the Goose survey that
pass the admission filter. Each is a question for the owner, not a decision.

1. **A function-level "no remaining bounds trap" claim** (from Goose
   `checkfree`). It would produce obligations, appear in `--report contracts`
   with an evidence class, and change no program meaning. Goose's
   specification fixes a minimum set of facts every implementation must prove,
   so acceptance does not depend on optimizer strength; that part is the one to
   study. Owner area: runtime safety obligations and
   [predictable failure discipline](../../docs/platform/PREDICTABLE_FAILURE_DISCIPLINE.md).
2. **A grow-only collection in std.** No `pop`, `clear`, or `truncate`, so
   length facts survive later pushes. An ordinary stdlib type with no language change.
3. **Guaranteed construction into the destination**, as a specification and
   checkable codegen property, not syntax. Measure first whether hidden copies
   of large linear values occur in real workloads.
4. **Indices validated once and stored in data** — later, after a graph or
   table workload and after item 2; explicit conversion back to integers.
5. **When explicit OS threads arrive:** only fully owned values cross threads,
   and a contained worker's abort ends only that worker with its message
   delivered to the waiter.
6. **Validated loading of flat offset-linked data** through ByteView or the
   binary-format work, never through stored references.

## 5. Not Imported

| Idea | Source | Conflict |
|---|---|---|
| Returning several frames up (`return E from f`) | Goose | Hidden control flow; same reason as removing `?` |
| Stored or relative references; `push` returning a reference | Goose | References are second-class |
| Compiler-assigned data stacks freed at scope exit; slot-reuse pools | Goose | Hidden memory model; stale slots are logic bugs by design |
| Implicit index↔integer conversion; truthiness narrowing | Goose | Permanent exclusions |
| Untyped generic parameters; inferred lifetime roots | Goose | Signatures stay explicit |
| Capturing blocks; case-function overloading | Goose | No closures; no overloading |
| `pure func` | Fai, Blorp | Capabilities do not establish purity; a purity judgement would be a new effect system |
| ARC/COW; automatic memory reuse; group borrowing | Blorp, Fai, Valen | Linear ownership |
| Runtime granting of effects | Jacquard | Adds a runtime layer; the compile-time capability set is the mechanism |
| Program-as-graph source of truth | zerolang | Text stays the source |
| Algebraic effect handlers; compile-time execution; macros | Emela, Mox | Anti-features |

## 6. Surveyed And Set Aside

Tov (TypeScript compatibility), Whim (PHP/Hack), Hike, Revo, Tauraro, Mox,
Emela, Vx (device placement in types; GPU-only interest), LogosLang (mostly
specified, not built), Revia (closed source), and mimi (too early) were reviewed
at README level and add nothing beyond the entries above.

## 7. Next Step For Each Entry

For each project pursued: read the source at the pinned commit, confirm or
correct the README claim, and record the forcing workload, the Concrete owner,
the evidence a change would need, and a recommendation to adopt, defer, reject,
or use an existing facility. Rejection or "already covered" is a successful
outcome. Re-run the repository search every six months and append newly
created languages here.
