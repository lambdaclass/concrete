#set document(
  title: "Evidence-Carrying Systems Code: A Lean-Implemented Compiler Pipeline for Auditable Low-Level Programs",
  author: ("Federico Carrone"),
)

#set text(font: "New Computer Modern", size: 9pt)
#set page(margin: (x: 1.6cm, y: 1.8cm), numbering: "1", columns: 2)
#set par(justify: true, leading: 0.58em, spacing: 0.72em)
#set heading(numbering: "1.1")
#set math.equation(numbering: "(1)")
#set enum(indent: 0.5em)
#set list(indent: 0.5em)
#set figure(gap: 0.35em)

#show figure.caption: set text(size: 8pt, style: "italic")

#show heading.where(level: 1): it => {
  v(0.72em)
  text(size: 11pt, weight: "bold", it)
  v(0.28em)
}

#show heading.where(level: 2): it => {
  v(0.38em)
  text(size: 9.5pt, weight: "bold", it)
  v(0.18em)
}

#show heading.where(level: 3): it => {
  v(0.35em)
  text(size: 9pt, weight: "bold", style: "italic", it)
  v(0.18em)
}

#place(top + center, scope: "parent", float: true)[
  #block(width: 100%)[
    #v(0.3em)
    #align(center)[
      #text(size: 14.5pt, weight: "bold")[
        Evidence-Carrying Systems Code: A Lean-Implemented Compiler Pipeline for Auditable Low-Level Programs
      ]
      #v(0.5em)
      #text(size: 10pt)[Federico Carrone]
      #v(0.18em)
      #text(size: 9pt, style: "italic")[Draft, May 2026]
      #v(0.55em)
    ]

    #pad(x: 1.2em)[
      #text(size: 8.5pt)[
        #text(weight: "bold")[Abstract — ]
        Systems code is checked by tests, sanitizers, and analyzers; proof assistants offer rigor but live in a different world. The bridge between them is brittle: a theorem about a function is rarely tied to the exact source the compiler actually built, and it silently drifts when the body changes. This paper describes a working compiler pipeline that treats that bridge as a first-class artifact. Concrete is a small no-GC systems language with linear ownership and capability-visible effects whose compiler is written in Lean 4. The compiler extracts an eligible pure fragment of its own validated intermediate representation into Lean objects, lets a developer attach kernel-checked theorems to individual functions through a registry keyed by a body fingerprint, and automatically revokes a `proved` claim when the source drifts from the fingerprint it was proved against. The contribution is the *workflow*, not a verified compiler: we make authority, effects, proof-eligibility, trust boundaries, and stale-proof detection ordinary compiler outputs over a single semantic anchor. We report three graduated case studies — a bounded header validator, a no-allocation bounded-state validator, and a constant-time tag comparison — each stating precisely what is proved, what is enforced, what is reported, and what is assumed. We state the trusted computing base explicitly, including a concrete Lean obstacle (`partial def` opacity) that bounds how far an initial compiler-correctness result reaches. We do not claim a verified backend, full constant-time guarantees, or complete language semantics.
      ]
    ]
    #v(0.35em)
    #line(length: 100%, stroke: 0.5pt + luma(180))
    #v(0.1em)
  ]
]

= Problem

Two communities have largely disjoint trust stories. Systems programmers trust code through tests, fuzzers, sanitizers, and static analyzers — fast, scalable, and approximate. Verification communities trust code through machine-checked proof — rigorous, but rarely applied to ordinary low-level code as it is actually compiled and shipped. The bridge between the two is the weak link.

The specific failure we target is *proof drift*. When a theorem about a function lives in a separate development — a model in a proof assistant, a contract in a verifier — there is usually nothing in the compiler that knows the theorem exists, knows which source it was proved against, or notices when the body changes underneath it. A reviewer reading the function cannot tell, from the compiler's own output, whether an attached property is currently true, stale, or never re-checked. Tests catch behavioral regressions; they do not catch a proof that quietly stopped applying.

This paper asks a narrow question: what does it take for a real compiler to carry *evidence* — capability requirements, trust boundaries, proof-eligibility, and kernel-checked theorems — as ordinary, drift-aware outputs, so that the bridge between "code that compiles" and "code with a proof" is maintained by the toolchain rather than by convention? We do not propose a verified compiler. We propose making the unverified-but-evidenced boundary legible and self-invalidating, and we report what a working implementation of that idea looks like.

= Thesis

The central claim is that low-level software becomes easier to trust when four kinds of information are *compiler artifacts* rather than reconstructed by hand:

- *authority*: which external effects a function is permitted to use;
- *trust boundaries*: where pointer-level techniques and foreign calls are concentrated;
- *proof-eligibility*: whether a function lies in the fragment the compiler can extract for proof at all;
- *proof currency*: whether an attached theorem still applies to the function's current body.

Let $f$ be a function. The design objective is that the sets $A(f)$ (capabilities required), $T(f)$ (trusted/foreign boundaries crossed), and the pair $(E(f), P(f))$ — proof-eligibility and the currently-valid attached theorem — are all derivable from the ordinary compilation pipeline and re-checked on every build. The novelty is not any one of these in isolation; it is that they share a single validated semantic anchor and that $P(f)$ is automatically revoked when $f$'s body fingerprint changes.

= Language Sketch

Concrete is a small no-GC systems language; we describe only what the evidence pipeline rests on.

*Capabilities.* Semantic effects are named compile-time capabilities declared in signatures: `with(File)`, `with(Console)`, `with(Alloc)`, `with(Unsafe)`. A function with no capabilities is pure — no I/O, no allocation, no FFI. The calling discipline is monotone: if $f$ calls $g$ then $A(g) subset.eq A(f)$, so authority cannot widen invisibly.

*Ownership.* Structs and enums are linear by default and must be consumed exactly once; `Copy` types (integers, opt-in small structs) may be reused. Borrows are explicit and scoped. Cleanup is explicit, usually via LIFO `defer`, with no garbage collector and no implicit destructor insertion. The checker enforces no-use-after-move, no double-free, no leak, no borrow conflict, and branch agreement on linear consumption.

*Trust boundaries.* Concrete separates *semantic effects* (capabilities) from *implementation techniques*. The `trusted` marker concentrates pointer arithmetic, raw dereference, and pointer casts at declaration sites; foreign calls remain explicit through `with(Unsafe)`. "This code may allocate" and "this code does raw pointer tricks" are kept as separate audit questions.

*Predictable profile.* A per-function gate can reject recursion, unbounded loops, allocation, blocking, or FFI, so a function's execution shape is part of its checked interface rather than folklore.

= Proof and Evidence Pipeline

== A single semantic anchor

The compiler runs `Parse → Resolve → Check → Elab → CoreCanonicalize → CoreCheck → Mono → Lower → SSA → clang`. The proof boundary sits *after* `CoreCheck` and *before* monomorphization, materialized as a validated Core artifact. By that point surface sugar is gone and legality checks have run, but the program is still close to source meaning. Reports, proof extraction, and later artifact workflows all speak about this one validated object rather than inventing parallel semantic authorities (Fig. 1).

#figure(
  block(
    inset: 7pt,
    stroke: 0.4pt + luma(180),
  )[
    `Source`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `Parse → … → CoreCheck`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `ValidatedCore  ← the semantic anchor`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `ProofCore extraction · reports · evidence bundle`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `Mono → Lower → SSA → clang`
  ],
  caption: [Validated Core is the shared anchor for proof and reports — not a separate verification compiler.],
)

== Extraction and eligibility

`ProofCore` extracts an eligible pure fragment of validated Core into Lean objects (`PExpr`/`PVal`) whose semantics use Lean's unbounded integer arithmetic. A function is *eligible* only if it is pure (empty $A(f)$), not `trusted`, not an entry point, and free of recursion, loops, allocation, FFI, and mutable assignment, using only supported constructs (integer/boolean arithmetic, comparisons, `let`, `if/then/else`, non-recursive calls; bounded `while_step` and functional array update for the bounded-state fragment). Eligibility is reported (`--report eligibility`) with a per-gate breakdown. Eligibility is *not* proof: many eligible functions carry no theorem.

== Registry, fingerprints, and drift

Theorems are attached through a registry (`proof-registry.json`) keyed by a body *fingerprint*. "Proved" means precisely: the function's extracted `PExpr`, evaluated under unbounded-integer semantics, satisfies the stated Lean theorem, *and* the current body fingerprint matches the one the proof was written against. A body change invalidates the fingerprint and flips the obligation to `stale`; comment and formatting changes do not. Renames are matched by fingerprint rather than by name. Detection is deterministic and runs on every build.

Each function has exactly one status — `proved`, `stale`, `missing`, `blocked`, `ineligible`, or `trusted` — and a diagnostic taxonomy of eight stable codes (E0800–E0807) names each failure with a repair class. Supported theorem shapes range from fixed-input regression anchors, through universal boundary theorems (one branch for all inputs), to full input–output contracts.

== Evidence as ordinary output

The pipeline emits machine-readable evidence: `--report proof-status`, a JSON `proof-bundle` (summary, explicit assumptions, registry entries, dependency graph), `--report authority` for transitive capability chains, `--report unsafe` for trust boundaries, and a project-level `concrete check`. A CI evidence gate (20 checks across 8 sections) fails the build on widened authority, new allocation/FFI/blocking, predictable-profile breaks, or stale/missing proofs. The workflow itself — fingerprinting, validation, stale detection — is compiler-enforced; the theorem check is discharged by the Lean kernel.

== Debugging infrastructure

Because the compiler is the trust root for evidence, miscompiles are first-class adversaries. The project maintains a wrong-code corpus (`tests/wrong-code/`) of captured compiler bugs retained as regressions, a test-case reducer (`Concrete/Reduce.lean`, `scripts/reduce`) for shrinking failures, evidence bundles for review, and `verify` gates in CI. Adversarial compiler bugs, once found, are reduced and frozen.

= Case Studies

We report the three examples that have *graduated* the project's evidence bar — every audit bar met, every drift gate green, listed in the showcase manifest. Each states its own scope; we reproduce those caveats rather than smoothing them.

== Bounded header validator (`parse_validate`)

A 9-function pure module that parses an 8-field `i32` header and validates six properties against a closed error taxonomy. Two theorems are kernel-checked at build: `validate_version_correct` (a full per-input contract for the version check) and `validate_header_fields_success`, the six-validator success-direction composition under wrapping `i32` arithmetic.

*Honest scope.* The composition theorem is on a scalar-parameter helper, *not* on `parse_header` itself, because ProofCore does not yet extract array indexing, enum, or struct construction. It proves the success direction only; the full iff is a 256-branch split that exhausts the Lean heartbeat budget. The behavioral oracle is a second implementation in Python agreeing across 600 randomized cases over three seeds — not a Lean equivalence proof.

== No-allocation bounded state (`fixed_capacity`)

A 20-function bounded message validator with a 16-slot ring buffer and an XOR-fold integrity tag, allocation-free. Four kernel-checked theorems cover canonical-empty (`ring_new_correct`), tag-of-zero-prefix, single push (`ring_push_zero_correct`), and the central composition theorem `ring_push_then_contains_correct`: push-then-contains finds the value end-to-end. It exercises the full bounded-state surface — functional array update (`arraySet`), bounded `while_step`, and BitVec arithmetic at `i32` width.

*Honest scope.* The message format is a deliberately tiny illustrative shape, not a real protocol; the XOR-fold tag is a structural check, *not* a MAC — an adversary can forge it trivially.

== Constant-time tag comparison (`constant_time_tag`)

A six-line OR-accumulate equality over `[u8; 16]`: always 16 iterations, no early exit, branch-free fold, single branch on the accumulator at the end. Three theorems are kernel-checked, including the substantive one, `ct_compare_same_tag_correct`: for *any* 16-byte tag (parametric in all sixteen byte values), `ct_compare(t, t) = 1`, plus the `u8` self-XOR-is-zero helper it rests on.

*Honest scope.* This is the sharpest illustration of the thesis: the code is simple but the *security* claim is subtle. Source-level no-early-exit and a fixed loop bound are *necessary* but not *sufficient* for machine-level constant time — LLVM may reintroduce branches and the CPU leaks through caches and speculation. The example's `assumptions.toml` records machine-level constant time as `assumed_not_proved`. It is not HMAC, not Ed25519, and must not be deployed as a real auth-tag check.

#figure(
  table(
    columns: (1.15fr, 0.55fr, 1.25fr),
    align: (left, center, left),
    inset: 5pt,
    stroke: 0.4pt + gray,
    table.header(
      [*Case study*], [*Thms*], [*Strongest checked claim*],
    ),
    [`parse_validate`], [2], [6-validator success composition (scalar helper, wrapping i32)],
    [`fixed_capacity`], [4], [push-then-contains over bounded mutable state],
    [`constant_time_tag`], [3], [`ct_compare(t,t)=1` for all 16-byte `t`],
  ),
  caption: [The three graduated case studies. "Thms" counts kernel-checked theorems; each carries explicit out-of-scope caveats in its README and `assumptions.toml`.],
)

= Trust Model

Concrete's claims fall into five classes — *enforced*, *proved*, *reported*, *trusted assumption*, *backend/target assumption* — and the strength of any statement is the class it sits in (Fig. 3).

#figure(
  table(
    columns: (0.9fr, 1.6fr),
    align: (left, left),
    inset: 5pt,
    stroke: 0.4pt + gray,
    table.header([*Class*], [*Meaning and root of trust*]),
    [Enforced], [Safe code cannot violate and compile (ownership, linearity, capability monotonicity). Trusts the checker; no mechanized soundness proof.],
    [Proved], [Lean kernel verified the stated theorem over extracted `PExpr`; fingerprint current. Trusts kernel + extraction.],
    [Reported], [Compiler observed but does not block (authority traces, alloc, layout). For audit, not enforcement.],
    [Assumed], [Named in `assumptions.toml` as `assumed_not_proved` (e.g. machine-level constant time).],
    [Backend], [Codegen Core→SSA→LLVM→binary is trusted, not proved.],
  ),
  caption: [Claim classes. A statement is only as strong as its class; the paper labels every claim accordingly.],
)

What "proved" does *not* mean is stated explicitly: it does not mean the compiled binary is correct (the proof is over `PExpr` with unbounded integers, not the emitted machine code); it does not mean the checker is sound; it does not cover cross-function composition beyond what a stated theorem captures; and eligibility is not proof. The trusted computing base is the Concrete compiler (parsing through emission, fingerprinting, stale detection), the Lean kernel, and the C toolchain and platform. The checker is actively verified by an adversarial suite (1272 trust-gate checks) and the CI proof gate, but not by a mechanized correctness proof.

= Toward Compiler-Side Soundness

The trust model above trusts extraction: nothing yet proves that `Core → ProofCore` *preserves meaning*. We report a deliberately small first result and, more importantly, the obstacle that bounds it. The first batch (`ProofSoundness.lean`) proves preservation for the *literal fragment only* — integer and boolean literals (rules R-01, R-02) — establishing the proof pattern (a tiny source-side semantics, the extraction step, the agreement theorem) that binops, lets, ifs, calls, and loops are expected to follow.

The obstacle is concrete and worth naming because it shapes the roadmap. The full extraction function `cExprToPExpr` is a `partial def` — Lean's kernel treats `partial def` as *opaque*, generating no equation lemmas, so `unfold`/`rfl` cannot reduce calls to it. The first attempt therefore proved only eval-versus-source agreement *without* the antecedent that extraction produces the expected `PExpr`. Closing the full preservation claim required writing a separate *non-partial* helper covering the literal cases, whose body mirrors the literal arm of `cExprToPExpr`. Consequently the real, mutually-recursive extraction function remains inside the trusted boundary; only a literal-only mirror is proved. We present this not as a near-miss but as a result of independent interest: locating exactly where the proved/trusted line falls, and why a recursion-checker limitation — not a missing theorem — currently holds it there.

= Related Work

The title deliberately echoes *proof-carrying code* (PCC) [1] and typed assembly language [2], where a proof or typing derivation travels with a binary and is checked at load time. Concrete differs in *what* carries the evidence and *when*: the evidence is a source-tied, drift-gated attachment maintained by the compiler across edits, not a fixed certificate shipped with a binary. The lifecycle — fingerprint binding and automatic staleness — is the contribution, not the proof object.

Verification-first systems languages — F\* and its Low\* subset [3, 4], Dafny [5] — start from a proof obligation and extract code as a downstream step. Concrete inverts the emphasis: it is systems-first, with proof as an *opt-in attachment* over a validated-Core anchor and a small eligible fragment, accepting that most code is enforced-and-reported rather than proved. CompCert [6] proves the compiler itself; Concrete explicitly does *not*, and its contribution is making the unproved-but-evidenced boundary legible and self-invalidating rather than eliminating it. RustBelt [7] establishes semantic foundations for Rust's type system externally; Concrete keeps a weaker but in-toolchain notion of currency tied to the exact compiled body.

Among practical systems languages, Rust is the nearest relative (no GC, ownership, explicit `unsafe`), but authority there is inferred from APIs and modules rather than declared in signatures, and semantic inspection is distributed across diagnostics, lints, MIR tools, and external analyzers rather than centered on one report surface. Zig shares the no-hidden-runtime, explicit-allocator ethos but does not treat effect visibility as a first-class surface. Austral [9] demonstrates linear types for a serious systems language and is closest in spirit; Concrete adds capability signatures, report-oriented workflows, and the validated-Core proof boundary. The Lean 4 kernel and language [8] are the proof substrate and the implementation language, which is what lets the compiler and the theorem checker share one kernel.

= Lessons

Three lessons generalized beyond Concrete. *Drift gates are the load-bearing feature.* The value of an attached proof in a changing codebase comes almost entirely from automatic revocation; without it, a proof is a comment that ages silently. *Examples force the proof surface, not the reverse.* Each graduated case study closed a specific extraction gap — `fixed_capacity` forced functional array update and bounded `while_step`; `constant_time_tag` forced `u8` bitwise reasoning — which argues for growing the provable fragment from forcing examples rather than speculatively. *Proof ergonomics, not expressiveness, is the bottleneck.* The binding limits we hit were a heartbeat budget on a 256-branch split and a `partial def` opacity barrier — engineering and recursion-checker frictions, not missing logical power.

= Limitations and Threats to Validity

The evidence is real but bounded. The proved fragment is small (pure, loop-free or bounded-state, integer/bytewise) and per-function; the strongest composition results are on scalar helpers, not the full array-returning entry points. Several theorems prove one direction or fixed cases rather than full contracts. The behavioral oracle for `parse_validate` is a second implementation, not a Lean equivalence. The "easier to audit" claim is supported by signatures and report outputs but is not backed by a controlled user study, and remains the least-defended part of the thesis. The empirical corpus, while non-trivial for an early language, is small relative to a mature ecosystem, and several references are necessarily to a moving implementation. Finally, every "proved" claim is contingent on the trusted computing base above — most consequentially an unproved backend and `partial def`-bounded extraction soundness.

= Future Work

The roadmap follows the forcing-example discipline: extend ProofCore preservation past literals (binops, lets, ifs, calls, then bounded loops), which first requires lifting the `partial def` opacity barrier for the mutual extraction block; close the failure-direction and array/enum/struct extraction gaps that currently push composition theorems onto scalar helpers; pursue a real-crypto candidate (an HMAC-SHA256 or Ed25519-verify subset) to force shifts, `bitand`, and multi-iteration inductive invariants; and mature packages and the backend contract. Compiler-correctness proofs remain explicitly downstream of a stable extraction surface, not a precondition for the workflow.

= Conclusion

The gap between systems code and machine-checked proof is usually bridged by hand and decays silently. Concrete shows a different option: a real, Lean-implemented compiler that carries authority, trust boundaries, proof-eligibility, and kernel-checked theorems as ordinary outputs over one validated semantic anchor, and that revokes a proof automatically when the source drifts from what was proved. The implementation is incomplete and the trusted base is large and explicit, but the workflow holds on three graduated case studies whose claims are stated to the exact scope they have earned. The contribution is not a verified language; it is a maintained, self-invalidating bridge — and a demonstration that the honest version of that bridge is buildable today.

= References

#set text(size: 8pt)

[1] G. C. Necula. "Proof-Carrying Code." *POPL*, 1997.

[2] G. Morrisett, D. Walker, K. Crary, N. Glew. "From System F to Typed Assembly Language." *POPL*, 1998.

[3] N. Swamy et al. "Dependent Types and Multi-Monadic Effects in F\*." *POPL*, 2016.

[4] J. Protzenko et al. "Verified Low-Level Programming Embedded in F\* (Low\*)." *ICFP*, 2017.

[5] K. R. M. Leino. "Dafny: An Automatic Program Verifier for Functional Correctness." *LPAR*, 2010.

[6] X. Leroy. "Formal Verification of a Realistic Compiler" (CompCert). *CACM* 52(7), 2009.

[7] R. Jung, J.-H. Jourdan, R. Krebbers, D. Dreyer. "RustBelt: Securing the Foundations of the Rust Programming Language." *POPL*, 2018.

[8] L. de Moura, S. Ullrich. "The Lean 4 Theorem Prover and Programming Language." *CADE*, 2021.

[9] F. Brennan. "Austral: A Systems Language with Linear Types and Capabilities." Language specification / technical report.

Internal references: `docs/CLAIMS_TODAY.md`, `docs/TRUSTED_COMPUTING_BASE.md`, `docs/PROOF_WORKFLOW.md`, `docs/PROOF_OBLIGATIONS_REGISTER.md`, `Concrete/ProofSoundness.lean`, and the `examples/{parse_validate,fixed_capacity,constant_time_tag}` READMEs.
