#set document(
  title: "Concrete: Explicit Systems Programming With Visible Authority and Ownership",
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
        Concrete: Explicit Systems Programming With Visible Authority and Ownership
      ]
      #v(0.5em)
      #text(size: 10pt)[Federico Carrone]
      #v(0.18em)
      #text(size: 9pt, style: "italic")[Draft, March 2026]
      #v(0.55em)
    ]

    #pad(x: 1.2em)[
      #text(size: 8.5pt)[
        #text(weight: "bold")[Abstract — ]
        Concrete is an experimental systems language centered on auditability. Authority is visible in function signatures, ownership and cleanup remain explicit, and pointer-level unsafety is isolated by explicit trusted boundaries. The language targets hosted systems programming without a garbage collector, virtual machine, or large hidden runtime. This paper presents the language model, compiler architecture, execution and standard-library direction, and evidence from real-program pressure testing. The main result is not ecosystem breadth; it is that a small language with visible authority and visible ownership can already carry parsers, interpreters, storage tools, integrity tooling, and networked programs while preserving a simpler audit story than abstraction-heavy alternatives.
      ]
    ]
    #v(0.35em)
    #line(length: 100%, stroke: 0.5pt + luma(180))
    #v(0.1em)
  ]
]

= Introduction

Most systems languages claim some combination of performance, control, and safety. Concrete aims at a narrower target: auditability. The relevant question is not only whether a program is fast or memory-safe, but whether important facts about its behavior remain easy to inspect. If a function allocates, reads files, opens sockets, or crosses a foreign boundary, that should be visible. If a library relies on pointer-level implementation techniques, that boundary should be explicit and inspectable.

This pushes the language toward a different set of tradeoffs than mainstream languages usually optimize for. Concrete prefers explicitness over abstraction towers, reportable compiler facts over opaque optimization folklore, and a small language surface over broad feature accumulation. It is best understood not as a broad Rust competitor, but as an attempt to identify a sharper point in the design space: explicit systems programming with visible authority, visible ownership, and a small runtime boundary.

This draft advances five claims:
- Concrete treats authority, ownership, and trust boundaries as first-class audit surfaces.
- The compiler architecture already exposes stable artifacts and report modes that preserve those surfaces.
- Evidence from the Phase H workload corpus is strong enough to validate the direction, even though the ecosystem is still early.
- The most interesting differentiator is report-oriented semantic inspection, not merely syntax.
- The remaining work is best understood as deepening a coherent model rather than replacing a failed one.

#figure(
  table(
    columns: (1.1fr, 1.55fr, 1.1fr),
    align: (left, left, left),
    inset: 5pt,
    stroke: 0.4pt + gray,
    table.header(
      [*Claim*], [*Why it matters*], [*Primary evidence*],
    ),
    [Visible authority], [Audit questions can be answered from signatures rather than reconstructed from bodies], [Capability-structured examples and reports],
    [Explicit cleanup can scale], [A non-GC language fails if explicitness collapses into constant ceremony], [Phase H parser/interpreter workloads plus scoped `defer`],
    [Reports are a real differentiator], [Semantic review becomes workflow rather than convention], [`--report caps`, `unsafe`, `alloc`, `proof`, `layout`],
  ),
  caption: [The paper's core claims and where the supporting evidence comes from.],
)

= Thesis

The central claim of Concrete is that low-level software becomes easier to trust when three kinds of information stay visible in the source and remain recoverable in compiler artifacts:

- authority: what external effects the code is permitted to use
- ownership: what values must be consumed, cleaned up, or borrowed explicitly
- trust boundaries: where pointer-level implementation techniques and foreign calls are concentrated

This claim can be stated as a compact design objective. Let $f$ be a function. Then the language should make the following sets inspectable:

$A(f)$, the capability set required by $f$

$T(f)$, the trusted or unsafe boundaries crossed by $f$

$M(f)$, the allocation and cleanup obligations induced by $f$

Concrete is designed so that these sets are not hidden implementation details. They are intended to be visible in signatures, declaration forms, and compiler reports.

= Motivating Example

The thesis is easier to evaluate with code than with slogans. The integrity-verifier style decomposition used in the Phase H corpus shows the intended surface clearly: capability signatures themselves become part of the audit trail.

#figure(
  block(
    inset: 7pt,
    stroke: 0.4pt + luma(180),
  )[
```con
fn sha256(bytes: &Bytes) with(Alloc) -> String { ... }

fn read_file_raw(path: &String) with(File, Alloc) -> Result<Bytes, FsError> {
    return std.fs.read_file(path);
}

fn report_ok(path: &String, digest: &String) with(Console) -> Int {
    println("ok:", path, digest);
    return 0;
}

fn verify_artifact(path: &String) with(File, Console, Alloc) -> Int {
    let bytes = read_file_raw(path)?;
    defer bytes.drop();

    let digest = sha256(bytes) with(Alloc);
    defer drop_string(digest);

    return report_ok(path, digest) with(Console);
}
```
  ],
  caption: [Illustrative Concrete surface: the auditing question "which functions can read files, allocate, or print?" is answered directly by signatures rather than recovered only from bodies.],
)

Here `sha256` can allocate but cannot read files, `read_file_raw` can read files but cannot print, and `report_ok` can print but cannot read files. Only `verify_artifact` bridges those authorities. That is the design target in one screenful of code.

= Claims And Evidence

The paper's argument is strongest when stated as three concrete claims.

== Claim 1: Visible authority improves reviewability

Concrete's capability surface turns "what may this code do?" into an interface question instead of a whole-program reconstruction problem. The motivating example is simple, but the same effect appears more strongly in the Phase H integrity-verifier workload: hashing, file access, and reporting are separated by signature rather than by convention. The point is not merely that capabilities exist, but that the boundaries are legible enough to be surfaced again in reports and checked for drift.

== Claim 2: Explicit cleanup can remain usable

A non-GC language does not win merely by exposing costs; it also has to avoid turning ordinary programming into mechanical bookkeeping. Phase H matters because it tested this against real programs rather than toy fragments. The strongest result is not zero ceremony, but that scoped `defer` moved an important class of cleanup from repetitive boilerplate into a stable idiom without changing the explicit-cost model. The paper therefore makes a narrower claim: the model has survived real pressure and improved under evidence-guided refinement.

== Claim 3: Report-oriented semantics are a genuine contribution

The most distinctive part of Concrete may not be the surface language at all. It is the decision to expose semantic facts as ordinary compiler outputs over validated artifacts. Capabilities, trust boundaries, allocation behavior, proof eligibility, interface shape, and layout facts become inspection surfaces. That creates a path toward semantic review workflows that is different from both style-only review and fully separate verification toolchains.

= Scope And Threat Model

Concrete is trying to make certain failure modes easier to inspect, contain, and review:

- unintentional authority creep
- diffusion of low-level unsafety through ordinary code
- hidden resource and cleanup behavior
- difficulty explaining why a given function is pure, effectful, or proof-eligible

It is not yet claiming to solve all of systems programming. In particular, the current project does not claim:

- complete package and dependency maturity
- a finished concurrency model
- a broad formal proof story for the full language
- fully mature backend plurality

The relevant risk is therefore two-sided. One failure mode is unsoundness or architectural confusion. The other is subtler: the language may remain honest but exhausting, preserving explicitness at the cost of day-to-day usability. Much of the project's current evaluation logic is about distinguishing those outcomes.

= Core Model

== Capabilities

Concrete uses named compile-time capabilities for semantic effects. A function that allocates, touches files, uses the console, opens the network, or crosses explicit low-level boundaries must declare the relevant authority in its signature. Effect requirements are therefore part of the user-visible interface rather than a body-level surprise.

The intended calling discipline is monotone: if $f$ calls $g$, then $A(g) subset.eq A(f)$. In other words, callers must explicitly carry the authority required by their callees. This is a simple rule, but it creates a much stronger audit surface than ambient access to files, networking, or allocation.

== The Three-Way Split

Concrete separates semantic effects from low-level implementation techniques. Capabilities describe what the code is allowed to do semantically. The `trusted` boundary marks where pointer arithmetic, raw pointer dereference, raw pointer assignment, and pointer-involving casts are intentionally concentrated. Foreign calls remain explicit through `with(Unsafe)` rather than being silently absorbed.

#figure(
  table(
    columns: (1.05fr, 1.35fr, 1.1fr),
    align: (left, left, left),
    inset: 5pt,
    stroke: 0.4pt + gray,
    table.header(
      [*Mechanism*], [*What it covers*], [*Visibility*],
    ),
    [Capabilities], [Caller-visible semantic effects], [Function signatures],
    [`trusted`], [Pointer-level containment for implementations], [Declaration sites],
    [`with(Unsafe)`], [Explicit foreign-boundary authority], [Function signatures],
  ),
  caption: [Concrete's central safety split.],
)

The split matters because it avoids conflating "this code can allocate" with "this code performs raw pointer tricks internally." Those are different audit questions, and the language keeps them separate.

== Ownership and cleanup

Concrete uses linear types by default for structs and enums. Linear values must be consumed exactly once. This gives the language resource-safety pressure without a garbage collector. Cleanup remains explicit rather than being driven by hidden collector work or implicit destructor insertion. Scoped `defer` reduces boilerplate while preserving the visibility of destruction paths.

The intended ownership model is therefore:

- Copy values may be reused freely
- linear values must be consumed exactly once
- borrows are explicit and scoped
- cleanup is explicit, often via `defer`

= Comparison To Nearby Languages

Concrete is easier to understand when positioned against neighboring systems languages. The goal is not to claim superiority on every axis. The goal is to show which combination of choices is unusual.

#figure(
  table(
    columns: (1.25fr, 0.85fr, 1.05fr, 1.0fr, 1.0fr),
    align: (left, center, center, center, center),
    inset: 5pt,
    stroke: 0.4pt + gray,
    table.header(
      [*Language*], [*GC / VM*], [*Visible effect system*], [*Explicit trust boundary*], [*Semantic reports as first-class output*],
    ),
    [Concrete], [No / No], [Yes], [Yes], [Yes],
    [Rust], [No / No], [No explicit capability set], [Unsafe], [Limited, tool-fragmented],
    [Zig], [No / No], [No explicit capability set], [Pointer/unsafe-style low-level surface], [Limited],
    [Go], [Yes / Runtime], [No], [No comparable boundary], [No],
  ),
  caption: [A coarse positioning table. Concrete's unusual combination is visible authority plus explicit trust containment plus report-oriented inspection.],
)

Rust is the closest neighbor in spirit: no garbage collector, strong ownership discipline, and explicit unsafety. The main difference is that Rust does not make authority a signature-level concept, and its semantic inspection story is distributed across compiler diagnostics, lints, MIR-based tools, Clippy, and external analyzers rather than centered on a single report surface. Zig shares the no-GC, low-level, explicit-cost ethos, but is less interested in effect visibility as a first-class language surface. Go offers a smoother operational experience, but through the managed-runtime and hidden-work model Concrete is trying to avoid.

= A Small Formal Sketch

Concrete is not yet presented here with a full formal semantics. The project already has a proof boundary and a proof-eligible subset, but this paper uses a lighter mathematical sketch to clarify the design.

Let a program be represented by a call graph $G = (V, E)$, where each vertex is a function. Associate with each function $f in V$:

$A(f)$: the declared capability set

$U(f)$: a Boolean indicating whether $f$ crosses an explicit unsafe or foreign boundary

$R(f)$: a report bundle containing compiler-derived facts such as authority traces, trusted wrappers, proof eligibility, layout, and allocation summaries

The design intent can be expressed by four simple properties:

1. Signature visibility:
   for semantically effectful code, $A(f)$ is visible in the source signature.
2. Containment visibility:
   pointer-level implementation techniques are concentrated so that $U(f)$ is sparse and inspectable.
3. Report recoverability:
   relevant semantic facts about $f$ should be derivable into $R(f)$ from the ordinary compiler pipeline rather than from a second semantic system.
4. Proof-eligibility filter:
   a pure function with empty authority set, no trusted origin, and no foreign boundary can be considered a candidate for the provable fragment.

This sketch is intentionally narrow. It does not formalize all of Concrete. It makes explicit the structural claim that important program facts should remain visible enough to inspect, compare, and eventually prove over stable compiler artifacts.

= Compiler And Execution Architecture

Concrete's current compiler pipeline runs:

`Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA -> clang`

The unusual part is not merely the number of passes, but the placement of the semantic anchor. The proof boundary sits after `CoreCheck` and before monomorphization, materialized as a validated Core artifact. By that point, surface sugar is gone, the main legality checks have already run, and the program is still close enough to source meaning to support proof-oriented extraction and human-readable reports. That is a better place to anchor inspection than either the surface AST, which is too sugar-heavy, or backend SSA, which is already too lowered. It also means the report system, the proof-eligible subset, and later artifact workflows can all talk about the same validated object rather than inventing parallel semantic authorities.

The execution model is deliberately thin. Concrete currently targets hosted systems programming on a POSIX-like environment with libc available. It has no garbage collector, no virtual machine, no hidden runtime initialization, no panic-unwind machinery, and no ambient cleanup hook. Programs begin in `main`, call into the compiled user program, and exit through ordinary process termination. Heap allocation currently goes through libc `malloc` and `realloc`, with abort-on-OOM as the explicit default policy.

Concrete therefore has a runtime boundary, but not a large managed runtime. The runtime boundary is the set of external symbols and conventions required to link and execute a compiled program.

#figure(
  block(
    inset: 7pt,
    stroke: 0.4pt + luma(180),
  )[
    `Source`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `ValidatedCore`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `ProofCore extraction / reports / later artifact workflows`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `Mono -> Lower -> SSA -> clang`
  ],
  caption: [The architectural shape that matters most for proof and report workflows: validated Core is the semantic anchor, not a separate verification compiler.],
)

= Report-Oriented Workflows

One of Concrete's most distinctive ideas is not a surface-language construct but an operational habit: treat compiler-derived semantic reports as first-class artifacts. The compiler already emits report modes for capabilities, unsafe and trusted boundaries, authority traces, proof eligibility, layout, allocation, interface shape, and monomorphization.

This matters for three reasons. First, it turns semantic review into a repeatable workflow: instead of inferring authority growth or trust-boundary drift from source diffs alone, the compiler can expose those facts directly. Second, it creates a bridge between ordinary compilation and later formalization: proof-eligibility is reported over the same validated Core boundary the compiler already uses internally. Third, it suggests a future where optimization, review, and regression checking operate over semantic facts rather than over prose and convention alone. In that sense, the report system is part of the language design, not just a debugging convenience.

= Standard Library Direction

Concrete's standard library is part of the language's safety story, not just a bag of utilities. The intended shape is:

- explicit about allocation
- explicit about ownership and handles
- bytes-first for low-level work
- small and sharp rather than broad
- neutral about future concurrency structure until that design is mature

The library is already structured into three layers:

- Core: pure computation and analysis-friendly modules
- Alloc: modules that rely on allocation but not on broader host services
- Hosted: modules that rely on POSIX and libc facilities such as files, networking, time, and processes

This layering matters for more than organization. It makes host assumptions auditable now and creates a clean path toward stricter execution profiles later. A future `core_only` or `no_alloc` profile is plausible precisely because the library already distinguishes pure computation, allocation-backed utilities, and host-coupled services instead of flattening them into one ambient standard environment.

= Why Now

There is a timely reason to care about this design space. Systems code increasingly carries review obligations not captured by memory safety alone: supply-chain trust, evidence of authority containment, high-integrity deployment constraints, and proof-adjacent reasoning about critical paths. At the same time, much modern tooling responds to complexity by adding more abstraction, more hidden runtime behavior, or more post hoc analysis.

Concrete explores the opposite strategy: make the language and compiler preserve the relevant facts in an inspectable form from the beginning. The project is therefore not only about language ergonomics. It is also about whether evidence-first development can become a practical discipline for systems programming.

= Evidence From Real Programs

The strongest evidence for Concrete does not come from toy examples. It comes from the Phase H workload corpus, which includes a policy engine, a large MAL interpreter, a JSON parser, a grep-like tool, a bytecode virtual machine, an integrity verifier, a TOML parser, a file-integrity monitor, a key-value store, a simple HTTP server, and a Lox interpreter.

That corpus established several important points. First, the language can already carry serious programs. Parsers, interpreters, CLI tools, storage workflows, integrity tooling, and networked code are no longer hypothetical targets. Second, the claimed audit differentiator is real in code that matters. The integrity verifier is the clearest example: capability signatures are not decorative. They act as a readable security decomposition. Third, optimization folklore is a poor substitute for measurement. Early performance conclusions were distorted by compilation without optimization. Under `-O2`, Concrete matched Python and system tools on text-heavy workloads and matched C on a dispatch-heavy VM benchmark once a missed-inlining cliff in vec builtins was removed. Fourth, explicit cleanup can become less noisy without becoming hidden. Scoped `defer` eliminated a substantial amount of repeated cleanup code in the JSON parser while preserving a clear destruction story.

#figure(
  table(
    columns: (1.5fr, 0.8fr, 0.8fr, 0.9fr),
    align: (left, center, center, center),
    inset: 5pt,
    stroke: 0.4pt + gray,
    table.header(
      [*Workload*], [*Concrete*], [*C/System*], [*Relative to C*],
    ),
    [JSON parse 9.3MB], [40ms], [--], [competitive with Python; no C baseline in corpus],
    [grep 13MB count-only], [35ms], [83ms], [0.42 times system `grep`],
    [VM fib(35), after inline fix], [257ms], [260ms], [0.99 times C],
    [SHA-256 9.3MB], [23ms], [25ms], [0.92 times system baseline],
  ),
  caption: [Representative Phase H results at `-O2` on Apple Silicon, emphasizing the most systems-relevant comparison: proximity to C-class performance where comparable baselines exist.],
)

= Evaluation

The main evidence from the current system can be summarized qualitatively as follows.

== Language viability

Phase H answered the question of basic viability. Concrete is no longer at the stage of proving that it can express real programs at all. That threshold has been crossed.

== Performance shape

The available evidence suggests that Concrete's overhead is not dominated by its explicit authority or ownership model. The clearest large gap found in the VM benchmark was due to backend shaping, specifically non-inlined vec builtins in a hot loop. Once corrected, the gap to C disappeared on that benchmark. This matters because it changes the interpretation of "language cost." The evidence points first to code generation shape, not to an inherent tax from the explicit model.

== Audit surface

Concrete's most unusual contribution is not raw speed. It is that capability requirements, trusted boundaries, and proof eligibility can already be surfaced through compiler reports over the ordinary compilation pipeline. This creates a tractable inspection story and suggests a future in which semantic regression checks can be built over artifact-level facts rather than over style rules and convention alone.

= Related Work

Concrete sits near several existing lines of work, but does not match any of them exactly.

Rust is the nearest practical relative. It combines explicit ownership with explicit `unsafe`, and it has set the baseline for what a non-GC systems language can demand from programmers. Concrete differs in two ways. First, authority is a signature-level concept rather than an effect inferred from APIs and module boundaries. Second, the project places more emphasis on report-oriented semantic artifacts as a first-class compiler product rather than leaving inspection distributed across diagnostics, lints, MIR-based tools, and external analyzers.

Zig shares the "no hidden runtime work" and "explicit cost model" ethos, and its allocator-passing discipline is one of the clearest examples of visible allocation in current systems language practice. Concrete is closer to Zig than to managed runtimes in spirit, but pushes further toward explicit authority tracking and toward a smaller trusted-containment story.

Austral is an important comparison because it demonstrates that linear types can support a serious systems language without collapsing into purely academic design. Concrete is philosophically aligned with that result, but differs in its particular emphasis on capability signatures, report-oriented workflows, and the validated-Core proof boundary.

Pony is relevant for its reference-capability discipline and its broader interest in static reasoning about effect and aliasing structure. Concrete does not inherit Pony's concurrency model or reference-capability system, but it shares the broader ambition of making semantically important distinctions explicit in the type-and-effect surface.

In the broader literature, capability systems, ownership and linear type systems, and proof-oriented intermediate languages all inform this space. Concrete's proposed contribution is their combination in a small-language setting where compilation, reporting, and later proof workflows are intended to share a single semantic anchor rather than fragment across unrelated representations.

= What Concrete Is Not

Concrete is not a garbage-collected language. It is not a virtual-machine language. It is not trying to recreate a giant iterator, async, or abstraction ecosystem. It is also not claiming to have already solved package architecture, proof automation, backend plurality, or concurrency maturity.

These omissions are design choices as much as they are missing work. The project is deliberately trying to avoid paying complexity costs before there is evidence that they buy real value.

= Current Limitations

The present system still has clear limitations.

- package, artifact, and workspace architecture remain the largest structural gap
- the standard library still needs deeper systems-module polish and stronger integration coverage
- some Phase H follow-through items remain open, including collection maturity and remaining string ergonomics
- the runtime story is intentionally thin today and not yet mature in the sense of concurrency plurality or stricter bounded-allocation profiles
- the proof story is real but still narrow relative to the full language

These are not small details. They define the difference between a strong experimental compiler and a complete language system.

= Threats To Validity

The evidence in this paper is real, but it is still bounded.

First, the empirical corpus is strong for an early language project, but still small relative to a mature ecosystem. Phase H proves more than toy viability, but not broad industrial generality.

Second, several of the strongest references are internal project documents because the paper is describing a moving implementation. That is appropriate for implementation accuracy, but weaker than a paper anchored primarily in external evaluation and prior literature.

Third, some of the most interesting claims are qualitative. "This code is easier to audit" is partly evidenced by signatures and report outputs, but still needs stronger comparative user studies or sustained downstream adoption to move from plausible claim to demonstrated result.

Fourth, performance interpretation must remain careful. The strongest result is that the explicit model did not introduce an obvious irreducible tax in the tested workloads. That is not the same as claiming uniform parity with C across all systems workloads.

= Discussion

The key strategic question after Phase H is no longer whether Concrete works at all. It is whether the language's explicit patterns stabilize into good idioms rather than remaining honest but exhausting ceremony. This is the right question. A language that is explicit but unbearable has failed just as surely as a language that is convenient but opaque.

Concrete's current results are promising because several explicit patterns have already moved from burden to discipline. Scoped cleanup is better than it was. Capability signatures are proving their audit value. The standard library has enough shape to support real workloads. The compiler already emits enough semantic structure to support report-first inspection. The remaining work is to deepen these strengths without dissolving them into hidden mechanisms.

= Conclusion

Concrete is an attempt to make systems programming more inspectable by default. Its central bet is that visible authority, visible ownership, and explicit trusted boundaries can produce a language that is simultaneously low-level and easier to audit. The current implementation is incomplete, but the evidence from real programs is already strong enough to justify the direction. Concrete has shown that a small language with no garbage collector, no large hidden runtime, and a report-oriented compiler can already carry serious workloads. The next challenge is not proving that the model works once. It is preserving that model while the project grows into a fuller language system.

= References

[1] Compiler Architecture. `docs/ARCHITECTURE.md`.

[2] Safety Model. `docs/SAFETY.md`.

[3] Execution Model. `docs/EXECUTION_MODEL.md`.

[4] Standard Library Direction. `docs/STDLIB.md`.

[5] Value and Reference Model. `docs/VALUE_MODEL.md`.

[6] Provable Subset. `docs/PROVABLE_SUBSET.md`.

[7] Phase H Summary. `research/workloads/phase-h-summary.md`.

[8] John Dennis Capability-Based Systems bibliography and classic capability-systems lineage.

[9] Rust language and unsafe boundary design literature.

[10] Zig language design on explicit allocators and no hidden control flow.

[11] Austral language work on linear typing for systems programming.

[12] Pony language work on reference capabilities and concurrency-safe alias control.
