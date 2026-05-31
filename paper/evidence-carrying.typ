#set document(
  title: "When Proofs Go Stale: Evidence-Carrying Source in a Small Systems Compiler",
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
#show raw.where(block: true): set text(size: 7.6pt)

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
        When Proofs Go Stale: Evidence-Carrying Source in a Small Systems Compiler
      ]
      #v(0.5em)
      #text(size: 10pt)[Federico Carrone]
      #v(0.18em)
      #text(size: 9pt, style: "italic")[Draft, May 2026]
      #v(0.55em)
    ]
    #pad(x: 1.2em)[
      #text(size: 7.9pt)[
        #text(weight: "bold")[CCS Concepts:] Software and its engineering → Formal software verification; Compilers; Imperative languages. \
        #text(weight: "bold")[Keywords:] systems programming, proof drift, Lean, capabilities, compiler evidence, verification workflow.
      ]
    ]
    #v(0.28em)
    #pad(x: 1.2em)[
      #text(size: 8.5pt)[
        #text(weight: "bold")[Abstract — ]
        A proof about source code can go stale in a depressingly ordinary way: the function changes, the model or theorem does not, and the build keeps looking green. Concrete is a small no-GC systems language, implemented in Lean 4, that treats this as a compiler problem rather than a documentation problem. The compiler extracts a proof-eligible fragment of checked Core into Lean objects, records attached theorems in a registry keyed by a body fingerprint, and revokes a `proved` claim when the source no longer matches what was proved. The same build also reports authority, allocation, trusted code, assumptions, and proof coverage. The result is not a verified compiler. It is a compiler that keeps evidence current enough for review. The current implementation has three non-toy graduated examples — a header validator, a bounded-state validator, and a fixed-time tag comparison — plus a partially proved extraction bridge. The backend, full language semantics, machine-level constant time, and SPARK-style VC generation remain outside the present claim.
      ]
    ]
    #v(0.35em)
    #line(length: 100%, stroke: 0.5pt + luma(180))
    #v(0.1em)
  ]
]

= A proof can rot

The thing that bothered me was not that systems programmers test and proof engineers prove. That division is old and often sensible. The part that kept looking wrong was the gap in the middle: a code review can show a function, and a proof development can show a theorem, but the compiler often cannot say whether the theorem is still about the function in front of you.

That is the failure this paper is about. Systems code is normally trusted through tests, fuzzers, sanitizers, and static analyzers: fast, useful, and incomplete. Proof assistants give a different kind of confidence, but the proof frequently lives beside the program rather than inside the compiler's account of the program. If the body changes, the theorem may become a historical fact about yesterday's code.

Concrete takes proof drift seriously enough to put it in the build. A theorem is attached to the body fingerprint it was proved against. Change the body and the claim downgrades. The theorem has not been refuted; it has simply stopped applying to this source. That sounds mundane, but it changes the review surface: a reviewer can ask the compiler which functions are proved, which are stale, which are only tested, which rely on assumptions, and which cross a trusted boundary.

The implementation has these load-bearing pieces:

- proof attachment by source-body fingerprint, with deterministic revocation on drift;
- compiler reports for authority, trusted code, proof eligibility, assumptions, and release bundles;
- case studies whose claims are classified as proved, enforced, reported, assumed, or trusted;
- the first extraction-preservation facts for the compiler path from checked Core to ProofCore.

The negative space is equally important. Concrete does not yet generate SPARK-style verification conditions for every array access, loop invariant, or arithmetic operation; it does not prove the emitted binary correct; and it does not make side-channel claims beyond source-level structural checks plus explicit assumptions.

#figure(
  table(
    columns: (1.0fr, 0.7fr, 1.35fr),
    align: (left, center, left),
    inset: 4.2pt,
    stroke: 0.35pt + gray,
    table.header([*Claim*], [*Class*], [*Evidence / trust root*]),
    [Authority tracking], [Enforced], [Capability checker and authority report],
    [Proof drift detection], [Enforced], [Body fingerprints and stale-proof gate],
    [Theorem correctness], [Proved], [Lean kernel checks theorem over extracted `PExpr`],
    [Source→ProofCore extraction], [Proved + trusted], [R-01..R-15 (R-05 via R-04; R-07 partial), R-18, R-19 extraction/eval coverage; R-20 extraction-only; backend equivalence still trusted],
    [Backend correctness], [Trusted], [Core→SSA→LLVM→binary not verified],
    [Machine constant-time], [Assumed], [`assumptions.toml`; source shape only],
  ),
  caption: [Claims versus evidence. Every claim in the paper is classified before it is stated.],
)

#figure(
  table(
    columns: (1.15fr, 1.55fr),
    align: (left, left),
    inset: 4.2pt,
    stroke: 0.35pt + gray,
    table.header([*Quantity*], [*Current value used in this paper*]),
    [Graduated showcases], [4 total; 3 non-toy case studies evaluated],
    [Registered production proofs], [8 across the three non-toy case studies],
    [Proof-status counts], [`parse_validate` 3/10; `fixed_capacity` 4/20; `constant_time_tag` 1/2],
    [Oracle coverage], [200 randomized cases/seed; `parse_validate` cross-checked across 600 (seeds 0/42/999)],
    [Wrong-code corpus], [22 registered cases retained as regressions],
    [Spec-drift regression], [Checked-in adversarial fixture downgrades proof status],
    [Extraction rules discharged], [R-01..R-15 (R-05 subsumed by R-04; R-07 partial), R-18, R-19 extraction/eval; R-20 extraction-only],
  ),
  caption: [Evaluation summary. The counts are small; the point is that each one is tied to a compiler artifact or a checked regression.],
)

= What Concrete is trying to make impossible

The central claim is that low-level software becomes easier to review when the compiler can answer the questions reviewers actually ask. What external effects can this function use? Where is the raw pointer code? Is this function inside the fragment the proof tool understands? If it has a theorem attached, was that theorem checked against this body or an older one?

Let $f$ be a function. The design objective is that the capability set $A(f)$, the trusted/foreign boundary set $T(f)$, and the eligibility/currency pair $(E(f), P(f))$ are all derivable from the ordinary compilation pipeline and re-checked on every build. The novelty is not any one of these in isolation; it is that they share a single validated semantic anchor and that $P(f)$ is #emph[automatically revoked] when $f$'s body fingerprint changes.

The larger design rule is #emph[no semantically dark constructs]. "Provable language" does not mean every construct is forced into the current pure `ProvableV1` subset. It means every construct has an explicit proof story: it is proved, enforced, reported, assumed, trusted, or explicitly open with a named gap. Pure value code may be proved through `ProvableV1`; capability calls may be enforced by authority typing; allocation may be reported or policy-gated; FFI bodies and backend behavior may remain trusted but named. The future `concrete audit` command is therefore not just a summary report — it is the program-specific rendering of this proof-story matrix.

= Keeping the theorem attached

Concrete is not proof-carrying code for binaries. It is #emph[evidence-carrying source code]: the source body, compiler facts, extracted proof object, registry entry, assumptions, and release evidence are kept in one compiler-maintained loop. The running example in this paper is `constant_time_tag.ct_compare`, a fixed-size tag comparison whose source-level constant-time shape is simple enough to inspect but subtle enough to require honest assumptions.

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```text
edit source body
    ↓
validated-Core fingerprint changes
    ↓
registry fingerprint no longer matches
    ↓
proof-status: proved → stale
    ↓
CI evidence gate fails
    ↓
release bundle downgrades the claim
```
  ],
  caption: [Proof revocation is automatic. Once the source body changes, the compiler will not let a theorem proved against the old body keep applying.],
)

The same example carries the paper's main evidence classes in one place. The function is pure, so proof extraction is allowed. Its theorem says equal tags return `1`. Its oracle exercises equal and unequal tags. Its `assumptions.toml` records that machine-level constant-time behavior is not proved. A reviewer therefore sees one source function with a theorem, a registry entry, an oracle, an assumption file, and a release-bundle claim that all agree about scope.

Revocation is not hypothetical. A checked-in regression keeps a function whose body still matches its registered fingerprint but whose registered spec no longer matches the extracted body; the compiler reports it as an error and downgrades the status, rather than trusting the stale theorem (@fig-drift). The same gate fires the moment an edit changes the fingerprint at all.

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```
error: spec drift for 'test_drift.simple_add' —
  registered spec 'driftTestSpec' does not match the
  source-extracted PExpr; the theorem is about a
  different function than the source

-- stale_proof [error] ----- test_drift.con:21
  `simple_add` has a registered proof, but the body
   changed.
  failure: stale_proof    repair: theorem_update
```
  ],
  caption: [Real diagnostic from the spec-drift regression. The proof is revoked and given a repair class; nothing downstream may rely on it.],
) <fig-drift>

= Language surface

Concrete is a small no-GC systems language; we describe only what the evidence pipeline rests on.

#emph[Capabilities.] Semantic effects are named compile-time capabilities declared in signatures: `with(File)`, `with(Console)`, `with(Alloc)`, `with(Unsafe)`. A function with no capabilities is pure — no I/O, no allocation, no FFI. The calling discipline is monotone: a caller must declare a superset of every callee's authority.

#emph[Ownership.] Structs and enums are linear by default and must be consumed exactly once; `Copy` types (integers, opt-in small structs) may be reused. Borrows are explicit and scoped, cleanup is explicit via LIFO `defer`, and there is no garbage collector and no implicit destructor insertion. The checker enforces no-use-after-move, no double-free, no leak, no borrow conflict, and branch agreement on linear consumption.

#emph[Trust boundaries.] Concrete separates #emph[semantic effects] (capabilities) from #emph[implementation techniques]: the `trusted` marker concentrates pointer arithmetic, raw dereference, and pointer casts at declaration sites, while foreign calls stay explicit through `with(Unsafe)`. "This code may allocate" and "this code does raw pointer tricks" are kept as separate audit questions.

#emph[Predictable profile.] A per-function gate can reject recursion, unbounded loops, allocation, blocking, or FFI, so a function's execution shape is part of its checked interface rather than folklore.

The verifier idiom below shows the surface: the auditing question "which functions touch files, allocate, or print?" is answered by signatures, not by reading bodies.

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```con
fn sha256(b: &Bytes) with(Alloc) -> String { ... }
fn read_raw(p: &String) with(File, Alloc)
    -> Result<Bytes, FsError> { return std.fs.read(p); }
fn report(p: &String, d: &String) with(Console) -> Int {
    println("ok:", p, d); return 0;
}
fn verify(p: &String) with(File, Console, Alloc) -> Int {
    let bytes = read_raw(p)?;   defer bytes.drop();
    let d = sha256(bytes) with(Alloc);  defer d.drop();
    return report(p, d) with(Console);
}
```
  ],
  caption: [`sha256` may allocate but not read files; `read_raw` may read files but not print; `report` may print but not read. Only `verify` bridges those authorities, and its signature says so.],
)

= Concrete as a Systems Language

A proof workflow grafted onto a proof-assistant toy would not be worth reporting. Concrete is a working systems language first, and the rest of the paper assumes that footing. This section records what the language is on its own terms, before any proof enters the picture.

#emph[Smaller on purpose.] Concrete's design stance is subtractive. Where Rust answers complexity with more machinery — lifetimes, traits, async, a large standard library — Concrete bets that a smaller language with explicit capabilities, linear ownership, and visible cleanup is easier for a human to audit and for a compiler to reason about. There is no garbage collector, no virtual machine, no hidden runtime initialization, no unwinder, and no ambient cleanup hook; programs begin in `main`, call into compiled user code, and exit through ordinary process termination. The runtime boundary is a small set of external libc symbols, not a managed environment.

Against the mainstream systems languages it most resembles (@fig-langs), Concrete shares no-GC with all of them and a safe-subset memory guarantee with Rust. What sets it apart is orthogonal to memory safety: authority is part of the signature, and the proof/evidence lifecycle lives in the compiler rather than in convention or a separate tool.

#text(size: 7.2pt)[
#figure(
  table(
    columns: (0.62fr, 0.74fr, 0.86fr, 0.66fr, 0.74fr, 0.78fr),
    align: (left, center, left, center, center, center),
    inset: 2.8pt,
    stroke: 0.3pt + gray,
    table.header(
      [*Lang*], [*Safe-subset\ mem. safety*], [*Ownership /\ cleanup*], [*Authority\ in sig.*], [*Trust\ boundary*], [*Evidence\ in compiler*],
    ),
    [C], [none], [manual], [no], [none], [no],
    [C++], [partial (RAII)], [RAII / manual], [no], [none], [no],
    [Rust], [yes (borrowck)], [own + borrow + `Drop`], [no], [`unsafe`], [external],
    [Zig], [debug-only], [manual + alloc param], [no], [none], [no],
    [Concrete], [yes (linear)], [linear + `defer`], [yes (caps)], [`trusted`], [native],
  ),
  caption: [Concrete against mainstream systems languages. It joins Rust on safe-subset memory safety and all four on no-GC; the distinguishing columns are signature-level authority and compiler-native evidence. "external" = via a separate tool such as Verus or Creusot.],
) <fig-langs>
]

#emph[A real program corpus.] Concrete has carried eleven non-trivial programs, each built to apply pressure rather than to demo: a policy engine, a ~1150-line Make-A-Lisp interpreter, a recursive-descent JSON parser, a grep-like tool, a 22-opcode bytecode VM, a SHA-256 artifact-integrity verifier, a TOML parser, a file-integrity monitor, an append-only key-value store with compaction, a single-threaded HTTP server, and a ~1050-line Lox tree-walk interpreter. Parsers, interpreters, storage, integrity tooling, and networked code are no longer hypothetical targets.

#emph[Performance.] Under LLVM `-O2`, the ownership and capability model adds no measurable runtime cost in the workloads we ran. On text and parser workloads Concrete sits in the same band as Python and system tools; on pure bitwise computation it reaches near-C quality; and on a dispatch-heavy VM loop it matches C once we forced the vec builtins to inline, which closed a 3× gap (@fig-perf). The largest gap we measured came from code generation, not from the explicit model.

#figure(
  table(
    columns: (1.45fr, 0.72fr, 0.78fr, 0.78fr),
    align: (left, center, center, center),
    inset: 4.2pt,
    stroke: 0.4pt + gray,
    table.header([*Workload (`-O2`)*], [*Concrete*], [*Python*], [*C / sys*]),
    [JSON parse 9.3 MB], [40 ms], [46 ms], [—],
    [grep 13 MB, count], [35 ms], [34 ms], [83 ms],
    [VM `fib(35)`, inlined], [257 ms], [15,223 ms], [260 ms],
    [SHA-256 9.3 MB], [23 ms], [20 ms], [25 ms],
  ),
  caption: [Representative Phase-H measurements, Apple Silicon, warm cache. These are author-run, single-platform numbers, not an independent benchmark suite; the VM row required an `alwaysinline` fix to vec builtins. Python/C columns are `json.loads`/`hashlib`/`grep`/`shasum` baselines.],
) <fig-perf>

#emph[A fact-producing compiler.] The same machinery that supports proof also makes Concrete unusually legible to tooling. The compiler answers queries about a function — its capabilities, transitive authority, allocation sites, recursion and loop-bound status, layout, and proof-eligibility — as ordinary reports over validated Core. A reviewer, a CI gate, or an LLM can ask the compiler what a function is allowed to do instead of inferring it from the body. When authority, effects, and execution shape are facts the compiler will answer, a code generator and a checker work from the same source of truth rather than each guessing separately — a property that grows more useful as more low-level code is written and reviewed by machines.

#emph[An audit, concretely.] Take a question a security reviewer actually asks of the HTTP server: #emph[which functions can reach the network?] In C or Rust this means reading bodies and trusting that no transitive call opens a socket. In Concrete it is a single report (@fig-audit): the `Network` capability names three functions — `send_string`, which declares it, and `handle_client` and `main`, which inherit it through the call chain. The sixteen `Alloc` functions — request parsing, path resolution, response building — cannot reach the network, and the reviewer establishes that without opening one of them. Monotonicity is what makes the answer trustworthy: a callee cannot acquire authority its callers do not already carry.

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```
$ concrete http.con --report authority
capability Network (3 functions):
      send_string    <- declared
      handle_client  <- handle_client -> send_string
  pub main           <- main -> handle_client
capability Alloc (16 functions):
      find_space, parse_method, parse_path,
      resolve_path, build_200_response, …
```
  ],
  caption: [Real `--report authority` output (abridged). "Which functions touch the network?" is answered by the report and the monotonicity rule, not by reading sixteen bodies.],
) <fig-audit>

#emph[Ergonomic no-GC.] Linearity gives resource safety without a borrow-checker's lifetime calculus, and scoped `defer` moves cleanup from repeated boilerplate into a stable idiom while keeping every destruction path visible — a concrete ergonomic win observed when `defer` removed a class of repetitive cleanup from the JSON parser without changing the explicit-cost model. The standard library is layered to keep host assumptions auditable: a pure #emph[Core] (computation and analysis-friendly modules), an #emph[Alloc] layer (allocation but no broader host services), and a #emph[Hosted] layer (POSIX/libc: files, networking, time, processes). The same layering is why a future `no_alloc` or `core_only` execution profile could be added without restructuring the library.

= How the compiler keeps score

== The proof boundary

The compiler runs
`Parse → Resolve → Check → Elab → CoreCanon → CoreCheck → Mono → Lower → SSA → clang`.
The proof boundary sits #emph[after] `CoreCheck` and #emph[before] monomorphization, materialized as a validated Core artifact (@fig-anchor). By that point surface sugar is gone and legality checks have run, but the program is still close to source meaning. Reports, proof extraction, and later artifact workflows all speak about this object, rather than each inventing its own account of the program.

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
    `Source`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `Parse → … → CoreCheck`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    *`ValidatedCore`* ` ← the semantic anchor`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `ProofCore extraction · reports · evidence bundle`
    #line(length: 100%, stroke: 0.3pt + luma(160))
    `Mono → Lower → SSA → clang`
  ],
  caption: [Validated Core is the shared anchor for proof and reports — not a separate verification compiler.],
) <fig-anchor>

== Registry and revocation

`ProofCore` extracts the eligible fragment of validated Core into Lean objects (`PExpr`/`PVal`) whose semantics use Lean values plus width-tagged BitVec operations where the source demands fixed-width behavior. Theorems are attached through a registry keyed by fingerprint; a registry entry is exactly the $(P_f, h_f)$ pair of the model:

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```json
{ "function": "constant_time_tag.ct_compare",
  "body_fingerprint":
    "[(let diff (int 0)) (let i (int 0)) (while (binop
     Concrete.BinOp.lt (var i) (int 16)) [(set diff (binop
     Concrete.BinOp.bitor (var diff) (binop Concrete.BinOp.bitxor
     (index (var a)(var i)) (index (var b)(var i))))) ...]) ...]",
  "proof": "Concrete.Proof.ct_compare_same_tag_correct",
  "spec":  "Concrete.Proof.ctCompareExpr" }
```
  ],
  caption: [A real registry entry. The fingerprint is the canonical Core body; any source change that alters it makes `h_f ≠ φ(f)`, flipping the function to `stale`.],
)

The pipeline emits machine-readable evidence: `--report proof-status`, a JSON `proof-bundle` (summary, explicit assumptions, registry entries, dependency graph), `--report authority` for transitive capability chains, and `--report unsafe` for trust boundaries. A CI evidence gate (20 checks across 8 sections) fails the build on widened authority, new allocation/FFI/blocking, predictable-profile breaks, or stale/missing proofs. Eight diagnostic codes (E0800–E0807) name each failure with a repair class. The workflow itself — fingerprinting, validation, stale detection — is compiler-enforced; the theorem check is discharged by the Lean kernel. The status report is human-readable and per-function:

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```
-- proved [one_direction] -- constant_time_tag/src/main.con:33
  ✓ `constant_time_tag.ct_compare` — proof matches
    current body.
  coverage: one_direction
-- not eligible  constant_time_tag/src/main.con:50
  `…main` cannot be proved: has capabilities: Console,
   is entry point (main).
Totals: 2 functions — 1 proved, 0 stale, 0 unproved,
        0 blocked, 1 ineligible, 0 trusted
```
  ],
  caption: [Real `--report proof-status` output. Eligibility, currency, and the reason a function is excluded are all compiler outputs.],
)

== Debugging infrastructure

Because the compiler is the trust root for evidence, miscompiles are first-class adversaries. The project maintains a wrong-code corpus of captured compiler bugs retained as regressions, a test-case reducer for shrinking failures, evidence bundles for review, and `verify` gates in CI. Adversarial compiler bugs, once found, are reduced and frozen. The evidence pipeline does double duty here: alongside reporting, it keeps understood compiler bugs from coming back.

= The small model

The workflow above reduces to three predicates: authority monotonicity, proof eligibility, and proof currency.

#emph[Authority.] Write $"callees"(f)$ for the functions $f$ calls directly. Capability monotonicity is checked by the compiler:

#figure(
  block(inset: 5pt, stroke: 0.35pt + luma(185), width: 100%)[
    $
    g in "callees"(f) ==> A(g) subset.eq A(f) \
    "pure"(f) <==> A(f) = nothing
    $
  ],
  caption: [Authority cannot widen invisibly down a call chain.],
)

#emph[Eligibility.] Let $cal(C)$ be the set of extractable constructs. A function is eligible exactly when all extraction gates pass:

#figure(
  block(inset: 5pt, stroke: 0.35pt + luma(185), width: 100%)[
    $
    E(f) <==> \
    A(f) = nothing \
    and not "trusted"(f) \
    and not "entry"(f) \
    and "body"(f) in cal(C)
    $
  ],
  caption: [Eligibility is a gate for proof attachment, not a proof by itself.],
)

#emph[Currency.] Let $phi(f)$ be the canonical body fingerprint and let $rho(f)$ be the proof registry entry, if one exists. Let $cal(E)$ be extraction from Core to ProofCore and $tack.r$ be Lean-kernel derivability:

#figure(
  block(inset: 5pt, stroke: 0.35pt + luma(185), width: 100%)[
    $
    "proved" <==> E(f) and rho(f) = (P_f,h_f) \
    quad and h_f = phi(f) \
    quad and tack.r P_f (cal(E)("body" f)) \
    "stale" <==> rho(f) = (P_f,h_f) \
    quad and h_f != phi(f) \
    "missing" <==> E(f) and rho(f) = nothing \
    "ineligible" <==> not E(f)
    $
  ],
  caption: [The proof-status function. A source-body change changes the fingerprint and revokes the `proved` status.],
)

The invariant that does the work: editing a body changes $phi(f)$, so any registered $h_f eq.not phi(f)$ flips the status to #raw("stale") on the next build. Proof revocation is automatic, not a discipline the developer must remember. Renames are absorbed because matching is by fingerprint, not by name.

#emph[What the fingerprint captures.] $phi(f)$ is a structural encoding of the elaborated Core body — operators, literals, calls, control flow, and local names — taken after type-checking and canonicalization. Comments and whitespace are absent by construction, so reformatting never revokes a proof. The encoding is conservative in the safe direction: a meaning-preserving structural edit (reordering commutative operands, an `x + 0`, a dead `let`) changes $phi(f)$ and forces re-confirmation, even though the proof, stated over the normalized `PExpr`, would still hold. The direction that would be dangerous — a meaning-changing edit that leaves $phi(f)$ unchanged — is bounded by what the encoding omits: it drops surface type annotations, relying on Core having already type-checked, so the residual risk is a width- or type-level change that alters runtime meaning without changing the structural shape. Tightening the fingerprint to carry operand widths is future work; today the gate errs toward over-reporting staleness, not under-reporting it.

= Threat Model and Claim Classes

Concrete's evidence is only useful if its trust boundary is explicit. Every claim in the paper and the artifacts is labeled with one of the classes below, and the strength of a statement is the class it sits in.

#figure(
  table(
    columns: (0.66fr, 1.78fr),
    align: (left, left),
    inset: 4.2pt,
    stroke: 0.35pt + gray,
    table.header([*Class*], [*Meaning and root of trust*]),
    [Enforced], [Safe code cannot violate it and compile — ownership, linearity, capability monotonicity, predictable-profile gates. Trusts the checker; no mechanized soundness proof.],
    [Proved], [Lean kernel verified the theorem over the extracted `PExpr` with a current body fingerprint. Trusts the kernel and the extractor.],
    [Reported], [Compiler-observed but non-blocking: authority traces, allocation, layout, trusted/unsafe boundaries. For audit, not enforcement.],
    [Assumed], [Explicit `assumptions.toml` entries marked `assumed_not_proved`, e.g. machine-level constant-time behavior.],
    [Trusted], [The TCB: compiler extraction and registry plumbing, the Lean kernel, and the LLVM/C backend, runtime, and platform. The backend (Core→SSA→LLVM→binary) is trusted, not proved.],
    [Not claimed], [Verified compiler, verified binary, complete language semantics, full side-channel safety.],
  ),
  caption: [Claim classes and the trust boundary. Every claim below carries one of these labels.],
) <tab-claims>

What "proved" does #emph[not] mean is worth stating plainly: not that the compiled binary is correct (the proof is over `PExpr` with unbounded integers, not the emitted machine code), not that the checker is sound, and not anything about cross-function composition beyond what a stated theorem captures; eligibility is not proof. The checker is exercised by an adversarial suite (1272 trust-gate checks) and the CI proof gate, but not by a mechanized correctness proof.

= What has been tried

The project currently has four graduated showcase entries. We omit `crypto_verify` from the evaluation table because it is explicitly a toy authentication scaffold; it is useful as proof-pipeline scaffolding, but not as evidence that Concrete handles real cryptographic code. The three case studies below are the non-toy graduated entries — every audit bar met, every drift gate green, listed in the showcase manifest.

Counts and theorem names are read from the live registries and `--report proof-status`, not from prose, and each example states its own scope; we reproduce those caveats rather than smoothing them. The evaluation is therefore about #emph[evidence hygiene] as much as theorem strength: can the compiler say what is proved, what is merely enforced, and what is explicitly assumed?

== Constant-time tag comparison

The smallest example makes the thesis sharpest: the code is trivial, but the #emph[security] claim is subtle. `ct_compare` is a fixed-16-iteration OR-accumulate over `[u8; 16]` with no early exit and a single final branch:

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```con
fn ct_compare(a: [u8; 16], b: [u8; 16]) -> i32 {
    let mut diff: u8 = 0;
    for (let mut i: i32 = 0; i < 16; i = i + 1) {
        diff = diff | (a[i] ^ b[i]);
    }
    if diff == 0 { return 1; }
    return 0;
}
```
  ],
  caption: [`ct_compare`: pure (no capabilities), so it is proof-eligible.],
)

The substantive attached theorem is #emph[universal] over all sixteen byte values — equal tags always pass, with all sixteen iterations executed:

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```lean
theorem ct_compare_same_tag_correct
    (b0 b1 … b15 : Int) (fuel : Nat) :
  let tag := PVal.array_ [.int b0, …, .int b15]
  eval ctTagFns ((Env.empty.bind "a" tag).bind "b" tag)
       (fuel + 200) ctCompareExpr = some (.int 1)
```
  ],
  caption: [Universal same-tag theorem, kernel-checked. Backed by helper lemmas (`bitxor_u8_self_zero`, the per-iteration `diff := 0 | (b ^ b) = 0` invariant).],
)

The theorem proves the positive direction. The companion oracle covers the negative direction empirically: 200 randomized cases per seed compare the Concrete implementation against a Python reference, biased toward each per-position byte difference plus equal-tag, multiple-differ, and high-bit cases. The release evidence records which direction the theorem covers and which the oracle covers.

#emph[Honest scope.] Source-level no-early-exit and a fixed loop bound are #emph[necessary] but not #emph[sufficient] for machine-level constant time — LLVM may reintroduce branches, and the CPU leaks through caches and speculation. The example's `assumptions.toml` records machine-level constant time as `assumed_not_proved`. This is not HMAC, not Ed25519, and must not be deployed as a real auth-tag check.

== No-allocation bounded state

A 20-function bounded message validator with a 16-slot ring buffer and an XOR-fold integrity tag, allocation-free; 4 functions carry registry-bound proofs. The central one is a #emph[composition] theorem — push then contains finds the value end-to-end — exercising functional array update (`arraySet`), a bounded `while_step`, and BitVec arithmetic at `i32` width:

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```lean
theorem ring_push_then_contains_correct
    (data_tail : List PVal) (fuel : Nat) :
  eval fixedCapacityFns
    ((Env.empty.bind "rb" (.struct_ "RingBuf"
       [("data", .array_ ((.int 0) :: data_tail)),
        ("head", .int 1), ("count", .int 1)]))
       .bind "val" (.int 0))
    (fuel + 30) ringContainsExpr = some (.int 1)
```
  ],
  caption: [Bounded-state composition theorem. Composes with `ring_push_zero_correct`: the post-push ring shape is exactly this contains-input.],
)

#emph[Honest scope.] The message format is a tiny illustrative shape, not a real protocol; the XOR-fold tag is a structural check, #emph[not] a MAC — an adversary can forge it trivially.

== Bounded header validator

A 10-function pure module that parses an 8-field `i32` header and validates six properties against a closed error taxonomy; 3 functions carry registry-bound proofs. Beyond the per-input contract on the leaf version check and the six-validator #emph[success-direction] composition (on a scalar-parameter helper, under wrapping `i32`), the #emph[failure direction is proved on the real `parse_header`] — which consumes an array and returns enum-tagged errors — by a family of per-variant theorems: `parse_header_too_short`, `…bad_version`, `…bad_type`, `…payload_too_big`, `…truncated`. Extraction therefore already reaches array indexing and enum construction; the residual gap is the #emph[success path] of `parse_header`, which requires extracting the `while`-loop body of `compute_checksum`, and the full combined `iff`, a 256-branch split that exhausts the Lean heartbeat budget. The behavioral oracle is a second implementation in Python agreeing across 600 randomized cases over three seeds — not yet a Lean equivalence.

#text(size: 7.6pt)[
#figure(
  table(
    columns: (0.95fr, 0.46fr, 0.46fr, 1.35fr),
    align: (left, center, center, left),
    inset: 3.2pt,
    stroke: 0.35pt + gray,
    table.header(
      [*Case study*], [*Proved*], [*Funcs*], [*Strongest checked claim*],
    ),
    [`constant_time_tag`], [1], [2], [equal tags always pass, for all 16-byte tags],
    [`fixed_capacity`], [4], [20], [push-then-contains over bounded state],
    [`parse_validate`], [3], [10], [success composition + 5 per-variant error theorems],
  ),
  caption: [Live counts from `--report proof-status`. "Proved" is registry-bound, drift-gated functions; "Funcs" is total functions in the module — most are eligible-but-unproved, as the model predicts ($E(f)$ ⇏ proved).],
)
]

= Why this is not SPARK yet

Concrete sits near the Hoare-logic line of tools: SPARK, Dafny, F\*, and Why3 @hoare1969axiomatic @barnes2012spark @leino2010dafny @swamy2016fstar @filliatre2013why3. Those systems center on contracts and verification-condition generation: a user writes preconditions, postconditions, loop invariants, and data-flow contracts, and the tool generates obligations that must be discharged automatically or interactively. Concrete currently starts one layer lower: it first makes proof attachment, authority reporting, stale-proof detection, assumptions, and release evidence part of the compiler's ordinary outputs.

This is weaker than SPARK in an important way. Concrete does #emph[not] yet generate obligations for every bounds check, division, overflow, loop invariant, or contract. A source-level contract system is on the roadmap, but is not part of the evaluated implementation. The paper's claim is therefore not "Concrete is already a SPARK replacement." The claim is that a small systems compiler can keep the proof/evidence lifecycle tied to the code it compiles, and that this lifecycle is the substrate on which contracts and generated obligations should later sit.

This sequencing is deliberate. If source contracts arrived before drift detection, proof-status classification, and assumption files, they would risk becoming decorative comments with mathematical syntax. In Concrete's intended design, a contract is only useful if it becomes an obligation with a status, a source span, a discharging theorem or assumption, and a place in the release bundle.

= Toward Compiler-Side Soundness

The claim classes above list #emph[extraction] as trusted: nothing stated so far proves that $cal(E): "Core" -> "ProofCore"$ preserves meaning. We report a first compiler-side result and, as importantly, the precise obstacle that bounds it.

The earliest discharged fragment was $cal(C)_0 = {"intLit", "boolLit", "ident", "binOp"}$. For each, extraction agrees with both the extracted-`PExpr` evaluator and a small source-side semantics. The integer-literal instance, proved #emph[against the real extractor], is:

$ cal(E)(#raw(".intLit n")) = #raw(".lit(.int n)") $
$ "eval"_P (#raw(".lit(.int n)")) = "some"(#raw(".int n")) = "eval"_S (#raw(".intLit n")) $

and the binary-operator rule is #emph[compositional] — given the operands' extraction/eval/source facts, the composite reduces to the same `evalBinOp` value across all three views, establishing the pattern later rules reuse:

#figure(
  block(inset: 6pt, stroke: 0.4pt + luma(180), width: 100%)[
```lean
theorem binop_preservation
  (op …) (lhs rhs : CExpr) … (pop) (pl pr : PExpr)
  (h_op : binOpToPBinOp op (CExpr.ty lhs) = some pop)
  (h_lhs : cExprToPExpr lhs = some pl)
  (h_rhs : cExprToPExpr rhs = some pr) :
  cExprToPExpr (.binOp op lhs rhs ty)
    = some (.binOp pop pl pr)
```
  ],
  caption: [The compositional extraction-preservation rule (R-04), proved against the real public extractor.],
)

#emph[The obstacle moved.] The first attempt stopped at a `partial def` opacity barrier: the extractor's mutual block called `mapM` over field, element, and arm lists, and Lean's kernel would not reduce it. That obstacle is now removed for the flagship-used surface: the list traversals were lifted into paired structural recursion, so the public extractor is non-partial over the relevant rules. The current register records extraction/evaluator coverage for literals, variables, binops, let, if, calls, structs, enums, match, casts, arrays, array updates, and flat bounded loops; `while_step` has extraction coverage and a proof-facing evaluator surface, but its full early-break/source-semantics story is still open.

This is still not a verified compiler. Several obligations remain below the line: BitVec operations such as `mod`, `bitxor`, and `bitor` are modeled in Lean, but their LLVM/backend equivalence is still trusted; source-semantics preservation is complete for only the smaller fragments; and the trust gates themselves (fingerprint determinism, spec-drift completeness, coverage classification) still need proof or independent validation. The result we claim is narrower but current: the proved/trusted boundary is now a per-rule matrix, not a single opaque extractor.

= Related Work

#text(size: 7.2pt)[
#figure(
  table(
    columns: (0.9fr, 0.48fr, 0.42fr, 0.7fr, 0.82fr, 0.72fr, 0.62fr),
    align: (left, center, center, center, center, center, center),
    inset: 2.6pt,
    stroke: 0.3pt + gray,
    table.header(
      [*System*], [*Sys*], [*No GC*], [*Src auth*], [*Proof attach*], [*Stale detect*], [*Verified comp*],
    ),
    [Rust], [yes], [yes], [no], [external], [no], [no],
    [Zig], [yes], [yes], [no], [external], [no], [no],
    [Verus/Creusot], [yes], [yes], [no], [external SMT/Why3], [CI re-run], [no],
    [SPARK], [partial], [yes], [contracts], [VCs], [toolchain], [no],
    [Dafny/F\*], [partial], [varies], [effects/contracts], [central], [toolchain], [no],
    [Lean/Coq], [no], [n/a], [n/a], [central], [n/a], [kernel],
    [CompCert], [compiler], [n/a], [n/a], [n/a], [n/a], [yes],
    [Concrete], [yes], [yes], [yes], [compiler registry], [yes], [partial],
  ),
  caption: [Positioning. Concrete's unusual cell is compiler-maintained proof attachment plus stale-proof detection in a no-GC systems language; it does not yet have a verified compiler.],
)
]

The title echoes #emph[proof-carrying code] (PCC) @necula1997pcc and typed assembly language @morrisett1998tal, where a proof or typing derivation travels with a binary and is checked at load time. Concrete differs in #emph[what] carries the evidence and #emph[when]: the evidence is a source-tied, drift-gated attachment maintained by the compiler across edits, not a fixed certificate shipped with a binary. The fingerprint-binding lifecycle and automatic staleness are the contribution, not the proof object.

Verification-first systems languages — F\* and its Low\* subset @swamy2016fstar @protzenko2017lowstar, Dafny @leino2010dafny, Why3 @filliatre2013why3, and SPARK Ada workflows @barnes2012spark — start from contracts and generated proof obligations. Concrete inverts the emphasis: it is systems-first, with proof as an #emph[opt-in attachment] over a validated-Core anchor and a small eligible fragment, accepting that most code is enforced-and-reported rather than proved. CompCert @leroy2009compcert proves the compiler itself; Concrete explicitly does #emph[not], and its contribution is making the unproved-but-evidenced boundary legible and self-invalidating rather than eliminating it — our extraction-preservation result is a single fragment, not a verified pipeline. RustBelt @jung2018rustbelt establishes semantic foundations for Rust's type system externally; Concrete keeps a weaker but in-toolchain notion of currency tied to the exact compiled body.

Among practical systems languages, Rust is the nearest relative (no GC, ownership, explicit `unsafe`), but authority there is inferred from APIs and modules rather than declared in signatures, and semantic inspection is distributed across diagnostics, lints, MIR tools, and external analyzers rather than centered on one report surface. Zig shares the no-hidden-runtime, explicit-allocator ethos but does not treat effect visibility as a first-class surface. C, the baseline these languages react to, leaves authority, effects, and cleanup entirely to convention; it is the audit problem in its starkest form, and also the performance bar Concrete aims to meet (@fig-perf).

The closest active work is the ecosystem for attaching proofs to Rust. Prusti @astrauskas2019prusti adds Viper-based modular specifications; Creusot @denis2022creusot translates Rust's MIR into Why3 and uses prophecy variables to reason about mutable borrows; Verus @lattuada2023verus verifies Rust directly with SMT and linear ghost types; and Kani @kani bounded-model-checks Rust to a fixed unrolling depth. All four express far richer properties than Concrete's eligible fragment, and we make no claim to match their proof power. They differ from Concrete in three respects that mark out this paper's contribution. First, they are external tools layered onto a language and compiler that are themselves unaware of the proof, whereas Concrete makes attachment, eligibility, and staleness ordinary outputs of the compiler that emits the code. Second, none binds a discharged proof to a body fingerprint and revokes it automatically on drift; drift is handled, if at all, by re-running the external tool in CI rather than by the compiler refusing to carry a stale claim. Third, authority and effects are not signature-level facts in Rust, so the capability and trust-boundary half of Concrete's evidence has no direct analogue. Concrete is weaker on proof power and stronger on lifecycle: the compiler, not a satellite tool, is what keeps the evidence current.

Two verified-systems landmarks mark the far end of the spectrum. Cogent @oconnor2016cogent is the closest in spirit — a linear, no-GC systems language whose certifying compiler emits a refinement proof relating the generated C to a functional specification, used for real file-system code. Cogent proves more than Concrete does, but through a specialized pipeline for a restricted language; Concrete keeps a lighter, opt-in attachment in a general-purpose compiler and foregrounds the drift lifecycle instead. seL4 @klein2009sel4, a fully verified OS kernel, is the touchstone for whole-system verification and marks the cost Concrete is not paying: it proves neither its compiler nor its binaries. In cryptography, EverCrypt @protzenko2020evercrypt shows verified, high-performance primitives extracted from F\*; our `constant_time_tag` is a small step toward that domain, honest that it proves equality reasoning rather than the machine-level timing guarantees such work targets. Austral @borretti2024austral demonstrates linear types and capability-secure design for a serious systems language and is closest in spirit; Concrete adds capability signatures, report-oriented workflows, and the validated-Core proof boundary. The Lean 4 kernel and language @demoura2021lean4 are the proof substrate and the implementation language, which is what lets the compiler and the theorem checker share one kernel.

= What Failed, and What It Taught Us

Four things shaped the design, and three of them generalize past Concrete. The `partial def` opacity barrier blocked the first extraction-soundness attempt — Lean would not unfold the mutually recursive extractor — and forced a structural-recursion lift; the lasting lesson is that #emph[proof ergonomics, not logical expressiveness, is the bottleneck], since the binding limits throughout were a heartbeat budget on a 256-branch split and recursion-checker frictions, not missing power. Drift gates caught real attachment bugs early — a missing proof-table callee, hand-written spec mismatches — which is why we treat #emph[automatic revocation as the load-bearing feature]: without it, an attached proof is a comment that ages silently. And the toy `crypto_verify` was too weak to carry a crypto-adjacent story, which forced `constant_time_tag`; more generally, #emph[examples force the proof surface, not the reverse] — `fixed_capacity` forced functional array update and the bounded `while_step`, `constant_time_tag` forced `u8` bitwise reasoning. We grew the provable fragment from forcing examples rather than speculatively.

= Cost of the Discipline

A workflow this explicit is only worth it if the overhead is bearable, so we report what it costs. The #emph[machine] cost is small: across the example and demo corpus the proof layer is roughly 2,300 lines of Lean holding 68 kernel-checked theorems plus the `PExpr` specs they reference, and the extraction-soundness layer adds another ~830 lines. These are checked by the Lean kernel on every `make build`; the reporting and drift commands themselves are cheap, with `--report proof-status` returning in about 0.3 s on these modules. Authority, eligibility, and fingerprint checks are deterministic and piggyback on the ordinary compile.

The #emph[human] cost is the real variable, and it splits in two. Writing a proof ranges from trivial to fiddly: `validate_version` closes in a three-line script (`by_cases` then `simp_all`), while the universal constant-time theorem needs three helper lemmas and a four-million-heartbeat budget to chain its sixteen iterations. Maintaining a proof is where the conservative fingerprint shows its price: because a meaning-preserving structural edit (a commutative reorder, an `x + 0`) flips the status to `stale`, a developer occasionally re-confirms a proof that was never actually broken. We regard that as the right trade — a false `stale` costs a re-check, a false `proved` costs trust — but it is a real friction, and it is the cost a future contract layer would have to keep low to be usable.

= Limitations and Threats to Validity

The evidence is real but bounded. The proved fragment is small (pure, loop-free or bounded-state, integer/bytewise) and per-function; many modules prove a minority of their functions (3/10, 4/20, 1/2). Some theorems prove one direction or fixed cases rather than full contracts; this is now surfaced by proof-coverage classes (`point`, `one_direction`, `iff`, etc.), not hidden behind a single green check. The `parse_validate` oracle is a second implementation, not a Lean equivalence. The "easier to audit" claim is supported by signatures and report outputs but is #emph[not] backed by a controlled user study, and remains the least-defended part of the thesis. The empirical corpus, while non-trivial for an early language, is small relative to a mature ecosystem. Finally, every "proved" claim is contingent on the trusted computing base set out above — most consequentially an unproved backend and a compiler-soundness bridge whose extraction/eval coverage is broader than before but whose source-semantics and trust-gate correctness remain incomplete.

= Future Work

Following the forcing-example discipline: turn the proof-story matrix into a per-program `concrete audit` view; strengthen the compiler-soundness bridge from extraction/evaluator facts toward source-semantics agreement and trust-gate correctness; add SPARK-like runtime obligations for bounds, division, overflow, casts, and loop variants; introduce source-level contracts only when they can generate obligations with statuses rather than decorative prose; close the success-path and combined-`iff` gaps on `parse_header`; pursue a real-crypto candidate (an HMAC-SHA256 or Ed25519-verify subset) to force u32 wrapping add, shifts, rotations, `bitand`, and multi-iteration inductive invariants; and mature packages and the backend contract. Compiler-correctness proofs remain explicitly downstream of a stable extraction surface, not a precondition for the workflow.

= Artifact and Reproducibility

The compiler, the three case studies, the proof registries, the wrong-code corpus, and the spec-drift fixture live in one public repository (`github.com/unbalancedparentheses/concrete2`). `make build` runs the Lean kernel over every attached proof, so a broken proof fails the build. Each number in this paper is regenerable from the command line: `concrete <example> --report proof-status` reproduces the proved-versus-eligible counts, `--report authority` the audit table, `--report proof-bundle` the per-example evidence, and `scripts/ci/proof_gate.sh` the full evidence gate. The drift diagnostic is produced by `tests/programs/adversarial_spec_drift`, and the performance figures by the harnesses under `bench/` and each example's `oracle/`.

= Conclusion

The gap between systems code and machine-checked proof is usually bridged by hand, and the bridge rots without anyone noticing. Concrete takes a different route: a real, Lean-implemented compiler that carries authority, trust boundaries, proof-eligibility, and kernel-checked theorems as ordinary outputs over one validated semantic anchor, and that revokes a proof automatically when the source drifts from what was proved. The implementation is incomplete and the trusted base is large and explicit, but the workflow holds on three graduated case studies whose claims are stated to the exact scope they have earned, and a first extraction-preservation result shows the compiler-side trust boundary is movable, not merely assumed. Concrete is not a verified compiler; it is a compiler that refuses to let its evidence quietly go stale.

#set text(size: 8pt)
#bibliography("refs.bib", title: "References", style: "ieee")
