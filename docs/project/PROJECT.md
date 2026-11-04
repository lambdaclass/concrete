# The Concrete Project

Status: standing reference. This document states why Concrete exists, what it is and is not, the
long-term language shape and the recorded project decisions. The design rules, the admission test
for new features and the list of what Concrete refuses are in [PHILOSOPHY.md](PHILOSOPHY.md); the
normative language rules are in the [language specification](../spec/README.md). Where this
document restates either, those are authoritative.

It replaces, as of 2026-10-29, the separate `IDENTITY.md`, `WHY_CONCRETE.md`, `LANGUAGE_SHAPE.md`
and `DECISIONS.md` in this directory. Its former principles and design policy sections moved to
PHILOSOPHY.md on 2026-11-04.

Contents:

1. [Short positioning](#short-positioning)
2. [Why Concrete](#why-concrete): the overview for C, Rust and Zig developers
3. [Identity](#identity): what Concrete is, what it optimizes for, and what it is not
4. [Philosophy](#philosophy): where the design rules live
5. [Language shape](#language-shape): the long-term structural commitments
6. [Language decisions](#language-decisions): recorded project commitments
7. [Related documents](#related-documents)

## Short positioning

Concrete is a minimalist, no-GC, linear/resource-aware systems language
implemented in Lean 4. It uses scoped second-class references, explicit
capabilities, predictable-execution analysis, and artifact-backed auditability to
make authority, trust, and proof status visible in ordinary systems code.

## Why Concrete

Concrete is not trying to make formal verification feel magical.

It is trying to make evidence hard to lose.

If you come from C, Rust, or Zig, the code shape should look familiar:
fixed-size values, explicit control flow, integer operations, arrays, no garbage
collector, and no runtime trying to hide what the program is doing. The unusual
part is not the surface syntax. The unusual part is that the toolchain keeps a
ledger of what a function is allowed to do, what has been proved, what was only
tested, and what is still trusted.

That ledger is the point of the language.

### Systems Code First

Concrete is a small systems language. It is not Lean with nicer syntax, and it
is not a proof assistant that happens to emit native code.

The baseline assumptions are deliberately ordinary:

- no garbage collector;
- fixed arrays and bounded loops where possible;
- wrapping integer operations when requested;
- explicit heap allocation;
- scoped, second-class safe references instead of returned/stored
  lifetime-bearing reference APIs;
- explicit FFI and unsafe boundaries;
- explicit capabilities for authority such as `Console`, `File`, and `Alloc`.

The goal is not to replace C or Rust everywhere. It is to cover a narrower
case: small, auditable systems code where the cost of being wrong is high enough
that tests alone are not the evidence you want.

### Authority Is Visible

In C, a function can call anything visible through a header. In Rust, effects are
usually a convention: a function's type tells you a lot about ownership, but not
whether it can read files, print, allocate, or cross a process boundary.

Concrete makes authority part of the function boundary:

```con
fn parse_byte(data: Int, offset: Int) -> Int {
    return data + offset;
}

fn report(result: Int) with(Console) {
    if result == 0 { println("ok"); } else { println("fail"); }
}
```

`parse_byte` has no ambient authority. It cannot write to the console, open a
file, allocate through `Alloc`, or call a hidden network API unless its signature
says so through capabilities.

That is less flexible than C. It is also easier to review.

### Visible Is Step One; Knowable Is the Point

Visibility alone is reporting. The deeper consequence of Concrete's refusals is
what they make computable: with no closures, no trait objects, no macros, and
whole-program monomorphization, callable values come from a closed set of named
functions, and indirect calls carry value identity rather than a name to
re-resolve. A function-pointer target may still be chosen at runtime, but its
possible targets are statically enumerable.

That gives per-function facts—authority, allocation, failure modes,
obligations—a tractable path to conservative whole-program facts from `main`
outward. A sound analysis must retain every possible target of an indirect
call, and today's reports do not yet turn every such edge into a complete
composition theorem. The advantage is narrower and still valuable: the
possible callable set is finite and source-visible rather than extended by
captured code, runtime type dispatch, or generated source.

Today `--report authority` and `--report caps` expose part of this structure,
while capability-bearing function-pointer types enforce authority at indirect
call sites. The direction of the proof work is to make target-set completeness
and transitive composition checked whole-program claims. That step is roadmap
work, not a shipped feature: R-0443 owns the first narrow, independently checked
authority-path property and explicitly leaves effectful functional correctness
and native behavior outside its claim.

### Evidence Is Visible

Most codebases have evidence, but it is scattered:

- unit tests in one directory;
- fuzzers somewhere else;
- informal invariants in comments;
- proof scripts, if any, outside the language;
- assumptions in issue trackers or people's heads.

Concrete's audit surface keeps those categories separate. A claim can be:

- `proved_by_lean` - checked by the Lean kernel;
- `proved_by_kernel_decision` - closed by a kernel-checked decision procedure
  such as `omega` or `bv_decide`;
- `proved_at_callsite` - checked from a caller's concrete arguments;
- `runtime_checked` - guarded dynamically in a gradual mode;
- `tested_by_oracle` - tested against a reference implementation;
- `assumed` or `trusted` - accepted as a named boundary;
- `missing` or `stale` - not currently valid evidence.

This is intentionally not one green badge. A C developer knows the difference
between "covered by a test" and "guaranteed by the type checker." Concrete keeps
that distinction in the report.

### Proofs Do Not Silently Drift

The common failure mode for external verification is synchronization. The code
changes; the spec or proof still exists; the report still looks comforting.

Concrete's strongest proof path ties the theorem to a stored digest of the
extracted source body. The compiler extracts a ProofCore expression, records a
body fingerprint, and checks that the registered proof is still attached to
the same body shape. A source link with no stored fingerprint is `unbound`, not
proved; a changed stored body becomes stale.

That is useful containment, not yet a complete semantic-subject digest. The
current hash does not cover every signature/type fact, source contract, or
transitive callee. Bugs 059/060 and R-0004 make those limits explicit rather
than letting “fresh” overclaim.

That is why the HMAC-SHA256 flagship matters. Its proof is not "there is a
handwritten model that looks like the implementation." The shipped claim is:

1. the Concrete source body extracts to this ProofCore body and has a stored
   fingerprint;
2. this ProofCore body refines an independent SHA-256/HMAC spec;
3. the named theorems replay through the Lean kernel;
4. if the represented body drifts, the claim turns stale.

That is the core thesis in one example.

### Concrete Can Prove Real Code Now

The current real proof artifact is HMAC-SHA256. The chain covers:

- block-to-word packing;
- SHA-256 schedule expansion;
- compression rounds;
- state serialization;
- padded multi-block hashing;
- outer HMAC composition.

The proof is kernel-checked and spec-drift-tied. It is also bounded and honest:
the README and audit files state the input bounds, the trusted backend boundary,
and the remaining assumptions.

The useful result was not only the HMAC theorem. The proof forced reusable
infrastructure out of the project:

- loop induction over extracted `while` programs;
- array get/set frame lemmas;
- bitvector automation;
- function-table and call-wrapper scaffolding;
- refinement theorem patterns.

That reusable layer is now `Concrete/ProofKit`.

### Trust Is Named

Concrete is not pretending the whole stack is proved.

Today:

- the Lean kernel checks proof terms;
- the Concrete compiler is written in Lean, but not fully verified;
- the source-body-to-ProofCore tie is checked by a stored fingerprint and the
  current spec-drift mechanism, with the full-subject gaps tracked in R-0004;
- LLVM, clang, the linker, libc, and the host kernel remain trusted;
- machine-level timing claims remain assumptions unless a lower layer checks
  them;
- unprofiled floating point is excluded from proof eligibility.

This is not a weakness to hide. It is the project rule: every claim must be
proved, enforced, tested, reported, assumed, or trusted. No vague middle.

### Contracts Are The Authoring Surface

Handwritten Lean proofs are too expensive to be the normal user interface.

The direction is source-level evidence:

```con pseudocode
spec fn ch_spec(x: u32, y: u32, z: u32) -> u32 {
    (x & y) ^ ((~x) & z)
}

#[ensures(result == ch_spec(x, y, z))]
fn ch(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ ((~x) & z);
}
```

The source states the claim. The compiler reports the obligation. The evidence
backend says how it was discharged: Lean, a kernel decision procedure, a
call-site check, a runtime check, or an explicit assumption.

Loop contracts follow the same principle:

```con pseudocode
#[invariant(0 <= i && i <= 8)]
#[variant(8 - i)]
while i < 8 {
    set i = i + 1;
}
```

This is closer to SPARK or Dafny ergonomics than to "write a proof assistant
program in your source file." There are no tactics in `.con` files.

### What C Developers Get

Compared with C, Concrete gives up some freedom:

- no ambient authority;
- no untracked pointer tricks in ordinary code;
- no implicit unchecked resource ownership;
- no silent proof/evidence claims.

In exchange, it gives you compiler-visible boundaries:

- which functions allocate;
- which functions can perform I/O;
- which code is trusted;
- which functions have no external authority (and what mutation their parameters allow);
- which loops are bounded or have obligations;
- which claims are proved, tested, assumed, or stale.

The intended feeling is not "C with a theorem prover glued on." It is "small C
style systems code where the audit trail is part of the toolchain."

### What Rust Developers Get

Rust already gives strong memory safety and an excellent ownership model.
Concrete is not trying to beat Rust at ecosystem maturity, async libraries, or
general-purpose ergonomics.

The difference is the evidence surface.

Rust's type system can tell you a lot about borrowing, lifetimes, and data
races. Concrete is trying to make different facts visible:

- this function needs exactly these capabilities;
- this proof is tied to this source body;
- this loop invariant was checked by this engine;
- this crypto implementation refines this independent spec;
- this assumption is still in the trusted base.

Concrete's linear ownership is simpler and less expressive than Rust's borrow
checker. That is deliberate. The project is trading some generality for a
smaller proof and audit story.

### What Concrete Is Not Trying To Be

Concrete should resist features that make ordinary systems code harder to audit:

- no row effects for now;
- no dependent programs in source;
- no hidden proof DSL;
- no general macro/comptime system yet;
- no graded modal type system in v1;
- no implicit async execution;
- no proof-directed optimization before the proof story is ergonomic.

Those ideas may be useful in other languages. Concrete's filter is stricter:
does the feature make authority, resource use, trust, and evidence more visible
to a reviewer?

If not, it waits.

### The Short Version

Concrete is for the narrow case where all of these matter at once:

- systems-shaped code;
- no garbage collector;
- explicit authority;
- predictable resource boundaries;
- a closed, statically enumerable callable set, so conservative program-wide
  analysis has a finite target space;
- kernel-checked proofs for selected functions;
- proof drift detected by the toolchain;
- honest reporting of what is still trusted.

If you want the broadest production systems language, use Rust.

If you want direct low-level control with a mature toolchain, use C or Zig.

If you want small systems code where the evidence ledger is part of the
language, Concrete is the experiment.

## Identity

This section states what Concrete is, what it optimizes for, and what it is not.

### What Concrete Is

A small systems language for evidence-carrying software.

Concrete is written in Lean 4 and is designed so capability requirements, predictable execution, resource risk, trust boundaries, and proof evidence can become compiler facts. It is not a proof assistant; it is the no-GC systems language that Lean 4 reasons about.

The bet is that for high-consequence code (firmware, security boundaries, safety-critical components), being able to answer "what authority does this module have?", "how predictable is this core?", "which assumptions are trusted?", and "which claims are proved?" matters more than having a large ecosystem or maximal expressiveness.

The deeper thesis is that Concrete should make four things explicit enough to audit, restrict, and prove:

1. authority
2. operational behavior
3. trust boundaries
4. evidence scope, method, status, and trust

That is the sense in which Concrete is trying to make operational power explicit, not just memory safety or proof objects in isolation.

Concrete is not "Lean without GC." The intended shape is narrower and more practical:

1. a minimalist systems language rather than a theorem-prover surface
2. linear/resource-aware ownership instead of a hidden runtime memory model
3. explicit capabilities and trust markers at boundaries
4. predictable core versus effectful shell as a normal design pattern
5. Lean 4 as both the implementation language of the compiler and the proof environment that can attach real theorems to Concrete code

The design rule is small analyzable core over feature growth. If a feature makes behavior less visible, muddies boundaries, or weakens the proof/audit story, it is suspect by default.

Concrete is not trying to stop at "the compiler can print interesting reports." The intended workflow is artifact-first:

1. the compiler emits stable facts, evidence, proof, and traceability artifacts
2. teams enforce policies from those artifacts
3. CI gates and AI tooling consume those artifacts directly
4. reviewers answer audit questions from those artifacts without reading compiler internals

If Concrete cannot support that workflow, the thesis is only partially successful.

That artifact-first workflow matters because Concrete is trying to be useful for real systems review, not just language research. The compiler should act like an audit machine over ordinary systems code:

1. what authority does this function or module have?
2. is this core predictable or not?
3. what is enforced, what is reported, what is proved, and what is merely trusted?
4. what changed since the last reviewed version?

Concrete is designed for humans first, not for LLMs. But the same explicitness also makes it unusually friendly to LLM-assisted coding, review, and optimization: authority, predictability, trust boundaries, and evidence are surfaced as compiler facts and queries instead of being left implicit in source code.

Short version:

1. Rust makes memory-safety discipline explicit.
2. Zig makes low-level control explicit.
3. SPARK/Ada makes specifications, obligations, and assurance workflow explicit.
4. Lean 4 makes proof a practical implementation and theorem-proving environment.
5. Concrete is trying to make operational power and evidence explicit in native systems code.

### Positioning Against Nearby Tools

Concrete's claim is not that no other tool can prove programs, write systems
code, or enforce safety. The claim is that these pieces are usually separated.

- **Rust / Zig / C / C++** are strong systems languages. Their proof story is
  usually external: tests, fuzzing, sanitizers, static analyzers, model
  checkers, or separate proof tools. The compiler does not normally maintain a
  source/spec/theorem attachment and downgrade evidence when it drifts.
- **SPARK / Ada, Dafny, F*, Why3** have mature verification workflows, but their
  center of gravity is high-assurance verification rather than a small
  C/Rust/Zig-shaped systems language implemented in Lean.
- **Lean / Coq / Isabelle** are excellent proof systems. They are not primarily
  normal low-level systems languages with explicit FFI, layout, authority, and
  no-GC runtime boundaries.
- **Austral** is close on linearity and capability-shaped safety, but it does
  not provide the same Lean-backed proof/evidence pipeline.

The intended Concrete workflow is: write ordinary systems-shaped code, keep
authority and trust visible, attach selected Lean theorems, and let compiler
artifacts catch body/spec/proof drift.

For the explicit map of what Concrete copies, adapts, or rejects from other
languages, see [INFLUENCES.md](INFLUENCES.md).

### The Three-Way Trust Split

Most systems languages have one escape hatch. Rust has `unsafe`. C has... everything. Concrete splits trust into three orthogonal mechanisms:

| Mechanism | What it covers | Visibility |
|-----------|---------------|------------|
| **Capabilities** (`with(Fs, Network)`) | Operational permissions visible to callers | In function signatures |
| **`trusted`** | Pointer-level and foreign-call implementation unsafety behind safe APIs | At declaration site only |
| **`with(Unsafe)`** | A caller safety obligation the language cannot establish (raw and foreign validity) | In function signatures; a `trusted` body absorbs it |

**Why this matters:** In Rust, `unsafe` covers raw pointer arithmetic, FFI calls, and transmute with identical syntax. You can't distinguish "contained implementation unsafety behind a safe interface" from "this function crosses a foreign boundary." Concrete makes these syntactically and semantically different, and the compiler reports on them separately.

**What `trusted` permits** is [TRUST-5](../spec/trusted.md#TRUST-5): raw-pointer operations and
foreign calls without declaring `Unsafe`. It absorbs `Unsafe` and nothing else: every operational
capability it uses still appears in its signature, and it does not relax linearity.

**Capabilities** are the reserved names of
[authority rule 13](../spec/decisions.md#decided-2026-11-02-authority), with no umbrella `Std`
and no aliases. A function can only call functions whose capabilities are a subset of its own.

### The Compiler as Audit Machine

The compiler doesn't just accept or reject programs. It answers questions about them:

- `--report authority` — per-capability function lists with transitive call-chain traces. "Why does `serve` need `File`? Because `serve → log_request → append_file`."
- `--report unsafe` — trust boundary analysis: how many trusted functions, extern functions, unsafe crossings
- `--report alloc` — allocation and cleanup summaries, leak warnings
- `--report proof` — which functions satisfy the current ProofCore eligibility gates (authority-free, not trusted, no extern calls, supported body)
- `--report layout` — struct/enum sizes, alignment, field offsets
- `--report mono` — what monomorphized code actually exists
- `--report interface` — public API surface
- `--report caps` — per-function capabilities
- `snapshot` / `diff` / semantic queries — stable artifacts for CI, review, and AI tools

These are structured compiler outputs, not linting. They derive from the same semantic analysis that type-checks the code. The goal is that a reviewer can understand a Concrete program's authority structure without reading the implementation.

### The Proof Story

The compiler is written in Lean 4. This enables two layers of proof:

**Layer 1 — prove the compiler:** type soundness, ownership/linearity coherence, capability/trust rule preservation, Core-to-SSA lowering correctness. These are proofs that the language rules work as intended.

**Layer 2 — prove user programs:** through formalized Core semantics, a Concrete function's behavior can be stated and proved as a Lean theorem. A hash function written in Concrete (real systems language, real FFI, real memory layout) could be proved correct in Lean (real theorem prover), with a well-defined semantic bridge between them.

The architecture keeps proof tooling separate from compilation. The compiler
produces stable artifacts (`ValidatedCore`, `ProofCore`); proof tools consume
them. `ProofCore` is the authority-free functional subset: eligibility also
requires no trusted/extern boundary and an entirely supported body. An empty
capability set alone does not imply totality, termination, or absence of traps.

The implemented proof surface includes kernel theorems over the evaluator and
source-linked proofs for selected stdlib and flagship functions. Exact coverage
belongs in generated proof/evidence reports and `PROVABLE_V1.md`, not a
hand-maintained theorem count in this stable identity document.

The important distinction is:

1. Concrete is implemented in Lean 4
2. selected Concrete functions can already carry Lean 4-backed proofs
3. the long-term goal is to enlarge the useful provable subset without turning ordinary systems programming into theorem-prover syntax

### Thesis-Level Direction

Concrete's long-term claim is the combination of:

1. visible authority at the function boundary
2. reportable and enforceable operational behavior such as allocation, blocking, recursion, and boundedness
3. explicit trust boundaries
4. proof-backed evidence tied to compiler artifacts

The normal program shape this pushes toward is:

1. a small authority-free or tightly-bounded core
2. an effectful shell at the edge
3. explicit authority and trust markers at the seam
4. artifacts that make the seam visible to reviewers and tooling

If that combination works, a reviewer can ask far more precise questions about a function than most mainstream systems languages make practical.

Concrete's vision is only validated if this remains honest and usable in practice:

1. one flagship example demonstrates the full thesis end-to-end
2. bad changes cause visible drift in authority, predictability, trust, or proof status
3. a second example in a different domain also fits the model
4. ergonomics and performance stay acceptable for the target systems use case
5. reports state clearly where claims are enforced, analysis-only, proved, or still trusted at the backend/toolchain boundary

### Biggest Remaining Multipliers

The biggest remaining multipliers for Concrete are not random language features.
They are the parts that most increase the value of the thesis:

1. a stronger flagship systems example with real Lean-backed proof
2. an explicit `ProofCore` boundary as a real compiler artifact
3. module/package policy strong enough to enforce architecture
4. a usable bounded-capacity predictable subset between `NoAlloc` and unrestricted allocation
5. trust-drift and CI evidence gates as normal workflow
6. an AI-native fact interface over stable compiler artifacts
7. later, proof-aware package artifacts so dependencies can ship evidence and policy data as part of the build story

Those are the parts most likely to make Concrete feel qualitatively different
from ordinary systems languages rather than merely more explicit.

### Research Directions

These are the most developed ideas in [research/](../../research). None are implemented yet, but each is grounded in the current compiler architecture.

The current thesis-validation center of gravity is:

1. [../research/thesis-validation/core-thesis.md](../../research/thesis-validation/core-thesis.md)
2. [../research/thesis-validation/objective-matrix.md](../../research/thesis-validation/objective-matrix.md)
3. [../research/thesis-validation/thesis-validation.md](../../research/thesis-validation/thesis-validation.md)

Those notes define the experimental target more directly than the older broad research notes do.

For the narrower question of which verification-language ideas actually fit
Concrete's philosophy, see
[../research/proof-evidence/verification-surface.md](../../research/proof-evidence/verification-surface.md).

#### Authority Budgets

Capabilities tell you what each function requires. Authority budgets extend this to modules and packages as enforceable constraints:

```con pseudocode
#[authority(Alloc)]
mod Parser {
    // Any function here that transitively reaches Network or File
    // is a compile error
}
```

At the package level: "dependency `json_parser` may only use `Alloc`." If the next release adds logging, the build fails. The transitive capability set is already computed by `--report authority`; budget checking is set containment.

This is unusual among mainstream systems languages. Rust, for example, has no mechanism to declare that a crate is limited to specific side effects.

See [../research/packages-tooling/authority-budgets.md](../../research/packages-tooling/authority-budgets.md).

#### Allocation Budgets

`with(Alloc)` is currently binary. The proposal adds gradations: NoAlloc (already works), Bounded (allocates but provably bounded), Unbounded (current default). The first step is classification by call-graph analysis — no language change needed.

The intended direction is more compositional than the usual whole-program or whole-crate story. Ada/SPARK have pool-level bounds; Rust gets `no_std` by convention. Concrete's proposed version is per-function and compositional.

See [../research/stdlib/allocation-budgets.md](../../research/stdlib/allocation-budgets.md).
R-0445 owns the first report-level allocation count/byte facts; later
obligation and profile tasks decide enforcement. Resource quantity is not
itself an authority capability.

#### Execution Cost Tracking

Structural classification of functions: bounded or unbounded loops, recursive
or not, max static call depth. For bounded functions, abstract instruction
counts or another conservative source-cost model. Concrete is unusually
tractable for this: no trait-object dispatch or closures, an enumerable set of
named callable values with explicit indirect edges, no hidden allocation, and
clean SSA CFG. R-0445 owns the early source certificate; R-0405 retains
hardware WCET.

See [../research/stdlib/execution-cost.md](../../research/stdlib/execution-cost.md).

#### Semantic Diff and Trust Drift

Diff trust-relevant properties across two versions — not source text, but semantic facts. Authority changes, new trusted boundaries, allocation shifts. The compiler already computes all these facts; semantic diff is structured comparison of report outputs.

See [../research/compiler/semantic-diff-and-trust-drift.md](../../research/compiler/semantic-diff-and-trust-drift.md).

#### Proof Addon Architecture

Proof tooling as a separate consumer of compiler artifacts, not fused into compilation. SMT discharge, symbolic execution, and Lean export would all work from the same stable `ValidatedCore`/`ProofCore` artifacts. This avoids making proof failure a compile failure.

See [../research/proof-evidence/proof-addon-architecture.md](../../research/proof-evidence/proof-addon-architecture.md).

### Where Concrete Fits

Concrete is not trying to replace Rust, Zig, or C for general-purpose systems programming. Its case is narrower: software that must be small, explicit, reviewable, and honest about power.

Target use cases:

- boot, update, and artifact verification tools
- key-handling and cryptographic policy helpers
- safety/security guard processes with tightly bounded behavior
- industrial control safety interlocks, medical-device policy kernels
- audited wrappers around critical C libraries or hardware interfaces
- cross-domain or high-assurance data-release policy engines

#### Competitive Stance

**vs. Rust:** not competing on ecosystem or macro power; competing on auditability, explicit authority, and trust boundaries.

**vs. Zig:** shares low-level explicitness, but pushes harder on ownership, capability tracking, and proof structure.

**vs. verification languages (F\*, SPARK/Ada):** wants the assurance discipline — specs, obligations, proof, trusted assumptions — while keeping low-level runtime, FFI, layout, explicit capabilities, and ownership first-class.

**vs. Lean:** Concrete is not a proof assistant. It is the low-level language that Lean 4 reasons about, and Lean 4 is also the implementation language of the compiler.

The intended relationship is:

1. Lean remains the proof language
2. Concrete remains the systems language
3. the interesting question is how much low-level, no-GC systems code can still carry useful proof-backed evidence

### Non-Goals

- feature-count competition
- hidden semantic behavior keyed off public names
- cleverness that makes auditability harder
- large convenience surfaces inside the compiler
- treating ecosystem size as more important than semantic clarity

## Philosophy

The design rules, the proposed order for settling conflicts between them, the admission test every
proposed feature passes and the list of what Concrete refuses are in [PHILOSOPHY.md](PHILOSOPHY.md). They
were this document's "Principles" and "Design policy" sections until 2026-11-04.

## Language shape

This section makes Concrete's long-term language shape decisions explicit instead of letting them
emerge from local convenience. It draws the commitments of the sections above,
[PHILOSOPHY.md](PHILOSOPHY.md) and [SAFETY.md](../verification/SAFETY.md) into a
single picture of what Concrete is becoming. The rules it commits to are stated in the language
specification, [docs/spec](../spec/README.md).

### The Language Concrete Is Trying To Be

A small, low-level language where:

- Every function's authority is visible in its signature
- Every resource has a clear owner and destruction point
- Every call target is either statically known or explicitly indirect
- Every trust boundary is marked and auditable
- The compiler is structured enough to eventually prove properties about programs

The target is not "systems programming for everything." It is software that must be small, explicit, reviewable, and honest about power.

### Structural Commitments

These are not aspirations. They are load-bearing constraints that current code depends on.

#### Dispatch model: static or explicitly indirect

All dispatch is monomorphized (compile-time) or through typed function pointers (runtime, visible). No closures, no trait objects, no implicit vtables. The call graph is either fully known at compile time or the indirection is visible at every call site.

**Depends on:** No closures, no trait objects decisions.
**Enables:** Static analysis, formal verification, predictable codegen.

#### Authority model: capabilities in signatures

Semantic effects (file, network, allocation, etc.) are declared in function signatures via `with(Cap)`. A function can only call functions whose capabilities are a subset of its own. This is checked at both surface and Core IR levels.

**Depends on:** Capability checking in Check and CoreCheck.
**Enables:** Authority reports, proof eligibility analysis, high-integrity profiles.

#### Trust model: three-way split

- `with(Cap)` — operational permissions visible to callers
- `trusted` — audited pointer-level and foreign-call containment behind safe APIs
- `with(Unsafe)` — a caller safety obligation (raw and foreign validity), absorbed by a `trusted`
  body ([TRUST-5](../spec/trusted.md#TRUST-5))

Each concept does one thing: `trusted` never discharges an operational capability. See
[SAFETY.md](../verification/SAFETY.md).

#### Ownership model: linear by default

All structs and enums are linear (must be consumed exactly once) unless marked `Copy`. A linear variable cannot be *overwritten while still live* (E0219); reassigning one whose old value has already been consumed is legal and is the ordinary fold/accumulate pattern (`acc = f(acc, x)`, including inside loops — see [OWNERSHIP_MODEL.md](../archive/design-records/OWNERSHIP_MODEL.md) "Linear rebind"). Branches must agree on consumption. Loops cannot consume linear variables from outer scope. Resources have deterministic, explicit destruction: the compiler rejects an unconsumed live value instead of silently auto-dropping it at scope exit. Generated recursive drop glue is allowed only inside an explicit owner-consuming operation such as `x.drop()` / `defer x.drop()`.

**Depends on:** Linearity checking in Check.
**Enables:** Resource safety without GC, deterministic cleanup.

#### Compilation model: whole-program monomorphization

Generics are resolved at compile time. No separate compilation of generic code. The output is concrete, specialized functions with no runtime type information.

**Depends on:** Monomorphization pass.
**Enables:** Predictable codegen, no hidden runtime, proof-friendly Core IR.

#### Phase separation: parse → resolve → check → elab → core → SSA → LLVM

Each pass has a clear input type, output type, and set of invariants. Early passes do not depend on late semantic information. Cross-file dependencies are declaration-level only (summaries and signatures).

**Depends on:** Pipeline architecture.
**Enables:** Incremental compilation (future), clear diagnostics ownership, proof extraction at Core level.

### What Concrete Will Not Become

The standing refusals, each with its reason and the roadmap condition under which it reopens, are
in [PHILOSOPHY.md, What Concrete refuses](PHILOSOPHY.md#what-concrete-refuses).

## Language decisions

This section records project commitments that shape the language and its evidence. The refusals
("no" and "not yet") that used to be listed here are in
[PHILOSOPHY.md, What Concrete refuses](PHILOSOPHY.md#what-concrete-refuses); the language rules
the owner decided are in [decisions.md](../spec/decisions.md).

### Recorded commitments

#### Evidence is multidimensional, not a total ladder

**Status:** Decided (2026-07-25); implementation pending
[ROADMAP R-0440](ROADMAP_TASKS.md#task-r-0440).

Evidence is represented by orthogonal facts: subject, claim kind, semantic
scope, method, status, assumptions, producer, validator, trusted dependencies,
freshness, and replay receipt. A broad native oracle test and a narrow
kernel-checked ProofCore theorem are not globally rankable because they establish
different claims over different scopes.

A policy may prefer one discharge method over another only after subject and
scope are comparable. Friendly composite labels such as `proved_by_lean` remain
useful views, but they may not erase the underlying dimensions or imply that a
narrower claim dominates broader evidence merely because its method is called a
proof.

#### Source-level resource certificates land early; hardware time stays scoped

**Status:** Decided (2026-07-25); implementation pending
[ROADMAP R-0445](ROADMAP_BACKLOG.md#task-r-0445).

Concrete will expose conservative per-function source-resource bounds before
broad stdlib expansion. The first certificate covers symbolic source steps,
call depth, source-model stack, allocation count/bytes, and the exact boundary
that makes any component unknown. It is built from shared compiler facts and
tested first on the parser-combinator workloads, so the language's restrictions
pay an immediate, inspectable benefit and can influence API design while the
surface is still small.

The certificate's semantic scope is `source`. It is not elapsed time, cycles,
native stack size, backend cost preservation, or WCET. Later obligation,
target-stack, measurement, and fixed-hardware phases strengthen or relate the
same facts without relabeling their scope. An honest `unknown` with a specific
blocker is a valid result; fabricating a bound or inferring hardware performance
from the source model is not. The source cost/layout model is versioned and
named in every certificate so changing what a “step” or source frame means
cannot silently preserve old evidence.

#### Predictable generated-code performance is an affirmative project claim

**Status:** Decided (2026-07-25); evidence machinery pending
[ROADMAP R-0416–R-0419](ROADMAP_BACKLOG.md#task-r-0416).

Concrete intends to make workload-scoped, replayable claims about generated-code
time, size, memory, and optimization stability. That commitment does not
authorize an unqualified “fast” badge or a comparison without a matched workload,
input, backend, target, toolchain, metric, uncertainty, raw samples, and replay
command. Before R-0416–R-0419 land, the honest status of an individual
comparison remains `performance_not_claimed`.

The decision is about maintaining the evidence capability, not pre-approving a
result. Optimization remains pull-gated by a reproduced gap or regression.
Retiring the project-level performance claim later would require retiring
R-0416–R-0419 and removing the corresponding public language together; dead or
informational benchmark scripts cannot substantiate it.

#### Trusted means pointer containment only

**Status:** Decided (2026-03-15); the current rule is [TRUST-5](../spec/trusted.md#TRUST-5), with
the [authority rules](../spec/decisions.md#decided-2026-11-02-authority).

`trusted` is an audited containment boundary for pointer-level operations and foreign calls behind
a safe API. It absorbs `Unsafe` and nothing else: it never hides an operational capability and never
relaxes linearity. See [SAFETY.md](../verification/SAFETY.md).

### How to add a new decision

1. Write a research note in `research/` with the full analysis (what, why, alternatives, precedent).
2. A language rule is an owner decision: it is recorded in [decisions.md](../spec/decisions.md).
3. A refusal is added to [PHILOSOPHY.md](PHILOSOPHY.md#what-concrete-refuses), Permanent with its
   reason or with the roadmap condition under which it reopens.
4. Any other project commitment is added to this section with its status, rationale summary and
   research link.

## Related documents

- [SAFETY.md](../verification/SAFETY.md): the three-way trust/capability/unsafe model
- [PROVABLE_SUBSET.md](../verification/PROVABLE_SUBSET.md): the proof-eligible subset definition
- [INFLUENCES.md](INFLUENCES.md): what Concrete copies, adapts or rejects from other languages
- [ABSENCE_IS_NOT_A_FACT.md](ABSENCE_IS_NOT_A_FACT.md): the gated rule that an unknown value never
  defaults to a reassuring one
- [STYLE.md](STYLE.md): naming, structure and the traversal forms for writing Concrete code
- [AGENTS.md](AGENTS.md): how work is done in this repository (playbooks, branches, validation,
  review, concurrent work)
- [EXAMPLES.md](EXAMPLES.md): the example inventory, promotion levels and the no-duplicate rule
- [../research/meta/design-filters.md](../../research/meta/design-filters.md): the original research
  note behind the design policy
- [../research/language/high-integrity-profile.md](../../research/language/high-integrity-profile.md):
  the high-integrity profile direction
