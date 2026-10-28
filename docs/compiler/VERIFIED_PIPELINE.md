# Verified-compiler pipeline

Status: planned, 2026-10-18. Owners: R-0491 – R-0514 in [ROADMAP.md](../../ROADMAP.md), with
R-0299, R-0300, R-0353, R-0413 and R-0460 linked where named. The architecture reconciles two
independent designs (a Claude design agent and Codex) and four Codex reviews of earlier drafts (42
findings, all applied).

## Goal

The best achievable assurance, whatever the duration: the guarantee CompCert and CakeML give — a
machine-checked theorem that the compiler preserves the language's meaning, so supported Core claims
transport to the corresponding native image through proved transport lemmas, under the recorded
environment, resource and trust premises. Concrete is written in Lean, so this is reachable in place.

**Part 1 establishes each semantic interface and states its obligations before the dependent
implementation is frozen.** Each new boundary includes a representative machine-checked proof
exercising the feature that motivates it, so the hard proof boundaries are tested early. It pays for
itself immediately: each semantics is also a differential oracle, each typed artifact removes a bug
class. **Part 2 extends these proofs to the complete admitted profile.** Avoiding later
restructuring is a design objective tested by Part 1's representative proofs, not a promise.

## What "no bugs" means here

No engineering process makes a compiler free of every bug. A verified compiler does something
stronger and exact: it turns "we tested it" into "it is a theorem", for named bug classes, relative
to a written specification. At the end of Part 2:

| bug class | status at the end | by |
|---|---|---|
| **Native miscompilation relative to checked Core** | excluded for the admitted native profile under recorded contracts, resource premises and the explicit observation relation | S15 refinement and transport theorems |
| **Ownership and memory violations** — use after move, double destroy, leak at a normal or structured exit, dangling or escaped reference, undeclared capability use | excluded in safe Core under satisfied trusted/FFI contracts; native memory validity additionally requires its preserved representation invariant; fatal exits and divergence have the stated reclamation limits | S14, S15 |
| **False evidence binding or interpretation** — "proved" for code the theorem is not about, or for a claim it does not state | excluded where statement generation, subject/dependency binding, evidence checking and verdict interpretation have proved soundness; remaining implementation and logical assumptions are recorded transitively | S2, S9, S16 soundness obligations |
| **Duplicated proof/interpreter meanings** | eliminated by sharing Core's normative evaluator; IO-driver, runtime, compiled-evaluator and native agreement remain separate obligations | S7, S15, the trusted-base inventory |
| **Source miscompilation by the front end** (`return 1` elaborated to Core returning 2) | excluded once parsing, resolution, desugaring and elaboration are proved against independently specified judgments; until then the guarantee begins at retained `CheckedCore` | S15 frontend correspondence |

What remains, stated rather than hidden, and how each is contained:

- **The specification can be wrong** — Core's semantics or a rule may not say what we intend. This
  is the one class proof cannot remove. Contained by: the written rules with accepted/rejected
  examples (S4), the semantics being executable and differentially tested against the binary and
  the corpus from the day it exists (S1, S7), small and reviewable definitions, Codex review, and
  the scheduled hunts.
- **The trusted base** (kernel, ISA model, OS contracts, resource premises, and for the LLVM profile
  LLVM and the system toolchain) — listed, minimized, and re-checked where possible (con-leche and
  nanoda for the kernel; hardware differential tests for the ISA model).
- **The front end** — until frontend correspondence is proved, the verified guarantee begins at
  retained `CheckedCore`: parsing, resolution, desugaring or elaboration may translate the surface
  program incorrectly. S15 proves successful frontend execution implements independently specified
  syntax, resolution and elaboration judgments, then composes that with Core-to-native correctness.
- **Bugs in `std` and trusted code relative to their contracts** — each trusted implementation is
  verified against its contract or listed with its audit (S16), and claims reaching it say so.
- **Programs that do what they say, but not what the author meant** — outside a compiler's reach;
  the claim and contract machinery is how a user states and proves what they meant.
- **Outside semantic preservation entirely:** compiler availability, acceptance of every valid
  program, diagnostic and debug-information accuracy, performance, constant-time behaviour and
  arbitrary security hyperproperties need their own specifications and gates.

Part 1 adds structural rejection and measured fault detection. Differential tests, mutation
campaigns and hunts provide evidence for exercised cases; they do not establish universal absence
of these bug classes. That is Part 2's job.

## Why the current pipeline cannot get there

The 2026-10-18 hunts reproduced fifteen defects (081 – 095) after 079 and 080. They trace to the
structure, not to individual functions:

| structural fact (measured 2026-10-18) | defects |
|---|---|
| The ownership/borrow/capability checker runs on the source tree and returns `Unit` (`Pipeline.lean` ~:113); elaboration rebuilds the program independently. The program checked is not the program compiled. | 087's borrow-block use-after-free, 090, 092, 093, 094 |
| The language's meaning is implemented several times: `Proof.eval`'s `PExpr` (unbounded `Int`, transparent casts, own match and statement rules), the interpreter, the SSA folder, and the emitter, which still decides semantics by type (`EmitSSA.lean` ~:636, ~:661 – 676). | 080, 084, 085, 086, 088 |
| Names are `String` after resolution: Core binders, field keys, `fnRef`, `Callee` (with `Coe String Callee`, `Core.lean` ~:48), SSA registers/blocks/phi predecessors, `PExpr` binders; the checker's environment is name-keyed. | 039 – 041, 044 – 045, 081, 082, 087, 093, 094 |
| Evidence status is computed separately by six surfaces; callees, theorems and specs are joined by name. | 079, 081, 083, 089 |
| No semantics for SSA or the backend; 510 `partial def`s in `Concrete/`; `Lower` does sequencing, defer, match compilation, promotion, layout and bounds checks in one pass; trait impls are validated by return type only. | 088's folder defect, 091, 095 |
| Prose and emitted contract text claim enforcement nothing performs (`Backend.lean` ~:38 – 47 says "wrap by default"; the emitter emits checked helpers). | 080's comment, R-18 – R-20 |

## Target architecture

### Semantic contract (fixed first; everything is stated against it)

- **Behaviours.** Every executable IR has initial/final states and observable behaviours: a trace of
  external events (I/O, FFI calls and returns) ending in *return*, *defined trap*, *silent
  divergence* or *infinite external interaction*. Stuck execution — a violated `Unsafe`
  precondition — is distinct from a defined trap. Resource exhaustion (stack, OOM) is an explicit
  behaviour with stated premises, never a silent mismatch.
- **Style.** Each IR has a total clock-indexed evaluator, deterministic for a fixed admissible
  environment and allocation oracle; behaviours quantify over admissible oracles. Prove stability
  of completed evaluations under clock extension, monotonicity of partial traces, and adequacy for
  termination, silent divergence and infinite interaction (CakeML needs monotone partial traces to
  build infinite behaviours; totality alone is insufficient). Pass proofs include clock transport or
  well-founded stuttering obligations. Forward-to-backward conversion is used only after proving its
  receptiveness/determinacy hypotheses (CompCert's theorem requires both) or an equivalent
  oracle-based theorem. Clock exhaustion remains inconclusive. Alternative not chosen: relational
  small-step semantics as the normative definition with a separate runner.
- **Refinement and observation.** Refinement is over maximal behaviours under related initial states
  and admissible environments, with an explicit observation relation for values, external memory and
  ghost erasure. Ghost trap kinds are not binary observations; a claim mentioning them transports
  only through a lemma that says so.
- **Floats.** Formats and bit observations, rounding, NaN policy (including observable NaN bits),
  subnormals, signed zero, conversions, contraction (no FMA unless specified) and exception/status
  observability are specified with a total formal reference model; the floating-point environment
  required across FFI boundaries is stated and enforced. Float correctness is proved for the profile
  or explicitly labelled trusted. **Traps** carry a ghost kind; the binary only aborts.
- **Resources and failure.** Linear conservation is required for resources whose scopes end through
  normal return or structured control flow, after explicit deferred cleanup. Fatal traps, process
  termination and resource-exhaustion aborts promise no cleanup or reclamation; divergence carries no
  eventual-release guarantee. A resource relation and a sufficient-resource theorem govern
  exhaustion: it may extend behaviours only through that relation, so a compiler that always reports
  out-of-memory does not satisfy the theorem. Stack overflow is a defined outcome only where generated
  checks or explicit environment premises justify it.
- **Scope of the first verified profile:** sequential. Foreign threads, asynchronous shared-memory
  mutation, signals and reentrant callbacks are excluded unless covered by the environment model;
  concurrency requires an explicit memory model, ownership-transfer rules and new refinement
  obligations before admission. Closures are deliberately absent: callbacks keep the
  function-pointer-plus-context model, and adding closures requires its own ownership and conversion
  specification. Dynamic linking is excluded until its resolution and loading conditions are covered.

### Language decisions this requires

- **References are strictly second-class:** locals and downward parameters only, never stored in
  aggregates or the heap, never returned. Today a reference can be stored in a struct field
  (bug 092's reproducer compiles); the language closes that. This restriction simplifies escape
  checking. Safe Core's value abstraction additionally relies on linear ownership, valid
  shared/exclusive borrows, and proved or explicitly assumed contracts for raw-memory and foreign
  operations.
- **Raw pointers** remain under `Unsafe`/`trusted`; authorization does not establish validity.
  Pointer arithmetic, integer↔pointer casts and address exposure are distinct operations with
  explicit semantics. Quasi-concrete addresses (a block gets an address only when exposed) are the
  candidate, with an explicit finite-address-space policy; address exposure can have semantic
  effects and is not treated as an erasable cast.
- **No implicit destruction** (OWNERSHIP_MODEL); linear conservation is required at reachable
  exits, not along divergence.
- Disputed surface rules (shadowing, match binding, statement vs value, literal typing) are fixed
  by writing them down (S4) and then defining them by elaboration into Core.

### IR chain

| IR | makes explicit | semantics / memory | evolves from |
|---|---|---|---|
| **HIR** | resolved surface: scoped `LocalId`/`DefId`, spans | defined by elaboration into Core | AST after Resolve/desugar |
| **Core** | the language definition: typed operations (checked/wrapping/saturating as distinct op kinds, signedness on the op, pointer arithmetic its own op), places vs operands, copy/move on every operand, borrows, explicit destruction, structured control, scoped `defer`, capability-tagged external operations | structured values, places and allocation identities for safe operations; a specified byte-and-provenance memory component for raw operations and FFI, connected to typed values by a representation relation; ownership instrumentation with a proved erasure theorem | `Elab/Core.lean`; `Interp.lean` becomes this semantics plus an IO driver |
| **Mono Core** | concrete type/capability substitutions, resolved impls and trait evidence | same semantics, separate artifact `CheckedMonoCore` produced by S8a | `Mono.lean` |
| **Place MIR** | typed non-SSA CFG: fixed evaluation order, match decision trees, cleanup/`defer` edges on every exit, loan boundaries, explicit runtime checks; symbolic fields, stable storage identities | Core's structured values and byte/provenance memory over stable places, preserving the typed/raw representation relation | the first half of `Lower.lean` (defer ~:1525 – 1554, match, promotion ~:559, bounds ~:171) |
| **Memory CFG** | layout from the single `Layout` authority, offsets, alignment, loads/stores, allocation/free, scalar representations | CompCert block–offset memory (bytes, pointer fragments, undef, permissions) | the second half of `Lower.lean`, `Layout.lean` |
| **BIR** | CFG with block arguments (no phi instruction; SSA is a well-formedness property, not semantics), `RegId`/`BlockId`, `add.ovf`-style ops returning (value, flag), `trap` terminator | block memory | `SSA.lean` (which has both `phi` and block params today) |
| **LCB-IR** (external) | the Lean Compiler Backend's checked QBE-style SSA input: typed integer, memory and call operations; checked arithmetic as explicit checks or LCB's checked ops | LCB's semantics, imported by Concrete's theorems, never restated | produced from BIR by Concrete's adapter (R-0517); draft PR #8 emits a restricted form from today's SSA |
| **Machine IR → allocated Machine IR** | AArch64 operations and addressing modes, virtual then physical registers, spills, calling convention, frames | block memory + frame | inside LCB (external dependency, its C01 – C03 and V01 – V05) |
| **Asm → loaded image** | linearized code, prologue/epilogue, encoding, relocations, startup | ISA model over decoded bytes | encoding inside LCB; linking, `rt`, startup and final-image checks are Concrete's (R-0505) |

LLVM is printed from BIR (block arguments to phis, `.ovf` ops to `*.with.overflow`). It is the
fast, **unverified** profile; its binaries are labelled so, and only the native profile — Concrete's
BIR → LCB-IR adapter plus the Lean Compiler Backend — carries proof transport. Unverified does not mean unspecified: the BIR → LLVM adapter expands Concrete's
defined checks and implements the specified conversion, shift and memory semantics; it introduces
no poison/UB-generating flags, attributes or metadata without their required preconditions;
unsupported mappings fail explicitly; retained-LLVM controls cover poison, undef, helpers and ABI
lowering.

### Where each check lives

- **Elaboration** produces Core; a declarative elaboration relation defines the surface language.
- **The authoritative checker runs on Core** and emits an immutable `CheckedCore` bound to the
  exact program and imported interfaces, with a derivation (consumed/frozen sets, loans per
  statement) that a small independent checker re-checks. It owns typing, literal ranges, Copy
  constraints, linear conservation at exits, loan overlap and provenance, recursive escape,
  exhaustiveness, trait signature *and effect* compatibility, `Unsafe` obligations. The source-tree
  checker survives only for early diagnostics until the switch, then for purely syntactic checks.
- **Capabilities are re-checked after Mono** over the call graph, using the same rules; the
  semantics tags each external event with its capability. Open indirect calls are checked against
  callable signature, capability and borrow contracts; closed-world target sets must conservatively
  cover every possible target and are not the sole authority for imported callbacks.
  Capabilities are not purity: mutation/effect summaries are separate.
- **Bounds, overflow and division are semantics, not static checks:** trapping operations in Core.
  A pass may remove a trap only when handed a discharged-VC certificate naming that operation.
- **Layout/ABI** checking emits representation certificates; each later IR's verifier emits its
  well-formedness artifact.
- Private constructors are API discipline, not soundness: artifacts bind their exact payload, and
  the soundness argument is the theorems.

### Passes

Uniform interfaces (stated in Lean as each boundary lands, proved in Part 2):
`WF Γ p → pass p = .ok q → WF Γ q ∧ Refines Γ p q`, and for validated passes
`WF Γ p → validate p q cert = true → WF Γ q ∧ Refines Γ p q`. Native memory safety needs more
than observable refinement — an invalid machine access could produce the expected result — so a
target-memory validity invariant is preserved alongside.

| pass | strategy |
|---|---|
| parse, resolve, hygienic desugar, elaborate | output independently checked by the Core checker; elaboration later proved against the declarative relation |
| specialization (Mono) | untrusted specialization search + total validator (S8a); proved substitution/dispatch theorem |
| sequencing, match compilation, cleanup/CFG lowering (Core → Place MIR), one job per pass | proved simulations |
| storage selection, layout/representation, check expansion, ABI lowering (→ Memory CFG) | proved simulations with value relations and memory injections |
| SSA construction/destruction (→ BIR) | untrusted + proved validator (parallel copies, cyclic CFGs) |
| folding, algebraic rewrites, DCE, memory optimizations | untrusted + proved certificate checkers; must preserve traps, effects and divergence; arithmetic only through `IntArith` |
| instruction selection, frame construction, encoding | proved against the ISA model |
| register allocation, scheduling, linking | untrusted + proved validators (location/dataflow and image/relocation correspondence) |
| BIR → LLVM printer | trusted, differential-tested; unverified profile |

### Proofs attach to Core

`PExpr` is retired. Claims are about Core's value semantics applied to the exact checked Core term,
embedded in the generated Lean file as data with its digest; ProofKit supplies a simp set that
evaluates the semantics symbolically, so no extraction or correspondence layer exists to drift.
Statements are generated from a typed `ClaimSpec` (domain, preconditions, postconditions, trace and
trap clauses, termination, coverage form). VC generation (TermIR) is proved sound against Core
(R-0460). Evidence distinguishes **Core-proved** from **binary-transported**: a claim transports
to a binary only through the compiler-correctness theorem for the profile that built it *and* a
theorem that its particular proposition is preserved by the refinement relation (refinement
preserves safety-style properties, not automatically existential behaviours, resource costs,
ghost-state properties, noninterference or relational claims); hashes identify artifacts, they do
not prove correspondence. Compilation preserves an `rt`/`std` implementation's semantics;
correctness against its advertised contract is a separate refinement proof, and audited
implementations remain named assumptions.

### Backend and runtime

The first native target is a full profile, not just an ISA: AArch64 + Apple's ARM64 ABI + macOS +
Mach-O (the host, `Backend.lean` ~:18), so native and LLVM binaries are differentially tested on
the same machine; Linux/ELF AArch64 follows as a second profile. Runtime helpers (today 875 lines of LLVM AST in `EmitBuiltins.lean`,
plus raw LLVM strings) are rewritten in Concrete as an `rt` module compiled through the same
pipeline; libc becomes FFI with stated contracts. Native code generation is delegated to the
separately developed [Lean Compiler Backend](https://github.com/unbalancedparentheses/lean-compiler-backend)
(LCB, a Lean 4 port of QBE 1.3; R-0517): Concrete owns the BIR → LCB-IR adapter and its refinement
proof, version pinning (a lake/nix input with LCB's upstream lock, the same Lean toolchain), linking,
`rt`, startup, final-image checks and theorem composition; LCB owns its IR semantics, lowering,
register allocation, encoding and their proofs. LCB starts with assembly text through a named-trust
assembler; its trust boundaries are carried into Concrete's theorem premises until LCB removes them.

### Trusted at the end

The Lean kernel (re-checked by con-leche and nanoda); the ISA model; every trust boundary the Lean
Compiler Backend names for its certified domain (today rendering, assembler and linker, until its
V04/V05 milestones remove them); the operating system as
specified by external-call contracts; listed resource premises; for the LLVM profile, LLVM and the
system toolchain; and the compiler's own execution only where no proved validator checks its
output (listed and minimized). Native execution of a validator is not kernel evidence: validators
are proved, and their compiled execution is in the trusted list. Evidence that depends on
native-evaluation or float axioms (`docs/verification/AXIOMS.md`) is labelled separately and never
supports an unqualified kernel-only verdict.

## Principles

1. **One normative specification per meaning, implementations proved against it.** Until proved,
   implementations are differentially tested against it.
2. **Identities, not spellings.** No IR, table, checker environment or evidence record joins on a
   `String` after Resolve. Display names live in side tables.
3. **Check the artifact that is compiled.** Every downstream stage consumes exactly the checked
   artifact.
4. **One authority per verdict, fed by checked evidence binding the exact proposition.**
5. **The compiler states the claim; the author proves it.**
6. **Enforcement is demonstrated:** a claim is tied to a gate leg and a targeted mutation the leg
   kills.
7. **Every comparison counts its comparisons;** too few completed comparisons fails.
8. **Totality where proofs need it:** semantics, checkers, validators and proved passes are total;
   untrusted search (heuristics, elaboration search, register-allocation search) may stay partial.

## Delivery: verified vertical slices

Decided 2026-10-19. The stages below (S1 – S16) are the architecture's components; they are not
delivered layer by layer. Building all of Core's semantics, then the whole checker, then the whole
lower pipeline would take years before any end-to-end guarantee existed, and would test the
architecture only when proving began. CompCert and CakeML each proved a small language end to end and
grew it. Concrete does the same: each **slice** carries a fixed language subset through every stage —
identities, typed operations, Core semantics, the Core checker, claims on Core, Place MIR, Memory CFG,
BIR, the native backend — and closes with its own end-to-end theorem. A later slice extends a pipeline
that already has one. Part 2's proofs are therefore not deferred: each slice proves its scope before
the next grows it.

**Coexistence and convergence.** `verified` is a versioned admission and assurance profile of
Concrete, with a stated language subset, target, environment contracts and theorem boundary. It rejects
unsupported reachable code, dependencies and feature combinations; it never silently falls back to LLVM
or an unproved pass. The existing profile remains supported while both converge on shared identities,
typed operations, checked Core, layout and validated lower IRs: replacements arrive as shadow paths and
the production compiler migrates incrementally. Sharing an implementation does not extend its proof
beyond the admitted subset. An old path retires only when its complete replacement supports the affected
programs and dependencies, meets correctness and compatibility gates, and has an explicit release and
rollback plan; corpus agreement alone is insufficient. The LLVM backend stays a supported performance
profile unless a separate product decision retires it. Verified and unverified modules compose only
through recorded contracts and explicit assurance boundaries.

**Early semantic risk retirement.** A scalar slice is a good first delivery but a poor test of
Concrete's distinctive architecture: second-class references simplify escape reasoning, not aliasing,
storage identity, reborrowing or call-boundary obligations. So before Core's interfaces freeze, reviewed
machine-checked design examples cover a linear move and destroy, a downward exclusive borrow across a
call, shared borrowing, permitted reborrowing, structured cleanup, and typed-value representation in
byte/provenance memory. They test the interfaces without admitting unfinished features to the profile.
Allocation lifetime, initialization, alignment, address exposure, foreign mutation and external-call
contracts are fixed at this boundary (S7 with R-0502).

**V1 milestones (queue row 3, R-0511).**
- **V1a** proves checked Core → executable BIR refinement for `i64`, `u64`, `bool`, locals and
  assignment, conditionals, a general loop with structured exits, non-recursive direct calls,
  comparisons, checked addition/subtraction/multiplication, and one capability-authorized `print_int`
  event, including terminating, trapping and divergent behaviours. Other widths, casts, division, shifts,
  recursion and further surface loop forms are explicit subsequent V1 increments. Its named target
  programs and regression set are listed under R-0511 in the [roadmap](../project/ROADMAP_TASKS.md#task-r-0511);
  they do not widen the subset. A Core → BIR theorem
  is an intermediate result, not the native guarantee; no throwaway VM is built.
- **V1b** carries the subset to native AArch64 through Concrete's BIR → LCB-IR adapter and the Lean
  Compiler Backend's restricted certified path (R-0517): the native theorem composes V1a, the adapter's
  refinement and LCB's preservation theorem for its exact domain (LCB's Stage 1, C01 – C03), carrying
  LCB's named trust boundaries, with final-image correspondence checked by R-0505. LCB’s Stage 1 exit
  is necessary but insufficient: V1b requires a pinned certified LCB domain covering every reachable
  operation and behaviour of the declared V1 subset, including checked-arithmetic trap paths, direct
  calls and the `print_int` external-event contract, together with the proved adapter, semantic bridges,
  final-image correspondence and native-exit obligations (R-0517). A smaller leaf-function intersection
  may ship as a separately named intermediate theorem with explicit admission restrictions; it does not
  complete V1b. C01 – C03 end at LCB's certified typed-assembly endpoint; rendering and encoding remain
  named assumptions until separately covered. Until the composed native exit passes, R-0517’s
  experimental fallback may ship under a narrower R-0333 candidate scope; it does not graduate V1b or
  the verified preview.
  Intermediate assembly-level results name their trusted assembler/linker boundary and are not labelled
  native-image proofs. `print_int` has a specified observable trace and implementation or
  external-service contract; internal calling conventions, startup, stack requirements and trap
  realization are covered. AArch64 stays the target: a simpler ISA would help only if an available
  formal model, toolchain and deployment environment reduced the complete proof burden.
- **Native exit.** For successful compilation of an admitted `CheckedCore` program to an identified
  image, under related initial states, admissible environments and the recorded resource premises,
  prove S15's maximal-behaviour refinement through the explicit observation relation, the preserved
  target-memory validity invariant and the sufficient-resource theorem; prove checker soundness for the
  admitted subset, basic capability containment, every used pass or validator, and transport of one
  nontrivial Core claim; re-check the proof closure with con-leche and nanoda and bind it to the exact
  retained artifacts. Fuel-bounded evaluation never stands in for a termination proof.
- **V1c: frontend assurance.** Early backend releases explicitly begin at retained `CheckedCore`. In
  parallel, the restricted surface syntax, resolution and elaboration judgments are specified
  independently of their implementations; before V1 is called source-to-native verified, successful
  parsing, resolution, desugaring and elaboration are proved to implement them, or proved certificate
  checkers cover those boundaries; V1b and V1c together are the first source-to-native theorem, and a
  Core-to-native theorem alone is never described as one. The first theorem may cover a single source unit with fixed imports;
  later slices extend it to package resolution and exact import roots. Each admitted surface extension
  carries its frontend obligation. Retained-Core inspection and differential testing are transitional
  aids, not correspondence proofs.
- **Evidence delivery.** A minimal canonical task identity and an independent retained-artifact consumer
  ship with every publicly authoritative slice claim from V1 on. Differential tests compare the
  normative semantics, the existing compiler and the new pipeline, with timeouts classified inconclusive.
- **Review.** An outside expert reviews Core's V1 semantics and the early design examples before they
  freeze.

**Cumulative growth (queue row 6).** V2 adds aggregates, bounds checks, stable places and
stack-local/downward second-class references, with borrow validity and target representation proofs.
V3 adds owned heap allocation, linear resources, destruction and `defer`, extending the borrow proofs to
heap-backed places and stating out-of-memory behaviour. V4 completes the admitted reference, reborrow and
resource interactions. V5 adds generics, traits and capability polymorphism (basic capability
containment and external-event authorization are proved from V1). V6 adds raw-pointer operations,
general FFI, callbacks and separately compiled module composition against the previously specified
memory and interface contracts. V7 completes a maintained feature inventory — floats, indirect calls,
module initialization, runtime facilities, unsafe libraries and every other actual feature, each with an
owner and exit — and the supported `std` components; concurrency and dynamic linking stay excluded until
separately specified. Every release proves the **cumulative** admitted language, including interactions
with earlier features. Extensions preserve earlier semantics through an embedding theorem, or declare
and review a versioned semantic change. Unsupported combinations are rejected explicitly. Whole-program
image construction is required from V1; general separate compilation is a later extension. Source
correspondence grows with the slices rather than waiting for V7.

**Evidence migration (R-0512).** The legacy evidence implementation receives soundness, compatibility
and security maintenance, but no new semantic claim mechanisms. An inventory records every supported
claim and consumer with its exact subject, proposition, coverage, transitive dependencies, assumptions
and replay version. A claim may migrate once its complete semantic dependency closure has supported Core
statements and sound statement generation, binding and evidence interpretation — native compilation
coverage is not required. It is re-proved or carried by an explicit proved bridge; any change in domain,
coverage or assumptions is recorded for review. Core-proved and binary-transported remain distinct.
Each migration ships with user-facing assurance and compatibility diffs; historical artifacts and a
versioned replay path are preserved; a superseded or invalidated claim never silently stays
authoritative. Subject identity, dependency binding and provenance remain required in the replacement:
it is their fragile implementations, not those responsibilities, that are retired. Legacy issuance code
is deleted only after supported claims and consumers have migrated or completed explicit deprecation;
bug-class controls transfer to the replacement before implementation-specific gates are removed; no
name-keyed lookup grants current authority.

**The shared front end is whole-language early (queue row 4, R-0514).** Core, its identities and its
checker are shared by both profiles; only lowering differs. So the authoritative checker on Core is made
whole-language alongside the first verified releases — closing the checked-is-not-compiled gap (087,
090, 092 – 094) for every user — while the slices prove its rules as they reach them. Since
2026-10-21 the same row also moves the existing profile onto one `Layout` authority (R-0502) and Place
MIR passes (R-0501), as shadow paths, because structural classes 8 and 12 keep producing wrong code in
the profile users run; their semantics and proofs still grow per slice.

R-0514 and V1 share one checker implementation and payload-bound artifact chain: `CheckedCore` →
checked specialization → `CheckedMonoCore` → lowering. Whole-language checker implementation coverage
does not imply whole-language soundness proofs. V1 proves the admitted fragment of that same checker;
later slices extend the proof coverage.

**Protections in the meantime (queue row 2).** S1 (prevention harness), S2 (one verdict authority),
S3 (demonstrated enforcement) and S4 (written language rules) protect users of the existing profile.
The rules the next slice needs are decided and enforced in the existing compiler first, with versioned
migration diagnostics, so the verified pipeline never models looser rules. Broader prevention coverage
continues throughout and does not block isolated proof development. R-0513 replaces incidental count pins
with checked inventories and splits a fast push tier from the full tier.

## Part 1 — the proof-ready pipeline

Ordered as a migration: the compiler builds, passes the full gate set and ships after every slice.
Each new boundary arrives as a shadow path with a disagreement gate, then retires its predecessor.

### Track A — stop the bleeding (now, in parallel)

**S1 Prevention harness (R-0491).** Depends on bug 080 landing (`--report model-diff`).
The extracted model, then Core's semantics, as a third side of the differential fuzzer over a
grammar covering every shape that broke (loop-body sequencing, `let` after assignment, `return` in
loops, if-without-else fall-through, discarded `expr;`, shadowing, renamed match binders, literals
at 2^63/2^64, every cast width pair). Outcomes classified (value, trap, rejection, unsupported,
exhaustion, crash), never skipped. model-diff for zero-argument, `bool` and aggregate signatures
plus a binary leg, with a pinned claimed-function inventory. The four 2026-10-18 hunts and the
checker hunt run before every release candidate. A hand-written adversarial suite of eight named examples,
each with its own gate leg, and scaled interpreter/native/model campaigns are scheduled under R-0491 in
the [roadmap](../project/ROADMAP_TASKS.md#task-r-0491).
**Exit:** each of 080 – 095 has a checked-in bug record, reproducer, expected outcome and
pre-fix/fixed SHA pair. The deterministic differential suite detects 080 and 084 – 088;
name-binding controls detect 081 – 082; evidence and cross-surface controls detect 083 and 089;
checker mutants detect 090 – 094; the binary leg detects 095. Each assigned control fails at its
pre-fix SHA and passes at its fix; generated features have pinned minimum completed-comparison
counts; missing inventory entries fail.

**S2 One verdict authority (R-0494).** `Verdict` with a private discharging constructor;
`Verdict.decide` consumes checked evidence binding proposition, clause, subject, coverage,
dependency closure and environment; one `ClaimJudgment` per claim and contract clause; receipt
decoding and currency checks feed it; contracts, vcs, obligation-ledger, audit, check-proofs (text
and JSON), proof-status and issuance render it. Delete `Proof.provedFunctions` and name-substring
replay checks; split `Report.lean` (6,405 lines) and `Main.lean` (4,194) by surface.
**Exit:** status spellings only in the renderer table; 089's reproducers read not-proved
everywhere; a cross-surface gate pins the expected positive and negative judgment for every claim
and clause.

**S3 Enforcement demonstrated (R-0498).** A claim → gate-leg → targeted-mutation manifest over
verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check`,
`Backend`, and emitted contract text; the backend contract is generated from BIR op semantics once
S11 lands. R-18 – R-20 relabelled shape-only until their proofs exist.
**Exit:** every in-scope enforcement phrase has a manifest row whose mutation its leg kills in a
qualified campaign.

**S4 Written language rules (R-0500).** Normative paragraphs in `docs/language/` for block scoping
and shadowing, match-binding positions and names, statement vs value position, sequential
assignment within a loop iteration, literal typing and range (signed minima preserved), cast
semantics per width pair, evaluation order, short-circuiting, and second-class references; each
with accepted/rejected examples and fixtures.
**Exit before advancing past queue row 2:** every rule has a normative paragraph, examples, cited
fixtures and S1 generator coverage meeting its minima. S7's exit later adds links from each
paragraph to its semantic clause.

### Track B — foundation

**S5 Identities and immutable artifacts (R-0492 identities, R-0497 artifacts).** `LocalId` for every binder, allocated at
Resolve so the source checker and Elab both consume it; `DefId` as a compilation-scoped declaration
handle allocated before body elaboration, preserving callable namespaces, specialization arguments
and extern/intrinsic distinctions, with a validated `DefId → DefinitionIdentity` mapping at evidence
boundaries; owner-checked `FieldIdx`/`VariantIdx`; `RegId`/`BlockId` in SSA. No `String → identity`
coercions: identities come from scope-aware environments through checked adapters;
capture-preserving alpha-canonicalization before hashing, frozen identity bytes preserved or
explicitly versioned. Pipeline artifacts become immutable and payload-bound (slice 1 sealed `ValidatedCore`,
`MonomorphizedProgram` and `SSAProgram`, 2026-10-23), with diagnostic artifacts typed separately.
Language: no block `let` shadowing a visible name; colliding import aliases rejected; match binders
equal or name their field.
**Exit:** `check_ir_no_string_identity.sh` rejects string identity joins in resolved tables, the
source checker's environments, Core, SSA and `PExpr`; 081, 082, 087, 093 and 094 reject or bind
correctly; targeted mutations show checker consumption/block-local accounting, `ElabVerify` and the
theorem-table identity checks load-bearing; compile-fail controls cover artifact forgery and
diagnostic-path bypass.

**S5 slice contract 1 — sealed, certified artifacts (R-0497 slice 1, 2026-10-23).**
- *Representation:* `ValidatedCore`, `MonomorphizedProgram`, `SSAProgram` and `ElaboratedProgram`
  get private constructors. The first three carry erased `Prop` certificates about their exact
  payload: `coreCheckProgram coreModules = .ok ()`; Mono's output equation over `source` plus
  `verifyPostMono coreModules = []`; and the lowering chain from `source` plus
  `ssaVerifyProgram ssaModules = .ok ()`. Mono and SSA artifacts name the artifact they were produced from. Two
  uncertified types are split off: `UnverifiedSSA`, the `ssa-unverified` debugging view, and
  `CoreSubset`, modules drawn from a `ValidatedCore` with a `Sublist` proof. Proof extraction reads
  a `CoreSubset`, and a whole program coerces to one.
- *Invariant made structural:* a value of a certified type is exactly what its producer accepted.
  Forging one, replacing its payload (`{ a with … }`), or passing an uncertified artifact where a
  certificate is required is a compile error. Changing the payload needs a proof about the new
  payload. The certificates are erased `Prop`s. The `source` fields are runtime data, and the
  compiler's emitted output is unchanged. Private constructors are API discipline. The
  certificates are the statement, and forging one past the seal needs a proof or an axiom.
- *Gates:* `check_pipeline_artifacts.sh`:
  - P: the certificates read as their equations, the producer chain type-checks, and no `sorryAx`;
  - F1–F7: forgery by `{…}`, `⟨…⟩` and `.mk`;
  - U1–U4: payload replacement;
  - D1–D4: `UnverifiedSSA` reaching `emit`, and a `CoreSubset` or `ElaboratedProgram` reaching Mono
    or the verify gates;
  - S: no `open private` of these constructors, and every declaration of `Pipeline.lean` in the
    built environment rests only on `propext`, `Classical.choice` and `Quot.sound`. A sorry with
    its warning silenced and an indented `axiom` are both refused (mutant-checked);
  - B: bug 180, the class instance this slice found.

  A public-constructor mutant turns F1–F3 and U1 red. The golden and snapshot gates show the
  compiler's output unchanged. The exception is bug 180's intended report change.
- *Not in this slice:* binding `ElaboratedProgram` to its resolved input; the identity half (R-0492);
  stage types beyond SSA as their IRs land; a mutation family in `test_mutation.sh` (it moves the
  pinned inventory count, so it waits for a landing train).

**S5 slice contract 2 — local binder identity in the checker (R-0492 slice 1, 2026-10-23).**
- *Representation:* `LocalId` (`Concrete/Frontend/LocalId.lean`, private constructor) names one
  local value binding of one function. Bindings are parameters, `let`s, pattern binders, `for`
  header binders, and borrow-block references and region names. Each function carries a
  `LocalTable` (`FnDef.locals`) produced by `Pipeline.resolve` after desugaring. One lexical walk,
  `Resolve/Locals.lean` (the former SCOPE-1 walk of `Check.lean`), records three things: the
  binder for each binding, the binding each use of a local name denotes (keyed by the use's
  position and spelling), and each SCOPE-1 collision. Desugaring moves inside `Pipeline.resolve`,
  so no path reaches Check with a body that skipped it (bug 202).
- *Invariant made structural:*
  - The checker's environment is keyed by binding identity (`TypeEnv.vars : List (LocalRef × VarInfo)`;
    a `LocalRef` is a `LocalId` with its spelling for messages, and compares by identity).
  - A use of a local name finds its binding through the table, never by searching the
    environment for the spelling.
  - The references a binding keeps to other bindings are `LocalId`s: `borrowedFrom`,
    `refParent`, borrow-block refs, and the roots of normalized places.
  - A binding that stays in the environment after its scope closed can no longer answer for a
    later use of its spelling (class 2: bugs 040, 041, 045, 087, 093, 094, 107, 124, 176).
  - E0301 is rendered from the same walk, so the shadowing rule and binding identity cannot
    disagree.
  - A function without a table, or with an ambiguous one, is refused rather than checked by name.
- *Gates:* `check_local_identity.sh`:
  - F: compile-fail controls. `LocalId` and `LocalTable` cannot be constructed or updated outside
    their module, and `TypeEnv.vars` cannot be searched by a spelling.
  - S: the minting functions are called only from `Resolve/Locals.lean`, and the checker keeps no
    name-keyed lookup helper and no private uid counter.
  - T: fifteen walk answers on a probe program with loops, a `for` header, branches, an arm,
    `let … else`, a struct destructure, a borrow block, a call through a fn-typed parameter,
    sibling reuse and one collision.
  - R: `Pipeline.resolve` attaches a resolved table to every function; bug 176's loop program
    still runs; bug 202's project `let … else` builds; a destructure's two `_` binders keep two
    identities (Codex round 2).
  - Mutation family 98 (a use denotes the outermost visible binding of its spelling) is killed by
    leg T.
  - *Why no program-level discriminator:* with SCOPE-1 enforced and loop scopes closed (bug 176),
    the newest binding of a spelling in the environment is always the visible one. A by-name
    lookup therefore gives the same answer on every accepted program. This slice removes the
    checker's dependence on those two rules for identity; it does not change what is accepted.
  - The name-keyed guards E0292 (live shadow) and E0238's shadowed-source rule could only fire on
    a binding from a closed scope, so they are removed.
- *Not in this slice:*
  - Elab still alpha-renames by its own scopes; it consumes `LocalId`s in a later slice, retiring
    that second walk.
  - Contract expressions (`requires`, `ensures`, invariants) still name parameters by spelling.
  - Resolve's pre-desugar E0100 walk is a separate visibility computation.
  - Core, SSA and `PExpr` keep their string binders (R-0492 steps 2–4).

**S5 slice contract 3 — Elab consumes `LocalId` (R-0492 step 2, 2026-10-23; depends on slice 2,
branch `vp-r0492`, landed in train 6; this step landed in train 8 from `vp-r0492-elab`).**
- *Representation:* Elab's name-keyed scope (`vars`, `renames`, `ghostVars`) is gone. `ElabEnv`
  holds the function's `LocalTable` and `bound : List (LocalId × BoundLocal)`. A `BoundLocal`
  records a binding's Core name, type, ghost flag, and the absolute frame position its binder
  took.
  - A use resolves through the table at its position (`useLocal?`).
  - A binder takes its identity from the table (`binderId`).
  - The Core name is the binder's spelling, or a fresh `name.bN` for an arm binder or for a
    binder the walk records as shadowing (`LocalTable.shadowsAt`; `_` binders and Elab's
    struct-destructure temporary count).
  - Evidence binder references are computed from the recorded position (`evBinderRefAt`), not by
    searching the frames for the spelling (`BodyScope.resolve?`).
  - The walk mints the destructure temporary (`destructureTempName`) and its synthesized uses.
- *Invariant made structural:* no front-end pass decides by spelling which binding a local name
  denotes. Check and Elab both read the one walk.
  - The Core names and evidence positions are the same as before. `--emit-core` is identical on
    all 1474 corpus files (`tests/programs`, example sources) against the pre-step-2 compiler, and
    `--report fingerprints` / `subject-facts` are identical on 111 example entry points.
    `check_local_identity.sh` leg E pins the exact name profile of the edge cases.
  - The walk's `shadows` fact counts exactly what Elab's old scope held as runtime variables.
    Region names and `ghost let`s do not count; `_` binders and the destructure temporary do.
  - One intended difference: a source use can no longer resolve to Elab's destructure temporary.
    A parameter spelled `__destr_P` used to be captured by the temporary in Elab while Check read
    the parameter; both now read the parameter. No corpus program is affected.
- *Not in this slice:*
  - `BodyScope`'s frames keep their spellings and are still tombstoned at block exit; they are
    now positional bookkeeping only.
  - Core, SSA and `PExpr` binders stay strings (R-0492 steps 3–4).
  - Contract expressions name parameters by spelling.

**S5 slice contract 4 — Core binders carry `LocalId` and ElabVerify checks them (R-0492 step 3,
2026-10-23; depends on contract 3, branch `vp-r0492-elab`).**
- *Representation:*
  - A Core local binder or reference is a `CLocal`, a Core name plus an optional `LocalId`, in
    every position that named a local by a bare `String`: `CExpr.ident`, `CStmt.letDecl`,
    `CStmt.assign`, `CMatchArm.varArm` and `enumArm` bindings, and the borrow block's variable
    and reference.
  - `CFnDef.paramIds` records the parameters' identities.
  - Elab fills every identity from the walk's table. The struct-destructure temporary has one,
    because the walk mints it. An evaluation-order temporary (`ev.t<n>`), a constant or a function
    value has none.
  - Migration follows the R-0002 technique, with one deliberate exception.
    - A `CLocal` coerces to its name, and equality, hashing, ordering, `Repr` and `ToString` are
      the name's, so no consumer changes behaviour and no printed or hashed form of Core moves.
    - A name does NOT coerce to a `CLocal` (`CLocal.ofName` is explicit). With that coercion, a
      string-literal pattern on a binder field (`| .letDecl "y" ..`) elaborated, matched only
      identity-less binders, and silently stopped matching Elab's Core. PipelineTest's
      binder-agreement test was caught this way. Such a pattern is now a type error (gate legs
      F7 and F8).
  - Mono's specializations carry the generic's `paramIds`.
- *Invariant made structural:* `ElabVerify` (`Concrete/Check/ElabVerify.lean`), which runs inside
  `Pipeline.elaborate`, refuses Core whose identities and names disagree. Its three rules:
  - B1: a binder identity is not already visible where it is introduced.
  - U1: a reference with an identity names a visible binding and spells that binding's Core name,
    and no newer visible binder has that Core name.
  - P0: a function Elab produced records one identity per parameter, and parameters are checked
    by B1 like any binder.
  - U2: a reference without an identity does not spell the Core name of a visible source binding.

  Elab's renaming (the bug 045 / 087 class) is therefore checked against the walk on every
  compile, not only by tests.
- *Gates:*
  - `check_local_identity.sh` leg V: the real Core of a probe carries identities and passes, and
    hand-made U1, U2 and B1 forgeries are refused.
  - Mutation family 100: Elab drops a reference's identity.
  - `--emit-core` is byte-identical on the corpus against the pre-step-3 compiler.
- *Closes:* bug 050's remaining gap. A fn-typed local spelled like a submodule function was
  renamed to that function when used as a value. The import-rename pass now skips a reference
  that carries an identity.
- *Not in this slice:*
  - Interp, Lower, Mono, CoreCheck and `PExpr` extraction still key locals by the Core name. They
    move to the identity one at a time, and then the coercions go.
  - `Callee.indirect` (a call through a fn-typed local) still names its local by a string.

**S5 slice contract 5 — the backends key locals by identity (R-0492 step 4, 2026-10-23; one
backend per slice; depends on contract 4).**
- *Representation:*
  - `CLocal.key : LocalKey` is the binding's `LocalId`, or its Core name for a compiler
    temporary. `CFnDef.paramKeys` gives the parameters' keys.
  - `Callee.indirect` carries a `CLocal`, and ElabVerify checks it as a use.
  - During the migration each backend switched the name coercion off for its own file
    (`attribute [-instance] CLocal.coeName`). The last slice removed the coercion itself, and with
    it `CLocal`'s `BEq`, `Hashable`, `Ord` and `LT` (all on the spelling) and `Callee`'s derived
    `BEq`. A consumer now asks for `.key` (which binding) or `.name` (the spelling, for printing,
    fingerprints, global-name disambiguation and the proof model); there is no implicit third way.
- *Slices:*
  - 4a, the interpreter: the environment and borrow bases (`RefPath.base`, with `baseName` kept
    for display) are keyed by `LocalKey`.
  - 4b, Lower: the variable map, loop phis, break/continue snapshots, promoted allocas and `&mut`
    write-backs are keyed by `LocalKey`. ElabVerify also runs after Mono, because Lower reads
    post-Mono Core.
  - 4c, Mono: no change was needed beyond switching the coercion off (it already copies
    `paramIds` into specializations, contract 4).
  - 4d, CoreCheck: the local environment is keyed by `LocalKey`, parameters bind by `paramKeys`,
    `Callee.indirect` is looked up by key; diagnostics still print the spelling.
  - 4e, `PExpr` extraction (ProofCore): the proof model's variables are spellings by design (a
    theorem quantifies over the names it states), so every place a Core binding becomes a model
    name is an explicit `.name`. One analysis was keyed by spelling and now keys by identity:
    loop-boundedness matches the condition variable and the stepped variable by `CLocal.key`.
    Two different bindings sharing a spelling can now only turn a "bounded" verdict into
    "unbounded", the safe direction.
  - Removal: the coercion and the spelling instances above are gone. The post-mono
    reference-binding verifier (`Verify.lean`, REF-6@1) was the one further consumer whose
    environment was keyed by spelling; it now keys by `LocalKey`.
  - Gate: `check_local_identity.sh` F9 – F13 are compile-fail legs (a `CLocal` used as a `String`,
    and `==`, `hash`, `compare` on `CLocal`, `==` on `Callee`); P9 is the control that the explicit
    forms elaborate.
- *Invariant made structural:* in a migrated backend, which binding a reference reads is the
  identity ElabVerify checked, not the spelling.
- *Byte identity:*
  - `--emit-core`, the reports (fingerprints, subject-facts) and `--interp` are identical on the
    corpus: 1569 Core files, 111 report entry points and 1451 interpreter runs.
  - `--emit-ssa` / `--emit-llvm` differ on 7 files, and only in register numbering and phi order.
    The old name-keyed variable map overwrote a dead sibling binding of the same Core name in
    place (consecutive `for (let mut i …)` loops). Keyed by identity, the new binding is a new
    entry, so phis are created in a different order. The programs' behaviour is the same, and
    every gate passes: run_tests, examples, codegen differential and fuzz.
- *Byte identity of 4c – 4e and the removal* (base: the 4b head): `--emit-core`, the reports,
  `--interp` (stdin closed) and `--emit-ssa` / `--emit-llvm` are all identical: 1569 Core files, 111
  report entry points, 1451 interpreter runs and 1562 SSA/LLVM programs, 0 differences.
- *Not in this step:* the reports, the proof soundness layer and Elab's own type-agreement walk
  still read spellings, explicitly (`.name`). They print or compare model or global names, not
  bindings.




**S6 Typed operations (R-0493).** **Immediate row-2 checkpoint:** make `IntArith` the arithmetic authority for the existing pipeline's
evaluation, folding and arithmetic-model generation, with emitted operations checked against the same
specified semantics. `check_single_arithmetic.sh` and differential controls cover every supported width
and operation family, including casts and defined traps. This checkpoint does not wait for S5; the
typed-operation migration below does.

Depends on S5. Integer ops typed by `IntTy` with
checked/wrapping/saturating as distinct op kinds and signedness on the op; pointer arithmetic its
own op; boolean, float and pointer ops distinct; signed literals canonicalized before range
checking; contextual literal types resolved before construction and re-validated after Mono; typed
casts; `IntArith` the only arithmetic (`check_single_arithmetic.sh`).
**Exit:** every integer op in Core and SSA carries its kind and width; the arithmetic lint passes;
signed-minimum fixtures pass; 088's reproducers agree on every side.

**S7 Core semantics (R-0496).** Depends on S5, S6. The semantic contract above for Core: value
semantics with allocation identities, capability-tagged external events, defined traps,
behaviours from clocks, an instrumented mode with ghost ownership (moved places, loans, frames,
capability use); FFI and OS as external-call contracts. The interpreter becomes this semantics plus
an IO driver. It is the oracle for S1 from here on. S4 fixes the relevant language rules before
this stage freezes their semantics. Before freezing Core, the raw-memory interface is specified
jointly with S11 (initialization, alignment, pointer fragments, allocation lifetime, address
exposure, foreign mutation), and a representative typed-value/raw-memory abstraction lemma is
proved. Representative proofs of clock stability and partial-trace monotonicity land with the
evaluator.
**Exit:** covers every construct the checker accepts; the interpreter is the semantics; the
differential fuzzer compares it with the binary; S4's paragraphs link bidirectionally to their
clauses; the representative lemmas are proved.

**S8 Checker rules, authoritative on Core (R-0499).** Depends on S5, S7. Declarative judgments in
`docs/language/CHECKER_RULES.md` for use-once, conservation at reachable exits, borrow validity
(a source may not be moved, destroyed or incompatibly mutated while a reference to it is live;
shared/exclusive aliasing), no escaping or stored reference, capability containment (a call's
resolved required capabilities, after removing `Unsafe` only at a `trusted` call site, are a subset
of the caller's declared set, through pointers, generics and FFI), raw dereference only in a
`trusted` body or an `Unsafe`-covered context, and bounds (an access establishes an in-range index
or executes the bounds trap). A Core checker implementing them runs as a shadow beside the source
checker with a disagreement gate, then becomes authoritative and emits `CheckedCore` plus its
derivation; capabilities are re-checked after Mono. A rule-violation fuzzer applies one
rule-breaking mutation to accepted programs (including destroying a source while a
permitted local or downward reference is live, conflicting borrows, reaching a capability through a pointer or
generic, raw dereference outside both `trusted` and `Unsafe`); valid programs that must trap are
not checker rejections. Positive controls: trusted raw-pointer operations and trusted absorption of
`Unsafe` accepted; trusted absorption of any other effect rejected. The instrumented semantics is
the runtime witness.
**Exit:** every rule has an enforcing site, an S3 manifest row and a mutation class; the CI seed
set rejects every mutant and accepts every well-formed program with minimum completed counts per
rule; the instrumented semantics runs the corpus and fuzzer programs with zero violations; 090 –
094 and the 087 borrow-block case are rejected by the Core checker; the source checker holds no
ownership, borrow or capability rule. Shadow gates compare the two checkers on the common supported
subset; intended acceptance changes (such as references in aggregates, now rejected) have normative
expected results in an explicit migration manifest. The historical 092/095 aggregate-reference
programs remain as rejection fixtures, with versioned historical binary checks, and an accepted
loop-mutation/downward-borrow fixture detects stale storage.

**S8a Checked specialization (R-0510).** Depends on S8. Core semantics under type/capability
substitutions and trait evidence; Mono as specialization search plus a total validator checking
substitution, implementation identity, reachable-instance closure, signatures, Copy constraints,
literal ranges and capability requirements; produces `CheckedMonoCore` bound to `CheckedCore` and
its interfaces. Unbounded specialization or exhausted search produces an explicit incomplete
result.
**Exit:** every lowering entry point consumes `CheckedMonoCore`; missing or substituted instances,
wrong impls, invalid conditional Copy and effect-erased callbacks are rejected by targeted controls
(091's impl-capability case among them); direct and indirect generic calls are covered by
differential tests and a proved substitution/dispatch example.

**S9 Claims on Core (R-0495).** Depends on S2, S7, S8. Typed `ClaimSpec`; statements generated against
Core's semantics on the exact checked term; ProofKit's symbolic-evaluation simp set; existing
`PExpr` theorems ported (through an explicitly proved bridge where cheaper), keeping weaker coverage
where that is what was proved; Core-proved and binary-transported status remain distinct. Legacy
issuance and replay retirement follow R-0512's inventory, compatibility and deprecation conditions.
R-0495 checks coverage relation and domain against the exact generated proposition; author
annotations confer no authority. Loop claims bind the exact checked Core loop and its
initialization, preservation and exit obligations; missing binding remains `spec_unbound`.
**Exit for each migration cohort:** every migrated positive claim replays against its exact generated
statement; negative fixtures refuse; wrong-coverage, narrowed-domain and substituted-loop controls are
refused through status, issuance, consumption and independent replay. Unmigrated supported claims
retain their declared maintenance and replay path. Legacy issuance code is removed only under R-0512's
final retirement conditions; historical replay remains available.

### Track C — the lower pipeline

**S10 Place MIR (R-0501).** Depends on S7, S8a (consumes `CheckedMonoCore`). Carve sequencing, match compilation, cleanup/`defer`
edges and check expansion out of `Lower.lean` into Core → Place MIR passes, one job each, with a
Place MIR semantics; per-pass differential against Core's semantics.
**Exit:** `Lower` no longer performs these jobs; each pass is total and differentially tested with
minimum counts; cleanup expansion is proved for one representative function with early exits and
`defer`.

**S11 Memory model, layout and BIR (R-0502, R-0503).** Block–offset memory with permissions and
quasi-concrete address exposure; `Layout` as the single proved authority; Memory CFG; BIR with block
arguments, `.ovf` ops and `trap` terminators, each with a semantics; LLVM printed from BIR; the
emitter decides no semantics; runtime helpers moved into the `rt` module; the backend contract text
generated from BIR op semantics. New lowering runs as a shadow path; the old `Lower` is retired
only after the complete replacement path meets its gates.
**Exit (R-0502):** Memory CFG has executable semantics and a verifier; `Layout` emits checked
representation certificates; Place MIR → Memory CFG comparisons cover related values and stores,
traces, traps, allocation and alignment cases; a representative memory-injection lemma is proved.
**Exit (R-0503, after R-0502):** LLVM is emitted only from validated BIR, under the adapter
contract above; BIR semantics and differential gates run; `EmitBuiltins.lean`'s LLVM-AST helpers
are replaced by `rt`; profile labels are emitted; the old `Lower` is retired only after the complete
replacement path meets its gates.

**S12 Validators (R-0504; with R-0299/R-0300).** The validator interface; validators for SSA
construction and for optimization certificates, each with a stated soundness `Prop` and one
representative proved instance. R-0299/R-0300 validate the LLVM profile: a pinned inventory of
proved functions records validation status per semantic edge and call closure; unsupported closures
stay `translation_trusted`/`blocked`; mismatches are `translation_mismatch` and never grant
native-code evidence.
**Exit:** SSA and optimization validators reject targeted semantic substitutions and unsupported
certificates; R-0299's BackendIR V1 validation and R-0300's independent replay pass their
admitted-subset, mismatch, unsupported and substitution controls; the emitted-LLVM profile checks
the retained `.ll` bytes including helper bodies; LLVM optimization, emission, linking and
runtime/ABI behaviour remain explicitly trusted for that profile.

**S12a Stated obligations and totality (R-0506).** **Immediate row-2 checkpoint:** enforce the transitive axiom policy on today's authoritative evidence
at status, issuance and consumption. Reject `sorryAx` and unregistered axioms; kernel-only evidence
permits only `propext`, `Classical.choice` and `Quot.sound`; registered native-evaluation and float
axioms require distinct trust labels. Positive controls and forbidden-transitive-dependency mutations
qualify this checkpoint independently of S5 and the new pass ledger.

Starts with S5 and grows as each boundary lands
— including S13 and linking — rather than after the lower pipeline: every pass's `Refines` and
every validator's soundness are type-checked Lean `Prop`s in `PassObligations.lean`, with explicit
premises and a ledger (stated / differentially tested / proved); `sorryAx` and unregistered axioms
are rejected transitively, kernel-only evidence permits only `propext`, `Classical.choice` and
`Quot.sound`, and registered native-evaluation and float axioms require separate trust labels; a totality lint covers semantics, checkers, validators and proved
passes (untrusted search may stay partial).
**Exit:** every registered boundary, including the native backend and linking, has a type-checked
obligation with explicit premises and status; evidence dependencies pass the declared axiom policy;
semantics, checkers, validators and proved passes pass the totality gate. The gate's first form is
the totality ratchet (`scripts/tests/check_totality_ratchet.sh`, 2026-10-28): it reads the elaborated
environment, holds every non-total compiler declaration in `docs/compiler/totality_baseline.tsv`
with its stage and reason, fails on a new one, on `sorry` and on an unregistered axiom, and lets the
baseline only shrink; the inventory and conversion order are in
[research/compiler/partial-def-inventory-2026-10.md](../../research/compiler/partial-def-inventory-2026-10.md). Before each boundary is frozen, its
semantic relations and obligations type-check and a representative preservation or
validator-soundness lemma is proved for its motivating feature, including native encoding and
linking.

**S13 Native AArch64 profile: Lean Compiler Backend integration, linking and image (R-0505, R-0517).**
For the full first profile (AArch64, Apple ARM64 ABI, macOS, Mach-O), native code generation is the
separately developed Lean Compiler Backend; Concrete builds no machine backend. **LCB owns** its IR
semantics, machine IR, instruction selection, frame construction, register allocation, assembly,
encoding and their proofs (its C01 – C03, V01 – V05 and E02 milestones are external dependencies,
tracked by R-0517, never scheduled as Concrete work). **Concrete owns:** the BIR → LCB-IR adapter,
which lowers checked overflow to explicit checks or LCB's checked operations and preserves traps,
capability-tagged effects and the runtime ABI, with its refinement proof against LCB's imported
semantics (not a restatement); the pinned dependency (an exact LCB version through a lake/nix input
recording LCB's upstream lock, built with the same Lean toolchain); **separate compilation and
linking** — module interfaces, compatible linking, symbol resolution and imported-contract
satisfaction, source-module linking related to independently compiled object linking, and compiler,
profile, LCB, interface, runtime and dependency identities bound into the image; `rt` and startup; FFI
ABI realization at the adapter boundary; and final-image checks. R-0508 owns the semantic composition
theorem: Concrete's Core → BIR refinement, the adapter refinement and LCB's preservation theorem for
its exact domain, with LCB's named trust boundaries as premises. The existing restricted adapter is
Concrete draft PR #8 (`feat/lean-backend-integration`, `--emit-qbe` from today's SSA).
**Exit:** corpus execution and performance against the LLVM profile are intermediate milestones.
The stage closes only when final-image validation checks actual instruction and data bytes, symbols,
relocations, entry state and startup against the recorded adapter output, LCB artifact and profile;
wrong-instruction, wrong-relocation, wrong-symbol, ABI and startup mutations are rejected; any
assembler, linker or loader behaviour still assumed (by LCB or Concrete) is named in the profile and
theorem premises.

## Part 2 — proofs

Each extends the representative proofs Part 1 landed to the complete admitted profile.

**S14 Type and ownership soundness (R-0507).** Progress and preservation for the Core checker over
Core's instrumented semantics; ghost erasure; trusted and FFI code enter as their contracts. Can
start as soon as S7 and S8 exist.
**Exit:** proved for the full checked subset; S8's hunt finds nothing the theorem does not exclude.

**S15 Compiler correctness and proof transport (R-0508; with R-0460).** Prove every ledger
obligation by simulation or validator soundness. Define refinement over maximal behaviours under
related initial states and admissible environments, with the explicit observation relation for
values, external memory and ghost erasure, and a preserved target-memory validity invariant; prove
native-to-Core refinement for executions satisfying the stated contracts, composed across separate
compilation and linking; state the resource relation and prove the sufficient-resource theorem.
Prove frontend correspondence: source bytes and import roots → HIR → `CheckedCore` against
independently specified syntax, resolution and elaboration judgments (a relation defined as
"whatever the elaborator produces" does not count). Prove VC generation against Core (R-0460).
Each binary-transported claim cites a theorem that its proposition is preserved by the refinement
relation; receipts record the theorem, profile and compiler build.
**Exit:** the composed source-bytes-and-import-roots-to-native-image theorem, including
independently specified frontend correspondence, is proved and re-checked by con-leche and nanoda.
HIR-only and `CheckedCore`-only theorems are intermediate milestones. Every binary-transported claim
cites its preservation theorem.

**S16 Trusted base and independent evidence check (R-0509; with R-0353, R-0413).** `std`'s trusted
implementations verified against contracts or listed with audit (the list only shrinks); the
published trusted-base inventory. R-0353 independently checks retained evidence — decoding,
authenticated roots, subject/dependency binding, exact propositions, replay and policy — without
trusting stored verdicts; it does not by itself establish source-to-model or native-code
correspondence, which carry their own recorded assurance. Codec and hostile-input prototypes may proceed beside queue rows 3 – 8. R-0353's complete cross-profile
consumer remains at row 10, after row 9 and R-0004's prerequisites; a minimal independently checked
retained-artifact consumer is required for every publicly authoritative V1 claim. Prove soundness of `ClaimSpec` statement generation, evidence-to-subject/proposition
binding, authoritative verdict aggregation and retained-artifact checking; audit exact elaborated
axiom dependencies, so kernel-only verdicts admit only the named standard logical axioms while
native-evaluation and float axioms stay separately labelled. R-0413 dogfoods the evidence model onto
the compiler.
**Exit:** the inventory names every remaining assumption and owner; R-0353's acceptance criteria
pass with every 079 – 095 fixture pinning its expected result vector, and no envelope or replay
success upgrades an unchecked semantic edge. Statement generation, subject/proposition binding, verdict aggregation and
retained-artifact checking have proved soundness, with transitive assumptions classified by the
axiom-policy gate.

## Order

```
Track A: S1 ─ S2 ─ S3 ─ S4                                         (now, parallel)
Track B: S5 ─► S6 ─► S7 ─► S8;   S8 ─► S8a;   S2+S7+S8 ─► S9
         S12a starts with S5 and registers each boundary as it lands
Track C:                     S8a ─► S10 ─► S11 ─► S12 ─► S13
Part 2:  S7+S8 ─► S14;   all of the above ─► S15;   S16 inventory starts now
```

S4 fixes the relevant language rules before S7 freezes their semantics. S9 depends on S2, S7 and S8.
S10 consumes `CheckedMonoCore` after S8a. S12a starts with S5; validator implementations follow
their IRs. S14 begins after S7/S8. S15 composes the frontend, checker/specialization,
pass/validator, linking and claim-transport obligations. The trusted-base inventory starts
immediately. A minimal canonical task identity and an independent retained-artifact consumer are
required for each publicly authoritative slice claim from V1 onward; rows 9 – 11 complete cross-profile
interoperability and schema stabilization and do not defer subject/proposition binding, provenance or
independent replay for claims already issued.

Stage order above is the dependency order *within* each slice. Queue row 2 delivers Track A's
protections, the language decisions and gate health; row 3 carries slice V1 through every stage; row 4
makes identities and Core checking authoritative across the supported language and moves the existing
profile onto one `Layout` and Place MIR (soundness proofs remain scoped to the admitted subsets);
row 6 grows the slices and retires the legacy evidence layer. New soundness defects still enter at row 0, and each fix names the stage that would have
prevented it; if none would, the plan gains one.

## Discipline

- **Feature freeze on the proof surface** (no new claim kinds, proof attributes, coverage labels or
  statuses) until S2 and S9 land.
- **No big-bang rewrite.** Each increment builds and passes R-0513's required affected checks. Before
  qualification or release, the full tier passes on the exact candidate commit, locally or on a
  recorded remote runner, including the claim-inventory step; local reproduction remains supported.
  Qualified slices receive review and are published to both remotes before dependent releases rely on
  them. New boundaries arrive as shadow paths with disagreement gates.
- **Capacity and priority.** Known authority and memory-safety defects preempt planned work and are
  repaired or contained fail-closed before affected releases, under the roadmap's defect disposition
  rule. Effort and integration slots are divided by the roadmap's
  [capacity allocation](../../ROADMAP.md#capacity-allocation): this pipeline holds the majority share
  and two slots (one shared-compiler migration checkpoint, one proof checkpoint); evidence integrity and
  adversarial validation have protected minimums; interop, application, usability and release work
  share one capped slot. The next checkpoint, its accountable maintainer
  and its required reviewer are recorded before it starts. Agents assist implementation and
  investigation but do not replace independent semantic review.
- **Stage completion.** A stage closes when its stated proof obligations, compatibility requirements and
  qualified regression gates pass. Hunts and mutations supplement these exits; a hunt that finds nothing
  is not proof of completeness.
- **Proof engineering from V1.** Reusable libraries for behaviour refinement, clock adequacy, stuttering,
  composition, memory injections, representation, substitution, renaming, frames and ownership. Each pass
  exposes stable well-formedness and simulation interfaces; where a slice does not yet need a stage,
  identity passes or proved trivial instances keep the declared artifact types. Proofs depend on
  documented semantic interfaces, not unrestricted unfolding of implementations. A semantics change ships
  with a dependency-impact review, migrated proofs and a checked compatibility or version-change
  argument; each release rechecks the cumulative theorem and its complete assumption closure.
- **Lean scalability.** Pinned Lean, libraries and external recheckers. Benchmarks for clean and
  incremental elaboration, kernel and independent rechecking, peak memory, proof-term and artifact size,
  validator execution and representative client proofs, with recorded budgets and scaling tests before
  large claim migration. Symbolic evaluation stays controlled and modular — no whole-program
  normalization or unlimited global simplification.
- **Compiler execution assurance.** The theorem is about the Lean compilation function; the executable
  that produced an image is a separate trust question. Lean code generation, native libraries and
  foreign implementations are recorded in the execution TCB; kernel-checked per-compilation certificates
  or proved validators whose results are checked by the declared trusted path come first. Native
  execution of a proved validator alone does not remove execution trust. A separate long-term plan
  reduces compiler and checker execution trust further (CakeML's bootstrapping is the reference point).
- **Ownership and review.** An accountable maintainer and a second reviewer for semantics/checker proofs,
  memory and runtime, backend/ISA, and evidence/release infrastructure. Roles may overlap, but major
  semantic interfaces get independent specialist review, with continuing expert review and maintainer
  succession budgeted. Automated review (Codex) assists rather than replaces this.
- **Specification validation.** Before each semantic extension is admitted: executable examples,
  rejection cases, representation counterexamples, observation boundaries and theorem premises reviewed,
  with accepted examples showing the subset is useful and non-vacuous. Memory/borrowing, unsafe
  abstractions, capabilities, runtime and linking are reviewed separately; decisions and unresolved
  assumptions are recorded.
- **Release strategy.** Versioned supported subsets, semantics, target profiles, assumptions and known
  limitations. Each release retains source and import identities, checked IRs, proof dependencies,
  compiler/runtime identities and final-image bindings. A supported production branch, a verification
  development track, migration guidance and rollback procedures. Real application acceptance,
  diagnostics, compile time and runtime performance are tracked alongside proof coverage. Breaking
  language changes are versioned with migration support. The release rhythm (nightlies, release trains)
  and the milestone scopes are defined once, in the roadmap's [Releases](../../ROADMAP.md#releases).
- **Review.** Every slice is reviewed by Codex (`codex exec -s read-only` on the commit, with the
  stage's section as specification); findings are fixed or answered in the commit message before
  publication. Review complements the gates.
