# Verified-compiler pipeline

Status: planned, 2026-10-18. Owners: R-0491 – R-0513 in [ROADMAP.md](../../ROADMAP.md), with
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
| **Machine IR → allocated Machine IR** | AArch64 operations and addressing modes, virtual then physical registers, spills, calling convention, frames | block memory + frame | new |
| **Asm → loaded image** | linearized code, prologue/epilogue, encoding, relocations, startup | ISA model over decoded bytes | new |

LLVM is printed from BIR (block arguments to phis, `.ovf` ops to `*.with.overflow`). It is the
fast, **unverified** profile; its binaries are labelled so, and only the native profile carries
proof transport. Unverified does not mean unspecified: the BIR → LLVM adapter expands Concrete's
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
pipeline; libc becomes FFI with stated contracts. Native output starts as assembly text through a
named-trust assembler, then owns encoding and validates the final linked, loaded image.

### Trusted at the end

The Lean kernel (re-checked by con-leche and nanoda); the ISA model; the operating system as
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

**Coexistence.** The verified pipeline is a `verified` compilation profile beside the existing
compiler. It shares the front end, accepts only programs inside the current slice and refuses
everything else with a named reason; the existing compiler keeps compiling the whole language. A
construct moves to the verified pipeline when its slice lands, and the existing path for it is
retired once the slice's gates pass on the corpus. The differential fuzzer compares both profiles on
every slice program from the day the profile exists.

**Slice V1 (queue row 3, R-0511).**
- *Language:* integer scalars of every width and `bool`; immutable and mutable locals; assignment;
  `if`/`else`; `while` and `for` with `break`/`continue`; direct calls, including recursion;
  checked arithmetic, division and shifts with defined traps; integer casts; comparisons; one
  capability-tagged output event (`print_int`) so traces are non-trivial. No aggregates, heap,
  references, pointers, generics, traits or FFI.
- *Stages exercised:* S5/S6 (identities, typed operations) for the slice, S7 (Core semantics), S8 (the
  Core checker; all V1 values are Copy, so linearity is trivial), S9 (generated claim statements on
  Core), S10 – S11 (Place MIR, a stack-only Memory CFG, BIR), S12a (stated obligations), S13 (native
  AArch64 for the slice).
- *Theorem:* for every program the checker accepts within V1, every behaviour of the native image is a
  behaviour of its `CheckedCore` (traces, traps and divergence preserved), re-checked by con-leche and
  nanoda; and one slice claim is transported to its binary through its preservation lemma.
- *Front end:* V1's theorem starts at `CheckedCore`. Elaboration correspondence for the V1 surface
  (HIR → `CheckedCore`) is V1b; parser correspondence joins at V7. The published theorem says which
  artifact it starts from.
- *Review:* an outside expert reviews Core's V1 semantics before it freezes.

**Later slices (queue row 5).** Each adds its constructs to every stage and extends the theorem:
V2 structs, enums, `match` and fixed arrays with bounds traps (value semantics, stack memory);
V3 heap and linear resources (`rt` allocation, destroy, `defer`, out-of-memory behaviour) with
ownership soundness (S14) for the covered subset; V4 second-class references and borrows (borrow
validity); V5 generics, traits, monomorphization and capabilities (S8a, post-Mono capability checks);
V6 raw pointers, `Unsafe`, FFI with the byte-and-provenance memory component, external-call contracts,
separate compilation and linking; V7 the rest of the language and `std`, frontend correspondence
from source bytes (S15), and retirement of the existing pipeline.

**Legacy evidence retirement (R-0512).** The current evidence layer — `PExpr` and its evaluator, the
name tables, the theorem-type classification generator, the frozen identity rows and their
amendments, attestations and per-surface status logic — receives soundness fixes only. As a slice
lands, every existing claim on functions within it moves to a compiler-generated Core statement; when
the last claim has moved, the layer and the gates that guard it are deleted.

**Protections in the meantime (queue row 2).** S1 (prevention harness), S2 (one verdict authority),
S3 (demonstrated enforcement) and S4 (written language rules) protect users of the existing compiler
for the years the legacy pipeline remains. The language decisions the slices depend on — strictly
second-class references, no block `let` shadowing, explicit match binders, typed literals — are
decided and enforced in the existing compiler first, so the verified pipeline never has to model the
looser rules. R-0513 makes gates assert properties instead of corpus counts and splits a fast push
tier from the full batch tier.

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
checker hunt run before every release candidate.
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
explicitly versioned. Pipeline artifacts become immutable and payload-bound (`ValidatedCore` is
publicly constructible today, `Pipeline.lean` ~:48), with diagnostic artifacts typed separately.
Language: no block `let` shadowing a visible name; colliding import aliases rejected; match binders
equal or name their field.
**Exit:** `check_ir_no_string_identity.sh` rejects string identity joins in resolved tables, the
source checker's environments, Core, SSA and `PExpr`; 081, 082, 087, 093 and 094 reject or bind
correctly; targeted mutations show checker consumption/block-local accounting, `ElabVerify` and the
theorem-table identity checks load-bearing; compile-fail controls cover artifact forgery and
diagnostic-path bypass.

**S6 Typed operations (R-0493).** Depends on S5. Integer ops typed by `IntTy` with
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
where that is what was proved; `Proof.eval` and extraction deleted; Core-proved vs
binary-transported status.
R-0495 checks coverage relation and domain against the exact generated proposition; author
annotations confer no authority. Loop claims bind the exact checked Core loop and its
initialization, preservation and exit obligations; missing binding remains `spec_unbound`.
**Exit:** every positive claim replays against its generated statement; negative fixtures refuse;
`PExpr`, its evaluator and the classification theorem scan are deleted; wrong-coverage,
narrowed-domain and substituted-loop controls are refused through status, issuance, consumption and
independent replay.

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

**S12a Stated obligations and totality (R-0506).** Starts with S5 and grows as each boundary lands
— including S13 and linking — rather than after the lower pipeline: every pass's `Refines` and
every validator's soundness are type-checked Lean `Prop`s in `PassObligations.lean`, with explicit
premises and a ledger (stated / differentially tested / proved); `sorryAx` and unregistered axioms
are rejected transitively, kernel-only evidence permits only `propext`, `Classical.choice` and
`Quot.sound`, and registered native-evaluation and float axioms require separate trust labels; a totality lint covers semantics, checkers, validators and proved
passes (untrusted search may stay partial).
**Exit:** every registered boundary, including the native backend and linking, has a type-checked
obligation with explicit premises and status; evidence dependencies pass the declared axiom policy;
semantics, checkers, validators and proved passes pass the totality gate. Before each boundary is frozen, its
semantic relations and obligations type-check and a representative preservation or
validator-soundness lemma is proved for its motivating feature, including native encoding and
linking.

**S13 Native AArch64 backend, linking and image (R-0505; with R-0297).** For the full first profile
(AArch64, Apple ARM64 ABI, macOS, Mach-O): Machine IR, instruction selection, frame construction,
register allocation (untrusted search + validator), assembly, an ISA model with its semantics;
assembly text through a named-trust assembler first, then owned encoding. **Separate compilation
and linking:** module interfaces, compatible linking, symbol resolution and imported-contract
satisfaction; source-module linking related to independently compiled object linking; compiler,
profile, interface, runtime and dependency identities bound into the image. FFI ABI realization
covers callbacks, external memory and aggregate argument/return classification. R-0505 owns ABI
realization, object/image validation and startup; R-0508 owns the semantic composition theorem.
**Exit:** corpus execution and performance against the LLVM profile are intermediate milestones.
The stage closes only when owned encoding and final-image validation check actual instruction and
data bytes, symbols, relocations, entry state and startup against the recorded Machine IR and
profile; wrong-instruction, wrong-relocation, wrong-symbol, ABI and startup mutations are rejected;
any assembler, linker or loader behaviour still assumed is named in the profile and theorem
premises.

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
correspondence, which carry their own recorded assurance. Codec and hostile-input prototypes may
proceed beside queue rows 3 – 6; authoritative use stays queue row 8, after row 7 and R-0004's
prerequisites. Prove soundness of `ClaimSpec` statement generation, evidence-to-subject/proposition
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
immediately; R-0353's authoritative use keeps queue row 8.

Stage order above is the dependency order *within* each slice. Queue row 2 delivers Track A's
protections, the language decisions and gate health; row 3 carries slice V1 through every stage; row 5
grows the slices and retires the legacy evidence layer. New soundness defects still enter at row 0, and each fix names the stage that would have
prevented it; if none would, the plan gains one.

## Discipline

- **Feature freeze on the proof surface** (no new claim kinds, proof attributes, coverage labels or
  statuses) until S2 and S9 land.
- **No big-bang rewrite.** Every slice builds, passes the full CI gate set locally (including the
  claim-inventory step otherwise run only on Linux CI), is reviewed by Codex, and is published to
  both remotes before the next builds on it. New boundaries arrive as shadow paths with
  disagreement gates.
- **Measure by hunting.** After each stage, rerun the hunts for its classes; a stage closes when its
  hunt finds nothing reproducible of its class.
- **Review.** Every slice is reviewed by Codex (`codex exec -s read-only` on the commit, with the
  stage's section as specification); findings are fixed or answered in the commit message before
  publication. Review complements the gates.
