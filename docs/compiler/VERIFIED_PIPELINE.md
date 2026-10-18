# Verified-compiler pipeline

Status: planned, 2026-10-18. Owners: R-0491 – R-0509 in [ROADMAP.md](../../ROADMAP.md), with
R-0299, R-0300, R-0353, R-0413 and R-0460 linked where named. The architecture reconciles two
independent designs (a Claude design agent and Codex) and two Codex reviews of earlier drafts (24
findings, all applied).

## Goal

The best achievable assurance, whatever the duration: the guarantee CompCert and CakeML give — a
machine-checked theorem that the compiler preserves the language's meaning, so every proof about a
program holds of its binary. Concrete is written in Lean, so this is reachable in place.

**Part 1 builds the pipeline** so every end-state theorem is statable and provable without
restructuring again. It starts now and pays for itself immediately: each semantics is also a
differential oracle, each typed artifact removes a bug class. **Part 2 proves the theorems**
against the structure Part 1 leaves.

## What "no bugs" means here

No engineering process makes a compiler free of every bug. A verified compiler does something
stronger and exact: it turns "we tested it" into "it is a theorem", for named bug classes, relative
to a written specification. At the end of Part 2:

| bug class | status at the end | by |
|---|---|---|
| **Miscompilation** — the binary does something the program does not | impossible for the native profile: every binary behaviour is a Core behaviour | S15 compiler correctness |
| **Memory and ownership unsafety** in accepted safe code — use after move, double free, leak at exit, dangling or escaped reference, undeclared capability use | impossible: accepted programs never reach such a state | S14 type and ownership soundness |
| **False evidence** — "proved" for code the theorem is not about, or for a claim it does not state | impossible: verdicts come from kernel-checked generated statements about the exact checked term, and transport to the binary is a theorem | S2, S9, S15 |
| **Model/runtime disagreement** | impossible: there is one semantics, and the interpreter is it | S7, S9 |

What remains, stated rather than hidden, and how each is contained:

- **The specification can be wrong** — Core's semantics or a rule may not say what we intend. This
  is the one class proof cannot remove. Contained by: the written rules with accepted/rejected
  examples (S4), the semantics being executable and differentially tested against the binary and
  the corpus from the day it exists (S1, S7), small and reviewable definitions, Codex review, and
  the scheduled hunts.
- **The trusted base** (kernel, ISA model, OS contracts, resource premises, and for the LLVM profile
  LLVM and the system toolchain) — listed, minimized, and re-checked where possible (con-leche and
  nanoda for the kernel; hardware differential tests for the ISA model).
- **Code before the checked artifact** (parser, resolver, elaborator) — it cannot produce an unsafe
  or miscompiled program, because everything after it is checked and proved; at worst it builds
  Core that differs from the surface text. Contained by proving elaboration against the
  declarative relation (S15) and by S1's differential tests until then.
- **Bugs in `std` and trusted code relative to their contracts** — each trusted implementation is
  verified against its contract or listed with its audit (S16), and claims reaching it say so.
- **Programs that do what they say, but not what the author meant** — outside a compiler's reach;
  the claim and contract machinery is how a user states and proves what they meant.

Until Part 2 lands, Part 1 does not prove these, but it makes each class mechanically caught
(per-pass differential against a real semantics, the authoritative Core checker, the single
verdict authority) instead of found by hunting.

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
- **Style.** CakeML's: deterministic, clock-indexed, functional semantics per IR, executable as
  written, with behaviours (including divergence and infinite interaction) defined from all
  clocks. Deterministic semantics let forward simulations yield backward simulation (CompCert's
  argument). Exhausting a clock is *inconclusive*, never evidence of failure or termination.
  Alternative not chosen: relational small-step semantics as the normative definition with a
  separate runner — more proof machinery and a second artifact to keep in agreement.
- **Floats** are IEEE 754 with fixed rounding; **traps** carry a ghost kind (the binary only
  aborts).

### Language decisions this requires

- **References are strictly second-class:** locals and downward parameters only, never stored in
  aggregates or the heap, never returned. Today a reference can be stored in a struct field
  (bug 092's reproducer compiles); the language closes that. This is what gives safe Concrete
  exact value semantics.
- **Raw pointers** remain under `Unsafe`/`trusted`; authorization does not establish validity.
  Pointer arithmetic, integer↔pointer casts and address exposure are distinct operations with
  explicit semantics (quasi-concrete addresses: a block gets an address only when exposed).
- **No implicit destruction** (OWNERSHIP_MODEL); linear conservation is required at reachable
  exits, not along divergence.
- Disputed surface rules (shadowing, match binding, statement vs value, literal typing) are fixed
  by writing them down (S4) and then defining them by elaboration into Core.

### IR chain

| IR | makes explicit | semantics / memory | evolves from |
|---|---|---|---|
| **HIR** | resolved surface: scoped `LocalId`/`DefId`, spans | defined by elaboration into Core | AST after Resolve/desugar |
| **Core** | the language definition: typed operations (checked/wrapping/saturating as distinct op kinds, signedness on the op, pointer arithmetic its own op), places vs operands, copy/move on every operand, borrows, explicit destruction, structured control, scoped `defer`, capability-tagged external operations | **value semantics**: structured values and allocation identities, no bytes; ghost ownership state in an instrumented mode | `Elab/Core.lean`; `Interp.lean` becomes this semantics plus an IO driver |
| **Mono Core** | concrete type/capability substitutions, resolved impls | same semantics, separate validated artifact | `Mono.lean` |
| **Place MIR** | typed non-SSA CFG: fixed evaluation order, match decision trees, cleanup/`defer` edges on every exit, loan boundaries, explicit runtime checks; symbolic fields, stable storage identities | value semantics over places | the first half of `Lower.lean` (defer ~:1525 – 1554, match, promotion ~:559, bounds ~:171) |
| **Memory CFG** | layout from the single `Layout` authority, offsets, alignment, loads/stores, allocation/free, scalar representations | CompCert block–offset memory (bytes, pointer fragments, undef, permissions) | the second half of `Lower.lean`, `Layout.lean` |
| **BIR** | CFG with block arguments (no phi instruction; SSA is a well-formedness property, not semantics), `RegId`/`BlockId`, `add.ovf`-style ops returning (value, flag), `trap` terminator | block memory | `SSA.lean` (which has both `phi` and block params today) |
| **Machine IR → allocated Machine IR** | AArch64 operations and addressing modes, virtual then physical registers, spills, calling convention, frames | block memory + frame | new |
| **Asm → loaded image** | linearized code, prologue/epilogue, encoding, relocations, startup | ISA model over decoded bytes | new |

LLVM is printed from BIR — a thin printer (block arguments to phis, `.ovf` ops to
`*.with.overflow`). It is the fast, **unverified** profile; its binaries are labelled so. The
native profile carries proof transport.

### Where each check lives

- **Elaboration** produces Core; a declarative elaboration relation defines the surface language.
- **The authoritative checker runs on Core** and emits an immutable `CheckedCore` bound to the
  exact program and imported interfaces, with a derivation (consumed/frozen sets, loans per
  statement) that a small independent checker re-checks. It owns typing, literal ranges, Copy
  constraints, linear conservation at exits, loan overlap and provenance, recursive escape,
  exhaustiveness, trait signature *and effect* compatibility, `Unsafe` obligations. The source-tree
  checker survives only for early diagnostics until the switch, then for purely syntactic checks.
- **Capabilities are re-checked after Mono** over the call graph including function-pointer target
  sets, using the same rules; the semantics tags each external event with its capability.
  Capabilities are not purity: mutation/effect summaries are separate.
- **Bounds, overflow and division are semantics, not static checks:** trapping operations in Core.
  A pass may remove a trap only when handed a discharged-VC certificate naming that operation.
- **Layout/ABI** checking emits representation certificates; each later IR's verifier emits its
  well-formedness artifact.
- Private constructors are API discipline, not soundness: artifacts bind their exact payload, and
  the soundness argument is the theorems.

### Passes

Uniform interfaces (stated in Lean now, proved in Part 2):
`WF p → pass p = .ok q → BackSim p q`, and for validated passes
`WF p → validate p q cert = true → WF q ∧ BackSim p q`.

| pass | strategy |
|---|---|
| parse, resolve, hygienic desugar, elaborate | output independently checked by the Core checker; elaboration later proved against the declarative relation |
| specialization (Mono) | proved substitution theorem; worklist coverage and specialization identity validated |
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
to a binary only through the compiler-correctness theorem for the profile that built it; hashes
identify artifacts, they do not prove correspondence.

### Backend and runtime

AArch64 first (the host triple, `Backend.lean` ~:18), so native and LLVM binaries are differentially
tested on the same machine. Runtime helpers (today 875 lines of LLVM AST in `EmitBuiltins.lean`,
plus raw LLVM strings) are rewritten in Concrete as an `rt` module compiled through the same
pipeline; libc becomes FFI with stated contracts. Native output starts as assembly text through a
named-trust assembler, then owns encoding and validates the final linked, loaded image.

### Trusted at the end

The Lean kernel (re-checked by con-leche and nanoda); the ISA model; the operating system as
specified by external-call contracts; listed resource premises; for the LLVM profile, LLVM and the
system toolchain; and the compiler's own execution only where no proved validator checks its
output (listed and minimized). Native execution of a validator is not kernel evidence: validators
are proved, and their compiled execution is in the trusted list.

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

**S5 Identities and immutable artifacts (R-0492).** `LocalId` for every binder, allocated at
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
an IO driver. It is the oracle for S1 from here on.
**Exit:** covers every construct the checker accepts; the interpreter is the semantics; the
differential fuzzer compares it with the binary; S4's paragraphs link to their clauses.

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
rule-breaking mutation to accepted programs (including destroying a source while an
aggregate-held reference is live, conflicting borrows, reaching a capability through a pointer or
generic, raw dereference outside both `trusted` and `Unsafe`); valid programs that must trap are
not checker rejections. Positive controls: trusted raw-pointer operations and trusted absorption of
`Unsafe` accepted; trusted absorption of any other effect rejected. The instrumented semantics is
the runtime witness.
**Exit:** every rule has an enforcing site, an S3 manifest row and a mutation class; the CI seed
set rejects every mutant and accepts every well-formed program with minimum completed counts per
rule; the instrumented semantics runs the corpus and fuzzer programs with zero violations; 090 –
094 and the 087 borrow-block case are rejected by the Core checker; the source checker holds no
ownership, borrow or capability rule.

**S9 Claims on Core (R-0495).** Depends on S2, S7. Typed `ClaimSpec`; statements generated against
Core's semantics on the exact checked term; ProofKit's symbolic-evaluation simp set; existing
`PExpr` theorems ported (through an explicitly proved bridge where cheaper), keeping weaker coverage
where that is what was proved; `Proof.eval` and extraction deleted; Core-proved vs
binary-transported status.
**Exit:** every positive claim replays against its generated statement; negative fixtures refuse;
`PExpr`, its evaluator and the classification theorem scan are deleted.

### Track C — the lower pipeline

**S10 Place MIR (R-0501).** Depends on S7, S8. Carve sequencing, match compilation, cleanup/`defer`
edges and check expansion out of `Lower.lean` into Core → Place MIR passes, one job each, with a
Place MIR semantics; per-pass differential against Core's semantics.
**Exit:** `Lower` no longer performs these jobs; each pass is total and differentially tested with
minimum counts.

**S11 Memory model, layout and BIR (R-0502, R-0503).** Block–offset memory with permissions and
quasi-concrete address exposure; `Layout` as the single proved authority; Memory CFG; BIR with block
arguments, `.ovf` ops and `trap` terminators, each with a semantics; LLVM printed from BIR; the
emitter decides no semantics; runtime helpers moved into the `rt` module; the backend contract text
generated from BIR op semantics. New lowering runs as a shadow path; when outputs agree on the
corpus the old `Lower` is deleted.
**Exit:** LLVM is emitted only from BIR; every IR has a semantics with per-pass differential;
`EmitBuiltins.lean`'s LLVM-AST helpers are replaced by `rt`; evidence surfaces carry the profile
label.

**S12 Validators and stated obligations (R-0504, R-0506; with R-0299/R-0300).** The validator
interface; validators for SSA construction and for optimization certificates; every pass's
`BackSim` and every validator's soundness stated as Lean `Prop`s in `PassObligations.lean` with a
ledger (stated / differentially tested / proved); `axiom` and `sorry` refused in anything evidence
depends on; totality lint over semantics, checkers, validators and proved passes. R-0299/R-0300
validate the LLVM profile: a pinned inventory of proved functions records validation status per
semantic edge and call closure; unsupported closures stay `translation_trusted`/`blocked`;
mismatches are `translation_mismatch` and never grant native-code evidence.
**Exit:** every pass and validator has a stated obligation exercised by differential tests; the
totality lint passes with no allowlist; R-0299's BackendIR V1 validation and R-0300's independent
replay pass with their mismatch, unsupported and substitution controls, and the emitted-LLVM
profile checks the retained `.ll` bytes including helper bodies; LLVM optimization, emission,
linking and runtime/ABI behaviour remain explicitly trusted for that profile.

**S13 Native AArch64 backend (R-0505; builds on R-0297).** Machine IR, instruction selection, frame
construction, register allocation (untrusted search + validator), assembly, an ISA model with its
semantics; assembly text through a named-trust assembler first, then owned encoding with
final-image validation and hardware differential tests.
**Exit:** the corpus and every proved example build and run natively; per-pass differential down
to assembly; performance against the LLVM profile measured and published.

## Part 2 — proofs

Each proves obligations Part 1 stated; none requires restructuring.

**S14 Type and ownership soundness (R-0507).** Progress and preservation for the Core checker over
Core's instrumented semantics; ghost erasure; trusted and FFI code enter as their contracts. Can
start as soon as S7 and S8 exist.
**Exit:** proved for the full checked subset; S8's hunt finds nothing the theorem does not exclude.

**S15 Compiler correctness and proof transport (R-0508; with R-0460).** Prove every ledger
obligation by simulation or validator soundness; compose them into
`Beh(native image) ⊆ Beh(Core)` under the stated resource and environment premises; prove VC
generation against Core (R-0460); derive transport of Core claims to native binaries; receipts
record the theorem and compiler build.
**Exit:** the composed theorem is proved and re-checked by con-leche and nanoda; every proved claim
reports binary-transported for its native build.

**S16 Trusted base and independent evidence check (R-0509; with R-0353, R-0413).** `std`'s trusted
implementations verified against contracts or listed with audit (the list only shrinks); the
published trusted-base inventory. R-0353 independently checks retained evidence — decoding,
authenticated roots, subject/dependency binding, exact propositions, replay and policy — without
trusting stored verdicts; it does not by itself establish source-to-model or native-code
correspondence, which carry their own recorded assurance. Codec and hostile-input prototypes may
proceed beside queue rows 3 – 4; authoritative use stays queue row 6, after row 5 and R-0004's
prerequisites. R-0413 dogfoods the evidence model onto the compiler.
**Exit:** the inventory names every remaining assumption and owner; R-0353's acceptance criteria
pass with every 079 – 095 fixture pinning its expected result vector, and no envelope or replay
success upgrades an unchecked semantic edge.

## Order

```
Track A: S1 ─ S2 ─ S3 ─ S4                                  (now, parallel)
Track B: S5 ─► S6 ─► S7 ─► S8 ─► S9
Track C:              S7 ─► S10 ─► S11 ─► S12 ─► S13
Part 2:               S7+S8 ─► S14;   S12+S13 ─► S15;   S16 continuous
```

Queue row 2 covers Track A and Track B through S8 (identities, typed operations, Core semantics,
the authoritative Core checker). S9 – S13 and Part 2 are a permanent track beside the delivery
queue. New soundness defects still enter at row 0, and each fix names the stage that would have
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
