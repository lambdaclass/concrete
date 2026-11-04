# Roadmap Decision Log

Status: reference — the dated decisions that shape [the current execution
queue](../../ROADMAP.md#the-queue). None creates another queue. Decisions on the
language rules that the project owner reserved are listed under
[owner decisions](../spec/decisions.md#owner-decisions) in ROADMAP.md and recorded in
[decisions.md](../spec/decisions.md); nothing here answers one of them.

Moved verbatim from ROADMAP.md on 2026-10-28 (the roadmap compaction); only heading levels and relative links were adjusted.

## Decisions

The dated decisions that shape the queue. Each names the tasks that carry it; none creates another
queue.

**Authority decision, 2026-11-01 (owner):** `with(...)` is the only permission mechanism, and linear values
are the only resource mechanism. An operation needs both, and the permission-token concept is dropped. `Std`
is removed as a capability. This follows a Codex and Grok review, which concluded "adopt with changes"; Kimi
could not run. The resulting rules are listed at the top of [ROADMAP.md](../../ROADMAP.md). It supersedes the
Root-token readings of OD38 and OD156. OD8's singletons remain, as linear values that the runtime creates and
hands to `main`.

**Roadmap restructure, 2026-11-02 (owner):** the roadmap is rebuilt around the new compiler in
phases A (Core and theorem), B (slice 1, then the old pipeline deleted), C (the file-copy program;
0.1 is cut here from the new compiler), D (the whole language; paused workloads resume as their
constructs are supported), E (verified growth), F (evidence as a product) and G (release and
ecosystem), with development throughput beside them. The old execution plan is archived verbatim; the
owner-decision answers have one home, [decisions.md](../spec/decisions.md); the bug dashboard moves to
`docs/bugs/` and legacy defects are recorded, never dispatched for repair; the capacity shares are
replaced by one compiler sequence. The tag `legacy-compiler-final` makes the freeze real. Codex reviewed
the plan and its corrections are adopted. The plan: [ROADMAP_RESTRUCTURE.md](ROADMAP_RESTRUCTURE.md).

**Decisions of 2026-11-02 (owner, and the owner's rule):** the owner paused RocksDB slice 1, the
ethlambda workloads and the usability track until the new compiler can run them, and set the rule
"anything where you and codex agree that is the best decision, record the solution". Under it: the
new-compiler architecture is approved with three conditions, 0.1 is cut from the new compiler, slice
1's move test uses an empty struct without `Copy`, a file-copy program orders the later slices, and the
roadmap is restructured around the new compiler. The language answers of the same day are in
[decisions.md](../spec/decisions.md#decided-2026-11-02).

**New compiler decision, 2026-11-01 (owner):** the current compiler is not migrated to the decided
language. A new compiler is written from the Lean definition, proof-first: a `ConcreteSpec` library
(grammar, Core, semantic contract, judgments, elaborator, reference evaluator, examples, generator ([revised 2026-11-04](#2026-11-04--lean-is-the-specification): the Lean definitions are the spec, and no prose is generated from them)) and
a new lowering chain below Core. The current compiler is to be frozen and tagged `legacy-compiler-final`,
used as a differential oracle (and, until replaced, by the site's browser compiler), and deleted from
main when slice 1 works end to end. This changes the implementation strategy only; the Lean-only
source decision still governs and no language question is settled by it. The architecture of the
library and the new compiler, a reviewed proposal at the time, was approved on 2026-11-02 (entry above). The first
deliverable is the target Core proposal for the owner. The owner: "i don't care about the current
pipeline, we could remove it and start from scratch". The plan is at the top of
[ROADMAP.md](../../ROADMAP.md).

**Lean-only source decision, 2026-11-01 (owner):** Lean is the only source of the language. Core, its
typing, ownership and effect rules, its execution semantics and the desugaring of every surface feature are
Lean definitions, written in Lean from the start. The spec, its rule registry and the conformance oracle are
generated from them, the reference interpreter is the Lean semantics itself (replacing `--interp`), and every other document
cites the generated spec ([revised 2026-11-04](#2026-11-04--lean-is-the-specification): the Lean definitions are the spec, and no prose is generated from them). This supersedes the prose-plus-Lean reading of the spec rewrite decision below.
The same day, a research review (Codex: adopt with changes) set the order of the compiler proof: semantic
contract, executable Core semantics, one end-to-end path, functional models replacing ProofCore,
open-module correctness, ghost permissions for trusted code, SMT-checked lowering. Both are at the top of
[ROADMAP.md](../../ROADMAP.md).

**Spec rewrite decision, 2026-11-01 (owner):** the owner chose to rewrite the specification around
one formal source: a small Core whose surface features are defined by desugaring, and a single Lean
definition from which the spec's formal parts, the reference interpreter, the conformance oracle and the
proof definitions are generated (the WebAssembly SpecTec model) ([revised 2026-11-04](#2026-11-04--lean-is-the-specification): the Lean definitions are the spec, and no prose is generated from them). This is the most important work in row 0s.
Steps 3, 4, 6 and 8 of the spec plan are carried out through it. The plan is at the top of
[ROADMAP.md](../../ROADMAP.md).

**Specification-first decision, 2026-10-30 (owner):** "the spec is the most important thing to do."
Row 0s (R-0555) becomes the top priority. Until its steps 1 – 6 hold (owner decisions, absorption with a
coverage audit, simplification, one readable document, nothing outside the spec, the conformance suite),
only row-0 silent soundness repairs and the landing of already-validated branches continue beside it, and
no other row starts. For that period this overrides the 2026-10-21 priority decision's ordering, the
capacity allocation and every concurrent-work permission in this log and in ROADMAP.md. After that, the
verified-compiler track resumes under the capacity allocation and carries steps 7 – 9 (migration under
exit 2q, Lean links with R-0496, the outside-reader test). The nine-step plan with its completion criteria
is at the top of [ROADMAP.md](../../ROADMAP.md). The owner's standing preferences apply: strict by default,
Austral by default with written deviations, verbose code is acceptable when it is clear and safer, the
grammar stays LL(1), and every language design decision is the owner's.

**Priority decision, 2026-10-21:** the verified compiler is the primary engineering track, protected
by an explicit [capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation) rather than by queue numbering. Two inputs
were weighed. An external review asked for exactly that, in the order: close known soundness defects
and fix the semantics proofs rely on; make the Core checker authoritative with explicit identities and
ownership/capability rules; V1a; V1b through the backend to the native image; then grow the subset
from real programs, with one small executable workload alongside and evidence integrity as a second
protected track. Separately, point repairs are not converging: probes found bugs 102 – 120 in about
two days, nearly all of them new instances of [structural classes](../bugs/STRUCTURAL_CLASSES.md)
whose owners are verified-pipeline Part 1 stages. Adopted: the capacity table (verified compiler at
least 55%, evidence integrity at least 15%, adversarial validation at least 10%, features at most 20%
in a single slot); the [defect disposition rule](../../ROADMAP.md#the-queue); Part 1's structure as
row 4's migration track once row 0's in-flight candidates are accepted, extended from identities and
the checker to one `Layout` (R-0502) and Place MIR (R-0501) in the existing profile, because classes 8
and 12 produce wrong code in the profile users run today; V1c as a named deliverable, so that a
Core-to-native theorem is never presented as source-to-native; bounded scope for the ethlambda port,
`std` breadth and tooling until V1b. Not adopted, with reasons: (1) repairing only memory-safety and
wrong-code defects: false proofs and unsound claims are as silent, and the evidence is the product, so
they are stopped at once too; and a silent defect may be contained fail-closed instead of repaired
when that is smaller. (2) Moving rows 9 – 11 ahead of the slices: every published V1 claim already
requires their bounded increments (minimal task identity, the independent retained-artifact consumer,
final-image binding), and the complete cross-profile consumer and the receipt freeze have no consumer
before V1b; track B's protected share keeps those increments from being starved instead. (3)
Whole-language Place MIR and layout proofs now: the structure moves now, and its semantics and proofs
stay per slice, as the vertical-slice decision requires. (4) Pausing feature work: RocksDB slice 1
continues in its slot, because executable workloads are how defects surface. A second round of
external advice, on day-to-day engineering, is attached to existing owners as acceptance milestones.
Adopted: a development-speed baseline (clean build, one-function-edit rebuild, peak compiler memory,
test turnaround, recorded per main commit) that must exist before row 4's first structural change lands
(R-0355, now named by row 4); a clean-machine package exercise with dependency upgrade and roll-back
(R-0285, R-0365); a shared fault-injection checkpoint for runtime, `std` and FFI, exercised composed in
RocksDB slice 1's companions (R-0030); a practical debugging checkpoint, in which a non-author diagnoses
a runtime trap and a failing foreign call, required before workloads expand beyond RocksDB slice 1
(R-0515 with R-0281); a usable `std` subset named from the first two workloads and validated for
ownership, errors and the foreign boundary together (R-0021, R-0488); and pilots that measure
maintenance across a compiler and a dependency upgrade (R-0515). Two placements differ from the advice.
The upgrade-and-roll-back half of the package exercise closes the four-adapter checkpoint rather than
RocksDB slice 1, so the slice stays small; slice 1's acceptance already covers install to test. The
debugging checkpoint does not gate slice 1, as the advice itself says. Concurrency, a public registry
and additional backends already wait in the backlog behind workload triggers (R-0399, the registry
task, the Wasm and freestanding targets); broad `std` expansion in this file (R-0031) is now bounded
the same way. No new task ID was needed. No exit or acceptance criterion is removed; exits assigned
before this date keep their obligations.

**Semantics decisions, 2026-10-21:** a list of ten language-semantics proposals tied to bug records and
four items of external advice were evaluated against probes of main (both consumers) and corpus
counts, and every recommendation was adopted as evaluated, each written into its canonical document
with its status on main, enforcing site, the bugs it prevents and its owning exit. **Adopted:**
references exist only as parameters, call arguments, borrow-block binders and arm-scoped pattern
binders, no reference binding is introduced by `let` and none is ever assigned (needed because
reference parameters are assignable today), and constants are values (MEMORY_SEMANTICS §15; exit
2i); one binding per visible scope chain, as SCOPE-1 already specified (exit 2i); the heap one-hop
field rule as a representation rule (MEMORY_SEMANTICS §5; bug 126's repair in exit 0b, R-0501);
evaluation strictly left to right in source order, with struct and enum literal fields in source order
and assignment evaluating its right-hand side first as the one exception (STATEMENT_EXPRESSION_MODEL;
exit 2j); explicit consumption before replacing a live resource (MEMORY_SEMANTICS §10); layout
unspecified unless a `repr` guarantees it, observed only through `sizeof` / `alignof`, with one
`Layout` authority (LAYOUT_CONTRACT §3.3; R-0502); every Concrete-defined function in a mangled
namespace disjoint from C, with `#[link_name]` for imports and an export attribute for the reverse
(FFI.md; R-0285, R-0283, R-0286, R-0510); ownership on failure stated by every consuming fallible
`std` and foreign-boundary API (ERROR_CONVENTIONS; R-0285, R-0283); recoverable versus terminal
failure restated (the spec's "Abort and failure" (docs/spec/statements.md)). **Adopted, staged:** `Option` and `Result` laid out per
instantiation now (bug 122, exit 0b) and referred to only by resolved identity later (R-0492,
R-0514; LAYOUT_CONTRACT §3.4); lossy numeric `as` casts become **compile errors** — the user's choice
over a trapping `as` — reached through named operations first (checked conversion returning an error,
or truncation, reinterpretation and rounding with exact bit semantics), then the rejection with
versioned migration diagnostics, then the corpus migration (ARITHMETIC_POLICY §8; exit 2k, with exit
2h centralizing today's cast semantics without waiting). **`main`:** `fn main() -> Int` stays, and
parameters on `main` are banned now (bug 111, exit 0b). **Open, not adopted:** whether `main`'s
return type narrows to MAIN_EXIT_MODEL's `fn main()` / `fn main() -> u8`; that end state stays a
recorded preference until a later surface pass decides exit codes. **Stance:** reference restrictions
are not relaxed and nothing is destroyed implicitly; the relaxations the decision names (immutable
scoped `let` references or place-based borrow blocks; owner reads under a shared borrow) wait for a
workload and would be new rule versions. Two items are pull-gated with no owner yet: fallible
allocation (`try_*` collection operations and a non-aborting allocator path) and a general place
`replace` / `swap`. The evaluation's probes also found defects, filed as bugs 127 – 131 (exits 0b and 2c), and
showed stale documentation (the wrapping-overflow text in NUMERIC_MODEL, `--report profile`'s
wrap-gap line, the removed `->` heap syntax), corrected with the decisions. No exit or acceptance
criterion is removed; exit 2i's acceptance and exits 2j and 2k's scopes are made precise.

**Tightening decisions, 2026-10-23:** the user approved selective tightening (a rule lands when it
closes a soundness hole or removes a case from the semantics the verified pipeline must formalize;
style-only rules wait for a workload) and answered the four open questions of
[TIGHTENING_DECISIONS.md](TIGHTENING_DECISIONS.md) with "do the best long term
solution for this problem and every problem". **Adopted:** every `&mut` argument is an implicit
reborrow and an unused `&mut` may lapse, as an exclusive scoped loan that suspends its parent (D1,
G9 / B1 / B2; R-0496 with exit 2i); the Austral model, "ok do the austral model with `&s.f`
sugar": only a variable (or a string literal, a static root) is borrowed, a reference to a part is
a projection through a reference, `&s.f` is front-end sugar, and loans are keyed by root, so a
borrow of a temporary is refused and disjoint field borrows wait for a workload (D2, REF-5, G12;
closes bug 182; 2 std sites and 1 train3 fixture migrate); every binder through a scrutinee of
reference type is a reference, Copy or not, carried explicitly through Core (D3; closes bug 181; 2
fixtures migrate; G17's Copy case reopens under it); every foreign function value is refused now
(bug 166), and function types will carry calling convention, capabilities and safety obligations
when the first callback workload needs them (D4, [FFI.md](../platform/FFI.md)). G17's moving
shapes are closed (refused on main, pinned by fixtures). The ordered implementation plan is TIGHTENING_DECISIONS §5; exit 2i owns D1 –
D3's checker rules and exit 0b the bug repairs.

**Owner decisions, 2026-10-25:** the project owner paused the 2026-10-23 tightening decisions for
negotiation, together with further language questions raised since (eighteen in all, OD21 from bug 262 and OD22 from bug 189), the
PROVABLE_V1 scope and bug 226's std visibility question. They are listed under
[owner decisions](../spec/decisions.md#owner-decisions) and held by exit 2n. Only the owner decides them: agents analyse,
recommend and ask, and do not choose, encode or implement an answer. The entry above and
TIGHTENING_DECISIONS.md stay as the record of what was proposed and landed; the owner's new answer
governs. Rules already on main stay as they are until the owner answers, and the answer decides
whether they stay.

**Delivery decision, 2026-10-17** (amended 2026-10-19: interop runs alongside the verified-pipeline track after the immediate protections; see the vertical-slice decision below; amended 2026-10-20: RocksDB slice 1's work proceeds now and is accepted once its [prerequisite exits](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites) hold, not after all of rows 0 – 2)**:** comfortable C/Rust/Zig interop is the next delivery investment
after the active soundness/identity work. R-0285 owns the integration workflow; the existing ABI
and export tasks supply the tested subset it needs. R-0490 then uses a complete ethlambda port
to find language, library and tooling gaps through execution. Bound the interop checkpoint to
its four acceptance adapters; further dependencies are integrated when a port slice needs them.
The application contract is dependency -> ordinary Concrete import -> normal build/test.
Reusable binding packages describe ownership, errors and authority once; generation and native
builds are tooling responsibilities. The [interop design](../platform/FFI_GLUE.md) defines the
shared boundary description, language-specific front ends, cache/upgrade behavior and exit bars.
Compiler safety, protocol conformance and regression testing remain continuous. Functional
proofs of client components follow the working port, through the evidence and contract owners
below. New soundness defects still enter at the front. The port is an engineering workload,
not a fourth verified flagship or a claim that foreign libraries have been proved.

**Soundness track, 2026-10-18 (now queue rows 2, 3, 4 and 6):** the evidence layer is now
strong — every theorem is re-checked outside Lean by con-leche and nanoda, and since bug 079 a claim is
proved only when its body and parameters are the specification its theorem evaluates. The gaps that
remain are recorded here so the queue does not read as if rows 0–1 settle soundness:

- **Core→VC soundness (H19, R-0460).** Each obligation-generation rule, including the soundness of
  its hypotheses, requires the named evidence recorded in `VC_BRIDGE_REGISTER.md`; proving claims
  on Core does not discharge these rows.
- **Model and compiler correspondence ([verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md)).**
  `PExpr` is retired in favour of claims on Core's semantics (R-0495, R-0496); the lower pipeline
  gets a semantics per IR (R-0501 – R-0503); translation validation of the LLVM profile is pulled
  forward (R-0504 with R-0299/R-0300); compiler correctness and proof transport to native binaries
  are R-0508. Until then each relation reports its admitted subset, and unvalidated backend and
  toolchain edges remain trusted.
- **The checker's own rules** — linear ownership, second-class references, capabilities, the Unsafe
  boundary — are enforced and gated but have no soundness proof; R-0499 moves them onto Core as
  declarative rules and R-0507 proves them.
- **Coverage classification and loop binding:** R-0495 owns checked proposition coverage and exact
  checked-Core loop binding. Unsupported classification or missing loop binding cannot grant proof
  authority.
- **Trusted std boundaries** (`trusted impl Bytes`, IO and others) rest on human audit; claims reaching
  them say "carries trust".
- **The bug ledger's open list** has entries later work appears to have closed (R-0010 owns
  mechanically checked per-bug states).

**Verified-compiler decision, 2026-10-18:** the goal is an end-to-end verified compiler —
CompCert/CakeML-class theorems that accepted programs are memory- and ownership-safe, that the
native binary only does what the program's Core semantics allows, and that supported Core claims
transport to the native image through proved transport lemmas under recorded premises. Hunting after bugs 079 and 080 found fifteen more (081 – 095) in an
afternoon, and they trace to the pipeline's structure: the checker runs on the source tree and
returns nothing the compiler uses, the language's meaning is implemented several times, names are
strings after resolution, evidence is computed per surface, and SSA and the backend have no
semantics. The [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md) (R-0491 – R-0510)
reconciles two independent architecture designs and Codex's reviews into one target: Core as the
language definition with an executable semantics, the authoritative checker on Core, `PExpr`
retired in favour of claims on Core, a Place MIR → Memory CFG → BIR lower pipeline each with a
semantics, LLVM behind a specified, unverified BIR-to-LLVM adapter, and native AArch64 code generation
delegated to the separately developed Lean Compiler Backend through a proved adapter (R-0517). Part 1 establishes semantic interfaces, states obligations and proves
representative cases before dependent implementations freeze; Part 2 completes proofs for the
admitted profile (delivered as vertical slices since 2026-10-19: rows 2, 3, 4 and 6). Every
slice is reviewed by Codex before publication, the proof surface is feature-frozen until the
verdict authority and claims-on-Core land, and the fixes for 081 – 101 land first (row 0).

**Usability decision, 2026-10-19:** a great language needs real users, good errors, tooling, a
reliable `std` and published performance, not only a sound compiler. Usability moves from the end of
the queue to a track beside the proofs (row 8, R-0515), in waves under the capacity rule (since 2026-10-21 the
feature track of the [capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation)); the external
user pilot that row 16 required at the end now starts after rows 0–2 qualify and the onboarding path is ready.

**Vertical-slice decision, 2026-10-19:** deliver the verified compiler as cumulative proved subsets
through the whole compiler, with explicit intermediate and source-to-native theorem boundaries, rather
than layer by layer, which would postpone any end-to-end guarantee — and the first real test of the
architecture — by years. Before shared interfaces freeze, representative ownership, borrowing and
memory-representation cases are proved. Frontend correspondence and safety proofs grow with the
admitted language. One converging compiler architecture carries distinct assurance profiles (`verified`
and the existing one), and the LLVM profile stays supported for production use. The proof track, the
shared front end (row 4), and the bounded interop and application-feedback tracks proceed alongside one
another after the immediate protections (amended 2026-10-20: each first deliverable waits only for its
[prerequisite exits](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites), not for all of rows 0 – 2), with explicit capacity for each
(amended 2026-10-21: the [capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation));
new authority defects take priority. Legacy claims migrate when their Core semantics and evidence boundaries are ready, preserving
supported replay and exact assurance labels. Property gates replace brittle corpus cardinalities while
keeping independent coverage inventories and minimum completed comparisons. No multi-year proof
milestone suspends supported releases or external-user feedback. The [plan](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)
holds the milestones and the discipline (proof engineering, Lean scalability, compiler-execution trust,
ownership and specialist review, specification validation, release strategy).

**Implementation and feedback decision, 2026-10-17** (amended 2026-10-19 and 2026-10-20)**:** once RocksDB slice 1's prerequisite exits hold ([feature prerequisites](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites); the rest of rows 0 – 2 continues alongside), start R-0285 with a working RocksDB C-API consumer and the
minimal integrated package/build code it needs. Prepare concrete manifest/lockfile
examples and only the resolution decisions needed by that slice, then implement,
run and measure it. Each later adapter repeats that feedback loop. Full registry,
range-resolution, schema breadth and functional-proof work are not entrance
requirements for this tested slice; their existing acceptance bars remain owned.
R-0285's four-adapter exit still precedes the bulk R-0490 port.

**Behavioral-contract decision, 2026-10-18 (planned):** make failure states,
interface laws and the exact meaning of proof claims explicit through the existing
API, contract and evidence machinery. R-0285 exercises resource recovery and
partial progress in the first applicable foreign-library slice; R-0488 carries
the same questions into ordinary library APIs. R-0031 owns a bounded equality/hash
law example, R-0271 owns its generic/instance proof meaning, and R-0482 owns checking
the claimed property against the actual theorem proposition. Their acceptance
criteria below extend those owners; they create no additional execution queue.
Start with ordinary types, explicit contracts and selected proofs. Add syntax or
type-system machinery only after a real workload establishes the need. Tested API
behavior can ship with the interop slices; formal error-state and generic-law
composition follow the queued typed-contract and evidence prerequisites. These
increments do not expand rows 0–1's recorded exits; newly reproduced false
guarantees still enter through the existing soundness-repair rule.

**Emitted-code validation analysis, 2026-10-18 (planned):** the
[Sparkle review](../../research/compiler/sparkle-emitted-code-validation.md) records
the existing SSA/LLVM foundations and a bounded check of actual emitted LLVM,
beyond optional `llvm-as` well-formedness checks. R-0299/R-0300 own that additional
semantic edge and its independent replay; their existing BackendIR V1 relation
keeps its scope. The small Lean 4 native backend this analysis anticipated is now
the separate Lean Compiler Backend, which Concrete consumes under R-0517 (R-0297
delegated); R-0413 keeps only the later Concrete-hosted comparison. Implementation
language alone supplies no correctness guarantee. Fresh-consumer checks join R-0285; deeper backend research does not
reorder the queue or delay its first executable dependency slice.

R-0484 (closed 2026-10-08 at `245cd51c`), R-0483 (closed 2026-10-09 at `4f005715`) and R-0208
(closed 2026-10-13 at `2b570c5a`) moved to [CHANGELOG.md](../../CHANGELOG.md); their accepted limits stay recorded in their task sections.

`ProofCache` remains performance-pulled. A second proof-producing kernel remains research-gated and
is not part of this strict queue. Why3 remains comparative prior art, never a backend.

## Roadmap opening records

The following blocks are preserved verbatim from ROADMAP.md. They are literal Markdown records, so their original relative links retain the
repository-root context. The live roadmap supplies navigable summaries and current pointers.

<a id="2026-10-30--specification-first"></a>

### 2026-10-30 — Specification first

```markdown
**The specification comes first (owner decision, 2026-10-30).** The criteria below are delivered in phases A–D under [the scheduling rule](#scheduling-rule). The spec is complete when it
is the one normative, simple and checkable statement of the language. In order:

1. **Owner decisions.** Every open question is answered. They are the OPEN entries in
   [docs/spec/decisions.md](docs/spec/decisions.md), and the ones the spec work has raised since land
   with it. Each carries a reviewed recommendation, the principle it rests on and its proof impact.
   Until it is answered, the spec says "undecided" there. **Done when:** no OPEN entry remains, except
   ones the owner defers explicitly.
2. **Absorb and audit.** Every language-normative statement in the repository moves into `docs/spec`,
   tracked by the absorption inventory, so there is one source. Coverage is then audited against the
   implementation, because absorbing only finds rules someone wrote down: every construct the parser
   and checker accept has a rule. Project policy, process and research stay where they are.
   **Done when:** the inventory has no unabsorbed row, and the coverage audit lists no construct
   without a rule.
3. **Simplify.** The decided removals are applied to the text, and the rules they make obsolete
   are retired by version, not silently deleted.
4. **Read as one document.** One generated single-page spec, a short overview, one shape for every
   rule (statement and examples, plus its diagnostic code where the rule is a refusal), the grammar
   in the spec, a glossary, and an index from diagnostic codes to rules.
5. **Nothing outside the spec.** Every diagnostic code that reports a language refusal maps to a rule,
   and every compiler site that enforces one cites it. A blocking gate, defined over the diagnostic
   ledger and the rule-site table, fails on an unmapped language refusal. Semantic guarantees and
   operational failures are classified separately, as `docs/spec/README.md` already does.
6. **Conformance suite.** Every rule's examples state their expected outcome: accepted with a result,
   refused with a code, or trapping. They are published as a suite another implementation could run.
   **Done when:** every rule has its examples. Pending rules are expected to fail until step 7.
7. **The code matches the spec.** Std, examples and tests migrate as their constructs land, with an owning task for every pending rule.
8. **Lean links.** Each chapter's rules link their Lean definition (`Semantics:`), and the definition
   governs. They arrive chapter by chapter with the Core semantics (R-0496), each together with its
   implementation, and prose, Lean and the implementation must agree on the chapter's examples.
9. **Outside-reader test.** Someone new to Concrete writes a working program from the spec alone,
   and whatever confused them is fixed.
```

<a id="2026-11-01-and-2026-11-02--authority-design"></a>

### 2026-11-01 and 2026-11-02 — Authority design

```markdown
**Authority design (owner decision, 2026-11-01; rules completed 2026-11-02).** `with(...)` is the
only permission mechanism; it is compile-time information, not a resource. Linear values are the only
resource mechanism. Every operation needs both: the value says which thing, and `with` says what is
allowed. The target's services are values too: `main` receives the target's grants (its allocator,
directories, environment, clock, devices) and consumes them. There is no `Root` type, no permission
token and no `Std` capability; every signature lists its real capabilities. Allocation takes an
`Allocator<A>` value on every target and is fallible; allocations carry their allocator's identity;
filesystem access goes through `Directory` values confined at lookup. The fourteen authority rules and
the allocation, filesystem, environment, time and network decisions are in
[docs/spec/decisions.md](docs/spec/decisions.md#decided-2026-11-02). The spec's trusted, types, memory
and ownership chapters are to be rewritten to them in Lean, and std, examples and tests are to
migrate to them.
```

<a id="2026-10-30--documentation-follows-the-spec"></a>

### 2026-10-30 — Documentation follows the spec

```markdown
**Then the documentation follows the spec.** Once the spec is rewritten, every other document (the
book, the site, the README, the project, compiler and platform docs, and the research notes that state
language facts) is updated to cite the new spec and lose any statement it now contradicts. The
absorption inventory and the doc-links and docs-drift gates track this: no document may restate a rule
the spec owns, or describe a removed feature as current.
```

### 2026-11-04 — Lean is the specification

```markdown
**Lean is the specification; no prose is generated from it (owner decision, 2026-11-04).** It revises
the 2026-11-01 Lean-source decision below. The owner: "i don't care if generating text from lean is
awkward, maybe we don't even need the text".

- The Lean definitions are the normative text. Each rule is a readable Lean definition with its rule
  ID, a doc comment stating in plain words what it means, and its conformance examples.
- Nothing generates `SPEC.md` or the chapters from Lean. This supersedes "Everything else is generated from
  Lean" and steps 4–5 of the 2026-11-01 sequence: there is no prose generator and no generator
  correctness obligation. The prose chapters in `docs/spec` are retired slice by slice as their rules
  land in Lean, not regenerated. Where a criterion or acceptance artifact of earlier decisions names
  "generated chapters", read it as "the slice's Lean rules with doc comments and examples".
- Derived machine views stay: the rule registry, IDs and manifests are generated from the Lean
  definitions and keep a staleness gate.
- The user guide (`site/content/guide`, which the website publishes) is non-normative; it teaches
  and links to the Lean rules, and states no rule of its own.
- The outside-reader test reads the Lean rules and their examples.
```

<a id="2026-11-01--lean-as-the-only-source"></a>

### 2026-11-01 — Lean as the only source

```markdown
**Lean is the only source of the language (owner decision, 2026-11-01).** This is the governing rule for phases A–D. It changes how steps 3, 4, 6 and 8 are carried out, not what they require:
every done-when criterion of steps 1 – 9 still holds. It supersedes the rewrite plan recorded earlier the
same day, which kept a prose source beside Lean.

- **Core in Lean from the start.** The normative language is a small Core, roughly 20 – 30
  constructs. Its syntax, its typing, ownership and effect rules, and its execution semantics are Lean
  definitions, each carrying its rule ID and a doc comment. There is no Markdown-first draft: existing
  chapter text is rewritten as Lean, not edited as prose. Rules stated on surface syntax today, such as
  the ownership checks, become Core rules (R-0499's checker on Core).
- **Desugaring in Lean.** Every surface feature (counted loops, method calls, `defer`, pattern forms,
  and so on) is defined by its lowering into Core, written as Lean functions, with no semantics of its
  own. Core's metatheory is shared by every feature, and each lowering carries its own correctness
  obligation.
- **Everything else is generated from Lean** (the WebAssembly SpecTec model). A Lean program generates
  `SPEC.md`, the chapters and the rule registry; a blocking gate fails when they are stale. The
  reference interpreter is the Lean semantics itself, compiled and run. It replaces `--interp` once it
  passes the existing interpreter-versus-compiled differential corpus. The conformance
  examples live next to their rule and run through both the Lean semantics and the compiler.
- **The compiler is proved against the same definitions.** R-0496's Core semantics is this Lean Core,
  so the spec, the reference interpreter, the conformance oracle and the verified-compiler theorems
  share one semantic definition. What remains are explicit correctness obligations at the translation
  and generation boundaries: desugaring, the generator, the compiler and linking.
- **Documentation follows from Lean.** The book, the site, the README and the compiler and platform
  docs cite the generated spec and state no rule of their own.
- **Implementation sequence**, one step at a time:
  1. record the owner decisions (step 1);
  2. Core in Lean: syntax, typing, ownership and effects, execution semantics;
  3. the desugaring of each surface feature in Lean;
  4. the generator and its staleness gate;
  5. retire the old chapters area by area, each replaced by its generated output;
  6. the documentation follows ("Then the documentation follows the spec", above), area by area as
     each chapter is replaced;
  7. the migration of std, examples and tests (step 7).

  Proposals 1 and 3 of [SPEC_STRUCTURE_PROPOSALS.md](docs/project/SPEC_STRUCTURE_PROPOSALS.md)
  (grammar productions in the entries; the conformance suite) are folded into steps 2 – 4.
```

<a id="2026-11-01-and-2026-11-02--new-compiler-and-retention-contract"></a>

### 2026-11-01 and 2026-11-02 — New compiler and retention contract

```markdown
**A new compiler, written from the Lean spec (owner decision, 2026-11-01).** The current compiler
is not migrated. It implements the language before the decisions of 2026-10-29 – 11-01, and its
recurring defect classes (spelling-based names, places mixed with values, checks spread across
passes) are structural. A new compiler is written from the Lean definition, proof-first, and the
current one is retired. The owner: "i don't care about the current pipeline, we could remove it and start
from scratch", then approved the transition below (freeze, tag, oracle, deletion after slice 1, the target
Core proposal first). This changes the implementation strategy only: the Lean-only source decision above
still governs, and no pending language question is settled by it. The architecture of `ConcreteSpec` and
the new compiler (below) was approved on 2026-11-02 under the owner's rule of that day
(Claude and Codex agreeing); the conditions are in phase B's done-when.

- **`ConcreteSpec`** is a new Lean library that imports nothing from any compiler. It holds the
  grammar as data (with its LL(1) check), Core with identities from the start, the semantic contract
  sized to each slice, the statics as inductive judgments (the only normative form; a decision
  procedure is an implementation, proved equivalent once a subset's rules stop changing), the one
  elaborator from surface to Core, the clock-indexed evaluator (the reference interpreter, which
  replaces `--interp`), the conformance examples and the generator of `SPEC.md`. The rule text is
  rendered from the judgment clause; doc comments are commentary and examples.
- **The new compiler** imports `ConcreteSpec` and calls its elaborator. Below Core it is a new
  lowering chain, built pass by pass with its proofs. A simple BIR → LLVM printer, labelled
  unverified, gives native code from the first slice; the Lean Compiler Backend follows. A
  conformance package above both runs every example through the spec and through the compiler.
- **When slice 1 works end to end, the old pipeline is deleted from main** in one change, together
  with the gates that test its internals. Kept: the test programs and bug regressions (re-run as
  slices cover them), examples and std (migrated to the decided language), the CI and gate framework,
  the docs and research, and the external native backend. Dropped: the old Core, checker,
  elaborator, lowering, SSA and LLVM emission, the old interpreter, the `PExpr`/ProofCore proofs, and
  the evidence, receipt and report subsystems, which are rebuilt on the new Core when proofs need
  them. Evidence the old compiler produced stays replayable from the tag; its
  issuance ends at the freeze and none of it is migrated (the
  [verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md)'s supersession notice). The browser compiler keeps running from the tag until the new compiler covers it.
- **What it supersedes:** the in-place migration of the current compiler in
  [VERIFIED_PIPELINE.md](docs/compiler/VERIFIED_PIPELINE.md) (its target architecture, semantic contract
  and proof strategy still apply, now to the new compiler); the migration of the compiler,
  which becomes its retirement (std, examples and tests still migrate); the old-pipeline items of
  the [legacy plan](docs/archive/ROADMAP_LEGACY_PLAN.md); and the unlanded cleanup branches of the old pipeline, which are to be
  closed.
```

<a id="2026-11-01--compiler-proof-order"></a>

### 2026-11-01 — Compiler proof order

```markdown
**How the compiler proof is built (research reviews, 2026-11-01).** Two surveys of verified-language and
verified-compiler projects, the first reviewed by Codex (adopt with changes), fix the order in which the
[verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md) is carried out for the new compiler. Every
source is in [INFLUENCES.md](docs/project/INFLUENCES.md#verification-and-compiler-influences-research-of-2026-11-01):

1. **The semantic contract first.** The final theorem is stated before any pass is proved: which
   outcomes a program has (return, abort, observable I/O, divergence), what a trace is, how authority
   and resources appear in it, and the memory abstraction. Values-to-bytes is a named proof boundary,
   not an implicit step.
2. **Executable Core semantics.** The planned form is a step function, so it runs as the reference
   interpreter. Running out of fuel is an execution-budget result of the interpreter, distinct from
   every program outcome; its relation to divergence is stated with the theorem. Type, ownership and effect
   soundness are proved over it.
3. **One minimal end-to-end path.** Each pass preserves behaviour and re-establishes the next IR's
   ownership and authority invariants, so no later pass relies on a check an earlier one skipped
   (Linear Core). Safety is split as in Owlang: the checker proves the ownership invariant and
   compilation preserves it, the ownership conditions at module boundaries are stated as interface
   contracts, and every check runs before optimization.
4. **Functional models for safe code**, in the style of Aeneas: a supported safe function is
   translated to a pure Lean function, with `&mut` arguments passed in and returned, and failure,
   non-termination and effects explicit. Because a Concrete reference is never returned or stored
   (Hylo's mutable value semantics), the translation is simpler, and the supported subset may need
   none of Aeneas's backward functions; that each model is exact (effects, divergence, allocation
   failure and trusted boundaries included) is a theorem to prove. Either the translator is verified
   or every model carries its own proof. Contracts are stated over these models; the plan is to
   discharge them with Lean's own program-verification tools (`Std.Do`, `mvcgen`, `grind`) instead of
   building a project verification-condition generator (the old TermIR generator retires with the old
   compiler). This replaces ProofCore.
5. **Open-module correctness and the C boundary**, in the style of CompCertO: per-module theorems
   that compose into a whole-program theorem, with explicit contracts for C code.
6. **Ghost permissions for trusted code**, in the style of Verus, so selected `trusted` functions
   discharge their trust assumptions instead of carrying them.
7. **SMT-checked lowering rules** where they pay off, later.
8. **Later, an optimizer by equality saturation**: an e-graph search whose rewrite rules are proved Lean
   theorems and whose every result carries a kernel-checked proof that the output equals its input
   (as the Lean egg tactic reconstructs), so the search and the extraction stay outside the trusted
   base.

Three more choices follow from the surveys:
- **The parser is generated from the grammar** (Lean data, which must pass the LL(1) check) by a
  verified LL(1) generator in the style of Vermillion. The lexer, the semantic actions and running the
  generated parser remain obligations before the parser leaves the trusted base.
- **Capability variables** ([authority rule 3](docs/spec/decisions.md#decided-2026-11-02-authority))
  take Scala's System Capless, whose metatheory is mechanized in Lean, and Effekt's
  effects-as-capabilities as metatheory references; callbacks that carry capabilities follow once
  foreign function values are admitted (OD4).
- **Agent-written proofs, kernel-checked**, as in Axon: agents may write proofs and translation-
  validation certificates, and only the Lean kernel's check counts. Trivet does this for LLVM's own
  optimization passes; for Concrete it could cover a named edge such as the BIR → LLVM printer, with
  LLVM, the backend and the linker still assumptions.

The plan also covers the front end (parsing and desugaring into Core) and the final linked artifact,
and the spec is tested independently of the compiler, through the conformance suite run on the Lean
semantics. Items 4 – 7 are the adopted direction; their supported subsets, representations and
residual assumptions are proposed to the owner before each is built.

The criteria above map to acceptance artifacts as follows. R-0555 owns the spec criteria;
R-0515 owns the outside-reader test as workloads become supported.

| criteria | phases | acceptance artifact |
|---|---|---|
| 1–2: decisions, absorption and audit | A | decisions and audit disposition in the target Core proposal |
| 3–6: simplification, one document, refusal coverage and conformance | B–D | generated chapters, rule-site inventory and conformance examples per slice |
| 7–8: matching code and Lean links | B–D | migrated corpus and correspondence proofs per slice |
| 9: outside-reader test | B–D | recorded reader program and resolved confusion per slice |
```

### 2026-10-22 — Development throughput priority

```markdown
**Decision (2026-10-22): the development cycle's feedback loop is a priority track.**
```

### 2026-11-02 — Owner recording rule

```markdown
The owner, 2026-11-02: "anything where you and codex agree that is the
best decision, record the solution". The [decision record](docs/spec/decisions.md#owner-decisions)
holds all OD answers, dates, revision notes and rationale.
```

### 2026-11-03 — Editorial supersession of legacy task directions

The approved new-compiler and restructure decisions supersede in-place repairs, shadow
cutovers and migration of frozen compiler internals. The A/B task bodies moved to
[the legacy archive](../archive/ROADMAP_LEGACY_PLAN.md#task-directions-before-the-coherent-plan)
retain historical detail and regression obligations. Their approved surviving requirements
remain live in the task register; the archive creates no legacy repair work. No language
answer, regression duty or acceptance control is retired by this editorial change.
