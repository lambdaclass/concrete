# Roadmap Decision Log

Status: reference — the dated decisions that shape [the current execution
queue](../../ROADMAP.md#the-current-execution-queue). None creates another queue. Decisions on the
language rules that the project owner reserved are listed, unanswered, under
[owner decisions](../../ROADMAP.md#owner-decisions) in ROADMAP.md; nothing here answers one of them.

Moved verbatim from ROADMAP.md on 2026-10-28 (the roadmap compaction); only heading levels and relative links were adjusted.

## Decisions

The dated decisions that shape the queue. Each names the tasks that carry it; none creates another
queue.

**Priority decision, 2026-10-21:** the verified compiler is the primary engineering track, protected
by an explicit [capacity allocation](../../ROADMAP.md#capacity-allocation) rather than by queue numbering. Two inputs
were weighed. An external review asked for exactly that, in the order: close known soundness defects
and fix the semantics proofs rely on; make the Core checker authoritative with explicit identities and
ownership/capability rules; V1a; V1b through the backend to the native image; then grow the subset
from real programs, with one small executable workload alongside and evidence integrity as a second
protected track. Separately, point repairs are not converging: probes found bugs 102 – 120 in about
two days, nearly all of them new instances of [structural classes](../bugs/STRUCTURAL_CLASSES.md)
whose owners are verified-pipeline Part 1 stages. Adopted: the capacity table (verified compiler at
least 55%, evidence integrity at least 15%, adversarial validation at least 10%, features at most 20%
in a single slot); the [defect disposition rule](../../ROADMAP.md#the-current-execution-queue); Part 1's structure as
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
[owner decisions](../../ROADMAP.md#owner-decisions) and held by exit 2n. Only the owner decides them: agents analyse,
recommend and ask, and do not choose, encode or implement an answer. The entry above and
TIGHTENING_DECISIONS.md stay as the record of what was proposed and landed; the owner's new answer
governs. Rules already on main stay as they are until the owner answers, and the answer decides
whether they stay.

**Delivery decision, 2026-10-17** (amended 2026-10-19: interop runs alongside the verified-pipeline track after the immediate protections; see the vertical-slice decision below; amended 2026-10-20: RocksDB slice 1's work proceeds now and is accepted once its [prerequisite exits](../../ROADMAP.md#feature-prerequisites) hold, not after all of rows 0 – 2)**:** comfortable C/Rust/Zig interop is the next delivery investment
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
feature track of the [capacity allocation](../../ROADMAP.md#capacity-allocation)); the external
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
[prerequisite exits](../../ROADMAP.md#feature-prerequisites), not for all of rows 0 – 2), with explicit capacity for each
(amended 2026-10-21: the [capacity allocation](../../ROADMAP.md#capacity-allocation));
new authority defects take priority. Legacy claims migrate when their Core semantics and evidence boundaries are ready, preserving
supported replay and exact assurance labels. Property gates replace brittle corpus cardinalities while
keeping independent coverage inventories and minimum completed comparisons. No multi-year proof
milestone suspends supported releases or external-user feedback. The [plan](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)
holds the milestones and the discipline (proof engineering, Lean scalability, compiler-execution trust,
ownership and specialist review, specification validation, release strategy).

**Implementation and feedback decision, 2026-10-17** (amended 2026-10-19 and 2026-10-20)**:** once RocksDB slice 1's prerequisite exits hold ([feature prerequisites](../../ROADMAP.md#feature-prerequisites); the rest of rows 0 – 2 continues alongside), start R-0285 with a working RocksDB C-API consumer and the
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
