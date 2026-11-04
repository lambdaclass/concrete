# A1 boundary artifacts

Status: draft for the owner's Core session. This document schedules nothing; the
[roadmap](../archive/roadmap-2026-11/ROADMAP.md#the-queue) tracks it. It proposes interfaces, not language
rules or an approved Core. A2 decides the whole shape; A3 formalizes the approved
result and returns substantive changes to A2. Sources: `ROADMAP.md:217–259`;
`research/process/design-review-2026-11-03.md:9`.

Every definition below is a mathematical sketch, not a Lean implementation or a
claim that an obligation is proved. Parameters left open are deliberate. “Qn” means
Core-session question n in `ROADMAP.md:243–259`. Options and recommendations remain
proposals. Lean remains the normative source; these sketches constrain that work,
rather than creating a second specification (`ROADMAP.md:62–85`).

## 1. Semantic contract objects

**Outcomes and maximal behaviours.** For each IR, let `State`, `Value`, `Event` and
`Env` be types. A maximal execution is a finite execution ending in `return v`,
`abort k`, `exhaust x` or `stuck p`, or an infinite execution. Its behaviour is its
finite or infinite event trace together with that endpoint; infinite executions
are classified as silent divergence after a finite trace, or infinite interaction
with an infinite trace. A finite prefix without an endpoint is not maximal. `k` is
a ghost trap reason; `p` identifies an unsatisfied semantic precondition, not a
program exception. Exhaustion is parameterized separately below. The obligations
are completeness of this classification, progress for checked safe states under
contracts, preservation, and ownership conservation at normal/structured exits.
Fatal aborts promise no reclamation; divergence promises no eventual release.
Sources: `docs/compiler/VERIFIED_PIPELINE.md:94–123`;
`docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:469–481`.

**Events and payloads.** As provisional representations under Q7/Q9, propose `Event := request CallId OpId GrantId ResourceId
Args | response CallId Result ExternalDelta | terminal TerminalPayload`.
`Args` and `Result` are operation-indexed typed values, including byte strings,
lengths, error results and ownership transfers; `ExternalDelta` describes changes
to externally reachable memory and resources. Responses match a preceding request.
An event records required permissions as ghost metadata separately from actual
grant provenance. Contracts relate prestate, arguments, response, poststate and
transferred obligations; event well-formedness forbids invented grants and unmatched
responses. Effect containment and confinement are separate obligations: an `Fs`
label alone cannot justify a path outside the supplied directory.
Sources: `docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:469–472`;
`research/process/design-review-2026-11-03.md:18–20`.

Event granularity is open under Q7/Q9: atomic completed calls versus split requests
and responses. Recommend split events to express blocking and foreign interaction (Q7/Q9).
For this sketch, pending admissible responses take silent environment-wait steps
in the combined program/environment transition system; response steps deliver the
contracted result. Waiting forever is silent divergence, with no fairness assumed.
Prove that waiting preserves validity and always enables a wait or response step,
so legitimate blocking is neither unsafe stuckness nor a finite maximal execution.
This representation remains a Q7/Q9 proposal.
For terminal output, Q7 options are logical writes with a terminal-flush contract,
or externally delivered bytes with explicit device/failure premises; recommend the
former distinction. No delivery guarantee is invented here. Terminal actions are
attributed to target authority separately from current-function permissions.
Source: `research/process/design-review-2026-11-03.md:20`.

**Identities.** Let `ResourceId` name an allocation or external resource,
`GrantId` name authority over resources, and `AllocId` name an allocator domain.
Under Q1/Q7/Q9, a provisional state contains a live-resource map, an ownership map and a grant-provenance graph;
identity equality is independent of source spelling and physical address.
`Depends(v,a)` is transitive through aggregates, outputs and pending cleanup.
Release requires that no live value depends on the allocator. Q1 options are
scoped generative brand binders with existential packaging, or explicit generation
indices carried through interfaces; recommend scoped binders with checked packaging.
Neither identifies repeated reset executions by their syntactic site. Q1 release
metadata options are stable allocator state with a release context, or a proved
relocatable representation; recommend stable state, never a stored safe reference.
Q7 options are target-specific grant records with an explicit policy order, or
opaque grants with contract-defined narrowing; recommend explicit schemas and
monotone narrowing. Freshness, substitution, frame, dependency preservation and
release validity must be proved for the chosen representation.
Sources: `docs/spec/decisions.md:372–383`;
`research/process/design-review-2026-11-03.md:10–13,19`.

**Admissible environments.** Sketch `Admissible : Profile → Env → Prop` as the
conjunction of operation contracts, grant provenance/confinement, oracle consistency,
ABI conditions and the sequential interference policy. For a fixed admissible
environment and allocator oracle execution is deterministic. Quantify over all
admissible environments, not one convenient script; exhibit an inhabitant for each
supported initial configuration. Foreign threads, signals, asynchronous mutation,
reentrant callbacks and dynamic linking require explicit admission contracts.
Q7's schema options above determine initial grants and narrowing; Q9 options for
primitive interfaces are typed contract-indexed operations or a generic call with
checked signatures; recommend typed operations. Obligation: external responses
preserve memory validity, ownership and the permitted frame.
Source: `docs/compiler/VERIFIED_PIPELINE.md:99–107,124–129`.

**Allocation and exhaustion.** Sketch
`allocStep a v s : Result (Heap v a × State) (Failure v × State)`.
Failure returns the input ownership obligation, allocates no error payload, and
leaves the allocator valid. Failed growth preserves the container and returns the
rejected element; failed reserve preserves original storage; partial construction
accounts for every acquired obligation. OOM here is an ordinary conserving error,
never `exhaust`. Q1 options for `Failure` are operation-specific records or a common
sum with typed recovery payloads; recommend operation-specific contracts. Q1 options
for “unchanged” are logical contents/ownership or full observable representation
including capacity and allocator state; recommend an explicit per-operation frame
covering all observables. This does not select the observable set.
Source: `docs/spec/decisions.md:384–391`;
`research/process/design-review-2026-11-03.md:13,26`.

Separately define `ResourceRel : CoreState → TargetState → Budget → Prop` and
`Enough : Program → Env → Budget → Prop`. Runtime stack, address-space and host
exhaustion must each have named bounds or checked failure paths; unchecked stack
failure is not silently a defined trap. For a bounded terminating source run,
`Enough` must supply a realizable target budget, and the resource theorem must
preserve its non-exhausting execution, including each allocation success guaranteed
by the related source oracle. Require a concrete allocation-success witness and
budget satisfying `Enough`, and an oracle progress clause: available suitable
storage implies success. An always-failing allocator violates that clause and the
witness. Mere existential matching to a source oracle that also always fails is
insufficient. Ordinary failure remains allowed when the related oracle lacks
resources. Q1 options are explicit capacity accounts or an abstract availability
predicate with realization/progress laws; recommend the latter interface with a
concrete witness. These are proposed adequacy premises, not a new infallible
allocation rule. Sources: `docs/compiler/VERIFIED_PIPELINE.md:117–123`;
`docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:866–871`;
`research/process/design-review-2026-11-03.md:26`.

**Observation equivalence and trap erasure.** For the observation profile approved
under Q7/Q9, define `ObsEq P rho b c` by relating the entire event sequence and
endpoints under a coherent identity bijection `rho` and related external memories.
The bijection covers resource, grant and allocator identities, preserves provenance
and aliasing, extends consistently on fresh identities, and frames unaffected
memory. Released identities cannot be inconsistently remapped in later events.
Payloads, external-memory deltas and endpoint values use the same correspondence;
observable order, abort and infinite interaction versus silent divergence agree.
Define `b ≈P c := ∃ rho, ObsEq P rho b c`, not literal equality of independently
renamed traces. `eraseTrap (abort k) = abort`; ghost ownership, permissions and trap
kinds disappear only through proved erasure. A property distinguishing ghost trap
reasons needs its own transport lemma. Q7/Q9 options for observations are logical
external operations or physical byte delivery; recommend explicit logical events
plus a separate realization relation. The theorem's `ObsRel` below instantiates
this judgment with its initial correspondence and endpoint obligations; it is not
an independently selectable relation. Sources:
`docs/compiler/VERIFIED_PIPELINE.md:108–116`;
`research/process/design-review-2026-11-03.md:20,25`.

**Stuckness.** `Stuck s := ¬ Final s ∧ ¬ ∃ e s', Step s e s'`, using the combined
program/environment steps (including silent waiting) above. Model failed
unsafe preconditions explicitly, rather than giving undefined execution arbitrary
observable permission. Under valid primitive/FFI contracts safe accepted states
cannot reach `Stuck`. A stuck source run cannot justify a successful native claim.
Q3 options are an enumerated precondition taxonomy or a contract-indexed validity
predicate; recommend an enumerated taxonomy instantiated by contracts, covering
initialization, bounds, alignment, provenance, lifetime, aliasing, ownership transfer
and foreign mutation. Source: `docs/compiler/VERIFIED_PIPELINE.md:94–98,132–140`;
`research/process/design-review-2026-11-03.md:21`.

**Fuel and divergence.** Sketch `eval : Nat → Env → State → RunResult`, where
`RunResult := completed Trace Endpoint | budget Trace State`. Budget is an
interpreter result, not an outcome. Completed runs are stable under increased fuel;
budget traces are monotone prefixes. Adequacy relates completed runs and compatible
limits of prefixes to maximal executions. One exhausted run proves no divergence;
silent divergence requires an infinite execution with only finitely many events.
Pass proofs supply clock transport or well-founded stuttering; conversion of forward
to backward simulation must discharge receptiveness/determinacy hypotheses.
Sources: `ROADMAP.md:145–148`; `docs/compiler/VERIFIED_PIPELINE.md:99–107`.

**Values to bytes.** Name `Rep layout injection typedStore byteStore : Prop`.
It relates typed values and allocation identities to disjoint valid byte/provenance
regions, with size, alignment, initialized bytes, pointer fragments, permissions,
exposed addresses and external frames. Prove load/store correspondence, allocation
and release preservation, injectivity for live objects and target-memory validity.
Q3 options are block-offset memory with explicit exposure, or fully concrete
addresses throughout; recommend block-offset memory with explicit exposure, without
settling provenance policy. Q9 options are structured values plus a raw-memory
component, or uniform byte memory; recommend the former with `Rep` as the named
boundary. Before Core freezes, check one representative representation lemma, not
just this signature. Sources: `docs/compiler/VERIFIED_PIPELINE.md:138–153`;
`docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:473–478`; `ROADMAP.md:231`.

## 2. End-to-end theorem signature

These are separate obligations, not premises to hide behind a single “correct” flag.
The intended theorem is conditional until every edge in its selected scope is
proved. Sources: `docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:856–883,948–978`.

1. **Acceptance.** `compile profile bytes imports = ok artifact` yields a retained
   checked Core `c`, an independently derivable well-formedness judgment and the
   exact admitted subset version. Unsupported syntax is refused explicitly.
2. **Coverage.** Define `Supported bytes imports` independently of compilation and
   prove `Supported s i → ∃ a, compile P s i = ok a`. Also require inhabited accepted
   success and trap cases from part 4 and the rejection controls. Refusing every
   program fails this obligation. This is subset completeness, not a promise to
   accept every valid whole-language program. Sources: `ROADMAP.md:230`;
   `docs/compiler/VERIFIED_PIPELINE.md:63–65,957–960`.
3. **Frontend correspondence.** Successful lexing/parsing, resolution and elaboration
   implement independent judgments on the exact source bytes and import roots:
   `SourceJudgment bytes roots c`. Defining this as “the elaborator returned c” is
   disallowed. Parser execution remains a separate assurance edge.
   Source: `docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:872–875`.
4. **Initial relation.** `InitRel P c image envC envT sC sT` includes entry ABI,
   related arguments/grants, startup, imports, memory representation and resource
   relation. Define supported configurations/inputs independently of `InitRel` and
   `EnvRel`. For every such configuration/input, every successful compilation from
   parts 1–3, and every admissible target environment, require a source environment
   and initial states satisfying `Admissible P envC`, `EnvRel P envC envT` and
   `InitRel P c image envC envT sC sT`, with the same resource premises used in
   part 6. Thus `EnvRel` is total over admissible target environments in scope,
   not merely inhabited on fixtures. Exhibit admissible target environments for
   every supported configuration, and realizable resource witnesses as in §1.
   For every related initialization under these premises, prove nonempty sets of
   maximal source and target executions (including infinite executions where
   appropriate). Neither an empty environment relation nor absent executions can
   discharge refinement.
5. **Final relation.** `FinalRel P resultC memC resultT memT` relates returned
   values and externally reachable memory, conservation at ordinary exits, and
   erased traps. Fatal exits require only their stated observations; they do not
   promise cleanup. Preserve target-memory validity throughout, not only at exit.
6. **Behaviour refinement.** Define `ObsRel P bc bt` as §1's approved
   `ObsEq P rho bc bt` for a coherent bijection extending the initial identity and
   memory correspondence in `InitRel`. Its endpoint clause requires returning
   executions to satisfy `FinalRel` on their results and external memories;
   abort/exhaustion and divergence obey §1's endpoint and resource rules.
   Checker safety (progress/preservation), resource adequacy, translation refinement
   and claim transport remain separately named obligations, each to be discharged.
   Translation refinement states: For related initial states and admissible related
   environments, `∀ bt ∈ MaxTarget image sT, ∃ bc ∈ MaxCore c sC, ObsRel P bc bt`,
   subject to the separately stated resource theorem. Relate every finite observed
   prefix as well; finite target prefixes must extend compatible source executions,
   and infinite limits must be justified by adequacy, not independently chosen
   incompatible witnesses. Preserve divergence classification after ghost erasure.
   Sources for 4–6: `docs/compiler/VERIFIED_PIPELINE.md:94–123,853–858`.
7. **Claim transport.** For each proposition pair `QC`, `QT`, prove
   `(∀ bc bt, ObsRel P bc bt → QC bc → QT bt)` before using
   `(∀ bc ∈ MaxCore c sC, QC bc)` to conclude the target claim. Exact functional
   models require their own correspondence proof, with effects, failure and
   divergence represented. Sources: `ROADMAP.md:154–163`;
   `docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:875–883`.
8. **Artifact binding.** The theorem/receipt identifies exact source and imports,
   subset and semantics versions, checked Core and each used IR, profile, compiler
   build, dependency/proof closure, runtime/startup, adapter and backend versions,
   object/link inputs and final image bytes. Hash identities require authenticated
   retained bytes and a checked binding path; an envelope does not prove semantic
   correspondence. Recheck proofs and assumptions independently; substitution of
   source, IR, runtime or image must invalidate the binding. Sources:
   `docs/compiler/VERIFIED_PIPELINE.md:825–842,870–886,962–968`;
   `docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:970–978`.

Q8 offers source → BIR with native differential qualification, or a conditional
source → linked-image theorem assuming every remaining LLVM/native edge. Recommend
the first as the initial proof endpoint, retaining the second as a separately
labelled conditional theorem. Neither is selected here. Q9 determines the concrete
Core state and judgments used above. Source: `ROADMAP.md:255–259`.

## 3. Assumptions and trusted-base inventory

This is an inventory schema, not a completed implementation audit. Each later record
needs a stable identity, version, scope, reason, owner, transitive dependencies and
removal evidence. Ordinary proved dependencies are not assumptions. Preserve a
machine-readable audit-command view and distinguish proof scope, backend scope,
dependencies and excluded side channels. Treat malformed inputs and artifact
substitutions as adversarial; do not claim constant-time or arbitrary security
hyperproperties. Sources: `docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:3365–3373,3457–3463,3593–3601`;
`docs/compiler/VERIFIED_PIPELINE.md:63–65,870–886`.

| Item | Trusted for, where applicable | Later work that can remove or reduce it |
|---|---|---|
| Logical axioms | The exact transitive closure; kernel-only permits `propext`, `Classical.choice`, `Quot.sound`; reject `sorryAx` and unregistered axioms. Native-evaluation and float axioms require separate labels. | R-0506 axiom audit; constructive replacement removes a particular dependency, never a relabelling. |
| Lean kernel | Checking proof terms against the stated proposition and admitted axioms. | R-0509 independent con-leche/nanoda replay reduces implementation dependence; foundational checking still remains. |
| Compiler execution | Running the proved Lean function faithfully: Lean code generation, native libraries and foreign implementations. | Kernel-checked per-compilation certificates or proved validators checked through the declared trusted path; later execution/bootstrapping work. Native validator execution alone retains trust. |
| Lexer/parser execution | Exact bytes become the syntax and semantic actions specified independently. | Verified grammar/parser generator plus lexer, actions and execution correspondence; frontend theorem alone does not certify an unbound executable run. |
| Runtime | Helpers, allocation, release, startup, terminal actions and FFI meet their contracts and confinement obligations. | R-0509/R-0514 verify individual implementations, compiled through covered edges; retain external contracts. |
| Adapters | BIR → LLVM printer, or BIR → LCB IR, preserves checked operations, traps, authority and ABI. | R-0503/R-0505 adapter refinement or sound translation validation for its exact domain. |
| LLVM/toolchain | LLVM optimization, lowering, assembly and native output implement the retained input under the profile. | R-0517 LCB certified path plus Concrete adapter/composition; a printer certificate removes only the printer edge. |
| Linker/loader | Symbol resolution, relocations, image layout, entry and startup correspond to recorded inputs. | R-0505/R-0508 linking proof and sound exact-image validation; runtime loader assumptions remain until covered. |
| OS | External-call results, isolation, grant policy and resource premises satisfy declared contracts. | Verified service implementations can reduce individual contracts; no listed task proves the entire OS. |
| Target | ISA/ABI model corresponds to hardware, memory/address bounds and target profile. | LCB domain proofs reduce code-generation trust; hardware differential tests test model fidelity but do not prove hardware. |

Inventory sources: `docs/compiler/VERIFIED_PIPELINE.md:175–185,230–254,795–818,820–842,947–952`;
`docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:889–942`. Research rationale: parser execution is
separate in `docs/project/INFLUENCES.md:628–635`; compiler execution and certificates
in `docs/compiler/VERIFIED_PIPELINE.md:947–952`; compositional foreign contracts in
`docs/project/INFLUENCES.md:582–589`. The current planned native profile is AArch64,
Apple ARM64 ABI, macOS, Mach-O, not an ISA-only assertion
(`docs/compiler/VERIFIED_PIPELINE.md:232–242`).

## 4. Slice 1 manifest for approval

Phase B requires scalars, `let`, construction/move/destructuring of an empty non-Copy
struct, one checked operation and return/trap, through native execution. Old V1a's
loops, printing and larger scalar set are not silently imported into this smaller
slice. Sources: `ROADMAP.md:218,110–120`;
`docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:948–962`.

**Exact proposed surface component subset (Q9, unapproved).** One source unit, no imports;
one empty declaration `struct S {}` without Copy; one function
`fn entry(x: u8) -> u8 { statements }`. Statements are exactly `let n = e;`,
`let S {} = n;`, and a final `return e;`. Expressions are exactly a local name,
a decimal literal in 0…255, `S {}`, or `e + e` at `u8`; parentheses only group
expressions. Each name has one binding, no shadowing; destructuring is the only
linear eliminator. No calls, loops, branching, assignment, heap, references, raw
operations, strings, traits, generics, `defer`, externs, explicit abort or I/O.
This is a proposed admission restriction, not a language-wide prohibition.
OD31's decided expected-type rule applies: `return` supplies `u8`, and a
`u8` expectation on addition propagates to its operands; a typed operand can
constrain its peer. A bare literal initializer with no expected type is refused,
with no default integer type. OD62 requires every named local and parameter to be
used; an intentionally unused binding is `_`, which cannot discard a linear
obligation. These decided restrictions are not Q6 choices.
Empty structs and struct-destructuring `let` are decided (OD65 and OD84,
`docs/spec/decisions.md:173,192`); the component subset/interface remains Q9.
Q4 static text and Q5 inference alternatives need no answer for this subset
because neither occurs. Sources for static restrictions:
`docs/spec/decisions.md:139,170,458–459`.

**Component versus executable (Q7/Q9, unapproved integration options).** `entry`
is an open component, not a complete language executable or a replacement for
`main`. Q9 options are component compilation with an explicitly bound native test
launcher, or extending the admitted subset to a language wrapper; recommend the
component first (Q9). A complete executable must have a root `main` returning only
`u8`, declaring grants within the target's offered parameters and `with(...)`,
and explicitly consuming every declared grant before normal return (abort excepted).
Undeclared grants are never minted. These are decided rules, not options here.
Sources: `docs/spec/decisions.md:298–300,355–360,431–432`.

For either executable integration, Q7/Q9 must specify the target grant schema,
startup/launcher ABI, input injection, entry call/result mapping and explicit grant
consumption. Q7/Q9 options for input injection are a language wrapper obtaining
input through a declared grant/contracted operation, or a bound native test launcher
calling the component through its ABI; recommend the latter for component tests
(Q7/Q9). The language-wrapper option adds calls, grant types/operations, effects
and consumption forms beyond this manifest, all requiring admission and proofs.
The native-launcher option alone establishes no complete decided-language executable:
that claim additionally needs the root `main` and its grant checks and semantics.
Bind wrapper/launcher, startup, target table, input bytes, grants, object/link inputs
and image in the receipt and prove their initialization/observation correspondence.
Neither Q7 nor Q9 may add a scalar input parameter to `main` or revise its decided
return/grant rules.

**Exact proposed Core subset (Q9, unapproved).** Types are exactly `u8` and nominal
empty `S`; function parameters contain exactly one `u8`. Atoms are `litU8`,
`copy LocalId` for `u8`, and `move LocalId` for `S`. Sequenced forms are exactly
`bind LocalId (atom a)`, `bind LocalId (constructS)`,
`bind LocalId (checkedAddU8 a b)`, `destructureS (move LocalId)`, and
`return a` at `u8`. Expression nesting lowers to fresh sequenced binders; overflow
in `checkedAddU8` produces a defined runtime trap. No other constructor is admitted.
Q9 alternative: explicit success/trap control edges in Core; recommend retaining the
checked primitive and expanding edges below Core (Q9). This is not approval of Core's
whole constructor set. The Q9 integer choice options are `u8` or `i64`; recommend `u8`
for the smallest subset and consistency with the decided `main` return type
(`docs/spec/decisions.md:298–300`; `ROADMAP.md:218,257–259`). Under Q9, addition is the proposed
single checked operation; subtraction or multiplication are alternatives, with
addition recommended (Q9). Q2 destruction-bound choices are deferred by using only
empty-struct destructuring, never `destroy`.

**Static refusal and accepted trap (Q6, undecided).** Options are mandatory literal
checking only; a specified decidable constant/dataflow analysis; or refusal whenever
an arbitrary proof establishes inevitable violation. Recommend a specified bounded
analysis whose refusal is about inevitable failure over the admitted input domain,
not possible failure for one input. Under either of the first two options,
`fn entry(x: u8) -> u8 { return x + 1; }` is the proposed accepted trap witness:
input 0 succeeds with 1; input 255 traps. Do not use literal `255 + 1` as the
accepted witness. Under a stronger rule that refuses potential overflow without
an input precondition, that witness is unavailable: the owner must either admit a
contracted runtime-input case or revise the slice's trap admission requirement.
No unconditional accepted fixture is asserted before Q6 is answered. Sources:
`ROADMAP.md:252–253`; `research/process/design-review-2026-11-03.md:24`;
`docs/spec/arithmetic.md:607–627`.

**Observations (Q7/Q8/Q9).** Proposed component endpoints are `returnU8 n` and
unqualified `abort`, with traces containing the terminal events required by the
target authority table, including diagnostics and flushing when applicable.
No empty trace is inferred from the absence of source I/O. The launcher contract
must relate component results and terminal events to recorded target observations;
ghost trap kinds are erased, but diagnostic payloads remain observable under the
selected profile. Q7/Q9 options are retaining these terminal events or an explicit
approved projection with a proved realization/transport contract; recommend retaining
them (Q7/Q9). Q7/Q8 options are entry-function result observations or whole-process
exit observations; recommend entry results with a separate launcher contract
(Q7/Q8). Sources: `docs/spec/decisions.md:355–360,428–430`;
`docs/compiler/VERIFIED_PIPELINE.md:108–116`;
`research/process/design-review-2026-11-03.md:27`.

**Required programs and checks, conditional on manifest approval.** The accepted
linear witness is `let a = S {}; let b = a; let S {} = b; return x;` inside the
entry function. Refuse reuse after this move; refuse two destructures of `b`;
refuse `let a = S {}; return x;` for an unconsumed normal exit. Retain separate
controls for duplicate consumption and post-move reuse even if one diagnostic
class covers both. Add OD31 refusal `let n = 1; return n;` (no expected
literal type) and OD62 refusal `let n = x; return x;` (unused Copy local), plus
`fn entry(x: u8) -> u8 { return 0; }` (unused named parameter). Keep these separate
from linear-exit and Q6 overflow refusals. Run the checked-operation witness at 0 and 255 for success and
trap under the chosen Q6 criterion; retain the chosen criterion's statically
refused overflow case separately. Check each completed result at sufficient fuel
and greater fuel for equality; check low-fuel budget prefixes extend to the same
result and are never reported as divergence or a program trap. Run each accepted
case through the spec and native path; pin expected refusal categories pending
generated rule IDs, without inventing diagnostic codes. Prove termination of this
finite, call-free subset so budget exhaustion cannot be mistaken for divergence.
Include frontend result-substitution and lowering checked-operation-substitution
controls, each required to break correspondence or binding. Sources:
`ROADMAP.md:33–39,218,233–235`;
`docs/compiler/VERIFIED_PIPELINE.md:99–107,957–960`;
`research/process/design-review-2026-11-03.md:33–34`.

**Proof endpoint and LLVM assumption (Q8).** Option A proves exact source bytes →
checked Core → BIR with independent frontend judgments and checker correspondence;
native differential execution qualifies examples but proves no native claim.
Option B additionally states source → exact linked image conditional on the BIR →
LLVM printer, LLVM optimization/code generation, assembler, linker/loader,
runtime/startup, ABI and OS/target contracts. Recommend A initially; B must enumerate
all these assumptions and bind their artifacts. LLVM output remains labelled
unverified in either case. Neither testing nor an independently checked Core proof
removes the LLVM assumption. Source: `ROADMAP.md:118–120,218,255–256`;
`docs/compiler/VERIFIED_PIPELINE.md:244–254`;
`research/process/design-review-2026-11-03.md:27`.
