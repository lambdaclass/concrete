# ISS-0047: site table admission and diagnostic provenance

Design round 3; proposed types/functions/laws below are proof and implementation
obligations, not completed Lean verification. Authority: redesign authorization supplied in the writer brief (owner, 2026-11-15: “yes do all this”),
[owner goals](../decisions.md#owner-goals-2026-11-15-provability-errors),
[Philosophy](../PHILOSOPHY.md#the-rules). Snapshot evidence below explicitly names `origin/wip/spec-integration-head:<path>:<line>`;
none of its Lean modules is imported into this branch. Owner goal, verbatim:
“the design is done so that a) everything can be proven b) we can find get errors with good results?” This branch starts at main
`038ec12142fbbe67e8c3ec81c336a048250a38da`; main issue rows remain authoritative.

## One decidable invariant

In Spec/Observe introduce the sole whole-table coordinate predicate:

```lean
def SiteTable.EntriesValid (sources : SourceTable) (sites : SiteTable) : Prop :=
  ∀ entry ∈ sites,
    match AList.lookup sources entry.2.path with
    | none => False
    | some bytes => entry.2.LocationOk bytes
```

Construct its Decidable instance by finite traversal. Factor the last conjunct of
existing `ProjectSources.Realizes` through it, keeping exact file spellings/bytes,
exact site table, file-key Nodup and site-key Nodup distinct. Extend Realizes with
`∀ i < j, ¬ PathConflict parsedFiles[i] parsedFiles[j]`, using the approved
path-policy relation below; exact-key Nodup alone is insufficient. Duplicate-good sites
satisfy EntriesValid but fail admission. Raw source/site lookup laws remain unchanged.
Delete `SiteTable.Valid`, self-lookup filters and compatibility aliases: lookup-visible
validity is weaker on shadowed raw tables, so no unconditional equivalence is claimed.
Derive membership from lookup, then trap coordinate conclusions from EntriesValid.
`Span.PathOk` follows the structured path; preserve useful conclusions but remove
redundant premises. Derive proposed `ExecAdapter.source_entries_valid` from `A.loaded`
and `LoadedProject.load_admitted`, without a new field or `WellFormed` premise.
Retire `sites_valid` after migration; retain WellFormed's event/request obligations.

Keep the raw site traversal subject `F : SourceTable → SiteTable → Option SiteIssue`
with `F s t = none ↔ EntriesValid s t`. `SiteIssue` retains exact entry index and
record. A refusal decomposes the input into a valid prefix, that invalid entry and
suffix. This is a raw semantic search law, not the compiler's cross-file scheduler.
For each entry select missing-file first; otherwise tag location failures in
`Span.LocationOk` conjunction order: end offset beyond byte length, wrong line,
wrong byte column. Include actual/bound values or expected/actual coordinates.
Span construction already ensures start ≤ end; hence bounded end bounds start.
Only compute coordinate mismatch after bounds pass: `lineAt`/`columnAt` clamp at EOF
and cannot explain an out-of-bounds span. Prove each reason iff its check fails and
all earlier checks at that entry pass.

Closed controls use empty bytes, a valid point at offset 0/line 1/column 1, and
`bad` differing only in line 2. Shadowed `[(0,good),(0,bad)]` violates EntriesValid;
a lookup-only finder misses it. Coordinate-ignoring and offset-only finders miss
`[(0,bad)]`. Reject-duplicate-good fails the reverse correspondence. Add bounds,
line and column failures separately, with a mutant reporting clamped coordinates
for a bounds failure. Quantify laws over same-type executable subjects; mutants
retain exactly the same inputs and premises, including any uniqueness premises.

## Semantic admission, raw inputs and erasure

Proposed `loadChecked (input : ProjectSources) (plan : CheckPlan input) : CheckedLoadResult`
remains total over existing raw inputs, including the missing-source fixture at
`origin/wip/spec-integration-head:ConcreteSpec/Proofs/Observe2.lean:450`.
Its refusals retain tagged file/site indices, exact records/fields and prerequisite
facts. File and site indices are disjoint sorts. Collisions retain both complete
records even when their values are equal. For the raw collision projection traverse
increasing later index `j`, then choose the least earlier conflicting index `i`.
For equal-key collisions the first-collision prefix is Nodup, so the prior match is
unique; this is NOT true for path-prefix conflicts.

Define decidable `PathConflict` on successfully parsed paths from the approved
November 16 policy: exact duplicate files conflict; differently spelled components
with equal ASCII case-folding conflict at a shared case-folded directory prefix
(or final file component). Unchanged directory prefixes shared by distinct files
are allowed. Compare components, never arbitrary string prefixes; parsing refuses
rather than rewrites. Use the path-policy definition from F03p, not a second policy.
`["src/a/one.con","src/b/two.con","SRC/a/three.con"]` has least conflicting
later index 2 with BOTH earlier indices conflicting; report `(0,2)`, not a unique
prior witness. Include same-spelling shared-directory acceptance and component-
boundary controls. Malformed records never receive guessed parsed identities.

`CheckedLoadResult` admits exact `LoadedProject` data or retains a nonempty finite
collection, at most one per attribution key selected below. Admission is all-or-
nothing. Prove `(∃ a, loadChecked r plan = .ok a) ↔ ∃ p, r.Realizes p` using
Realizes strengthened with PathConflict freedom and factored EntriesValid. The
snapshot `LoadedProject.load_admitted` must be restated/proved for this strengthened
relation; do not claim unchanged equivalence to its exact-key-only definition.
Prove rejection iff no realization, preservation, uniqueness and admission reorder
invariance. Old category-priority refusal equality is not promised. A retained
Option loader is exactly the success erasure under a canonical raw plan.
`CheckPlan` carries finite nodes and constructive coverage/dependency certificates,
never assumed outcomes. Node identities and prerequisite edges are determined
solely by raw input, with exactly one node per required check and no omitted checks;
provenance changes only attribution/order. Its total raw constructor groups by tagged file or site
occurrence. `SourceLoadRefusal.Names` becomes a membership corollary, not occurrence
precision. Raw fixture types remain unchanged. The source-input carrier remains
raw, but its program-facing adapter moves outside Spec as specified below.

A lookup-only loader dropping site Nodup must fail on shadowed good/bad input;
a keep-first loader must fail preservation/uniqueness on duplicate-good input.
Test coordinate-ignoring acceptance, rejecting valid projects, later-collision and
equal-record index substitution. Do not construct an invalid adapter with an
impossible `loaded` proof to test these laws.

## Located boundary

Place only program-independent carriers (document indices, bounded ranges and
byte-derived coordinates) in an Ident-only leaf. They contain no `Var`, program,
parser or elaborator derivation. No serialized table syntax is assumed.
A document has occurrence identity, raw path spelling and actual SourceText; it
need not have an admitted path. `ByteRange` proves `start ≤ end ≤ bytes.length`.
`ByteLocation` adds byte-derived line/column. Conversion to checked Span additionally
requires an admitted path for the DOCUMENT, never the rejected candidate path.
Rendering retains raw document identity when conversion is unavailable. External
path arguments use their actual argument bytes and range, not an invented manifest.

`OccurrenceRef` is a tagged index (file declaration, site entry, registration,
request/proof attribute); `Origin` holds document id, range and component selector.
Proposed `LocatedProjectInput` in Diagnostics contains documents, raw ProjectSources,
origins and a foundation-level `WellLocated` certificate. This decidable certificate
requires total origin coverage of EVERY indexed entry and every checked component,
valid document references and bounded byte-derived locations, including both sides
of collisions. It does NOT assert that candidate paths, candidate coordinates or
table identities are semantically valid, or that these bytes decode to this table.
It therefore supports explicit malformed-table fixtures with real bounded origin
bytes, without requiring an impossible sound-elaborator output. Such fixtures test
the located checker, not the producer theorem. Generated entries can use a bounded
zero-width range at their producing node, retaining distinct occurrence identities.

`Decoded` lives in each actual producer module OUTSIDE Spec, above its parser or
elaborator dependencies. It relates an instrumented derivation, its exact raw erasure
and origins; prove `Decoded → WellLocated`, raw erasure and exact consumed-range/
producing-node links by induction over the actual producer constructors. There is
no existing total parser/elaborator postulated at A2i and no foundation import of a
future one. Cached/imported entries retain original documents and checked derivation
links. Production wrappers require these producer proofs; core located checks accept
WellLocated inputs. The stronger production provenance theorem is conditional on
Decoded; basic bounds and selection laws are not. No axiom or raw-to-located coercion
is introduced. Raw values lacking bytes remain valid semantic inputs.

The trust boundary is a module AND carrier split. Spec operations consume nominal
identities, semantic tables and coordinate facts only. Move program-facing
`ExecAdapter` (including sourceInput/loaded), `stepsObservation`, `reportObservation`,
`launchObservation`, producer relations and all combinations of Var-bearing programs
with raw documents/spellings to `ConcreteSpec/Diagnostics/` outside `ConcreteSpec.Spec.*`.
Do not leave forwarding declarations in Spec whose reachable type/value recombines
these carriers. Keep the byte-free observation semantics in Spec and prove the
moved adapters project those same relations. Prim imports neither Observe nor a
producer/Diagnostics module. An Ident-only byte carrier is safe only while its
reachable closure contains no Var; measure each migrating declaration, not its name.
Use existing nominal Name/Numbers boundaries for semantic identities, no new wrapper
or exemption to hide raw text. Run unchanged local_identity positive/rejection
controls, trust_ban and root import-closure gates at F18/F19; a rejected declaration
requires moving the boundary, never suppressing the scanner.

`checkLocatedProject x` constructs the plan from x.origins and runs the shared
semantic check kernel once, selecting and decorating its facts. Prove exact refusal-
erasure equality with `loadChecked x.raw (planOf x)` for this SAME supplied plan,
including selected indices/reasons; success data are identical. Across two valid
origin layouts for the same raw input, prove identical underlying indexed check
facts and admission/success, with EACH result selected by its own plan. Selected
refusal collections need not agree: grouping two independent failures into one
document retains one; splitting them retains two. Moving ranges can select a
different reason. Controls prove both effects while preserving raw facts and success,
and refute a mutant that changes admission based on provenance. A decoration-loss
mutant must fail same-plan exact erasure, not an impossible cross-plan equality.
No source coordinates are inferred from raw tables alone.

Every located refusal has exactly one approved rule, one primary origin and any
related evidence. Primary for a collision is the later declaration; earlier is
related. Missing-file/invalid-span errors point at the PRODUCING declaration, not
the absent file or invalid candidate span. Prove exact occurrence/range identity,
bounds, byte-derived coordinates and rule/check association. If a producer cannot
construct verified provenance, it cannot call the compiler diagnostic boundary;
that integration is unfinished, never silently reported as an ordinary source error.

## First error per file

Governing owner record [29-list item 12](../decisions.md#recovered-29-12), verbatim:
“12. Each file is checked separately, and checking a file stops at its first error. (yes)”
No multi-error recovery is introduced. “Stops” constrains reported checking results
and subsequent artifact use; computing internal prerequisite facts does not expose
later errors or execute user code. A file's later valid declarations may still be
needed to establish another file's prerequisites.

Attribution is the primary origin's DOCUMENT OCCURRENCE, not its path spelling or
candidate Span path. Raw-only plans use disjoint tagged file/site/request occurrence
keys; these do not claim physical provenance. Located plans regroup all underlying
facts before selection. Within each document, order nodes by
`(primary.startOffset, primary.endOffset, occurrence preorder index, check rank)`;
raw order is tagged occurrence preorder then rank, without fabricated offsets.
Rank orders checks at one occurrence: path parse, source conflict, site collision,
missing source, bounds, line, column; request checks use ISS-0050's order.
Results enumerate document input order. Collision primary is the later table index,
with least earlier conflicting index as related evidence, regardless of byte order.

Construct a finite acyclic prerequisite graph from raw data and the plan's checked
coverage certificate. Evaluate it to COMPLETION by finite topological traversal;
its ready-node order affects neither eligibility nor the final selection order.
Each node is pass, eligible failure, or blocked. Precisely, a node is eligible iff
ALL its immediate prerequisites passed; an empty prerequisite set is eligible.
Only eligible nodes evaluate their decidable check. A blocked node retains a path
to an eligible failing ancestor, never counts as passed, and emits no cascade.
Prove traversal independence, termination, dependency closure and coverage of every
admission conjunct. On a nonpassing graph, following failed/blocked prerequisites
must end at an eligible failure, so a refusal collection cannot be empty.

Specify edges, not an informal notion of an earlier error:
* Path parsing is a root per raw source occurrence. Compute each parse result once
  as total prepared data. There is ONE conflict node per later occurrence j,
  eligible when j parses; scan ALL successfully parsed earlier occurrences and
  select the least conflicting i. Failed earlier parses are not prerequisites of
  this node. Preparing an earlier parse result is distinct from requiring that it
  passed. Pairwise comparisons are internal facts of this single check, not extra
  failing nodes with identical selection tuples. A malformed prior cannot mask
  another prior's conflict. Prove the node's failure iff such a parsed prior exists.
* Site uniqueness is checked at every later occurrence against its full prefix.
  Coordinates are not hidden by uniqueness; duplicate and coordinate failures are
  independently reportable facts before per-file selection.
* For a site's structured path q, compute matching parsed source candidates by
  EXACT path identity. If none exist, missing-source is an eligible root failure;
  malformed raw source strings cannot match q and are never prerequisites here.
  If candidates exist, resolution depends on their parsing and every path-conflict
  fact involving those candidates (including case-conflicting different paths).
  A conflict blocks source resolution/coordinates with its conflict root; otherwise
  the unique exact candidate supplies bytes. Missing-source passes when a candidate
  exists; coordinates require successful resolution, then bounds → line → column.
  Thus an unrelated malformed source or collision never masks genuine absence.
* Obligation registration/request edges are defined in ISS-0050. Global admission
  gates artifact construction and authoritative discharge, not independent diagnostic
  facts for unrelated identities. No node executes user code or searches for proofs.

For EACH attribution key, select the minimum eligible failing node under its tuple,
AFTER evaluation completes. Output is EXACTLY these per-key minima, with none for
keys lacking eligible failure. Earlier eligible nodes at that key pass; earlier
blocked nodes need not. Global admission succeeds iff all required nodes pass,
equivalently no eligible failures exist, independently of grouping and order.
Do not use topological readiness order as source priority, and do not stop graph
evaluation when retaining a diagnostic.

Required closed controls, through the located entry points with WellLocated fixtures:
* u at 10 depends on valid d at 100; independent v at 50 fails; u fails and wins.
* doc0 fails early but its later d passes and enables doc1's failure; retain both.
  Change d to failure: block its consumer and retain the actual root, no cascade.
* Earlier coordinate failure then later malformed declaration in ONE document;
  two documents with failures; same-occurrence tie; A-B-B-A; missing source despite
  an unrelated unparsable source; matching-source conflict blocks coordinates.
* Same raw facts under one-document/two-document plans and reversed ranges test
  different selected collections with identical admission; fixed-plan erasure
  still preserves exact selected reasons/indices.
Mutants pick later/category-priority errors, emit two per file, stop globally,
stop evaluating after a file failure, use least-ready order, suppress independent
collisions, merge document occurrences or emit cascades. Compare the same graph,
input and supplied plan except in the explicitly different-plan controls.

## Assembly and adjacent reporting issue

`Frame.assembleProject` returns the informative load result on refusal and builds
only on admission. Preserve the exact whole refusal collection, including origins,
rules and related locations. Any retained Option interface is its proved erasure;
source-launch/build relations use that erasure with unchanged success data.
Source admission is never translated into a runtime status/startup refusal.
Prove all payload preservation laws, not merely successful assembly soundness.
Mutants swap rule/reason, replace primary by another valid occurrence, lose a
collision entry, substitute equal-record indices, trust candidate coordinates or
erase assembly refusal. All must fail exact laws at fixed inputs.

ISS-0084 separately owns the confirmed `ExecAdapter.terminal?` / `reportObservation`
diagnostic gap: an unregistered trap site becomes absence in `trapTerminal`, terminal
conversion and final-report observation. Admission validates supplied entries, not
registration of every site an arbitrary execution can reference. Scope excludes
ordinary nonterminal evaluator reports: `.stuck`, `.badAnswer`, `.outOfFuel`, and
non-request `.noAnswer` intentionally have no external observation. Inventory also
records `.normal` and unmapped returned status as terminal conversion none arms,
and `.noAnswer` request projection failure as a separate boundary to classify, not
an automatic bug or authorization to invent observations. Missing launch-grant
provenance belongs to held startup policy (ISS-0073), not this allocated trap issue.
ISS-0084 closure requires a typed `unmappedTrapSite kind site` error retaining
the exact runtime trap kind and SiteId through terminal conversion and final-report
observation, with payload-loss controls;
existing registered-site trap theorems remain intact. An absent SiteId has no
guaranteed producing byte occurrence; no fabricated location or static RuleRefused
identity is required. This is runtime observation failure, not static admission.
Preserve intentional nonterminal absence as a separate result case. Do not expand its scope just
because another arm returns none.

## Rule registration owner question

Open question, not a chosen language rule: approve per-check declarations below,
or one composite rule for each admission/discharge entry point? Existing authority
[diagnostics decision](../decisions.md#decided-project-diagnostics-2026-11-06)
requires typed results and rule identities;
`origin/wip/spec-integration-head:ConcreteSpec/Proofs/Observe2.lean:722`
explicitly leaves loader registration open. Check the actual decision anchor before
registering; each rule doc comment must link its governing owner record, as required
by `ConcreteSpec/Spec/Rules.lean` and `scripts/spec/rules.py`. Candidate rule predicates take concrete
data (EvidenceContext, ObligationTable, ObligationId), never SafetyEvidence whose
claim is Prop-valued. Pass unapplied predicate constants to rule examples; doc
comments must include an EXISTING governing Q*/OD*/P*/R-* row ID accepted by the
exporter. The owner answer must establish that mapping; allocate no IDs or revisions here.

| Semantic refusal | Candidate declaration (proposed only) |
|---|---|
| malformed source path | SourcePathPermitted (reason tag from approved path check) |
| duplicate file identity | SourceFileKeysUnique |
| duplicate site identity | SiteKeysUnique |
| missing source bytes | SiteSourceRegistered |
| out-of-bounds span | SiteRangeBounded |
| wrong line / wrong byte column | SiteLineMatches / SiteColumnMatches |
| duplicate obligation identity | ObligationIdsUnique |
| context mismatch (field tag retained) | EvidenceContextMatches |
| requested identity mismatch | EvidenceRequestMatches |
| missing obligation | ObligationRegistered |

Option A: approve these per-premise declarations with one rule per refusal; costs
more registration/example/revision tracking, gives stable precise blame. Option B:
register `ProjectSourcesPermitted`, `ObligationTablePermitted`, `EvidenceDischargePermitted`
with reason tags; fewer declarations, broader revisions and less precise rule links.
Recommend A under Philosophy 3/5/6: the checked premise and cited authoritative rule
stay directly connected. Constructor/context-field tags remain payloads, not invented
additional rules. Exact names, granularity and decision links return to the owner;
no registration or diagnostic-completeness claim before the answer. Semantic admission
and raw search proofs do not depend on that answer.

No manifest/table syntax is REQUIRED by this design, so no new syntax decision is
needed to prove it. If a later consumer requires serialized tables, the explicit
owner question is: A, keep tables internal and instrument existing source/proof/config
producers (recommended; moderate producer proof cost, no grammar/schema expansion);
or B, add a separately designed serialized table surface (large grammar, versioning,
malformed-syntax rule/provenance and parser-proof cost). B needs its own full proposal
and answer before implementation; neither option is silently encoded here.

## Governing records and concrete landing sequence

Main's ISS-0068 still records held SourcePath/Span extraction. The newer source-backed
owner path answer is at `origin/wip/record-2026-11-16b:docs/decisions.md:3725`
(ASCII, slash separators, strict refusal/no rewriting, case collisions including
directory prefixes, 100-byte components/200-byte paths and enumerated forbidden
names). This branch does not copy that decision file or claim its entry is on main.
F03p/F04p must reconcile main's hold with that authority, implement the single path
conflict relation and qualify dependent interfaces before located work lands.
ISS-0073 on main still holds startup/ABI for F28/F29. ISS-0048's initial-world
ownership is decided separately, not startup/ABI authorization.

Coordinator plan `scratchpad/f03/plan.md` (supplied writer input) rows F18–F20p,
F28–F29p budget F19 under 1.7k lines (large), F20 under 2.7k (large), F20p under
0.6k (small); these extraction estimates do NOT include all new diagnostic work.
Each line below is a landable slice only with all affected compiling consumers:

1. F03p/F04p approved paths/Span and full dependent controls after F03/F04:
   medium implementation/proofs; reconcile the November 16 policy first.
2. Shared byte-only carriers and Diagnostics-level WellLocated inputs; later
   producer-module construction/Decoded soundness, decoding erasure,
   bounds/coverage and per-file scheduling: medium-to-large; measure actual parser,
   elaborator and cache/import producers, no provenance oracle. Keep lower imports
   acyclic; no new manifest parser. This is explicit added work beyond predicate factoring.
3. Owner rule-mapping answer → decision-linked registration/examples and registry
   gates: small-to-medium, prerequisite for the located diagnostic slices.
4. F18 follows F10/F04/CAP and accepted ISS-0050; independent semantic search/admission
   then checked/located discharge and consumer migrations as detailed there. F19
   follows F10/F04p and accepted ISS-0047, incorporating ISS-0045 coordinate delegation
   FIRST (adjacent hunk at
   `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:278`), main numeric variable identity,
   ISS-0048 explicit launch world, EntriesValid and checked raw admission: large.
5. F19 located loader/scheduler and informative assembly integration, erasure and
   refusal-preservation proofs: large additional slice after 2–4 as applicable;
   Frame integration waits for F28 authority instead of importing unapproved startup.
   The loader boundary is independently useful before Frame; closure waits for both.
6. F20 migrates Observe2, report/output/environment/source evidence and all adapter
   literals; F20p closes approved path controls. F28/F29 migrate Frame/build/launch
   consumers after ISS-0073 authority; F29p covers end-to-end source-policy refusal
   propagation. Medium-to-large consumer/proof work beyond the extraction budgets.
   F35's hosted compiler switch preserves all distinctions and production diagnostics.

Measure changed declarations/consumers before implementation, revise budgets, and
split only independently useful preparation; never split a semantic API switch from
required consumers. Each slice has exact-SHA gates and eligible reviews when landing
is authorized. This writer round changes designs only and runs no review/push.

## Anchored inventory and proof method

All snapshot evidence is historical migration input, not current-main implementation:

| Snapshot anchor | Declarations/consumers |
|---|---|
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:167` | SiteTable.Valid |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:180` | SourceLoadRefusal, LoadedProject, SourceLoadResult |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:202` | parseFiles, firstDuplicate, siteIssue, findSiteIssue, load |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:251` | Realizes, Names |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:318` | trapTerminal |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:454` | ExecAdapter and loaded |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Observe.lean:576` | WellFormed, terminal?, reportObservation, launchObservation |
| `origin/wip/spec-integration-head:ConcreteSpec/Proofs/Observe2.lean:80` | findSiteIssue_none, load_admitted, load_locations, load_refusal_names |
| `origin/wip/spec-integration-head:ConcreteSpec/Proofs/Observe2.lean:1078` | sites_valid |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Frame.lean:907` | assembleProject; startup refusal is distinct |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Ident.lean:812` | LocationOk |
| `origin/wip/spec-integration-head:ConcreteSpec/Proofs/ObserveOutput.lean:200` | OutputDemo.adapter |
| `origin/wip/spec-integration-head:ConcreteSpec/Proofs/Adequacy.lean:1871` | erasingAdapter/keepingAdapter |
| `origin/wip/spec-integration-head:ConcreteSpec/Proofs/Frame.lean:2235` | LoadBoundaryDemo |

Inventory actual calls/struct literals (including Environment.adapter and demo
constructors) in the extraction source with `git grep` at its explicit ref, then
inspect namespace-local uses. These snapshot files must not be copied to main as
part of this design. F18/F19 consumer manifests must use current-main interfaces.
`load_realizes` does not exist; use `LoadedProject.load_admitted`. EntriesValid and
source_entries_valid are proposed names, not existing evidence. Reviewer CheckM is
legacy-only. All laws quantify over the real Spec admission/execution relations;
use finite structural recursion, existing decidability and equality transport, with
no new axiom, trusted search, classical choice or second semantic relation.
Provenance instrumentation and check-plan laws require kernel proofs and positive/
negative closed controls; prose plausibility is not qualification.
