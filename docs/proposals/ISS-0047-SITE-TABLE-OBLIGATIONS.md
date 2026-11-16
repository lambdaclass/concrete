# ISS-0047: site table admission and diagnostic provenance

Design round 2; proposed types/functions/laws below are proof and implementation
obligations, not completed Lean verification. Authority: [redesign authorization](../decisions.md#decided-2026-11-15-audit-redesigns),
[owner goals](../decisions.md#owner-goals-2026-11-15-provability-errors),
[Philosophy](../PHILOSOPHY.md#the-rules). Evidence anchors refer to integration
snapshot `aa44140a33747fa37ece59c76090972e564c65b4`, not newer main line numbers.

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
exact site table, file-key Nodup and site-key Nodup distinct. Duplicate-good sites
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
remains TOTAL over the
existing raw type (including `SourceLoadDemo.missing`, with no bytes anywhere).
Its semantic refusals retain tagged file/site occurrence indices, offending raw
records/fields and prerequisite facts. File and site indices are disjoint sorts.
Collision refusals retain both complete records and indices, including equal values.
For the raw first-collision projection of each table, traverse by increasing later index `j`; a collision exists when its
key occurred in the prefix. Choose the least such `j`, then its unique earlier match
`i` (prefix before the first collision is Nodup). On A-B-B-A choose `(1,2)` rather
than `(0,3)`. `firstDuplicate`'s old earlier-key scan is intentionally replaced.
For source files collisions compare successfully parsed path identities, including
any additional collision checks required by the approved path policy; a malformed
record has its own refusal, never a guessed parsed identity.

`CheckedLoadResult` is either exact `LoadedProject` admission or a NONEMPTY finite
collection of semantic refusals, at most one per attributed file occurrence under
the scheduler below. Admission remains all-or-nothing. Prove the full existing
`load_admitted` biconditional using factored Realizes, rejection iff no realization,
exact preservation, uniqueness and admission reorder invariance. Refusal ordering
may change; old category-priority refusal equality is NOT promised. A retained raw
Option loader is solely this result's success erasure using the canonical raw plan
over identical raw inputs. `CheckPlan` stores finite indexed checks, grouping/order
and a constructive coverage/dependency certificate, not assumed check outcomes.
A total raw-plan constructor gives the existing one-argument API.
Existing `SourceLoadRefusal.Names` is a weaker derived membership corollary, never
a substitute for occurrence precision. Existing fixture types and `ExecAdapter.sourceInput`
remain unchanged; adapters retain their admission evidence over those raw inputs.

A lookup-only loader dropping site Nodup must fail on shadowed good/bad input;
a keep-first loader must fail preservation/uniqueness on duplicate-good input.
Test coordinate-ignoring acceptance, rejecting valid projects, later-collision and
equal-record index substitution. Do not construct an invalid adapter with an
impossible `loaded` proof to test these laws.

## Located boundary

Place shared provenance carriers alongside SourceText/Span/RuleId in the lower
Ident layer (or an Ident-only leaf), not in Observe. Prim never imports Observe.
No serialized site/obligation manifest is assumed: these are elaborator tables.
A byte document has a stable document-occurrence identity, raw path spelling and
actual `SourceText`; its identity does not require its raw path to parse successfully.
A `ByteRange` carries `start ≤ end ≤ bytes.length`; derived line/column use the
existing byte functions. A diagnostic for a malformed path can therefore point
into its real declaring source/config document without trusting the rejected path.
A shared `ByteLocation` carries document identity, range and byte-derived line/column;
its validity law is exactly bounds and the line/column conjuncts of `Span.LocationOk`.
It does not require the rejected raw path to be a SourcePath. Projection to a public
checked Span requires an admitted path for that DOCUMENT, not the candidate path
field being rejected. Diagnostic rendering preserves raw document identity when
that conversion is unavailable; it never invents an admitted path. Capture an external
source-path argument as its actual argument byte sequence and indexed range when
that is the producing input, rather than pretending it was a serialized table file.

Proposed `OccurrenceRef` is a tagged index (file declaration, site-table entry,
obligation registration, operation or proof attribute). Proposed `Origin` contains
the document id and range, plus a field/component selector when applicable.
`LocatedProjectInput` contains `(documents, raw : ProjectSources, origins)` with
origins for every checkable occurrence and relevant field, including both collision
occurrences. A `Decoded documents raw origins` proof is required at its constructor.
This is a relation on ACTUAL decoding/elaboration derivations, not another admission
semantics: erase instrumentation from the existing parser/elaborator derivation to
obtain exactly `raw`; each origin is the range consumed for that indexed occurrence
in that SAME derivation. Repeated equal strings and zero-width generated nodes are
identified by occurrence and derivation, never by searching bytes for equal text.
Generated site/obligation entries map to the producing operation/proof attribute;
record the producing-node link and derive it during elaboration. Imported/cached
entries carry their original document and checked derivation links, not new spans.

Implementation must define this relation inductively over the real producer's
constructors, with erasure/coverage/bounds proofs for every constructor. For an
existing raw in-memory value with no producer bytes, `Decoded` need not be inhabited.
Do not add an axiom, a free-form asserted origin map or a total raw-to-located coercion.
Fixtures testing diagnostics construct real byte inputs and instrumented derivations;
raw semantic fixtures continue unchanged. No new total parser is postulated here.
If an external table-input surface is later required, its syntax needs an owner
proposal; neither these designs nor Concrete.toml's existing policy tables authorize it.

Proposed authoritative compiler entry `checkLocatedProject (x : LocatedProjectInput)`
constructs `x.plan` from its verified origins and runs `loadChecked x.raw x.plan` ONCE, then projects its semantic failures through `x`'s
verified origins. Raw validity is the semantic kernel's success predicate, independent
of decoration. Prove, for every raw `r`, every document set `d`, origins `p` and
`h : Decoded d r p`, erasing diagnostics from `checkLocatedProject ⟨d,r,p,h⟩` equals
`loadChecked r (planOf h)` with the same admitted data/refusal indices and reasons. Conversely
admission of any verified lift iff `∃ project, r.Realizes project`. These quantified
laws keep raw inputs working without promising every raw value has a byte origin.
Two byte layouts decoding to the same tables may give different diagnostic ranges;
they must have identical semantic erasures. No coordinates are derivable from tables alone.

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
No multi-error recovery is introduced. The compiler checks each document independently
until its first failure; one failed document does not stop checking other documents.

Attribution is to the primary origin's DOCUMENT OCCURRENCE, not its path string or
the file claimed by a candidate Span. A repeated file declaration in one config/source
file belongs to that declaring document; a site generated in another source belongs
to that source. For raw-only checking, the attribution key is the tagged input file
occurrence or, for sites, the producing-site occurrence; this is a semantic grouping,
not a claim of physical file provenance. The located scheduler regroups by verified
physical document origins; it must not merely decorate the raw collection after
raw grouping has suppressed a needed failure. Thus `loadChecked`'s raw grouping is
parameterized: default occurrence grouping for fixtures, document grouping supplied
by `Decoded` at the compiler boundary. Erasure equality above uses that SAME supplied
grouping/order; success/admission is invariant under either grouping.

Define a finite ordered check plan, derived from indexed input and verified origins.
Within each document order by `(primary.startOffset, primary.endOffset, occurrence
index, check rank)`. The rank breaks ties ONLY at the same occurrence: path parse,
file-key collision, site-key collision, missing source, bounds, line, column; discharge
uses the context/request/lookup order in ISS-0050. Tie occurrence indices are producer
preorder indices, not table key sorting. Raw order uses tagged occurrence preorder,
then rank, without fabricating byte positions. This tuple is the ready-node priority:
construct the plan by finite stable topological sorting, selecting the least ready
node (document index, then this tuple) at each step. A dependency necessarily precedes
its consumer even if its range is later; “first” means this explicit check-plan order,
not a promise to diagnose an error whose prerequisite cannot be checked. Prove the
sort respects every dependency and tuple order among simultaneously ready nodes.
Document results are enumerated in
document-input order, not host filesystem order. Collision primary is always the
later indexed declaration, with origins preserving producer order within a document.
The diagnostic plan includes a collision check at EVERY later occurrence against
its full prefix, with earliest prior matching index as related evidence; it cannot
reuse a single global first-collision result and lose another document’s collision.
For the global first-collision law that prior match is unique; for later per-file
collisions it need not be unique, so choose the least earlier match explicitly.
This same plan rule applies to obligation admission (ISS-0050); only raw admission
uses its single first-collision projection. No discharge executes after failed admission.

Check nodes have explicit decidable prerequisites: parse before its key comparison,
resolved source before coordinates, admission before executing a built artifact.
Malformed source identity suppresses dependent coordinate checks, not independent
checks in that file or another file. Missing bytes are a site error only when lookup
absence is genuine in the successfully admitted source identities, not caused by
an earlier failed source declaration. Any failed prerequisite has its OWN root
refusal. Evaluate prerequisites in a finite topological traversal; schedule eligible
checks in document order; after a document fails, execute no later checks attributed
to it. Cross-file prerequisite failure blocks dependents with its root failure,
without manufacturing cascades. A blocked node never masquerades as successful
admission. Prove prerequisite closure and that any invalid Realizes input has a root
failure (structural induction on finite dependencies); this is part of implementation.
The plan cannot include execution of user code or an unbounded search for evidence.

Selection law: for each document `f`, output contains exactly one failure iff the
eligible check plan for `f` has a failure before its stop; it is the least failing
node in that plan, with all earlier eligible checks successful. Documents without a
failure have none. The output is EXACTLY the map of these per-file minima, with no
extra/omitted file. Eligibility and roots are defined from the original inputs and
prerequisite results, not from which diagnostic a mutant chose to retain. This law
covers ALL checks together, so later malformed paths cannot outrank earlier coordinate
errors in the same document merely by category. Global admission succeeds iff all
checks succeed; collecting first errors is not partial project admission.

Controls: earlier coordinate failure followed by later malformed declaration in ONE
physical document; two different failing documents; A-B-B-A; two failures tied at one
occurrence; malformed-source dependency versus independent failure in a second file.
Mutants choose the later/category-priority failure, emit two in one file, stop the
whole project at the first file, suppress another file's first error, merge equal
path spellings from distinct document occurrences, or report dependent cascades.
Keep the same plan, grouping, prerequisites and input in every mutant comparison.

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
ISS-0084 closure requires typed unregistered-site refusal, exact site/request origin
and approved rule, preservation through final reports, and payload-loss controls;
existing registered-site trap theorems remain intact. Do not expand its scope just
because another arm returns none (agy I2 is adopted as an inventory, with Codex's
explicit nonterminal-scope correction).

## Rule registration owner question

Open question, not a chosen language rule: approve per-check declarations below,
or one composite rule for each admission/discharge entry point? Existing authority
[diagnostics decision](../decisions.md#decided-project-diagnostics-2026-11-06)
requires typed results and rule identities; snapshot Proofs/Observe2.lean:722–729
explicitly leaves loader registration open. Check the actual decision anchor before
registering; each rule doc comment must link its governing owner record, as required
by Spec/Rules and scripts/spec/rules.py. No IDs/revisions are allocated in this design.

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

Main's ISS-0068 records the held SourcePath/Span extraction. The newer owner answer
is now faithfully copied from `origin/wip/record-2026-11-16b` into
[November 16 file paths](../decisions.md#decided-2026-11-16-file-paths): ASCII, slash
separators, strict refusal/no rewriting, mixed-case spelling with case-collision
refusal including prefixes, 100-byte components/200-byte paths, and the enumerated
forbidden components/names. This supersedes the former blanket character-policy
hold ONLY within its stated scope; F03p/F04p must implement and prove that policy
before dependent location interfaces land. Do not retain AY-2/3/4 as provisional
approval or assume older path predicates already implement the new answer.
ISS-0073 on main still holds startup/ABI for F28/F29. ISS-0048's initial-world
ownership is decided separately, not startup/ABI authorization.

Coordinator plan `scratchpad/f03/plan.md` (supplied writer input) rows F18–F20p,
F28–F29p budget F19 under 1.7k lines (large), F20 under 2.7k (large), F20p under
0.6k (small); these extraction estimates do NOT include all new diagnostic work.
Each line below is a landable slice only with all affected compiling consumers:

1. F03p/F04p approved paths/Span and full dependent controls after F03/F04:
   medium implementation/proofs; reconcile the November 16 policy first.
2. Shared located carriers and instrumented producer construction, decoding erasure,
   bounds/coverage and per-file scheduling: medium-to-large; measure actual parser,
   elaborator and cache/import producers, no provenance oracle. Keep lower imports
   acyclic; no new manifest parser. This is explicit added work beyond predicate factoring.
3. Owner rule-mapping answer → decision-linked registration/examples and registry
   gates: small-to-medium, prerequisite for the located diagnostic slices.
4. F18 follows F10/F04/CAP and accepted ISS-0050; independent semantic search/admission
   then checked/located discharge and consumer migrations as detailed there. F19
   follows F10/F04p and accepted ISS-0047, incorporating ISS-0045 coordinate delegation
   FIRST (separate adjacent Observe:278–307 hunk), main numeric variable identity,
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

Snapshot Spec/Observe.lean:167 SiteTable.Valid; :180 SourceLoadRefusal; :190 LoadedProject;
:196 SourceLoadResult; :202 parseFiles; :213 firstDuplicate; :219 siteIssue; :227 findSiteIssue;
:237 load; :251 Realizes; :260 Names. Spec/Observe:318 trapTerminal, :454 ExecAdapter,
:461 loaded, :576 WellFormed, :593 terminal?, :618 reportObservation, :639 launchObservation.
Proofs/Observe2:80 findSiteIssue_none, :95 load_admitted, :160 load_locations,
:245 load_refusal_names, :1078 sites_valid. Spec/Frame:907 assembleProject;
Startup is :868, its refused125 constructor :869 (not a source-load failure).
Spec/Ident:812 LocationOk; SourcePath/Span anchors are snapshot-only pending F03p.

Explicit missed consumers now included: Proofs/ObserveOutput.lean:200–212 OutputDemo.adapter;
Proofs/Adequacy.lean:1871–1889 erasingAdapter/keepingAdapter; Proofs/Frame.lean:2235–2340
LoadBoundaryDemo; Observe's Environment.adapter and all source/report demo constructors.
Inventory actual calls and struct literals with
`git grep -n -E 'SiteTable\.Valid|ProjectSources|LoadedProject|ExecAdapter|SourceLoadRefusal|assembleProject' -- ConcreteSpec`,
then inspect namespace-local calls; keep the bounded declaration inventory, not raw English hits.
`load_realizes` does not exist; use `LoadedProject.load_admitted`. EntriesValid and
source_entries_valid are proposed names, not existing evidence. Reviewer CheckM is
legacy-only. All laws quantify over the real Spec admission/execution relations;
use finite structural recursion, existing decidability and equality transport, with
no new axiom, trusted search, classical choice or second semantic relation.
Provenance instrumentation and check-plan laws require kernel proofs and positive/
negative closed controls; prose plausibility is not qualification.
