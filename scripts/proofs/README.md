# Axiom golden comparison: proposed design

Status: design only, 2026-11-09. No exception is implemented. The executable
contract remains the header of `axioms.sh`. This design implements the coordinator’s
four conditions, not the integration tool’s prose exception. Reviewers must judge
and fix by the best long-term design, without cutting corners for speed, diff size,
or review rounds. Population P and standard axioms A remain unchanged.

## Pinned API evidence and blocking prerequisite

Read the supplied `REV-goldenrule-codex-0e0376f.md`, its Transitive reproduction,
and `integration-golden-flow.sh` (reference only). Inspected the actual sources at
`$(lean --print-prefix)/src/lean/Lean` on this host; `lean-toolchain` pins
`leanprover/lean4:v4.34.1`. The prefix resolved to
`/nix/store/94bmilywgajdnqvix1fs35fj94kala0w-lean4-release-4.34.1`.
Ran `timeout 20s` source searches/reads and `timeout 30s lean <temporary Probe.lean>`
(no spec import or build). `#eval Lean.versionString` printed `"4.34.1"`.
Successful `#check`s established these actual APIs:

- `Lean.LocalDecl.isAuxDecl` and `isImplementationDetail` take **LocalDecl**,
  not ConstantInfo. They cannot certify imported theorem origin.
- `Lean.Meta.auxLemmasExt : EnvExtension AuxLemmas` is explicitly a local,
  non-olean cache in `Meta/Tactic/AuxLemma.lean`. `mkAuxLemma` creates ordinary
  theorem declarations; its cache maps type/privacy/defeq to name/universes,
  not helper to parent. Name shape, missing source ranges, and theorem kind
  are insufficient substitutes.
- `Lean.Meta.getMatcherInfoCore? : Environment → Name → Option MatcherInfo`
  reads matcher metadata (`Meta/Match/MatcherInfo.lean`). MatcherInfo describes
  parameters/alternatives, not an owning theorem. It certifies a matcher only.
- `Lean.Meta.isEqnThm? : Name → CoreM (Option Name)` reads `eqnsExt.mapInv`
  (`Meta/Eqns.lean`). This extension is also local. `getEqnsFor?` populates it
  and may realize new equations; querying it after arbitrary generation would
  change the population. `declFromEqLikeName` uses name structure and is not
  itself compiler-origin evidence. Matcher equations have a distinct route in
  `Meta/Match/MatchEqs.lean`; do not conflate these APIs.
- `Lean.privateToUserName? : Name → Option Name` understands private names;
  `Lean.Expr.getUsedConstants : Expr → Array Name` extracts graph edges.

A temporary probe checked the equation inverse after `#print equations eqParent`:
`eq-parent-before=(some eqParent)` and `eq-parent-after=(some eqParent)` after
`getEqnsFor?`. This proves a local realization case, not imported persistence.
A first graph probe using elaborator `env.find?`/`value!` panicked (Lean still
returned exit 0); it is not validation. The corrected probe used
`env.checked.get.find?` and exhaustive ConstantInfo matching, with output:

```text
’parent’ depends on axioms: [onlyHelper]
’helper’ depends on axioms: [onlyHelper]
parent-reaches-helper=true
definition-reaches-equation=false
```

(The printed declaration quotes above are typographic; the Boolean lines are
literal.) Here `axiom onlyHelper : True`, `theorem helper : True := onlyHelper`,
`theorem parent : True := helper`; `eqParent` is a two-case Bool definition.
No committed controls or acceptance claims follow from handwritten helper names.

**Blocking question for the coordinator/owner:** the pinned imported environment
cannot supply general helper origin plus exact ownership through the APIs above.
Options are (a) add persistent compiler-produced origin/owner metadata at helper
creation, retaining it in oleans; or (b) narrow the requested supported classes
and keep all unsupported removals refused. Recommendation: (a), with an upstream
or explicitly maintained toolchain change, because the requested genuine
`_proof_`/`_simp_` renumbering acceptance otherwise lacks its required evidence.
This is a toolchain scope decision; accepting inferred provenance instead would
be a **trust-policy decision**, and is not authorized by this note. No such
inference is chosen. The coordinator must allocate/name the owning ROADMAP row
for persistent helper provenance before implementation; writers allocate no R IDs
and this note does not invent one. There is no known existing row established here.

The structural design below requires that prerequisite. A partial implementation
that merely rejects everything does not complete the task or its positive control.

## Origin, exact ownership, and graph evidence

At compiler helper creation, persist structured `(helper Name, origin kind,
owner Name)` facts, including cached-helper reuse. The metadata must describe the
actual generating declaration, not a prefix inferred later. Reject zero or
multiple distinct owners; do not pick the first owner or an arbitrary user of a
shared helper. Audit each supported generator and persistence across imports.
Existing matcher/equation facts can corroborate class-specific facts, but cannot
stand in for absent general ownership. Explicit source declarations, including
quoted identifiers and namespaces, acquire no generated-origin record. A name
regex may optimize lookup, never grant eligibility. Unknown generators refuse.

Resolve the exact baseline helper through the baseline environment metadata.
Require one exact owner, then exactly one full inventory row keyed by that exact
structured Name in **each** inventory. Compare complete row bytes, including the
axiom field and empty trailing field. Neither a sibling nor a longer prefix is an
owner. Parent definitions without theorem rows fail. Never deduplicate before
checking multiplicity. Preserve the existing inventory serialization, but reject
noninjective printed-name collisions using the enumerator’s `enc` identities.

Private scope/module numeric components remain part of Name identity. Use Lean’s
private-name API only to interpret metadata; do not strip scopes to match rows,
use a regex to guess module boundaries, or transfer ownership between declarations
with equal user names. A private helper may have a public owner; the stored exact
owner resolves this without guessing. A renumbered private **parent** is not an
identical row and fails this exception. Conflicting private owner records fail as
ambiguous even if one candidate has an unchanged row.

Use the **baseline** checked kernel environment for the removed helper’s graph.
Match ConstantInfo as `CollectAxioms.collect` does in `Util/CollectAxioms.lean`:
visit type and value for def/theorem/opaque, type for axiom/constructor/recursor,
and type plus constructor edges for inductives; quotient has no edges. Extract
expression constants with `getUsedConstants`. Missing bodies/declarations fail,
rather than appearing as leaves. Use visited sets and memoized adjacency, sorted
encoded Names, and deterministic BFS to emit a shortest owner-to-helper path.
Require a positive-length path; the helper cannot justify itself. Record every
edge and its origin (type, value, or constructor list), with declaration content
hashes and the environment manifest. Recompute edges when checking the evidence;
a supplied path alone is not trusted. Do not equate axiom-set inclusion with
reachability. Imported precomputed axiom sets do not provide constant edges.

Also recompute baseline parent/helper axioms and check against their inventory
rows and helper-axiom inclusion in parent axioms. Inconsistent evidence refuses.
The candidate parent’s identical axiom row then covers the baseline helper’s
axioms; this does not assert identical statements or implementation. A is still
run on every candidate row, including additions. No rule permits a surviving
row’s axiom change or handwritten deletion. Added rows retain existing golden
update/review requirements; this exception cannot silently authorize additions.

Independently generated equations not reached by the parent are refused with a
separate diagnostic even if their axiom sets happen to agree. No equation
exception flag is proposed. A future policy needs an explicitly approved owning
document specifying evidence, scope, controls and review obligations; allowing
it is a trust-policy decision. Do not silently add it to this rule.

## Explicit baseline and comparison

Proposed interface: `axioms.sh --baseline <full-commit-sha>`; no implicit HEAD~,
merge-base search, reflog, environment-variable baseline, or network fetch.
The coordinator chooses and records the baseline (for example an explicit merge
base). Resolve the SHA locally, verify the pinned toolchain matches, and read its
committed golden. Generate metadata/inventory from its freshly built sources in
a private full checkout on concrete-ci-2. Require its golden to match that build
exactly. Never import baseline oleans into the candidate checkout. Bind outputs
to source SHA, golden hash, toolchain identity, enumerator version and build
manifest; reject stale/mismatched artifacts. A candidate checkout with relevant
uncommitted changes cannot produce SHA-bound acceptance evidence.

The current candidate golden still equals the fresh candidate inventory exactly
(using the existing comment convention). Baseline mode adds a mechanical audit
of the old-to-new golden delta: only proven eligible removed rows are excused;
all other removals/changed rows fail. Additions undergo A and the existing golden
review workflow. Compare by exact declaration identity before classifying deltas,
so a changed axiom field cannot masquerade as a removed helper plus an addition.
Without `--baseline`, preserve the existing byte-exact G comparison and behavior.
`--update` remains an explicit regeneration operation, never proof of eligibility;
combine neither baseline acceptance nor automatic golden writes with it.

## Reports, refusal diagnostics, and controls

Write a deterministic JSON report (schema `axiom-helper-removals-v1`) plus concise
stdout. Each accepted removal includes baseline/candidate SHAs and manifest hashes,
removed helper encoded/display Names, origin fact, exact parent encoded/display
Names, both complete parent rows as escaped byte strings, their hashes, baseline
helper row, and the ordered reachability edges/content hashes. JSON preserves
trailing tabs and line endings. Emit `G-HELPER-ACCEPT helper=<H> parent=<P>`.
Failure emits `G-HELPER-REFUSE code=<CODE> helper=<H> parent=<P-or-none>`.
H/P are injectively encoded Names; structured report fields carry readable names.
An ordinary unexplained delta emits `G-BASELINE-REFUSE code=ROW_CHANGED name=<N>`.
Exit nonzero on any refusal; malformed/missing evidence is an infrastructure
failure (exit 2), never an accepted empty comparison. Keep reports on failure.

Controls must compare exit status **and exact diagnostic line**, with substituted
fixture encoded Names, and inspect the full report. Each fixture first passes its
unplanted leg. Do not accept arbitrary build failure, timeout, or unrelated error.
Validate in this order: artifacts/serialization, same-name changes, provenance,
unique ownership, parent row multiplicity/equality, reachability, axiom consistency.

| Control and isolated planted condition | Exact expected diagnostic template |
| --- | --- |
| Genuine compiler renumbering: compile two source variants inducing different generated helper numbers, same exact inventoried owner row; compiler metadata and old graph present | `G-HELPER-ACCEPT helper=<H> parent=<P>` |
| Explicit source `foo_proof_1`, then separately `bar.match_2`, removed | `G-HELPER-REFUSE code=NOT_GENERATED helper=<H> parent=none` |
| Generated helper’s exact owner P absent, only prefix/sibling Pextra present on both sides | `G-HELPER-REFUSE code=PARENT_MISSING_BASELINE helper=<H> parent=<P>` |
| Exact parent row missing only from baseline | `G-HELPER-REFUSE code=PARENT_MISSING_BASELINE helper=<H> parent=<P>` |
| Exact parent row missing only from candidate | `G-HELPER-REFUSE code=PARENT_MISSING_CANDIDATE helper=<H> parent=<P>` |
| Parent present twice in either inventory | `G-HELPER-REFUSE code=PARENT_DUPLICATE helper=<H> parent=<P>` |
| Parent’s complete row differs | `G-BASELINE-REFUSE code=ROW_CHANGED name=<P>` |
| Conflicting private owner records, individually valid candidates | `G-HELPER-REFUSE code=PARENT_AMBIGUOUS helper=<H> parent=none` |
| Compiler equation metadata, unique inventoried owner but no owner-to-helper path | `G-HELPER-REFUSE code=EQUATION_UNREACHED helper=<H> parent=<P>` |
| Other generated helper not reached by owner | `G-HELPER-REFUSE code=HELPER_UNREACHED helper=<H> parent=<P>` |
| Plant helper-only axiom into evidence while claiming reached owner with unchanged row | `G-HELPER-REFUSE code=AXIOM_EVIDENCE_MISMATCH helper=<H> parent=<P>` |
| Surviving helper axiom field changed (cannot be reclassified as removal) | `G-BASELINE-REFUSE code=ROW_CHANGED name=<H>` |
| Stale baseline manifest or wrong golden hash | `G-BASELINE-ERROR code=ARTIFACT_MISMATCH` |

The conflicting-metadata and impossible helper-only-axiom fixtures exercise the
validator with deliberately corrupted evidence; do not claim Lean naturally
produces them. Separate Lean transitivity regression: assert exact axiom arrays
`[onlyHelper]` for both reached helper and parent, and a true reachability witness;
then plant an extra axiom only into an independent equation proof and assert the
parent does not reach it or acquire that axiom. The equation removal must produce
`EQUATION_UNREACHED` above (keep parent rows present to isolate that cause).
The supplied Transitive example additionally asserts propext on parent and eq_1
without inferring reverse reachability. Standard nonstandard-axiom controls still
exercise A with its existing exact `nonstandard` result; do not alter P or A to
make any fixture pass. Add strict-mode byte mismatch, added nonstandard row,
unsupported-origin, malformed report and missing-body controls with respectively
existing G refusal, existing A refusal, `NOT_GENERATED`,
`G-BASELINE-ERROR code=REPORT_INVALID`, and
`G-BASELINE-ERROR code=GRAPH_INCOMPLETE`; pin their full existing messages when
turning this design into executable controls.

## Cost, implementation surface, and limits

No concrete-ci-2 runtime measurements were made in this design round. Cost is two
fresh-source build validations (baseline and candidate), one metadata/graph export
per environment, O(V+E) graph storage/traversal per exported closure with memoized
adjacency, and deterministic sorting. Reuse built inputs only with exact manifests;
new baseline cache misses may require a full baseline spec build. Controls compile
small independent Lean fixtures rather than rebuilding the spec for each mutation.
Do not cap graph size by silently dropping edges; resource exhaustion is INCOMPLETE.
The coordinator should measure wall time, peak RSS and report size on concrete-ci-2
before choosing bounds. No gate or control is removed to meet a runtime budget.

After prerequisite resolution, implementation touches `theorems.lean` (new evidence
mode; existing inventory modes unchanged), `lib.sh` (explicit isolated baseline
and artifact plumbing), `axioms.sh` (comparison/report/controls), a dedicated
structured comparator and focused fixtures/tests, and this document/header.
`axioms.golden` changes only if an actual reviewed regeneration requires it.
Persistent compiler provenance requires a separate toolchain change outside this
writer’s authorized surface, plus migration tests; no ConcreteSpec edit is proposed.

Coordinator validation on concrete-ci-2, after implementation: in `nix develop`,
use `bounded_run 1800 -- bash scripts/proofs/axioms.sh` for strict mode, then
`bounded_run 1800 -- bash scripts/proofs/axioms.sh --baseline <full-baseline-SHA>`
at the exact candidate SHA with its explicit baseline; run the dedicated fixture
suite under the same bound. A timeout is incomplete, never a pass. Record both
SHAs and individual control diagnostics. Adjust bounds only from measured costs.

Limits: axiom rows do not establish statement/body equality or semantic coverage;
unchanged parents are an axiom-accounting condition only. Shared cached helpers
without unique compiler ownership, absent imported metadata/bodies, private parent
renumbering, definitions without inventoried rows, unknown generators, and
unreached equations remain refused. Existing Lean metadata is compiler evidence,
not a defense against hostile metaprograms forging compiler extensions; changing
that trust boundary requires a separate owner decision. This note does not make it.
