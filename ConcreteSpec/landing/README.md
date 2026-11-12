# Foundation landing inventory (piece P)

`inventory.tsv` pins the complete `ConcreteSpec` population of integration snapshot
`aa44140a33747fa37ece59c76090972e564c65b4`, the current-main baseline
`1a50c64ff1b6f3a2899522f279eccc86d3b004a2`, and the supplied [landing plan](plan.md).
This is an extraction inventory, not approval of the historical rules or proofs.

Run on CI/server, inside the repository's Nix shell:

```sh
nix develop -c bash scripts/spec/landing_inventory.sh
nix develop -c bash scripts/spec/landing_inventory.sh --remaining F03
```

The check needs four permanent lightweight tags under `refs/tags/spec-inventory/` on `origin`: `source`, `baseline`, `evidence`, and `dossier`. `PIN_TAGS` in `scripts/spec/landing_manifest.py` declares their exact commit SHAs. Every run verifies each local tag target, or verifies and fetches its advertised origin tag when the local tag is absent, without moving local refs or `FETCH_HEAD`. Configure repository tag protection to forbid updating or deleting `spec-inventory/**` before publishing them. Wip branches are never input anchors.

To add an input, choose its immutable commit, add its name/SHA to `PIN_TAGS`, create a lightweight `spec-inventory/<name>` tag with `git -c tag.gpgsign=false tag spec-inventory/<name> <full-SHA>`, and have the publishing harness push that tag after confirming the protection rule covers it. Add a changed/missing-tag refusal control and document the input purpose. Never retarget an existing tag; use a new name for a new snapshot. The publishing harness owns tag publication.

Unavailable pinned objects are an error; it never substitutes a moving branch. It writes all build artifacts in a private
`mktemp` directory, builds every source module (including orphans) in the parsed import graph's
order, and reads constants, kinds, ranges and term dependencies from Lean. All original sources
build with `autoImplicit=false` and warnings refused. No regex identifies Lean declarations.

Lean discards anonymous examples from the persistent environment. A second disposable build
names each example using its parser-provided keyword byte offset, retaining the complete original
command context, binders, type and proof. Its synthetic definitions preserve existing visibility and
modifier order and export the example's
actual dependencies, including through generated helpers. Only this instrumented build disables
`linter.defProp`, because a synthetic definition may contain a proposition. The original build
retains that linter. These synthetic names never enter the source declaration population.

The TSV contains these record types:

- `snapshot`: immutable source/baseline identifiers and the plan digest.
- `piece`: explicit prerequisite graph and unresolved authority/design holds.
- `declaration`: every persistent constant, including constructors, recursors, private helpers,
  generated equations and auxiliaries; `name` is Lean's namespace-qualified name.
- `command`: every parsed source command, with documentation included in its digest and range.
  Anonymous examples have `kind=example`. Context commands are also counted, so no source command
  disappears because it lacks a persistent constant. `kind=module-realization`, at range `0:0`,
  records an additional compiler phase for whole Spec modules; its digest covers the entire module.
- `finding`: each nonblank line of the pinned evidence dossier and every spec issue in the audit
  issue snapshot. Keeping raw evidence, including rejected findings and invalidations, avoids
  selecting only favorable conclusions. Each retains an exact source address and digest.
  `repair` here requires reconciliation, not acceptance of the historical allegation.
- `contract`: ten separately digested C1–C10 declaration manifests. Select their members using
  the `contract` column; closure also reconciles authority, evidence, provenance and retirement.

A declaration's `piece` is its extraction destination. `BASE` requires the actual constant in the
freshly compiled pinned baseline with a matching kernel fingerprint. Authored declarations also
require the same complete source command. `migration` names a known non-BASE piece and is mandatory only on BASE command/declaration rows; it is empty on every other row. Dispositions use the closed set `retain`, `repair`, `replace`, `owner-held`, `retire`; landed receipts alone use `present`.
No unchanged fragment is claimed to implement its future checked view. Remaining counts exclude
`BASE`; `--remaining all` lists all outstanding declaration names. Update assignments explicitly
only to repair the extraction schedule; checking never fills an omitted row. `--write` regenerates
the initial assignment from the pinned plan and atomically replaces the TSV only after all checks
and controls pass.

To record progress after a repair lands, keep the source rows and their digests immutable. Add
`landed` records mapping original declaration names to declarations in a full landed commit SHA.
For a piece whose names survive unchanged:

```sh
nix develop -c bash scripts/spec/landing_inventory.sh --record-landed F05 --landed-ref <full-SHA>
```

For renamed or relocated declarations, add explicit `landed` rows: `name` is the original name,
`source` is the landed commit SHA, `note` is the replacement qualified name, and `digest` is the
replacement declaration's kernel fingerprint, which must equal the original snapshot's `term_digest`. `--census-output <path>` exports the fresh source census for inspection;
it never supplies cached input. The checker refuses every snapshot pin as progress and requires each landed commit to be an ancestor of the checked `HEAD`. CI checks out complete history (`fetch-depth: 0`); shallow ancestry is refused. Each distinct receipt SHA is validated once and must descend from the inventory introduction commit. The checker freshly compiles each landed revision and the checked HEAD, and requires the mapped declaration's kernel fingerprint to match the snapshot at both revisions. Remaining declaration counts exclude verified mappings.
Presence verifies progress in reconstruction; it does not attest semantic equivalence, reviews,
authority answers, or contract closure. `BASE` remains the original baseline, never a progress label.
`--write` resets the initial inventory and must not be used to advance landed work.

Ranges describe complete commands, rather than line slices used as implementation sources.
A separate elaboration pass observes the local constant-map delta after every command, including
zero-range helpers and opaque partial-definition implementations. Each declaration's note starts
with its observed command identity (`file:start:end`). Authored declarations and single-emission
computational helpers move with their command; equal text at distinct
source positions remains distinct. This includes body helpers whose dependencies are absent from an
opaque declaration's signature. Shared realizations can be emitted by several importers. Their
`regeneration` column chooses one actually observed emission with the same `term_digest`; the
judgment checks that emission's actual dependencies and command destination. It never guesses an
origin from a name or places a helper in an arbitrary earlier piece. `dependencies` preserves the
original snapshot term's dependencies even when another observed emission supplies the helper.

Some equations are originally first materialized in later proof modules. The extra whole-module
phase asks Lean's registered reserved-name generators to materialize the snapshot's lazy theorems
when their definitions are available. Successful emissions are observed and fingerprinted exactly
like ordinary command emissions, with dependencies expanded through any new intermediate helpers.
Only theorem constants can use these additional regeneration points; computational helpers retain
ordinary command origins. A request cannot manufacture BASE membership. Split-module contexts are
not used as whole-module regeneration points, and dependencies must still precede every chosen
emission. A small two-module control observes an equation regenerated with its defining module.

`term_digest` hashes a structural DAG encoding of the kernel type, universe parameters, safety and
computational body, including constructor/recursor data. Metadata is excluded. For theorems, the
identical kernel type suffices by proof irrelevance; proof bodies can differ across importers. A
generated declaration qualifies for BASE only if the actual baseline constant has this fingerprint,
even when its emitting module differs. The freshly compiled baseline is a closed environment and
retains its own proofs/bodies; source snapshot proof dependencies do not replace those baseline
proofs. No missing baseline constant can be inferred from dependency closure. Where a command
already exists on BASE but emits a new artifact, that artifact belongs to the migration piece.
The coordinator's law/control direction is encoded in `law-control-placement.json`. The checker recomputes this evidence from immutable round-3 schedule commit `61998c215112b8ac5e011b242779a553334d74c7`, applying only the approved add0 citation correction and restored held Frame span. It checks every Lean-resolved annotation pair, rather than selecting a fixed number of cases. A control's later material requires the law to follow its control piece. Dependent commands follow moved laws and auxiliaries; mutually shared support and consumers converge on an available consumer piece. `moved-laws.json` records relocations, and `VALIDATION.md` lists the moved laws. This data records the coordinator ruling; it does not change the supplied plan.

Shared fixture commands are explicitly declared by snapshot command identity in `shared-fixtures.json`; selection never tests declaration-name substrings. The generator moves these fixtures with their earliest consumers, and the judgment requires equality with its earliest consumer piece and refuses unused fixtures; `fixture-selection.json` records the Lean-observed consumers and selection basis for each identity; `fixture-selection-changes.json` explains each addition/removal from the reviewed fixture set. Snapshot Instance/Planted law annotations define groups: laws, annotated Instance/Planted commands and single-law mutants must share a destination (or BASE migration destination). Laws move later when their controls require later material, as directed by the coordinator; controls never pull unrelated dependencies earlier. A mutant explicitly referenced by several laws is reusable support, declared in `shared-fixtures.json` and placed at its earliest consumer; it is available to every later law rather than reassigned repeatedly. Whole-module realization phases and global attribute registrations are material commands, never scoped templates. Attribute targets and doc-comment law/mutant names are resolved by Lean in their elaboration namespace and open-declaration context, and participate in dependency checking. Namespace/section openings, matching
ends, and context directives such as variables/open/options are templates: their notes enumerate every later destination in scope requiring a copy;
the judgment checks balanced pairs and complete destination lists. Named group boundaries use
Lean's declaration selection start, and anonymous commands use their parsed command start.
This is extraction provenance for this census, not the patched-toolchain provenance qualification
required by T3. In particular, an importing proof module can re-emit an imported auxiliary.

The M01–M12 prerequisite edges follow the freshly parsed baseline import graph, including
transitive paths through Spec modules. T2 explicitly remains held for its accepted bounded design.
The expensive census runs in the Spec build CI job, outside the Lean-free Root checks shell and
local fast push gates. Every census uses a private copy and leaves caller sources/artifacts intact.

The judgment refuses missing/extra census records, duplicate identities, empty/unknown pieces,
changed source digests or dependency lists, split commands, prerequisite cycles, incomplete contract prerequisite closures, and dependencies
on a later/unrelated piece. It checks both declaration terms and elaborated examples. It reports
all pieces before issuing its final dependency verdict. Its controls use normal file discovery/import traversal to plant a real unassigned declaration,
and require specific refusals for duplicates, premature commands and anonymous examples, identical-text
independent relocation of identical-text commands (including a deliberately collapsed-key implementation), invalid zero-range BASE assignments, import cycles and unknown progress pieces.
Private/noncomputable/unsafe examples, opaque body origins and source/artifact isolation have small
production-census regression controls. Progress controls also verify complete-history ancestry, identical-fingerprint relocated mappings, stale M01 refusal, later deletion and isolation from inherited Git repository variables.

This checks source coverage and dependency closure of the extraction schedule. A landing still
requires the plan's repairs, complete-context extraction, actual reconstructed candidate builds,
exact-SHA qualification, independent reviews and owner answers. An `owner-held` row remains held;
assigning it a destination does not authorize its policy. Historical findings/approval receipts
are carried for reconciliation, never inherited as reviews of reconstructed candidates.

Remaining-example counts exclude BASE examples. Examples have no landed receipts, so every non-BASE example remains outstanding until explicit command progress is implemented; a declaration receipt cannot silently discharge an example. The current initial schedule has 1,745 remaining anonymous examples out of 1,894 total.
