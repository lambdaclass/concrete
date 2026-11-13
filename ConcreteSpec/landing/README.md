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

The check needs four permanent tags (lightweight or annotated) under `refs/tags/spec-inventory/` on `origin`: `source`, `baseline`, `evidence`, and `dossier`. `PIN_TAGS` in `scripts/spec/landing_manifest.py` declares their exact commit SHAs. Every run verifies each local tag target, or verifies and fetches its advertised origin tag when the local tag is absent, using depth-one tag-only fetches without moving `FETCH_HEAD`. Configure repository tag protection to forbid updating or deleting `spec-inventory/**` before publishing them. Wip branches are never input anchors.

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
require the same complete source command. `migration` names a known non-BASE piece and is mandatory only on BASE command/declaration rows; it is empty on every other row. Dispositions use the closed set `retain`, `repair`, `replace`, `owner-held`, `retire`.
No unchanged fragment is claimed to implement its future checked view. Remaining counts exclude
`BASE`; `--remaining all` lists all outstanding authored source units. Update assignments explicitly
only to repair the extraction schedule; checking never fills an omitted row. `--write` regenerates
the initial assignment from the pinned plan and atomically replaces the TSV only after all checks
and controls pass.

Progress is derived once from a fresh census of committed HEAD. Its units are authored
source declarations and source commands with no authored declaration of their own (including
anonymous examples, attributes and context directives). A declaration must select its own
identifier in Lean's parsed source; a generated constant inheriting another declaration's range
is not authored. Compiler-generated equations, recursors and auxiliaries, and synthetic
module-realization phases, never enter the numerator or denominator. Their inventory records
retain dependency/emission information for their owning commands, not separate progress obligations.
A declaration counts exactly when HEAD contains the same qualified name and full digest:
complete command source (statement, proof/body, doc comment and inline attributes), separately
resolved attribute commands, and kernel shape. Other source commands compare their source digest
by the same content-and-sequence matcher used for BASE membership: file, enclosing
namespace/section sequence, command kind and source digest, with an ordinal among identical
commands in that scope. Parsed positions order the sequence but never enter matching identities.
The original positional names remain audit keys. Moving an unchanged example past unrelated
declarations or changing an import header therefore preserves its progress; changing its source does not. A command containing multiple authored declarations is
represented by those declarations, never counted again. Anonymous examples count in their
piece's landed total and stop contributing to remaining-examples when their digest matches.
A doc-comment-only or proof-source change therefore differs. Progress uses the current checked revision, without ancestry requirements, introduction commits
or historical progress builds.
Deleting source removes it from the measurement. Each piece reports landed source units over
its total source units, plus remaining units. `--remaining all` lists those units.

F01 is one property unit, `header:F01`: the root `ConcreteSpec.lean` import closure
must reach every HEAD spec/proof module, and every HEAD proof file (including later additions)
must pass the exact direct-import algorithm in `scripts/proofs/imports.py`. The inventory calls
that same algorithm on freshly built, isolated HEAD sources. An unneeded import unlands F01;
a legitimate new direct import keeps it landed. `headers.json` records the property and the
reviewed F01 revision, which must resolve to an ancestor of HEAD. The Spec CI checkout
retains commit history (`fetch-depth: 0`) for this check and review-reference ancestry;
no historical proof builds are needed. It is provenance, not frozen
header bytes. Headers do not affect declaration/command matching.

`BASE` is the original baseline, never a progress label. Source reconstruction does not confer
owner approval or contract closure. Add a `replacement` record to `inventory.tsv` to reconcile
changed or intentionally retired units, preserving all snapshot rows. Its `name` is a unique
identifier, `piece` is the superseded units' common non-BASE piece, `disposition` is `replace`,
and `note` is JSON with exactly these fields (all other TSV columns empty):

- `supersedes`: nonempty distinct snapshot progress-unit names. Declarations use qualified names;
  commands use `command:` followed by the snapshot audit name, including its original range.
- `action`: `replace` or `retire`. Retirement requires empty `declarations` and `commands`;
  replacement requires at least one target. Both require a reason and accepting review.
- `declarations`: replacing qualified names mapped to 64-character full progress digests.
- `commands`: replacing position-free identities, each an object with `module` (dotted module
  name), `scope` (the ordered enclosing `[kind, digest, ordinal]` triples), `kind`, `digest`,
  and `ordinal` (zero-based occurrence among identical commands in that scope).
- `reason`: nonempty explanation, including why retired material is intentionally omitted.
- `review`: `{ "verdict": "ACCEPT", "reference": { "path": "ConcreteSpec/landing/reviews/ID.txt", "commit": "<40-hex>" } }`.
  The artifact commit must be an ancestor of HEAD. Review records live only under
  `ConcreteSpec/landing/reviews/` with a `.txt` suffix. Each contains exactly one
  `COMMIT: <40-hex>` naming the reviewed revision (an ancestor of the artifact commit),
  a `RECORD: <replacement name>` or a `UNIT: <superseded name>` line for every covered unit,
  and a final `VERDICT: ACCEPT` line. The gate parses these fields from the committed artifact;
  the verdict in the inventory alone is insufficient. The reviewed COMMIT must contain a
  replacement row with the same name, piece, disposition and substantive JSON fields
  (all fields except the later-added review reference). An older review of the name or
  superseded units alone cannot approve a new reconciliation. Notes refs are unsupported.

The judgment refuses unknown or already-superseded units, generated constants, owner-held or
BASE units, cross-piece supersessions, malformed schemas and missing reviews. A target that is
also a snapshot unit must be the superseded declaration itself; independent same-piece units
are refused. Each replacement record supersedes exactly one obligation. Targets cannot be shared
between records.
All target declarations and commands must match together at HEAD, and every superseded unit
must be absent by identity, or itself be a target at the recorded digest; partial
replacement does not close. Declaration identity is its qualified name. Example-command
identity is its Lean-elaborated type closed over the complete local context, plus its
occurrence among equal identities. Explicitly typed and inferred examples use the same
elaboration-info path; proof syntax and source positions never enter this identity. Missing,
ambiguous or unresolved type identities refuse reconciliation. Other commands use parsed syntax plus occurrence. The census lists
every collision group with all occurrence keys. Reconciliation first matches each HEAD
command by its own exact key. A superseded key still at HEAD always refuses, regardless
of whether equal-statement twins have landed. Pinned targets and exact-key twins explain
only their own occurrences. Remaining equal-statement occurrences are assigned only when
there is a single unmatched snapshot key; multiple candidate keys refuse as ambiguous,
rather than using snapshot counts to assume that other pieces have landed. This ambiguity
is reachable when a proof edit changes a command's full key and multiple snapshot twins
lack exact-key matches. Missing snapshot or HEAD identities are refused. Position
slots are available only to synthetic tests with the explicit `fixture_slots=True` flag.
A retained superseded unit outside the pinned targets refuses with `superseded unit still present`.
Reviewed retirement closes only when every superseded unit is absent at HEAD, without
replacement material. Results report `reconciled by
replacement` or `reconciled by retirement`, distinct from `identical full digest`, preserving the
original denominator. Silent edits or deletion cannot close an obligation. This supports F02's
replaced examples and all repair/replace command obligations without restoring obsolete rules.
`--head-digests PATH` exports `declarations` (HEAD full digests) and `headCommandKeys`
(the command objects accepted by replacement records) from the fresh census. Regeneration
preserves the additive reconciliation records.

Before landing, the coordinator creates the following lightweight tags at these exact objects
and pushes each tag to origin. Never force or
retarget a tag; investigate an existing mismatch. For each row, run
`git -c tag.gpgsign=false tag spec-inventory/NAME SHA`, then `git push origin refs/tags/spec-inventory/NAME`. Verify origin with `git ls-remote origin
refs/tags/spec-inventory/NAME`. Annotated tags are also supported and checked by peeled commit.
The gate accepts matching local tags and fetches absent tags with bounded depth from the remote
named by `git config landing.inventoryRemote` (default `origin`). It names every missing remote
tag in its error. The coordinator must push these before landing;
a local pass alone does not publish them.

| Tag suffix | Commit |
|---|---|
| source | aa44140a33747fa37ece59c76090972e564c65b4 |
| baseline | 1a50c64ff1b6f3a2899522f279eccc86d3b004a2 |
| evidence | 5db3018e24eb284894b3615833aa373c76944475 |
| dossier | 2bef2b5e20dffb5b277f9451be6fef969cf6c898 |

`--census-output <path>` exports the fresh snapshot census for inspection, never cached input.

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
The coordinator's law/control direction is checked against current rows and their dependency
closure, with no historical schedule anchor. `law-control-placement.json` covers every
Lean-resolved annotation pair. Laws and their controls co-land; unavailable dependencies force
relocation. `relocations.json` covers every declaration and command relocated from the plan's
assignment, including fixture and auxiliary changes; the read-only gate recomputes and compares
it. Every law-driven move retains a note naming the causal law and control, including cascades.
Each relocation's `reason` is an array of checked objects, never free-form cause text;
`note` retains the source annotation for audit. The closed kinds are `law-control` (the row's
command belongs to the named resolved pair), `law-dependent` (it transitively uses that pair),
`law-support` (the pair transitively uses it), `shared-fixture` (its command is in
`shared-fixtures.json`), `shared-fixture-consumer` (it uses that listed fixture), `regenerated`
(its zero-range declaration has the census-validated observed emission), and `auxiliary-dependent`
(it uses the named auxiliary at that observed emission). Every reference is checked against the
row graph, shared-fixture list and census; a real but unrelated law/control pair is refused too.
`VALIDATION.md` lists every relocation and reports plan deltas for the coordinator without
rewriting the supplied plan.

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
production-census regression controls. Progress controls assert exact reasons for identical source, changed documentation, changed proof source, changed separate attributes and deletion, and check isolation from inherited Git repository variables.

This checks source coverage and dependency closure of the extraction schedule. A landing still
requires the plan's repairs, complete-context extraction, actual reconstructed candidate builds,
exact-SHA qualification, independent reviews and owner answers. An `owner-held` row remains held;
assigning it a destination does not authorize its policy. Historical findings/approval receipts
are carried for reconciliation, never inherited as reviews of reconstructed candidates.

Remaining-example counts exclude BASE examples and measure unmatched anonymous source commands. Generated lemma realization cannot affect source progress; a two-module production-census control checks this together with changed defining source and missing anonymous examples. Pin controls also refuse moved local lightweight tags and retargeted annotated tags.
