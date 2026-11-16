# ISS-0047: site table obligations

Design only. Owner authorized both redesigns on 2026-11-15 (“yes do all this”).
Goals: total proofs and informative refusals. Follow [PHILOSOPHY.md](../PHILOSOPHY.md).
Anchors resolve at `origin/wip/spec-integration-head`,
`aa44140a33747fa37ece59c76090972e564c65b4`; proposed laws below have not been run in Lean.

## One decidable invariant

Introduce the sole whole-table coordinate predicate in `Spec/Observe`:

```lean
def SiteTable.EntriesValid (sources : SourceTable) (sites : SiteTable) : Prop :=
  ∀ entry ∈ sites,
    match AList.lookup sources entry.2.path with
    | none => False
    | some bytes => entry.2.LocationOk bytes
```

Supply its constructive `Decidable` instance by bounded list traversal and existing decidability
of `Span.LocationOk`. No unbounded existential search or classical instance is needed.
`ProjectSources.Realizes` reuses this predicate as its final conjunct, retaining exact file
spellings/bytes, exact sites, file-key Nodup and site-key Nodup separately. Duplicate-good entries
satisfy EntriesValid but cannot be admitted. Source lookup keeps its current raw-table behavior.

Delete `SiteTable.Valid` and the proposed self-lookup filter view and compatibility aliases.
Migrate callers to EntriesValid; lookup-visible validity is strictly weaker on shadowed raw input,
so this migration is deliberate, not an unconditional equivalence claim. Preserve trap resolution
and byte-location conclusions by deriving membership from lookup and using EntriesValid.
`Span.PathOk` follows structured paths; remove it as a redundant hypothesis in new table laws,
while preserving existing externally useful PathOk conclusions.
Derive the proposed `ExecAdapter.source_entries_valid` from `A.loaded` and
`LoadedProject.load_admitted`; do not add a proof field or an unused `A.WellFormed` premise.
Keep event/request fields of `ExecAdapter.WellFormed` unchanged. Retire the duplicate
`ExecAdapter.sites_valid` view after migrating callers; no extra WellFormed alias is needed.

## Laws and premise-preserving controls

Laws quantify over the exact executable subject being tested, with identical inputs and premises
for the implementation and mutant. Closed cases are constructive proof obligations.

* `F sources sites = none ↔ SiteTable.EntriesValid sources sites`, with
  `F : SourceTable → SiteTable → Option SourceLoadRefusal`, instantiated by
  `LoadedProject.findSiteIssue`. Success/refusal soundness additionally decomposes the list
  into an all-valid prefix, the first invalid entry, and a suffix; the returned payload names
  that exact occurrence and distinguishes missing bytes from invalid coordinates.
* Closed source bytes `[]`, valid point span `good` at offset 0, line/column 1;
  `bad` differs only in line 2. On `[(⟨0⟩,good),(⟨0⟩,bad)]`, EntriesValid is false.
  A lookup-only issue finder sees only good and returns none, violating correspondence.
  An offset-only/ignore-coordinates finder returns none on `[(⟨0⟩,bad)]` and also fails.
  A finder that rejects duplicate-good entries fails correspondence in the opposite direction.
* Preserve the full `LoadedProject.load_admitted` biconditional with factored Realizes,
  rejection iff no realization, exact preservation, uniqueness, admission reorder laws,
  diagnostic naming and trap-resolution conclusions. Do not weaken to success-only soundness.
  Test a loader that checks lookup-visible validity and drops site Nodup on the shadowed
  good/bad table: it admits a project for which Realizes and EntriesValid are false.
  A coordinate-ignoring loader fails on the bad singleton; a keep-first loader fails exact
  preservation/uniqueness on duplicate-good input. These are executable admission controls,
  not fabricated invalid adapters retaining an impossible `loaded` proof.
* Adapter EntriesValid is an admission corollary without WellFormed premises. Old raw-table
  lookup-visible examples are restated explicitly as shadowing counterexamples, not silently
  strengthened or supplied inconsistent Nodup premises. Reprove all affected trap/report laws.

## Diagnostic boundary

Each refusal must project to exactly one registered rule and one primary precise input location.
Related locations may supplement that primary. Add located input occurrences: stable occurrence
indices plus byte ranges into actual input/manifest bytes, with line/column derived from bytes.
Generate provenance during total parsing, before checking candidate paths or site coordinates;
prove ranges bounded and occurrences tied to the decoded records. Externally supplied metadata
is located at its actual serialized declaration. In-memory fixtures must supply such a located
input too; a raw SiteId is not a source location. This changes the boundary explicitly.

Never validate diagnostic provenance using the candidate span being rejected. Malformed path,
missing-file and out-of-range/incorrect-coordinate errors point to the relevant input declaration,
not to fictitious bytes in a missing file. Refusals retain raw input and parser/component byte
ranges, including repeated components, rather than reconstructing an occurrence from string equality.

`duplicateSite` retains both complete `(SiteId × Span)` entries and their distinct occurrences;
`duplicateFile` retains both raw file entries and their parsed common path. Keep existing outer
priority: first malformed file, file collision, site collision, then first site issue.
Preserve the existing first-colliding-key scan; choose its earliest later matching occurrence,
make that later declaration primary, and retain the earlier occurrence as related evidence.
Equal entries remain distinct occurrences. Prove the selection instead of relying on membership.

Change authoritative `Frame.assembleProject` to an informative result preserving the exact
`SourceLoadRefusal`; build only on admission. Any retained Option interface is a proved erasure,
and diagnostic consumers use the informative result. Preserve source-launcher and build relations
through this erasure; source admission never becomes a runtime status or startup refusal.

Prove for every refusal: the selected reason is true; earlier checks pass; the payload identifies
the selected occurrence(s); its single rule denotes that failing check; its single primary
location is the specified occurrence in actual bytes. Prove assembly preserves the refusal
payload and diagnostic projection exactly. Existing `SourceLoadRefusal.Names` is only a corollary.
Controls: swap rule/reason, replace the primary with another existing valid input location,
lose either colliding entry, replace an occurrence with an equal-valued duplicate, trust malformed
candidate coordinates, and erase the assembly refusal. Each must fail its corresponding law at
unchanged inputs. Membership-only and valid-location-only laws cannot catch these substitutions.

## Proof boundary and owner dependencies

Use total finite parsing, traversals, decidable equality and constructive induction. Prove all
new rules and controls without new axioms, held assumptions or classical choice. Located inputs
are constructed and checked, not an assumed provenance oracle. Existing runtime/native assumptions
are neither strengthened nor evidence for these source-boundary laws.

ISS-0068 is an explicit unresolved dependency: `SourcePath.ComponentOk` currently embeds AY-2/3/4
backslash, control-character and Unicode policies. Laws here refer to the current definitions;
they neither approve, extend nor revert those policies. Parser/provenance migration must preserve
the conflict until the owner answers. Do not declare this design dependency-free.
Rule registration is separately pending: `Proofs/Observe2.lean:722–729` records the owner's open
choice about registering the loader. Present a check-to-rule mapping for approval, including
whether loader refusals use the loader rule or approved per-check rules; do not decide or register
it here. Exactly-one-rule diagnostic completion is blocked on that answer, not waived.
ISS-0048 (startup refusal state) and ISS-0073 (startup/ABI) remain open and untouched.
`launchObservation`'s erased missing-grant provenance and unregistered-site `trapTerminal` /
`ExecAdapter.terminal?` / `reportObservation` failures remain adjacent findings: ISS-XXXX,
coordinator to allocate, owned by startup/report diagnostic boundaries. Closure requires typed
rule/location refusals, preservation laws and payload-loss mutants there; these proposals do
not claim to settle startup policy or complete all compiler diagnostics.

## Resolved names and anchored migration evidence

All paths below are under `ConcreteSpec/` at the pinned ref. Mechanical source-name checks found:

* `Spec/Observe.lean:167,180,192,199,205,217,227,237,251,260`:
  `SiteTable.Valid`, `SourceLoadRefusal` (all five cited constructors), `LoadedProject`,
  `SourceLoadResult`, `LoadedProject.parseFiles`, `LoadedProject.siteIssue`,
  `LoadedProject.findSiteIssue`, `LoadedProject.load`, `ProjectSources.Realizes`,
  `SourceLoadRefusal.Names` exist. `firstDuplicate` is at :212.
* `Spec/Observe.lean:318,350,454,461,576,593,618,639`:
  `trapTerminal`, `launch`, `ExecAdapter`, its `loaded` field, `ExecAdapter.WellFormed`,
  `ExecAdapter.terminal?`, `reportObservation`, `launchObservation` exist.
  `A.loaded`, `A.state`, `A.WellFormed`, `S.initState`, `input.files`, `requested.site`
  and `e.context.obligation.site` are field/local projections, not missing global names.
* `Proofs/Observe2.lean:80,95,160,245,1078`: `LoadedProject.findSiteIssue_none`,
  `LoadedProject.load_admitted`, `LoadedProject.load_locations`,
  `LoadedProject.load_refusal_names`, `ExecAdapter.sites_valid` exist.
  `Spec/Ident.lean:134,143,147,725,754,812,823`: `LoadCollision`, its `first`/`second`,
  `SourcePath.ComponentOk`, `SourcePath.Refusal`, `Span.LocationOk`, `Span.PathOk` exist.
* `Spec/Frame.lean:721,868,907`: `Frame.KeptPair`, `Startup.refused125` (constructor),
  `Frame.assembleProject` exist (the latter two resolve under `ConcreteSpec.Core.Frame.ABI`); `Proofs/Frame.lean` contains `startup125_admitted` and
  `LoadBoundaryDemo`. Observe's `Demo.signature` and `Environment.adapter` also exist.
* Missing: `load_realizes` has no declaration; use `LoadedProject.load_admitted`.
  `SiteTable.EntriesValid`, `ExecAdapter.source_entries_valid`,
  `WellFormed.source_entries_valid`, `lookup_eq_some_iff`, `lookup_eq_none_iff`,
  `reverseLookup`, `headOnlyLookup`, `alwaysNoneLookup`, `lookupValidOnly`, `ignoreCoords`
  are proposed names, not existing declarations. Do not cite them as implemented evidence.
  Lean library names `List.find?`, `List.filter`, `Iff.rfl`, `Classical.choice`,
  `Option.none`, `DecidableEq`, `LawfulBEq` resolve by `#check` under `nix develop`
  (Lean 4.34.1, `import Init`); they are external to the tracked spec;
  `Law`, `Helper`, `Representation lemma`, `Planted` are proof labels, not declarations.

Migrate Spec/Observe, Proofs/Observe2, Proofs/ObserveReport and Frame spec/proofs and demos.
Reproduce affected uses with `git grep -n -E
'SiteTable\.Valid|ProjectSources\.Realizes|LoadedProject\.(findSiteIssue|load)|ExecAdapter\.sites_valid|SourceLoadRefusal|assembleProject'
<ref> -- ConcreteSpec`. Inspect namespace-local calls in the anchored declarations too.
Unanchored `WellFormed`/`loaded` inventories and unrelated English matches are removed.
