# Specification landing inventory

Run from the repository root with `nix develop -c bash scripts/spec/landing_inventory.sh`.
The Spec build job runs the same gate. It builds the checkout and loads its elaborated
`ConcreteSpec` and `Concrete` roots. Compiler evidence is used for F30. On main the report describes main. On a candidate it describes
what that candidate would land.

The inventory checks the structure of landing evidence, and two independent reviewers judge whether it covers the piece’s required behavior.

The [Lean plan](../../scripts/spec/landing_inventory.lean) owns the piece list and its evidence.
Its original scope reconciles the 51 IDs in the `F01`–`F40` foundation and migration tables of
`origin/wip/spec-inventory:ConcreteSpec/landing/plan.md`, including the split IDs and CAP.
The headline includes the original rows and the three recorded additions. The report also
shows active scope and keeps the historical rows visible for reconciliation. For F03p-design
and F39-design, design proposals never land on main and stay on WIP branches as writer briefs.
The Lean spec carries the normative decisions for F03p and F39.
[Owner decision on design proposals](../../docs/decisions.md#decided-2026-11-17-proposals-in-lean)

For F27, wiping is out of the spec and only the owner can reopen it. Secret, no-leak and
constant-time stay.
[Owner decision on wiping](../../docs/decisions.md#decided-2026-11-16-secret-wiping)

| Addition | Placement and record |
| --- | --- |
| R | Rule registry and rule examples, a prerequisite of later registered rules under ROADMAP R-0615. [Registry landing](../../changelog.d/2026-11-13-spec-rule-registry.md) |
| F03b | F03 follow-on for the nominal byte carrier and round trips. The new zero-offset Laws and their controls are here; F04 owns the preceding byte-coordinate Laws. [SourceText landing](../../changelog.d/2026-11-17-spec-f03b-sourcetext.md) |
| F26a | Independently landed C7 accounting slice of F26 under ROADMAP R-0613. F26 retains leakage and release obligations. [Accounting landing](../../changelog.d/2026-11-15-spec-f26a-accounting.md) |
| M01–M12 | The separate theorem-qualification table in the historical plan. The [proof-label convention](../../docs/decisions.md#decided-2026-11-13-proof-labels) and [ROADMAP](../../ROADMAP.md) R-0700 govern its completion. |

Pending rows cite the repository records that own their open obligations. The foundation
contracts C1–C10 are ROADMAP R-0607 through R-0616. ROADMAP R-0571 owns integration
and interaction proofs across those contracts. The inventory measures landed slices of that
work. Contract closure and the freeze have their own criteria.

A piece counts as landed when its completion evidence passes and it has no remaining
obligations or historical disposition. The Lean plan defines completion independently of
status. Spec-only pieces with separate proof pieces must resolve their specific named spec
declarations when they claim landing. Other reservations require listed, authored evidence
from each reserved module and namespace. Each proof reservation also requires a cited Law theorem
and a separate planted control in its own module and namespace. M09 uses its existing named
Law and still requires a control from its qualification reservation. F03p checks every expected
source-path rule for its defining module, unique registration, positive revision, and linked
acceptance and refusal theorems from Spec.Rules. Each registry reservation names the expected
rule-defining module and requires cited acceptance and refusal theorems for the same uniquely
registered rule authored in that module. The local rules contribute partial evidence while project-wide admission
and its registry evidence are outstanding. Shared modules have piece-specific evidence. Migration namespaces
reserve names for evidence produced by the migration. If a name or defining module changes, update the completion requirements in
the same patch. Partial rows retain the references for their remaining work.

Lean selects authored theorems with a `Law` doc tag from the spec root’s import closure, including `Law:`
and tags followed by whitespace. A missing or unassigned Law fails the gate. Instances,
Planted controls, Helpers, and Representation lemmas can supply named evidence without
counting as Laws. Existing build and proof checks still qualify proof validity and coverage.
The inventory checks root reachability independently of the rows. The existing
`scripts/checks/spec_imports.sh` owns the Basics direct-import rule.

Built-in controls reject deleted rows, empty evidence, absent or renamed declarations,
duplicate IDs and names, wrong or missing modules, generated declarations, an unassigned or mistagged Law,
a Law-tagged definition, borrowed declarations from shared or pre-existing modules,
an output reservation with only a helper, missing source-path registry entries and examples,
a cited registry example pair for a rule defined in a different module,
overlapping reservations, uncited declarations in reserved namespaces, and an orphan module
with an empty plan. A reduced spec root also checks that compiler imports cannot make a spec
module or its evidence eligible.

| Old feature | Disposition and reason |
| --- | --- |
| Pinned snapshots, tags, and fingerprints | The checkout and the Lean plan are the inputs to the presence check. |
| Retirement and replacement receipts | Historical dispositions cite the owning record. Git records reviewed scope and identity changes. |
| Relocation manifests and scheduling logic | The plan records the defining module for each name. ROADMAP owns sequencing and closure. |
| Source, dependency, and toolchain drift checks | The inventory checks names, modules and Law tags. Build and proof checks qualify implementation changes. |
| Command records, generated auxiliaries, and anonymous-example instrumentation | Use named declarations and the root import closure. |
| Per-piece remaining lists | Lean rows retain references to the records that own outstanding work. |
