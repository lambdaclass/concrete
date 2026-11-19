# Specification landing inventory

Run from the repository root with `nix develop -c bash scripts/spec/landing_inventory.sh`.
The Spec build job runs the same gate. It builds the current checkout and loads the resulting
`ConcreteSpec` environment. On a main checkout the result describes main. On a candidate it
describes what the candidate would add.

The [Lean plan](../../scripts/spec/landing_inventory.lean) is the inventory's only plan. It
covers the foundation and migration pieces, including the shared capability base (CAP), rule
registry (R), and the separately landed SourceText and accounting slices (F03b and F26a).
Toolchain, review, owner decisions outside those pieces, and contract closure have their own
qualification procedures. The old extraction branch, `origin/wip/spec-inventory`, is
historical reference.

Each piece has a scope, expected evidence grouped by defining module, and any remaining work.
A piece counts as landed when its evidence passes and its remaining list is empty. A missing
expected declaration, a wrong defining module, or a missing expected Law tag fails the gate.
F01 also checks the root's import closure against the spec files and checks the direct Basics
dependency. The existing spec import check still covers the repository-wide import policy.

The initial plan records all currently tagged Laws and names representative model definitions,
entry points, and controls. Its count measures landing milestones. Proof validity, coverage,
and approval still belong to the existing spec and review gates. A new declaration with a
`Law` doc tag is reported by name and module until it is assigned. Helpers are inventoried when
a piece explicitly names them. Use stable, public, named declarations for any example or control
that counts as landing evidence. Existing anonymous examples still elaborate as build checks.
Give one a name when adding it as evidence.

When landing a piece, record its actual names and modules alongside the implementation and
remove completed items from its remaining list. For a partial landing, retain the outstanding
work in that row or give the independently landed slice its own row. For a rename or relocation,
change the corresponding name or module in the same patch. The gate runs planted controls for
a missing declaration, an additional Law, and a renamed declaration with both the stale and
updated plan.

| Old feature | Disposition and reason |
| --- | --- |
| Pinned snapshots, tags, and fingerprints | Dropped. The checkout and the Lean plan supply the two inputs to the presence check. |
| Retirement and replacement receipts | Dropped. A reviewed plan edit records changed scope or identity alongside the source change, with ordinary Git history preserving the previous obligation. |
| Relocation manifests and scheduling logic | Dropped. The plan records the defining module for each name. Schedules and dependency closure are outside the presence check. |
| Source, dependency, and toolchain drift checks | Dropped. Name, defining module, and Law tag are the stable obligations this inventory measures. Build and proof checks qualify changes to the implementations. |
| Command records, generated auxiliaries, and anonymous-example instrumentation | Dropped. Named declarations and the root import property supply observable evidence without source instrumentation. |
| Per-piece remaining lists | Kept as Lean data. They report outstanding scope, including work whose eventual declaration names are still unsettled. |
