# Foundation landing inventory (piece P)

`inventory.tsv` pins the complete `ConcreteSpec` population of integration snapshot
`aa44140a33747fa37ece59c76090972e564c65b4`, the current-main baseline
`1a50c64ff1b6f3a2899522f279eccc86d3b004a2`, and the supplied [landing plan](plan.md).
This is an extraction inventory, not approval of the historical rules or proofs.

Run on CI/server, inside the repository's Nix shell:

```sh
nix develop -c bash scripts/checks/spec_landing_inventory.sh
nix develop -c bash scripts/checks/spec_landing_inventory.sh --remaining F03
```

The check needs the pinned source, baseline and evidence git objects. A fresh CI checkout fetches
missing commits by exact SHA from `origin`, without moving refs, `FETCH_HEAD` or shallow boundaries.
Unavailable pinned objects are an error; it never substitutes a moving branch. It writes all build artifacts in a private
`mktemp` directory, builds every source module (including orphans) in the parsed import graph's
order, and reads constants, kinds, ranges and term dependencies from Lean. All original sources
build with `autoImplicit=false` and warnings refused. No regex identifies Lean declarations.

Lean discards anonymous examples from the persistent environment. A second disposable build
names each example using its parser-provided keyword byte offset, retaining the complete original
command context, binders, type and proof. Its synthetic private definitions export the example's
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
  disappears because it lacks a persistent constant.
- `finding`: each nonblank line of the pinned evidence dossier and every spec issue in the audit
  issue snapshot. Keeping raw evidence, including rejected findings and invalidations, avoids
  selecting only favorable conclusions. Each retains an exact source address and digest.
  `repair` here requires reconciliation, not acceptance of the historical allegation.
- `contract`: ten separately digested C1–C10 declaration manifests. Select their members using
  the `contract` column; closure also reconciles authority, evidence, provenance and retirement.

A declaration's `piece` is its extraction destination. `BASE` means its complete source command
already exists as a parsed command on the pinned baseline; `migration` names the later checked-view/adaptation work.
No unchanged fragment is claimed to implement its future checked view. Remaining counts exclude
`BASE`; `--remaining all` lists all outstanding declaration names. Update assignments explicitly
as reconstruction progresses; checking never fills an omitted row. `--write` regenerates the
initial assignment from the pinned plan and is intended only for maintaining this inventory.

Ranges describe complete commands, rather than line slices used as implementation sources.
Ranged declarations and their source command must share a piece. The plan's shared fixtures move
with their earliest consumers. Zero-range compiler artifacts receive a dependency-closed
regeneration assignment; their recorded source module remains the one reported by Lean. This is
**not compiler provenance** and does not replace T3's patched-toolchain provenance work. In
particular, cached auxiliary emission in an importing proof module is not evidence that the
proof's author wrote that auxiliary.

The judgment refuses missing/extra census records, duplicate identities, empty/unknown pieces,
changed source digests or dependency lists, split commands, prerequisite cycles, incomplete contract prerequisite closures, and dependencies
on a later/unrelated piece. It checks both declaration terms and elaborated examples. It reports
all pieces before issuing its final dependency verdict. Its controls plant a real compiled
unassigned declaration and a duplicate assignment and require the specific refusal reason.

This checks source coverage and dependency closure of the extraction schedule. A landing still
requires the plan's repairs, complete-context extraction, actual reconstructed candidate builds,
exact-SHA qualification, independent reviews and owner answers. An `owner-held` row remains held;
assigning it a destination does not authorize its policy. Historical findings/approval receipts
are carried for reconciliation, never inherited as reviews of reconstructed candidates.
