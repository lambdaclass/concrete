# Piece P validation

Round 3 status, 2026-11-11: **not validated; ISS-0054 remains open**. Writer base
`5410d4631b9d7f12f862b3c473f0815d3f6c7f53`; uncommitted repairs are not an exact-SHA
qualification. No commits, pushes, review tools or review notes were made by the writer.

`nix develop -c bash scripts/spec/landing_inventory.sh --write` refuses the production
remote because `refs/tags/spec-inventory/source` is absent. All four input tags must be
published and protected by the harness/admin before production validation can pass.
The gate does not fall back to wip refs or unanchored snapshot objects.

A full fresh-census run in a disposable clone with explicitly tagged input fixtures passed
ancestry/snapshot rejection, repaired progress, Git isolation, census instrumentation,
named boundaries, coverage/duplicate/order controls, discriminating identical-text keying,
hold preservation, closed schemas and unknown-piece refusal. Its final dependency judgment
failed: strict law/control co-landing conflicts with existing path-policy holds in F20p/F29p.
The generator now refuses to reschedule those held rows without an explicit plan answer.
The tagged fixture tests tag availability/identity, not GitHub protection configuration.
The working inventory has not been replaced: `--write` writes only after the entire gate passes.
Remaining anonymous examples in the initial schedule: **1,745** (1,894 total; 149 BASE).

The round-2 results below are historical and do not qualify the round-3 changes.


Round 2, 2026-11-11. Source snapshot: `aa44140a33747fa37ece59c76090972e564c65b4`.
Baseline: `1a50c64ff1b6f3a2899522f279eccc86d3b004a2`.

The complete gate exited 0 on `concrete-ci-2`, in an isolated scratch clone:

```sh
nix develop path:<scratch> -c bash scripts/spec/landing_inventory.sh --write \
  --census-output <scratch-census.jsonl>
```

It freshly compiled all 29 baseline modules and all 62 snapshot modules, observed command
elaboration, and separately compiled the snapshot with its examples named. Original builds used
`autoImplicit=false` and refused warnings. The census, baseline verification, observed regeneration
choices, complete dependency judgment and all controls passed. No cached census supplied gate input.

```text
control PASS: repaired/relocated progress preserves source evidence and verifies commit/digest
control PASS: progress Git isolation preserves caller refs
control PASS: reserved-name equation regeneration observed
control PASS: private/noncomputable/unsafe examples; opaque body origins; census isolation
control PASS: discovered import cycle refused
control PASS: named plan boundaries and held fixture destination
control PASS: duplicate assignment refused
control PASS: planted unassigned declaration refused through census discovery
control PASS: premature command assignment refused by dependency order
control PASS: invalid zero-range BASE assignment refused
control PASS: identical-text commands retain separate identities
control PASS: anonymous example premature dependency refused
control PASS: unknown remaining piece refused
landing inventory PASS: source=aa44140a33747fa37ece59c76090972e564c65b4; baseline=1a50c64ff1b6f3a2899522f279eccc86d3b004a2
```

Population: 17,355 persistent declarations; 6,034 parsed source commands plus 28 whole-module
realization phases; 1,894 anonymous examples; 19,340 evidence/issue records; 95 piece records;
ten contract manifests. There are 3,442 verified BASE declarations and 13,913 remaining declarations.
These are extraction counts, not authority, semantic-equivalence or proof-qualification claims.

BASE membership is checked against actual baseline constants and their kernel fingerprints, not
inferred from dependencies or an importer's filename. For example, `ConcreteSpec.Borrow.lookup_restore._f`
exists on the baseline with the identical computational fingerprint and legitimately stays BASE.
A different emitting module does not turn an existing generated constant into unlanded work.
`controlInput` is held in F29p; the named Adequacy boundaries are F24a/F25a, the destructor mutant
is F10, execution examples are F35, and `CExpr.eval.match_5` remains with F06.

The tested six-file code bundle and the copied inventory were hashed on both machines and matched.
The bundle is SHA-256 of sorted `scripts/spec/` paths `landing_census.py`, `landing_controls.py`,
`landing_inventory.lean`, `landing_inventory.sh`, `landing_manifest.py`, `test_landing_progress.py`,
each encoded as UTF-8 path, NUL, file bytes, NUL:

- Code bundle: `c11addb485b2bb1bf462a98692649fb338758f59549e76a4a94cef3356709374`.
- Inventory: `c65a8bc1ee834b6a6ce22acb744bb84b55615ebffcd552607dd601fc28def022`.
- Complete transcript: `7cb3305665de627e8c382999a1c7f52127b43f66c20a4e2e194fd0b9f51b591b`.

Bounded reproducers confirmed the original baseline, boundary, command-collision, example,
progress and gate-placement defects before repair. Additional bounded controls verified wrapper
timeout reporting and completion with inherited Git variables while preserving a disposable
caller's refs. Python/Bash syntax, documentation links, workflow YAML, root inventory and issue
schema checks passed under Nix. No full local suite, review tools, review notes or pushes ran.

The hashes bind the tested writer tree; the harness still owns exact-commit publication and reviews.
