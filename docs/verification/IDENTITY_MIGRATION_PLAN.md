# Identity migration — atomic plan (R-0482 queue row 1)

Status: APPROVED 2026-10-13 (P1–P4 accepted as recommended) and IMPLEMENTED. Outcome as predicted in
section 5: 323/323 rows mapped, 0 shared, 0 refused, the normalized freeze byte-identical. Departures
from this plan, each made during implementation: an artifact's module identity is the source path
relative to the entry file (the entry itself rendered `«entry»`); D1 is detected by ARTIFACT, since two
undeclared packages of one name share one scope digest by construction; `IssueEnvironment` no longer
carries a scope at all (receipts take scope and context from the ProofCore); `--report
package-identity` was added so scope, artifact, context and owner scoping are observable; and the
receipt decoder now reads the schema before refusing unknown keys. It applies the ratified definitions in
`IDENTITY_SEPARATION.md` and is judged by the baseline frozen at `61b2a261`:

- `identity-raw-baseline-pre-r0482.tsv`: raw bytes, for the exact map;
- `identity-rows-pre-r0482.tsv`: normalized associations.

Inventory is at `78d19e30`, with file:line references.

## 1. Shape of the change

One published commit replaces the old scheme everywhere. The pieces that change together:

- the identity types and their producers;
- every artifact that stores the bytes (generated references, the classification table, receipts,
  reports);
- every consumer that selects or compares them;
- the gates that pin them.

Development happens on a branch in reviewable steps (section 7). Main receives one squashed,
validated commit, so no published revision mixes `defIdV1` and `defIdV2` (no mixed-version
fallback).

## 2. Target types (`Concrete/Proof/DefinitionIdentity.lean`)

```text
inductive ScopeOrigin
  | declared (namespace : String)              -- manifest `origin`; a namespace CLAIM
  | undeclared                                 -- manifest without `origin`
  | standalone (artifact : PackageArtifactIdentity)
  | toolchain (component : String)             -- bundled std only

structure PackageArtifactIdentity   -- private mk; of? : List (moduleId × source) → Except …
  modules : List (String × String)  -- sorted (module identity, content digest), never deduplicated

structure PackageScopeIdentity      -- private mk; produced only by the resolver
  origin : ScopeOrigin
  name   : String                   -- declared name; «standalone» rendering for .standalone

structure ResolutionContextIdentity -- private mk
  deps : List (String × String × String)  -- sorted (binding name, scope digest, artifact digest)

structure DefinitionIdentity        -- unchanged shape, new meaning
  scope : String                    -- PackageScopeIdentity.digest (was: over-bound package digest)
  moduleIdentity declarationIdentity implementationIdentity : String
```

**Canonical renderings and digests:**
- `pkgScopeV1`: the origin tag `D`/`U`/`S`/`T` plus a length-prefixed payload, then the name.
- `pkgArtifactV1`: length-prefixed pairs.
- `resolutionCtxV1`: the sorted dependency triples.
- `defIdV2`, with `S<len>:<scope digest>` in place of `P`.

**Deleted, not deprecated:** `PackageIdentity` and its three producers (`packageIdentityOf`,
`synthetic`, `syntheticForModules`), together with the `pkgIdV1`, `pkgOriginV1`, `pkgRootV1` and
`pkgSyntheticV1` encodings. A `defIdV1` value anywhere is `needs_recheck`.

**Refusals added:**
- `incompleteInventory`: a module without its source, or a source without its module;
- `duplicateModule`;
- `ambiguousUndeclared`: two `undeclared` scopes with the same name at one boundary;
- `reservedOrigin`: anything but the resolver asking for `.toolchain`.

## 3. Producers (each replaced in the same commit)

| Site | Today | After |
|---|---|---|
| `Project.lean:379` dependency with manifest | `packageIdentityOf(dep toml, mods, [], srcs)` | scope from dependency name and origin; artifact from its OWN paired modules |
| `Project.lean:373` dependency without manifest / refused | `syntheticForModules` or an `unidentified:<name>` key | `.standalone (artifact)`; `unidentified:` keys are removed, and an unidentifiable dependency refuses the load |
| `Project.lean:493` root with manifest | `packageIdentityOf(root toml, ALL modules incl. deps+std, all dep names, ALL sources)` | scope from root name and origin; artifact from root's own modules; context from the dependency triples |
| `Project.lean:498` fallback root key | `syntheticForModules(root)` | deleted; root identity is the same producer |
| `Project.lean:509` `extractProofCore? validCore packageIdentity` | **every definition, dependencies and std included, scoped under the ROOT package** | each definition scoped by its OWNING package's scope via `moduleOwners` (`:385`); std definitions under the std scope |
| `Project.lean:353` std auto-injection | by name, unless `selfName == "std"` or a dependency is named `std` | the bundled std gets `.toolchain "std"`; a user package or dependency named `std` gets an ordinary origin and never the reserved one |
| `Project.lean:132` `parseDependencies` | a duplicate dependency name is appended silently | duplicate binding name refuses |
| `Main.lean:478–512` `resolvePackageIdentity` | manifest identity or synthetic over the caller's source map | resolver scope or `.standalone` from the ONE manifestless producer (D6) |
| `Main.lean:3343`, `:3834` | synthetic over the invoked file only | the one producer over the complete resolved inventory; an incomplete inventory refuses |
| `Main.lean:1592` | synthetic over the full map | the one producer |
| `DebugBundle.lean:282`, `Reduce.lean:144` | synthetic over module NAMES only | stop deriving identity (D6); take it from the producer or print `unidentified` explicitly as debug text |
| `ProofCore.lean:2833` `DefinitionIdentity.of?` | package digest | scope digest of the owning package |
| `ProofCore.lean:3369`, `Main.lean:1599`, `:3317` assumption package key | package digest or `unidentified:` | scope digest |
| `TableResolve.lean:194` | re-mints external rows with a package digest | re-mints with a scope digest; an unknown scope refuses |
| `scripts/gen/classifications.lean:169` | writes `packageIdentity` into scoped rows | writes the scope digest |

## 4. Stored bytes and consumers

**Generated and checked-in:**
- **`Concrete/Proof/GeneratedAttestations.lean`:** 63 references regenerated by
  `attestation_refs.sh`. Symbols keep `<table>_<scope8>_<decl>` (D5). The generator refuses two scopes
  sharing an 8-hex prefix and two distinct full keys rendering one symbol.
- **`Concrete/Proof/ClassificationTable.lean:237–238`:** 20 package digests (4 distinct), regenerated by
  `refresh_classifications.sh`.
- **Selection sites (44) rewritten from the map, not by hand:**
  - `Concrete/Proof/Proof.lean`: 24 selections, 25 symbol names;
  - `proofs/Examples/HmacSha256/Proofs.lean`: 15;
  - `proofs/Examples/ProofPatterns/Proofs.lean`: 5.
- **Literals in gates:**
  - `check_gate_mutation_coverage.sh`: 20 symbol anchors (`:1189–1224`) and the `pkgSyntheticV1`
    anchor (`:1101`), which is replaced by a family on the one manifestless producer;
  - `check_attestation_manifest.sh:238`: the exclusion digest;
  - `check_dependency_edges.sh:3151`: a hard-coded package digest.

  These are migrated in the same commit, and the campaign golden is regenerated.

**Receipts** (none checked in; issued into temp directories by gates):
- **`workspace`** (`Main.lean:2736`, `:2958`; `Receipt.lean:238–292`, `:359`, `:555`, `:594`)
  carries the over-bound package digest. It is replaced by two typed fields:
  - `scope`: the claim subject's `PackageScopeIdentity` digest;
  - `context`: `ResolutionContextIdentity`.
- **`root`** preimage moves to `depRootPreimageV3` (nodes and edges render `defIdV2`).
- **Schema:** `receiptV3`. V2 stores are read as written under another envelope: not current, and the
  report names what moved, like V1 today.
- **Root vs context in the report:** a receipt whose ROOT matches but whose CONTEXT differs reports
  "context changed — re-validate under policy". A ROOT change reports "the claim's dependency closure
  changed — re-establish". They are distinct lines, and neither reads as the other (section 6, control
  R).

**Reports:**
- `--report attestation-join` (`Report.lean:2296`, `:2322`): the package column carries the scope
  digest; the header names it `scope`.
- `--report generated-implementations` (`:2366`).
- `--report assumptions` JSON (`AssumptionSummary.lean:705–707`): `package`, plus
  `package_identity_ambiguous`, which becomes the D1 refusal rather than a flag.
- Shadow displays (`:1957`, `:2524`).

**Comparators:** these keep their logic and change only through the field's meaning:
- `sameDefinition` users: `DependencyRoot:120`, `Correspondence:162,177`, `DependencyEdge:295–305`,
  `ProofCore:2373–2386`, `Issue:238,290`;
- `AttestedPFnDef` (`Proof.lean:427,484,517`).

**D1 boundaries:** the refusal of ambiguous same-named undeclared packages applies at:
- the resolver: one resolved graph containing two `undeclared` scopes of one name;
- the attestation surface: two such scopes in one `GeneratedAttestations`;
- the table join (`TableResolve:133,190`);
- the receipt join (`Main.lean:2786`).

In today's corpus the 7 manifestful proof fixtures have distinct names. The 4 `app` and 2
`declared_ok` test packages never share one resolution, so no current fixture is refused; controls D
add the refusing cases.

## 5. The old-to-new map

`scripts/tools/identity_migration_map.sh` is new. Its inputs are the raw baseline and the migrated
tree's `identity_rows.sh --raw`. It pairs rows by CONTENT KEY, never by digest:

| Row kind | Pairing key |
|---|---|
| subject | (fixture label, module, decl, implementation, theorem, tables) |
| dependency | (fixture label, consumer name, table, callee module, decl, implementation) |
| reference | (table prefix, fixture label, module, decl, implementation) |
| selection | (file, holding declaration, model) → its reference's key |
| link | (file, fn, fingerprint), which must be byte-unchanged |

**Output:** one typed row per baseline row.
- `mapped <kind> <old digest/symbol> -> <new digest/symbol>`.
- `shared <kind> <old…> -> <new>` when distinct old rows legitimately become one new row. No such case
  is expected in this corpus: scopes stay distinct, the composition trio is three standalone scopes,
  and the two `check_nonce` twins are two scopes. Any `shared` row therefore needs a recorded
  justification.
- `refused <kind> <old…> <reason>` when a row has no counterpart.

**Refusal conditions:**
- any baseline row absent from the map (totality);
- a new row with no baseline row, unless it is a declared addition;
- a pairing key matching more than one row on either side (ambiguity, never guessed: the `check_nonce`
  twins are separated by fixture label, which is how the F6 migration had to pair them);
- any implementation digest that differs between paired rows.

**Expected outcome, stated in advance so a surprise is visible:**
- **All 323 normalized rows unchanged.** Labels replace digests, implementation digests do not include
  the package, symbol templates keep their shape, and links are untouched. So
  `identity-rows-pre-r0482.tsv` stays byte-identical across the migration, and its gate keeps passing
  unchanged.
- **Every package, definition and symbol byte changes once:** 24 scopes, 63 references, 86 + 41
  definition digests.
- **0 `shared`, 0 `refused`.**
- **The fixture ↔ scope bijection still holds** in this corpus. It is not required; if it fails, the
  map, not the normalizer, is authoritative.

The map is committed beside the baselines (`identity-migration-map-r0482.tsv`) and pinned by the gate
in the migration commit.

## 6. Controls that must pass on the migration commit

| Id | Control | Form |
|---|---|---|
| B′ | unused std edit | renames **no** manifestful symbol and moves no scope (the `check_identity_rows.sh` control B rewritten; today it renames 42) |
| O1 | `check_identity_overbinding.sh` control 1 | std edit moves 0 of 7 manifestful scopes (flips) |
| O2 | control 2 | sibling edit in `proof_pressure` (manifestful) moves its ARTIFACT, not its scope; `check_nonce`'s symbol unchanged (flips) |
| O2s | standalone exception | a sibling edit in a manifestless fixture moves its standalone scope; a dependency-only edit does not |
| O3 | own body edit | unchanged: moves the implementation digest |
| S | reachable-edit sensitivity | editing a definition a claim reaches moves exactly that claim's root, and not the roots of claims that don't reach it. Demonstrated in-package (`crypto_verify`: `verify_message` reaches `check_nonce`) and across a dependency package (a test project whose claim calls a dependency function) |
| R | context vs root | editing an UNREACHED function of a dependency moves the dependency's artifact and the dependent's context, but no claim root. The receipt reads "context changed — re-validate", not "re-establish" |
| G | origin sensitivity | changing a declared `origin` moves every definition scoped by it (the deferred overbinding row, now representable) |
| T | tagged-origin impersonation | a manifest `origin = "«undeclared»"`, one `origin = "concrete-toolchain"`, and a user package named `std` each get scopes distinct from the variants they imitate |
| D | D1 refusals | two undeclared same-named packages in one resolution refuse; the same in one attestation surface refuses; declared-origin and distinct-name cases pass |
| C5 | D5 collisions | a forced 8-hex scope-prefix collision refuses generation; two full keys rendering one symbol refuse |
| C6 | D6 inventory | a missing module source refuses identity production; the debug paths derive none |
| M | the map | total, 0 unexplained `shared`/`refused`, implementations unchanged; the normalized freeze byte-identical |
| V | old bytes | a `defIdV1`/receiptV2 artifact reads `needs_recheck` / not current; no fallback |

**Re-pinned in the same commit:**
- `check_identity_freeze.sh`: populations are expected unchanged except package counts. 21 packages
  stay 21 scopes; GAP 8 stays 8 because sharing is per-scope.
- `check_attestation_manifest.sh` (accounting and exclusion), `check_attestation_refs_order.sh`,
  `check_classification_freshness.sh`, the receipt gates, `check_dependency_edges.sh`'s synthetic
  probes, and `run_ci_gates_local.sh`.

**New mutation families,** one per new production decision, then a full campaign on the published
commit:
- tag rendering;
- reserved origin;
- the D1 refusal;
- the artifact pairing;
- the owner-scoped extraction at `:509`;
- context-vs-root reporting;
- the map's totality.

## 7. Branch sequence (each step builds; only the tip is published)

1. Types and canonical forms beside the old ones, with unit probes for tags, refusals and canonical
   bytes. No producer uses them yet.
2. Producers: the resolver (`Project.lean`), the one manifestless producer, owner-scoped extraction.
   The tree is red from here until step 4.
3. Generators: regenerate the attestations and the classification table; rewrite selections and gate
   anchors from the map tool's output.
4. Receipts (`receiptV3`, root V3, scope/context), reports, and the context-vs-root report lines.
5. Gates: B′, O1/O2/O2s, S, R, G, T, D, C5, C6, M, V; re-pins; delete `PackageIdentity`.
6. Map committed; the full trust gate, the full local CI gate set and the campaign anchors; squash.
   Then publish with CI and a full campaign on that exact commit.

## 8. Decisions (accepted 2026-10-13, all as recommended)

- **P1 — std nodes in claim roots.**
  - **Measured:** no current claim outside std reaches a std definition. Std callees are refused as
    `unscopedEdge` because report-path ProofCores contain only user modules (`Main.lean:1574`).
  - **Recommended:** the migration fixes the scoping (`:509`: std definitions under the std scope) but
    leaves claim closures that reach std fail-closed, as they are today. Admitting std nodes into report
    roots then becomes its own slice, with a fixture whose claim calls std and the reachable-std control.
  - **Alternative:** include it in the migration, which enlarges the atomic commit.
  - **Closure record for control S.** The migration (`ab01f856`) demonstrated S **in-package only**;
    the cross-package half required by section 7 was NOT run at closure, because P1 kept such claims
    fail-closed and there was no cross-package root to move. It was carried as an open obligation of
    the P1 slice, not counted as a passed control.
  - **P1 slice (2026-10-17, manifestful projects only).** The report path reads the loader's
    whole-program ProofCore through a scoped view (`ProofCore.scopeToUser` keeps the program's
    populations; `closureView` is what closures, correspondence and edge typing read), and the call graph
    resolves a cross-package callee through the program's linker aliases — only when the spelling has
    exactly one target defined exactly once. `check_identity_separation.sh` X1–X5 now carry the
    deferred cross-package S control with a manifestful (declared-origin) dependency, the context-only
    control, the D3 manifestless control (an unreached edit there DOES move its callers' roots, since its
    scope is its artifact), std reached/unreached, a closure of only trusted std methods (roots, carries
    trust, still not proved — bug 079), and the refusals: missing evidence, an unsupported dependency
    body, an unjustified edge. Standalone compilation still injects no std and is unchanged; adding std
    imports there needs std recorded as its own scope/artifact in the resolution context first.
- **P2 — receipt fields.**
  - **Recommended:** `workspace` → `scope` + `context` under `receiptV3`, as in section 4.
- **P3 — publication.**
  - **Recommended:** squash the branch to one commit on main. Intermediate red states are never
    published, and the commit is the migration.
- **P4 — acceptance strength.**
  - **Recommended:** require the normalized freeze to stay byte-identical (the expectation in section
    5). Any difference is explained in the map or the migration is wrong.

## 9. Risks

- **Owner-scoped extraction at `:509`** changes which scope a dependency's definitions carry inside a
  project ProofCore. Every consumer of `ctx.pc` (`Main.lean:3360,3425,3686`) must agree, and the
  assumption summary keys too.
- **The receipt schema bump** touches the R-0208 machinery (toolchain binding, policy). Its gates must
  pass unchanged apart from the envelope version.
- **A large, atomic commit:** the campaign anchors, classification table and golden move together.
  The mutation inventory grows by roughly 7 families. Budget one full campaign.
