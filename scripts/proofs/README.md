# Proof checks and axiom-check design

Status: current toolchain documentation (2026-11-11) and axiom-check design
(2026-11-09). The axiom-check design below does not change executable behavior.
The current P/A/G contract remains documented
in the header of `axioms.sh`, including its full per-theorem golden comparison.

The governing owner decisions are 2026-11-08, “yes to 6 things”, item 2 (the
axioms golden becomes a property check over standard axioms, with only named
theorems pinned), and 2026-11-09 (exact proof-axiom provenance comes from the
compiler). Compiler provenance is decided, not an open policy question. A one-off
manual review of the 27 unexplained golden lines cannot replace it.

## Target contract

- **P: complete population.** Retain the import-closure and source-module coverage
  checks. Enumerate every constant in the ConcreteSpec population, of every kind,
  including private declarations and every compiler-generated helper. The source
  theorem-module check supplements environment enumeration; source text and name
  patterns never select the population.
- **A: standard axioms only.** Compute transitive axioms for every constant in that
  population and require a subset of `Classical.choice`, `Quot.sound`, and
  `propext`. Check generated helpers independently, including equations not reached
  by their generating declaration and helpers generated for definitions. No
  unchanged parent row or reachability witness can exempt a constant from A.
- **G: source-named theorems only.** `axioms.golden` pins exactly the source-named
  theorems and their transitive axiom sets, including private source theorems.
  Compiler-produced provenance determines this subset. Added, removed, renamed,
  or axiom-changed source-named theorems require an explicit golden update and
  review. Generated helpers have no golden rows; helper renumbering alone needs
  no golden edit. All helpers still undergo P, A, and provenance validation.

There is no baseline-SHA removal exception in the target contract. It requires
neither an old environment nor a helper-to-parent graph to excuse a golden delta.
The generating declaration may be a definition without a golden row; an equation
may be unreachable from its generator. Neither circumstance excludes it from A.
Golden equality pins axiom accounting, not statement or proof-body equality.

## Verified Lean API evidence

`lean-toolchain` pins `leanprover/lean4:v4.35.0`. Source paths below are relative
to `$(lean --print-prefix)/src/lean/Lean`. Source inspection and successful Lean
`#check` probes establish the API names and limitations below. These existing APIs
do not supply general persistent helper provenance.

- `Lean.LocalDecl.isAuxDecl` and `Lean.LocalDecl.isImplementationDetail` take **LocalDecl**,
  not ConstantInfo (`LocalContext.lean`). They cannot certify imported theorem origin.
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
- `Lean.privateToUserName? : Name → Option Name` understands private names
  (`PrivateName.lean`); `Lean.Expr.getUsedConstants : Expr → Array Name`
  extracts graph edges (`Util/FoldConsts.lean`).

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

## Compiler provenance persisted with declarations

The compiler must record structured declaration-origin facts when declarations
and helpers are created, and persist them with the declarations in oleans. Record
whether each declaration is source-named or generated, its exact structured Lean
Name, and, for each helper, the exact generating declaration and generator kind.
Record cached-helper reuse explicitly without replacing the generating declaration
with an arbitrary later user. Preserve actual multiple generation facts if a
shared artifact has them; never invent a unique owner by choosing the first user.
Every generator needs coverage, including equations, matchers, auxiliary proofs,
and helpers whose generator is itself generated.

The imported environment must retain these facts without replaying elaboration or
realizing new equations. Validate metadata completeness and consistency against
the full population before selecting G. Missing, unknown, malformed, or conflicting
origin records fail closed; they cannot silently remove a declaration from A or G.
Exact generating identities must resolve in the environment, independent of
whether their declarations are theorems or have golden rows.

Names, regexes, missing source ranges, theorem kind, handwritten annotations, and
manual review are never origin evidence. Explicit source declarations such as
`foo_proof_1` or `bar.match_2` remain source-named. Preserve structured private
scope/module components: `Lean.privateToUserName?` interprets private names but
cannot establish origin or merge identities. Use the injective `enc` identity
from `theorems.lean` for comparisons and reject ambiguous display serialization;
a printed-name collision must not merge declarations or hide a golden difference.

Transitive axioms come from `Lean.collectAxioms`, not the provenance records.
`Util/CollectAxioms.lean` traverses types and values of definitions, theorems and
opaque declarations; axiom/constructor/recursor types; and inductive types plus
constructor edges. Quotient declarations add no graph edges. Imported precomputed
axiom sets are axiom evidence, not helper ownership or reachability evidence.
The earlier graph probes above explain why reachability cannot classify origin
or replace checking independently generated equations.

## Interim state and implementation prerequisites

Until compiler provenance is available and validated across imports, keep the
current executable full-theorem golden check as an **interim** safeguard. The
axiom-check design introduces no baseline mode,
name-based filtering, manual allowlist, or helper-removal exception. The interim
full golden is not the agreed end state.

Its exit condition is a pinned compiler/toolchain providing complete persistent
origin/generating-declaration metadata, successful import and generator controls,
and a validated consumer implementing P/A/G above. The coordinator must allocate
the owning roadmap task and arrange the toolchain work; no new R ID is invented
here. Task allocation and implementation remain prerequisites, not a request to
redecide compiler provenance.

Implement compiler metadata in a separate toolchain change, then extend
`theorems.lean` to export full constant axiom facts and provenance without changing
the population during enumeration. `lib.sh` supplies isolated artifact plumbing;
`axioms.sh` validates full-population A and compares only the provenance-selected
source-theorem rows for G. Migrate `axioms.golden` by verified compiler
classification, removing generated rows while preserving all source-named rows
and checking their freshly computed axiom sets. An explicit regeneration is not
proof of provenance. Do not use manual classification of the 27 lines as the
migration mechanism.

Bind inventories, provenance and diagnostic artifacts to the exact source SHA,
pinned toolchain, enumerator version and build manifest. Refuse stale or mismatched
artifacts and preserve failure reports. Emit deterministic identities, origin,
generating declarations and transitive axioms, with separate diagnostics for A,
G and provenance failures. Infrastructure failures and resource exhaustion are
INCOMPLETE, never successful empty populations. Measure runtime and memory on
concrete-ci-2 before setting bounds; do not drop declarations to meet a budget.

## Implementation acceptance controls

Controls must first pass without their planted defect, then assert the precise
failure and offending structured declaration identity. An unrelated build failure
or timeout does not satisfy a negative control.

| Case | Required outcome |
| --- | --- |
| Genuine compiler helper renumbering between two source variants, same source-theorem axiom rows | P/A/provenance pass; G unchanged, with no helper rows |
| Generated helper uses a nonstandard axiom | A fails naming the helper, even with unchanged source-theorem rows |
| Independently generated equation not reached by its generator uses a nonstandard axiom | A fails naming the equation |
| Helper generated for a definition without a golden row | A and provenance still check it |
| Source theorem named `foo_proof_1` or `bar.match_2`, public or private | Included in G; removal or axiom change fails G |
| Source theorem added, removed, renamed, or changed to use a different standard axiom | G differs until explicit reviewed regeneration |
| Missing, unknown, conflicting, or malformed provenance | Provenance failure; no fallback to names or manual review |
| Cached reuse, nested generation, private scopes and cross-module imports | Exact compiler generation facts survive; all constants still checked |
| Duplicate identity, colliding printed names, stale manifest or missing declaration | Explicit evidence failure, never silent deduplication |
| Source module outside the root import closure | P fails |

Retain the transitivity regression evidenced above: reached helper and parent both
have exactly `[onlyHelper]`; an independently generated equation need not be
reachable from the parent. This is a graph limitation test, not permission to
skip A. Test metadata persistence in a fresh importing process, rather than only
in the environment that created the helper. Test that classification does not
realize equations or otherwise change the enumerated population.

After implementation, run the property/golden gate and focused compiler-provenance
fixtures under `nix develop` on concrete-ci-2, using `bounded_run` from
`scripts/checks/lib/bounded.sh`. Record results against the exact candidate SHA
and toolchain identity. The axiom-check design makes no runtime or implementation
acceptance claim. Compiler metadata remains within the existing compiler trust
boundary; protection against hostile metaprograms forging it is not established
by these checks.

## Lean 4.35.0 toolchain migration

The project and Nix official-release archive pin Lean 4.35.0; CI's lean-action
reads the same `lean-toolchain`. All four Nix asset hashes are the release's
published SHA-256 digests. The existing LCB revision `4bea4b3` builds with 4.35.0
as a Lake dependency, so its source pin is retained (its standalone toolchain
file still declares 4.34.1). No theorem or compiler definition required a change.

`external.pins` pins both Lean tools' toolchains as well as their revisions.
The exporter is now the upstream v4.35.0 revision, matching our olean format.
`lake_tool.sh` compares each revision's checked-in declaration with its pin,
including on cache hits, and dispatches `elan run --install TOOLCHAIN lake build`
without rewriting the declaration. External tool installations use the cache-local
`$CONCRETE_EXT_TOOLS/elan` home (or `~/.cache/concrete-ext/elan`), overriding
ambient `ELAN_HOME` before the project build as well, to protect shared user toolchains
from Nix elan patching. Both `ELAN_TOOLCHAIN` and `LEAN_PATH` are cleared.
Use `nix develop .#extcheck` for this gate; Rust is absent from the default shell.
The shell supplies `CONCRETE_EXT_ELAN` by absolute path only; it adds no Lean,
Lake or leanchecker proxies to PATH, preserving the caller’s project toolchain.
The required root `external_toolchain.sh` control invokes the production driver
against local sources and executable stubs. It checks fresh dispatch, cache reuse,
corrupted cache declarations, pin mismatches, exporter/project agreement, and
isolation from ambient Lake, toolchain overrides and the shared elan home.
It checks the actual extcheck PATH and rejects a simulated proxy leak as a negative
control. CI keys the external cache on the pins, builder, project toolchain and
both Nix flake files.

R-0208's measured-build policies and receipts under `legacy/` describe the frozen
compiler and remain historical evidence, as does `scripts/oracle/ORACLE.lock`.
They do not accept this new checker or authorize receipts for it. The current
proof gates establish axiom accounting, fresh kernel replay and independent
re-checks for the current sources; they do not inherit legacy checker acceptance.

The 4.35.0 axiom golden adds exactly the following compiler-generated theorems
under `ConcreteSpec.Surface0`. Each proves Boolean reflection for its existing
`inferInstanceAs` decidability instance in `Spec/Surface0.lean`; each has an empty
transitive axiom set. No previous row changes or disappears.

| Added suffix | Existing predicate / instance |
| --- | --- |
| `instDecidableBidiControl._proof_1` | bidirectional control character |
| `instDecidableByteEscape._proof_1` | byte escape and decoded byte |
| `instDecidableByteRaw._proof_1` | raw byte literal character |
| `instDecidableCommentRaw._proof_1` | raw comment character |
| `instDecidableControl._proof_1` | control character |
| `instDecidableForbiddenRaw._proof_1` | forbidden raw character |
| `instDecidableIdentContinue._proof_1` | identifier continuation character |
| `instDecidableIdentStart._proof_1` | identifier starting character |
| `instDecidableScalar._proof_1` | UTF-8 scalar |
| `instDecidableStringEscape._proof_1` | string escape and decoded scalar |
| `instDecidableStringRaw._proof_1` | raw string character |
| `instDecidableWhitespace._proof_1` | whitespace character |
