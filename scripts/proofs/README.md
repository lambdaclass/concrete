# Proof checks and axiom-check design

For theorem-comment classification, use the [owner-accepted project convention](../../docs/decisions.md#decided-2026-11-13-proof-labels), including its scope limits.

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
`${CONCRETE_EXT_TOOLS}/elan-<identity>` home (or `~/.cache/concrete-ext/elan-<identity>`), overriding
ambient `ELAN_HOME` only for pinned tool builds, to protect shared user toolchains
from Nix elan patching. Project builds and queries retain the caller’s `ELAN_HOME`.
Both `ELAN_TOOLCHAIN` and `LEAN_PATH` are cleared.
Use `nix develop .#extcheck` for this gate; Rust is absent from the default shell.
The shell supplies `CONCRETE_EXT_ELAN` by absolute path only; it adds no Lean,
Lake or leanchecker proxies to PATH, preserving the caller’s project toolchain.
The required root `external_toolchain.sh` control invokes the production driver
against local sources and executable stubs. It checks fresh dispatch, cache reuse,
corrupted cache declarations, pin mismatches, exporter/project agreement, and
isolation from ambient Lake, toolchain overrides and the shared elan home.
It checks the actual extcheck PATH and rejects a simulated proxy leak as a negative
control, and rejects global-home redirection, redirection after locking, premature locking
and unscoped tool builds. All post-build stubs assert the caller home and an unlocked
cache; build stubs assert the locked phase. The full driver completes, including fidelity and corruption
controls, using the production inventory reader and synthetic checker inputs. Resolved
elan path/version keys isolated homes and Lake tool trees, whose keys also include their declared toolchain.
Nanoda trees use the resolved cargo and rustc paths plus `rustc -vV`. Every tool tree includes the hash
of the self-contained `lake_tool.sh` recipes (including the nanoda stack patch). CI keys
the external cache on the pins, builder, project toolchain and both Nix flake files.

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

## Local identity performance (ISS-0066)

The classifier still checks the inferred type of every elaborated subterm of
all declarations reachable from semantic roots, and checks type expressions as
carriers themselves. The cache stores results of that same classifier; it adds
no namespace, matcher, recursor, body or dependency exemption.

Ordinary fixtures use distinct module and declaration names. One imported
environment contains them and the main library, and every reachable constant is
classified once. Each target reports only roots in its own transitive module
import closure. If taint originating outside that closure could reach a target
root, it would be a dependency of that root and therefore inside the closure.
This is why sharing traversal preserves standalone outcomes. The controls check
both an unrelated clean fixture beside refused fixtures and an explicit import
of a refused fixture. Identically named declarations in different namespaces
must retain their independent verdicts.

Carrier entries containing locals, semantic-expression caches and Lean's
inferType/normalization caches share the lifetime of one declaration. Closed
carrier expressions (no free variables, metavariables or loose bound variables)
are reused across declarations, including their constructor-ancestry key. Opened binders have fresh free-variable
identities; imported expressions have no metavariables. Cache entries involving
locals are never reused in another declaration's context. Shape controls still
use independent environments because they redefine the nominal identity types.
The two Examples refusals share a separate replacement Examples environment,
with each unauthorized declaration required by name.

Reproduce scanner phase and wall timings, and peak resident memory, with:

```sh
nix develop -c bash -c 'LOCAL_IDENTITY_PROFILE=1 /usr/bin/time -v bash scripts/proofs/trust_ban.sh'
```

Measurements below are sequential, warm-build runs on Linux x86_64, AMD Ryzen 9
9950X3D, Lean 4.35.0, inside nix develop. The workstation was shared with other
sessions. Scanner invocations and fixture compilations were sequential; controls
were never profiled in parallel. Peak RSS is the maximum child resident set
reported by GNU time, not the sum of independently running processes. Baseline source:
22724c4be398ab399cef3ab3f756cfb6df72591b, with phase-timing instrumentation only.

The representative control is the original compiling lambda callback:
`def leak (vs : List ConcreteSpec.Var) : Nat := (vs.map (fun v => toString v.id)).length`.
Both versions reject specifically at `semantic diagnostics dependency: leak`.
The no-op benchmark elaborates the same scanner definitions but replaces the
entry point with `pure ()`, isolating startup/elaboration from environment work.

| Representative callback | Before | After |
| --- | ---: | ---: |
| Wall time | 18.970 s | 8.300 s |
| Import | 0.290 s | 0.372 s |
| Graph construction | 10.893 s | 0.222 s |
| Semantic traversal (1,259 declarations) | 6.260 s | 5.986 s |
| Peak RSS (KiB) | 2,190,472 | 2,071,724 |

The old no-op scanner took 1.320 s with 1,637,196 KiB peak RSS. Its process and
elaboration overhead is small beside graph construction and traversal; removing
repeated environment scans and irrelevant reverse edges addresses the measured
cause. Shape controls still start separate processes to keep incompatible
nominal declarations isolated. Scanner invocations fall from 68 to 8; the
Examples pair is compiled together but each refusal is asserted by its own name.

| Complete `trust_ban.sh` | Before | After |
| --- | ---: | ---: |
| Wall time | 27m36.59s | 5m51.34s |
| Peak RSS (KiB) | 2,744,888 | 2,132,488 |
| Result | PASS=39 FAIL=0 | PASS=39 FAIL=0 |

The final shared batch imported in 0.334 s, built its graph in 0.552 s, and
traversed 7,012 reachable declarations in 48.306 s (51.107 s process wall time).
The replacement Examples environment imported in 0.285 s, built its graph in
0.221 s, and traversed 2,010 declarations in 15.391 s (17.825 s wall time).
The batch retained 187,508 closed carrier results; Examples retained 120,787.
Context-bearing entries and Meta state were released after each declaration.
All 69 original controls remain, with the two import-isolation controls added.

The after run measures the uncommitted writer tree based on the baseline SHA
above; the harness owns its commit. Measured source SHA-256 values:

- `local_identity.py`: `b8c482adb88553bfe4d0e951b203fd63c626fde7c12a65f473c763af7d6eef72`
- `local_identity.lean`: `a7ef755d42ab43c092d87f8295af2066613a1bbd510c00ed254833be73c83a39`
