# F03p: checked source paths and spans

Status: design proposal for F03p-design, round 1; no Lean implementation or new owner decision.
Issue: ISS-0068. Unallocated follow-up records use ISS-XXXX / R-XXXX; this proposal allocates none.

## Authority and boundaries

Read against main `038ec12142fbbe67e8c3ec81c336a048250a38da`, extraction snapshot
`origin/wip/spec-integration-head` (`aa44140a33747fa37ece59c76090972e564c65b4`), and
owner-record snapshot `origin/wip/record-2026-11-16b`
(`3a58416031a0998276950e31ef8f5f6bb58c5bd9`). The latter is authority pending its
landing, not a claim that this checkout already contains that record. Read it with
`git show origin/wip/record-2026-11-16b:docs/decisions.md` and its ISS-0068 row.
The file-path record has anchor `decided-2026-11-16-file-paths` in that pinned
branch; use the same anchor in `docs/decisions.md` after its landing.
The landing plan is the coordinator's scratchpad `f03/plan.md`, header and rows
F03p-design, F03p, F04p, F20p, F29p (in particular lines 9, 187, 234–235, 263).
Its older authority holds are superseded only within the new answer's scope.

The November 16 record distinguishes the coordinator's question summary from the
owner's exact answer. The summary, quoted as summary rather than owner wording:

> ASCII only; `/` is the only separator; refuse `\\`, drive prefixes, empty and `.` components, spaces, control characters, leading or trailing dots and Windows reserved names; never rewrite a path.

> uppercase A-Z allowed, but refuse two paths that differ only in letter case, including directory prefixes.

> at most 100 bytes per path component and 200 bytes per whole path.

The owner's reply, verbatim:

> agree, mixed, cap. yes to round 18

The record expressly says:

> details beyond the supplied question remain unsettled.

Round 18 concerns the separate spec-inventory branch; it is not permission for
extra implementation or review rounds here.

[November 14 structural authority](../decisions.md#decided-2026-11-14-trap-sites-and-law-tests)
already confirms the recommendation:

> Every source file gets one checked path inside the project. The loader refuses empty paths, paths with `..`, absolute paths and duplicates.

It also requires every trapping place to map to exactly one valid source location,
with a proof. The owner answered “ok do all this”. Thus parent components are
**decided refused**, not a new owner question. This is component traversal; whether
internal repeated dots such as `a..b` are allowed is a separate character-detail
question below, not a reinterpretation of traversal.

[Recovered item 12](../decisions.md#recovered-29-12) states:

> Each file is checked separately, and checking a file stops at its first error. (yes)

That limits per-file checking, not the set over which project collisions must be
checked. It does not choose the byte or precedence of overlapping path errors,
nor authorize stopping all independent file checks after one file fails.

This document proposes a Lean design, not another normative path specification.
Follow [PHILOSOPHY](../PHILOSOPHY.md): every fact once, Lean as the specification,
and proofs over the real execution/admission relations. No provisional parser,
policy parameter with an arbitrary default, or temporary fixture chooses an open side.

## One admission boundary, checked views

Keep `SourcePath.parse` as the single local admission function. It returns
`Except SourcePath.Refusal SourcePath`; `SourcePath.render` only renders already
checked data. Strengthen the representation with proof fields for local validity
and whole-path byte length. It must remain impossible to construct an invalid
`SourcePath` through an unchecked external constructor. A locally valid path is
not evidence that a project containing it is admissible.

Use raw UTF-8 byte offsets for diagnostics, including invalid non-ASCII spellings.
A String input can retain its UTF-8 bytes without normalizing; a future filesystem
adapter must separately account for names not representable as Lean String.
Do not replace source bytes with a Unicode character count. Once ASCII is proved,
prove byte length equals scalar-list length and reuse that bridge in existing
component/render proofs. Store component boundaries as ranges into the exact input
or derive them in the scanner with a proved slicing correspondence. No second
normalizing split or host-dependent filesystem interpretation is admissible.

Have one decidable local predicate (proposed name `SourcePath.RawOk`) assembled
from the authoritative component predicates and two cap constants. The parser
computes a validated witness or a violation witness against that predicate.
Do not keep `ComponentOk`, a Boolean validator and `componentReason` as three
independent policy implementations: derive decisions and diagnostic evidence
from the same rule definitions. Diagnostic selection needs the owner answers below;
its completeness proof must rule out the old catch-all control-character reason.

Project admission is necessarily contextual. Introduce one checked path-set
function (proposed name `SourcePath.admitProject`) consuming all raw file paths,
calling `parse`, and checking exact duplicates and case collisions. It returns
all exact identities in input order or reason-bearing failure data. This is the
sole project path check used by `LoadedProject.load`; it is not a second local
parser. Whether the final error carrier collects one failure for each file or
selects a project failure is held below. No partial project is executable.

Define prefix occurrences as every nonempty component prefix of every locally
admitted path, including the complete path. Each occurrence retains its source
file and component boundary. Define ASCII folding once (`A`–`Z` only), for
comparison only. Project case safety means: for any two occurrences with equal
folded component sequences, their exact component sequences are equal. This
catches `Src/a.con` with `src/b.con`, as well as `A.con` with `a.con` and
file/directory prefix case differences. Repeated identical `src` prefixes are
allowed; repeated identical full file paths are refused by the separate duplicate
rule. Do not use `Nodup` on folded prefix occurrences: that would refuse normal
shared directories. Folded comparison keys never become stored identities.

The domain of this predicate is the **whole supplied project path set**, not only
files mentioned by sites, one module, or adjacent entries. Dependency aggregation
is owner-held; the implementation cannot silently define the supplied set to
exclude files in the chosen scope. Filesystem existence, symlinks, mounts and
file/directory exact-name conflicts require their own established authority;
syntactic path admission alone proves no filesystem containment theorem.

## Rule-to-law obligations

Names below are proposed Lean declaration names, not allocated rule IDs. Each Law
must name its actual subject and have an application with satisfiable premises
plus a Lean-refuted mutant preserving those premises. Helpers expose representation
facts; a definitional projection alone is not admission correctness.

| Decided fact | Single checked owner | Required Laws and refusal evidence |
|---|---|---|
| Nonempty project-relative identity; no absolute paths or `..` components | `parse`, via `RawOk` and component relation | `parse_iff`: success iff exact rendering and `RawOk`; `parse_refusal_sound/complete`; reason distinguishes empty, absolute and traversal, retaining input and offending boundary |
| Slash-only separator; no backslash, empty or single-dot component; no drive prefix | Same `parse` | `parse_iff` in both directions, `render_safe`; dedicated reason for each exclusion and witness byte/boundary; split includes trailing empties |
| ASCII only; spaces and controls refused | Same `parse` | ASCII projection and individual refusal Laws; bytes outside ASCII, space, NUL, tab and DEL controls; no loss of raw spelling |
| No leading/trailing dots or Windows reserved names | Same `parse` | Component projection and refusal Laws with matched component and rule; precise reserved-name semantics require the owner answer below |
| Uppercase A–Z allowed | Same `parse` | Positive mixed-case admission instances and completeness, so an all-lowercase-only mutant fails |
| Never rewrite | `parse` and exact `render` | `accepted_render`, `parse_render`, `render_injective`, `refusal_input`; project success preserves raw names, bytes and order; case folding cannot alter output |
| At most 100 bytes/component, 200 bytes/path | Same `parse`; one constant each | `component_bytes_le`, `path_bytes_le`, and completeness at equality; reason includes limit and offending range; whole length includes slash bytes |
| Exact duplicates refused | `admitProject` | Project admission biconditional, duplicate refusal soundness/completeness, with both original input occurrences |
| Case-only collisions, including directory prefixes | Same `admitProject`, prefix relation above | Project admission iff all local `RawOk`, no duplicate files and prefix case safety; collision witness contains both paths, prefixes and folded equality/exact inequality |
| First error per file | File-check result consumed by loader | At most one reported failure per file; every selected failure is real; no later file-local checking after its selected first failure; independent files remain checkable |
| Valid stored byte coordinates | `Span.LocationOk`, loader site binding | `Span.locationOk_iff`, loader admission iff strengthened `ProjectSources.Realizes`; exactly one source binding and correct start coordinates for each admitted site |

For caps, exercise 99/100/101-byte components and 199/200/201-byte paths where
all other predicates hold (construct multiple components for whole-path cases).
Prove the universal bounds, not just these examples. For collision completeness,
prove existential witness detection and admission invariance under permutation of
the path set. Diagnostic ordering may depend on owner-approved ordering; admission
must not. Test nonadjacent collision pairs, shared exact prefixes, different
suffixes, uppercase-only valid names and three-file sets. Mutants must actually
change the production path-set subject, not a toy equality check.

Refusals should carry raw input, typed rule reason, component range, and a validated
byte location; collisions additionally carry both input occurrences and prefix
ranges. Empty components and EOF failures need boundary locations, since there
may be no offending byte. Prove bounds (`offset ≤ raw UTF-8 byte length`), rule
soundness, and that both collision witnesses belong to the input set. Rendering
may say which rule failed and where; it must not offer a rewritten name as an
accepted identity. The location convention and tie-breaking remain questions,
so this is a required evidence shape rather than an implemented byte choice.

## Owner questions before implementation

These are questions with recommendations, not decisions. The affected parser,
scanner, diagnostics and fixtures wait for explicit answers. Do not implement a
stricter approximation: refusing an undecided spelling also chooses a side.

| Question | Options and costs | Recommendation for owner approval |
|---|---|---|
| Which remaining printable ASCII characters are admitted? | Permit all except the listed exclusions: broad, but permits `:`, `*`, `?`, quotes, angle brackets and pipe away from drive syntax. Explicit portable whitelist: narrower and easier to audit but rejects more names. A complete extra blacklist: flexible but easier to omit a hazard. Internal dots such as `a..b` need explicit treatment. | Approve an explicit complete portable alphabet (letters of both cases, digits, `_`, `-`, internal `.`), with existing endpoint-dot rules; approve internal repeated dots or reject them explicitly. This is additional policy, not inferred from “ASCII only”. |
| What exactly counts as a Windows reserved name? | Exact component against a closed list: simple, but `CON.txt` may escape. Case-insensitive stem-before-first-dot matching against a versioned closed list: more portable, more proof cases. Host API query: platform/version dependent and unsuitable as the Lean authority. Scope of `CON`, `PRN`, `AUX`, `NUL`, `COM1`–`COM9`, `LPT1`–`LPT9` and any extra device spellings must be explicit. | Approve a closed ASCII list and case-insensitive stem matching, with its full list shown before approval. Never silently import host Windows rules or extend the list. |
| Do collision checks cross dependency/package roots? | Per project only: simple but misses collisions when materialized together. Entire dependency closure in one namespace: strongest but may reject unrelated roots. Per materialization namespace with explicit root identities: models actual coexistence, requires namespace evidence. | Approve checks over each complete materialization namespace, dependencies included when they coexist there. Until answered, do not choose a dependency aggregation rule. |
| Which byte/boundary does each refusal identify? | First contributing byte/component start: stable and easy to explain. First byte proving failure (e.g. byte 101/201): precise for caps, more state. Whole component range only: useful context but does not answer “which byte”. Empty/EOF needs a boundary convention; collision needs one or both sides. | Approve zero-based raw-byte boundary offsets, first violating byte for character/cap failures, component-start for reserved names/traversal, EOF or separator boundary for empties, and both first case-differing bytes for collisions. Show the full per-reason table for approval; no conversion to source-file line/column. |
| What is first among overlapping path failures, and how does project failure reporting compose? | Rule priority: stable but may skip an earlier byte. Earliest byte with fixed rule tie-break: source ordered, needs a total ordering. Existing list-first loader: cheap but cannot alone establish separate checking of every file. Collect one failure per file plus project failures: more data and attribution work. | Approve earliest byte with an explicit rule tie-break and independent per-file results; then check project collisions over locally valid names, without admitting a partial project. Ask how a collision is attributed and ordered and whether independent site errors are reported after path failures. This extends neither the first-error decision nor the old global-stop algorithm by assumption. |
| What about exact file/directory prefix conflict (`a` and `a/b.con`)? | Refuse at syntax-set admission: portable but a new condition. Leave to a separately checked filesystem/materialization boundary: smaller syntax relation, later error. | Approve refusal at materialization admission with a typed witness; it is distinct from case-only collision and cannot be smuggled into that predicate. |
| What path encoding does the host adapter accept? | Only String-representable names: simple, but requires named failure for invalid encoding. Raw bytes: complete OS boundary, larger interface change. | Approve a byte-preserving host boundary and explicit encoding refusal before String admission; do not treat lossy conversion as path parsing. F03p need not add a host adapter, but must not claim coverage of arbitrary OS names. |

Parent `..`, ASCII rejection, normalization/rewrite prohibition, uppercase allowance
and the numeric caps are not reopened by these questions. The exact reserved-name
matcher, new punctuation restrictions and diagnostic conventions are not settled
by an agent's recommendation.

## Extraction repairs and landing closure

Retain complete Lean commands, doc comments, attributes, instances and private
support; discard historical approval annotations as qualification evidence. Build
a command-level manifest with retain/repair/replace/owner-held/retire dispositions
before writing each slice. Count changed Lean lines including controls and consumer
migrations; the estimates below are budgets, not measured implementation sizes.

| Slice and prerequisites | Complete change and evidence closure | Size/effort target |
|---|---|---|
| F03p after this accepted design, authority record on main and necessary answers | Adapt `Spec/Ident.lean:704–826,917–923`: component predicate, strengthened SourcePath, exact renderer, reason-bearing scanner/parser, project path-set checker and Span definitions; `mainSourcePath`, `tabSpan`, both location examples. Include admission Laws and controls needed to qualify new behavior in this slice; do not defer its correctness to F04p. Reuse F03 byte arithmetic and `tabSource`. | Under 1,000 changed Lean lines; medium. Measure closure first; split independently useful representation support if it cannot fit, never omit evidence. |
| F04p after F03p and F04 | Adapt all `Proofs/Ident.lean:13–279,1300–1305,2269,2341–2355`: representation/render/refusal family and remaining controls, Span projection, `otherTabSource` refusal, `tabPointSpan` and `wrongTabSpan` support. New admission evidence already landed in F03p is imported, never duplicated. | Under 1,000 changed Lean lines; medium. New proof growth may require bounded theorem-family slices, each independently qualified. |
| F20p after F20 and approved project scope/reporting | Adapt `Proofs/Observe2.lean:967–1005` and complete support; atomically migrate production `LoadedProject.load`, `parseFiles`, `SourceLoadRefusal`, `Names`, `ProjectSources.Realizes` and their dependent Laws if not already migrated in F20. Use `admitProject`, preserve exact names/bytes, strengthen Realizes with prefix case safety. Include malformed-path, collision, cap and mixed-case loader controls. | Original held controls under 600 changed Lean lines; small only if F20 already has the adapted loader. Otherwise remeasure the entire consumer closure and split a preceding loader integration slice; never fit the budget by leaving Realizes stale. |
| F29p after F29 and F20p | Adapt `Proofs/Frame.lean:2334–2353` and support; transport malformed and new collision/cap refusals through actual final boundary/Realizes equivalence. Preserve empty/backslash/control cases and prove no admitted project exists, plus positive valid mixed-case coverage. | Under 600 changed Lean lines; small, subject to measured support closure. |

The sequential path is authority/answers → F03p → F04p; observation F19 waits
for F04p, F20 supplies its own complete loader semantics/evidence, then F20p;
F29p waits for both F29 and F20p. If F20 has not landed, put the new loader contract
and all its Laws in F20 at first arrival rather than landing an obsolete policy and
repairing it later. Later control-only slices are not permission to defer the first
consumer's proof. Any additional split uses coordinator-allocated labels/IDs.

Specific departures from the old snapshot:

- `DrivePrefix` must use an explicit ASCII-letter predicate, not an undocumented
  reliance on `Char.isAlpha`. Extend local admission with ASCII, space, endpoint
  dot, reserved-name and byte-cap evidence after the open matcher details settle.
- Remove AY-2/AY-3 “assumed-yes” wording in repaired commands and link current
  authority. AY-4's acceptance of other Unicode scalars is contradicted: retire
  `precomposed`/`decomposed` as admitted SourcePath constructors and
  `unicode_exact` as an acceptance claim. Replace them with exact non-ASCII
  refusal inputs and normalization-mutant controls. Never-rewrite still needs a
  positive ASCII case-changing/repair mutant; two rejected Unicode strings alone
  do not demonstrate successful identity preservation.
- Replace `componentReason`'s catch-all with proved typed failures and byte
  witnesses. Migrate every positional Refusal constructor, including loader and
  frame controls; no default location to keep old fixtures compiling.
- Keep `split_render`, injectivity, parse/render and refusal retention where still
  valid, repairing proof destructuring for stronger invariants. Preserve control
  and backslash negative cases, and add every newly decided rule's controls.
- Add the prefix collision relation and checked whole-set operation; the old
  `firstDuplicate` on exact SourcePath identities cannot implement this rule.
  Strengthen `Realizes` independently of executable success, then prove both
  directions for the actual loader. Do not define Realizes as “load succeeded”.
- Retain Span's ordered byte range, positive stored line/column and
  `LocationOk` source checks. `Span.PathOk` is only a derived compatibility view,
  never an independent weak path-admission policy. Retire it if unused; otherwise
  prove it from SourcePath. A Span alone does not establish source membership.
- Reuse `tabSource` from F03 and `otherTabSource` from F04 exactly once. Import
  the latter into later controls; do not recreate these fixtures or byte arithmetic.
  The `tabSpan` projection must hold for actual bytes and fail for wrong column,
  different line structure, and out-of-bounds ends. Site uniqueness and total
  trapping-site coverage belong to the loader/consumer relation, not PathOk.

## Qualification for implementation slices

Each slice must build its full import closure and qualify against its exact SHA
under the plan's common proof-integrity, site, compiler-consumer and repository
checks. Laws run over the real Spec parser, project checker, loader and final
relations. Concrete controls refute mutants in Lean with premises retained;
a failing compilation or timeout is not a refutation. No host parser, separate
policy oracle, stored assertion of correctness, or refusal-all implementation
substitutes for the biconditionals. Document all source dispositions so the late
Span and final-boundary commands cannot disappear during extraction.

This round changes only this proposal. The requested validation is
`nix develop -c scripts/checks/run.sh`; its result is reported separately against
the current base plus this uncommitted document. It is not a result for a future
implementation or a committed candidate. No review tools, notes, commits or pushes
are part of this writer round.
