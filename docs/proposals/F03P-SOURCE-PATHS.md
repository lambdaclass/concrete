# F03p: checked source paths and spans

Status: round-2 authoritative writer brief for F03/F19 Lean writers; not a landing artifact.
Owner instruction (2026-11-17, “ok yes”): design proposals do not land on main;
they are writer instructions only, and decisions live in the Lean spec. The harness
retains this brief outside main. No Lean implementation is changed this round.
The same owner instruction requires a separately registered rule for every distinct
refusal: per-check rules, never one composite path rule. Apply this without asking again.
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
The prerequisites are the F03 byte-coordinate interface and F04 byte-coordinate
laws already on main: see [ISS-0068](../../ISSUES.tsv), row 43,
[the F04 extraction disposition](../../ConcreteSpec/Proofs/Ident.lean), lines 38–52,
and [foundation milestones](../../ROADMAP.md), lines 424–427. No scratch document
is required to understand or implement this brief. Line numbers for extraction
below refer only to the pinned integration snapshot, not current main.

The November 16 record distinguishes the coordinator's question summary from the
owner's exact answer. The summary, quoted as summary rather than owner wording:

> ASCII only; `/` is the only separator; refuse `\`, drive prefixes, empty and `.` components, spaces, control characters, leading or trailing dots and Windows reserved names; never rewrite a path.

> uppercase A-Z allowed, but refuse two paths that differ only in letter case, including directory prefixes.

> at most 100 bytes per path component and 200 bytes per whole path.

The pinned decision record, lines 3732–3751, contains that summary and the
owner's reply (line 3746), verbatim:

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

Make `SourcePath.parse` the single local admission function over raw bytes. It returns
`Except SourcePath.Refusal SourcePath`; `SourcePath.render` only renders already
checked data. Strengthen the representation with proof fields for local validity
including whole-path byte length in that same predicate. It must remain impossible to construct an invalid
`SourcePath` through an unchecked external constructor. A locally valid path is
not evidence that a project containing it is admissible.

Use a byte-preserving input carrier: a validated byte sequence whose storage is
`ConcreteSpec.Numbers` with a proof every element is below 256. Reuse F03
`SourceText` arithmetic through a proved exact view; do not introduce a second
line/column scanner. A thin String adapter encodes UTF-8 exactly and calls the
byte parser; it is not the primary admission boundary. Every byte >= 128 is
refused, including ill-formed UTF-8. Host adapters must preserve bytes or explicitly
refuse unrepresentable input; replacement-character conversion violates never-rewrite.
This is implementation of ASCII-only, not an encoding question for the owner.

Store exact path bytes and a proof of the single `SourcePath.RawOk` predicate
in `SourcePath`; that predicate includes the separator-inclusive 200-byte bound.
Components and their validity are derived views. Component ranges belong in
scanner/refusal/collision witnesses, never in identity fields. Render injectivity
then follows from exact bytes and proof irrelevance. `renderBytes` is primary;
`render` returns the exact ASCII identity as `ConcreteSpec.Name`, proved to encode
to the same bytes. The host String view is a presentation projection of that Name;
all old String equalities in loader/proof consumers migrate through the exact view.

Have one decidable local predicate (proposed name `SourcePath.RawOk`) assembled
from the authoritative component predicates and two cap constants. The parser
computes a validated witness or a violation witness against that predicate.
Do not keep `ComponentOk`, a Boolean validator and `componentReason` as three
independent policy implementations: derive decisions and diagnostic evidence
from the same rule definitions. Diagnostic selection follows the convention below;
its completeness proof must rule out the old catch-all control-character reason.

Project admission is necessarily contextual. Introduce one checked path-set
function (proposed name `SourcePath.admitProject`) consuming all raw file paths,
calling `parse`, and checking exact duplicates and case collisions. It returns
all exact identities in input order or reason-bearing failure data. This is the
sole project path check used by `LoadedProject.load`; it is not a second local
parser. The final carrier collects a nonempty deterministic list of independent refusals
and their blocked dependents, following the decided diagnostic contract below. No partial project is executable.

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
files mentioned by sites, one module, or adjacent entries. For this foundation the set is every source file relative to one project root
(decisions.md:3487, “one checked path inside the project”; line 3743, “relative to
the project root”). Future multi-package layout is Phase F / R-0383, not an F19
prerequisite. It must later choose between isolated package roots (cheap, needs a
separate coexistence check), one flat closure (rejects even two packages each using
`src/lib.con`), or explicit root namespaces (more representation/proof work, recommended
when that work is commissioned). No dependency policy is implemented here.
Filesystem existence, symlinks and mounts are outside this contract;
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
the path set. Diagnostic ordering follows the decided file/span/rule ordering; admission
must not. Test nonadjacent collision pairs, shared exact prefixes, different
suffixes, uppercase-only valid names and three-file sets. Mutants must actually
change the production path-set subject, not a toy equality check.

Refusals should carry raw input, typed rule reason, component range, and a validated
byte location; collisions additionally carry both input occurrences and prefix
ranges. Empty components and EOF failures need boundary locations, since there
may be no offending byte. Prove bounds (`offset ≤ raw byte length`), rule
soundness, and that both collision witnesses belong to the input set. Rendering
may say which rule failed and where; it must not offer a rewritten name as an
accepted identity. The following engineering convention fixes byte selection and overlapping failures;
it changes no accepted spelling.

## Diagnostic contract and remaining owner questions

Apply the diagnostic decision already approved at [decisions.md](../decisions.md),
lines 892–904: “option (a) on every question”. The approved
[diagnostic contract](../archive/crucial-2026-11-06/DIAGNOSTICS.md), lines 15–19,
requires `refused (rs : NonEmptyList Refusal)`; lines 35–38 require primary and
related spans; lines 61–63 say: “A judgment that needs a refused result is reported
as blocked by r inside the result, never as another refusal. Order is by file,
span, then rule ID.” Apply also decisions.md:1650: “Each file is checked separately,
and checking a file stops at its first error.” None of these is an owner question.

Check every file independently. Select its first local failure, then stop that
file's dependent checks. Check collisions among all locally admitted identities;
collision records name both occurrences, with the lexicographically earlier exact
path primary (input ordinal breaks identical-path ties). Coalesce pair witnesses
in deterministic order. Collision participants' dependent site checks are blocked;
independent admitted files' site checks continue. A site on a malformed path is
blocked by that refusal, not reported again as missing. Keep raw file identity
and occurrence provenance for this association. No executable partial project is
returned. Every report item has a real premise witness; sort output by exact raw
file bytes, span, then registered rule ID, using occurrence ordinal only for equal
keys. Never sort by folded identities.

Offsets are zero-based byte boundaries, with EOF equal to byte length, as in
`Spec/Surface0.lean`'s `ScalarFailure`, `FirstError`, and `SourceFailure`.
Carry a half-open offending component range as context as well as the offset:

| Check | Selected offset |
|---|---|
| Empty whole path | 0 |
| Initial slash / ASCII-letter-plus-colon drive introducer | 0 / 1 (colon) |
| Empty component | Separator ending the empty component; EOF for trailing empty |
| Single dot / parent traversal / reserved name | Component start |
| Backslash / non-ASCII / space / control / additional prohibited punctuation if approved | First offending byte |
| Leading / trailing dot | First / last component byte |
| Component / whole cap | First excess byte, indices 100 / 200 respectively |
| Duplicate / case collision | Both full-path starts / both first case-differing prefix bytes |

Classify `.` and `..` before generic endpoint-dot rules; define the generic
endpoint-dot refusal premises to exclude these exact components. This keeps each
reason reachable. Otherwise select the earliest witness offset, breaking ties by
a documented fixed per-check order (empty, absolute, drive, traversal, single-dot,
empty-component, non-ASCII, backslash, control, space, leading-dot, trailing-dot,
reserved, component-cap, whole-cap, then any approved added character rule).
The registered rule identity is stable; priority is separate from its spelling.
Prove reason selection agrees with this order, not merely that some failure exists.

Drive syntax is a whole-path initial ASCII letter followed by colon, not an
arbitrary component beginning with a letter and colon. Thus `dir/a:b` and
`dir/ab:c` receive the same colon policy from Q1; neither is a drive. `RawOk`
includes the whole-path drive exclusion, so parse-of-render cannot admit `c:`
through a merely component-local invariant. Do not choose noninitial colon policy
before Q1 is answered.

Only these three questions require the owner. No executable predicate, constructor,
fixture or registered example may partially choose a side before its answer.

| Question and plain-language background | Every option and its cost | Recommendation (requires explicit answer) |
|---|---|---|
| Q1: ASCII alone permits punctuation that common filesystems cannot store. Which remaining printable characters and internal repeated dots may a filename contain? | All printable ASCII except decided exclusions: broad, but allows noninitial `:`, `*`, `?`, quotes, `<`, `>`, pipe. A complete whitelist: less expressive, easiest to audit. A complete extra blacklist: flexible, more omissions to audit. Independently, allowing internal `a..b` preserves ordinary names; forbidding it simplifies appearance but rejects names unrelated to traversal. | Whitelist exactly `A-Z a-z 0-9 _ - .`, with the already decided endpoint-dot exclusions, and allow internal repeated dots. This adds policy and is not yet implemented. |
| Q2: Windows treats device names specially even with file extensions; does `con.con` count? | Exact case-insensitive whole-component matching against `CON PRN AUX NUL COM1–COM9 LPT1–LPT9`: small but extensions escape. Match the stem before the first dot against that same complete ASCII list, ignoring ASCII case: more proof cases, catches extensions. Host API matching: version/platform-dependent, prevents a closed portable Lean authority. Additional device spellings require an explicit list and add refusals. | The stated closed list, case-insensitive stem-before-first-dot matching, no additional spellings. This supplies the currently missing exact matcher, not a new assumption about OD142. |
| Q6: A regular file `a` cannot also be the directory containing `a/b.con`. Current case-collision authority rejects `A` beside `a/b.con` but does not settle the exact-case pair. | Add an exact proper-file-prefix exclusion to the single project checker: a new typed rule and proof family, uniformly rejects both cases. Admit exact prefixes syntactically: fewer rules but permits an unmaterializable source set and preserves the asymmetry. | Refuse exact proper-file-prefix conflicts in `SourcePath.admitProject` with both occurrences and a dedicated registered reason. This follows PHILOSOPHY rules 1 and 5; there is no second materialization admission boundary. Until answered, do not add the exclusion or claim the asymmetry resolved in Lean. |

Q3 is deferred to Phase F rather than left as a foundation blocker. Q4/Q5/Q7
are closed by the existing authority and engineering choices above. Representation,
byte-coordinate work and proof infrastructure can proceed now. Concrete local
admission/fixtures wait on Q1/Q2; final project admission and F19 wait additionally
on Q6. No stricter temporary checker, refusal-all implementation or default policy
stands in for an answer.

## Extraction repairs and landing closure

Retain complete Lean commands, doc comments, attributes, instances and private
support; discard historical approval annotations as qualification evidence. Build
a command-level manifest with retain/repair/replace/owner-held/retire dispositions
before writing each slice. Count changed Lean lines including controls and consumer
migrations; the estimates below are budgets, not measured implementation sizes.

Budgets are gross changed Lean lines (Spec, registry, Proofs and all consumer
migrations), not promises to omit closure. Measure the command manifest before
implementation. If a family exceeds its budget, request coordinator labels and
split only at independently qualified boundaries.

| Slice / authority dependencies | Exact targets and complete closure | Estimated changed Lean lines |
|---|---|---|
| F03p, after Q1/Q2; project-check portion also Q6 | `Spec/Ident.lean`: replace the held `SourcePath.DrivePrefix` through `Span.PathOk` group and held `mainSourcePath`, `tabSpan` and location examples. Define byte carrier, `RawOk`, parser/render, refusals, project predicate/checker and Span. `Spec/Rules.lean`: register every distinct check with its own accepted/refused conformance examples. `Proofs/Ident.lean`: include the minimal actual admission/refusal biconditionals, identity/bounds, prefix detection and reason witnesses needed to qualify those new rules, plus corresponding planted controls. | 1,800–2,600; plan two qualified families (local then project), roughly 900–1,300 each, with allocated labels before implementation |
| F04p, after F03p and existing F04; no further authority | Remaining `Proofs/Ident.lean` path family (snapshot commands in 13–279), Span projection (1300–1305), and complete Span controls (2341–2355). Reuse F03 `tabSource` and F04 `otherTabSource`; never extract snapshot line 2269 again. Prove `Span.pathOk` once for all spans from SourcePath; migrate projection proofs without duplicating F03p correctness. | 500–900 |
| F19 first arrival, after F04p and Q1/Q2/Q6; no Q3 dependency | `Spec/Observe.lean`: replace `ProjectSources`, `SourceLoadRefusal`, `SourceLoadResult`, `LoadedProject.parseFiles/load`, `SourceLoadRefusal.Names`, `ProjectSources.Realizes`, `SiteTable.Valid`; use sole project checker and list diagnostics with blocking. Add independent full-set prefix case safety (and Q6 outcome), exact identity/source preservation and site uniqueness/coordinates to Realizes. Include complete loader soundness/completeness and refusal witness evidence in `Proofs/Observe.lean` at first arrival, registry examples in Spec/Rules, and trust-safe execution adapter/terminal closure described below. | 1,500–2,300 path-related lines beyond base F19; split unrelated observation families first, never land a weak loader |
| F20, after strengthened F19 | `Proofs/Observe2.lean`: migrate every positional path/refusal fixture in complete commands intersecting 847–1072, including 1003–1011 in full. Import first-arrival loader Laws; remaining observation proofs use the strengthened relation. | 350–650 migration lines beyond base F20 |
| F20p, after F20 | Additional production-loader controls for nonadjacent prefix collisions, shared directories, case preservation, caps and new reasons. No production policy or correctness deferred here. | 350–600 |
| F22 and F23, after F19 and F04p respectively | F22 `Proofs/ObserveSource.lean` coordinate consumers use canonical byte views; F23 `Proofs/ObserveReport.lean` migrates all PathOk references (snapshot 249–799), structured path/refusal fixtures (complete commands intersecting 818–850), and report ordering/blocking Laws. | 150–300 F22; 250–450 F23 |
| F29 then F29p, after F20p and relevant report closure | `Proofs/Frame.lean`: command closure across 2334–2368 and 2429–2457. The empty-path invalidity helper at 2340–2341 belongs with F29's consuming examples 2354–2368, not stranded in F29p. F29p takes backslash/control helpers with their Instance/Planted consumers 2429–2457 and new cap/collision final-boundary controls. Prove actual boundary/Realizes equivalence, preserving satisfiable positive mixed-case controls. | 150–250 F29 migration; 450–750 F29p |

Sequence: byte/representation support may proceed immediately; answered Q1/Q2 →
qualified local F03p; Q6 → qualified project F03p → F04p → strengthened F19 →
F20 → F20p → F29/F29p. F22/F23 follow their consumers' prerequisites above.
F03p-design itself never lands. F20p adds controls, not an overdue contract.

### F19 local-identity closure (mandatory, unchanged gate)

`scripts/proofs/local_identity.lean:102–114,191–193,339–365` rejects any reachable
Spec declaration pairing `Var` with raw spelling carriers, including through
structure fields, bodies and inferred application types. A new wrapper alone is
not a scanner exemption. Use the already admitted nominal boundaries:
`ConcreteSpec.Name` for exact path/rendered identity text, and `ConcreteSpec.Numbers`
for program/source bytes (see `Spec/Basics.lean:24–45` and ISS-0074).
The validated byte carrier above stores Numbers, never an unannotated `List Nat`
or `List UInt8`. Make F03's `SourceText` representation use the Numbers boundary
without changing its byte facts; exact views preserve `lineAt`/`columnAt`.

F19 semantic `Terminal.trapped`, loaded sources, source-input payloads, and execution
adapter observations carry these types. A rendered trap line is nominal `Name`,
not String; scanner/path text views must not be unpacked inside declarations
whose inferred types include execution operands/programs/configurations containing
`Var`. Pure byte/identity rendering helpers take only source/site/trap data;
the execution adapter calls helpers returning nominal outputs. Raw host String/Char
conversion lives in presentation/diagnostic modules outside `ConcreteSpec.Spec`;
its exact byte correspondence is proved in Proofs and consumed at the presentation
boundary. Move no admission or execution semantics out of Spec merely to evade a gate.
Audit generated recursors, Repr/DecidableEq instances and helpers as well as public
signatures: an inferred raw carrier in a body also fails the scan.

F19 must pass unchanged `scripts/proofs/trust_ban.sh` on its entire import closure,
including execution/observation connections, with positive/negative identity controls.
No allowlist change, blanket wrapper exemption, or encoding through declaration
identity to conceal local spelling is permitted. These carriers contain source bytes
or path/diagnostic identities, never local variable spellings. Include all resulting
consumer migrations in F19's measured budget.

### Extraction manifest and module ownership

Line windows above locate groups, not textual cut points. Enumerate complete Lean
commands by declaration name with their attributes, doc comments and private
support before extraction; extend both ends through command completion. In particular
never stop Observe2 at 1005 or Frame at 2353. Record all retain/repair/replace/retire
and owner-held dispositions, and all transitive consumers, in that slice's manifest.
`otherTabSource` is already on main; its only disposition is reuse.

Spec owns readable definitions, rule doc comments, accepted/refused conformance
examples and per-check registration. Proofs owns Law theorems and planted mutants.
A slice spans both modules when necessary for qualification; this is not permission
to put Law or Planted families into Spec. Every distinct refusal, including cap,
collision and site checks, gets its own registered rule and reachable witnesses;
no composite registration substitutes for these. Coordinator allocates missing IDs.

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
  never an independent weak path-admission policy. Keep it as the compatibility view and prove it once from SourcePath in F04p;
  F19/F23 consume that theorem, never revalidate paths. A Span alone does not establish source membership.
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

This round changes only this writer brief. The requested validation is
`nix develop -c scripts/checks/run.sh`; its result is reported separately against
the current base plus this uncommitted document. It is not a result for a future
implementation or a committed candidate. No review tools, review notes, commits or pushes
are part of this writer round.
