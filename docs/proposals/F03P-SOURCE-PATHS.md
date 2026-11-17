# F03p: checked source paths and spans

Status: round-4 authoritative writer brief for F03/F19 Lean writers; not a landing artifact.
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
below refer to the pinned integration snapshot unless explicitly labeled current-main.

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
**decided refused** as component traversal. The filename whitelist below allows
internal repeated dots such as `a..b`.

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

Use exactly one nominal byte/source-text carrier: make F03 `SourceText` a
`structure` with a field declared syntactically as `ConcreteSpec.Numbers` and a
proof every element is below 256. Never use an `abbrev` or `def` alias for this
carrier, nor introduce a separate byte-sequence structure alongside it. Raw paths
and loaded source text use this same carrier; ASCII path admission is a predicate
over it, not another byte representation. Preserve F03 `SourceText` arithmetic
through proved exact projections; do not introduce a second line/column scanner. A thin String adapter encodes UTF-8 exactly and calls the
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
calling `parse`, and checking exact duplicates, case collisions and exact proper
file/directory prefix conflicts. It returns
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
the project root”). Future multi-package layout is Phase G / R-0383, not an F19
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
| Component whitelist and endpoint-dot exclusions | Same `parse` | Component projection and refusal Laws, with the first disallowed byte or offending endpoint; include acceptance of internal repeated dots |
| Windows reserved stems refused | Same `parse` | Refusal Laws with the matched component and stem; include a mixed-case reserved stem with an extension and a nearby accepted spelling |
| Uppercase A–Z allowed | Same `parse` | Positive mixed-case admission instances and completeness, so an all-lowercase-only mutant fails |
| Never rewrite | `parse` and exact `render` | `accepted_render`, `parse_render`, `render_injective`, `refusal_input`; project success preserves raw names, bytes and order; case folding cannot alter output |
| At most 100 bytes/component, 200 bytes/path | Same `parse`; one constant each | `component_bytes_le`, `path_bytes_le`, and completeness at equality; reason includes limit and offending range; whole length includes slash bytes |
| Exact duplicates refused | `admitProject` | Project admission biconditional, duplicate witness soundness and existential detection completeness, with both original input occurrences; reporting obeys selection/blocking below |
| Case-only collisions, including directory prefixes | Same `admitProject`, prefix relation above | Project admission includes prefix case safety; collision witness contains both paths, prefixes and folded equality/exact inequality |
| Exact proper file/directory prefix conflicts refused | Same `admitProject` | Project admission iff all local `RawOk`, no duplicate files, prefix case safety and no exact proper file prefix; dedicated registered rule with sound witnesses for both original occurrences and complete pair detection |
| First error per file | File-check result consumed by loader | At most one reported failure per file; every selected failure is real; no later file-local checking after its selected first failure; independent files remain checkable |
| Valid stored byte coordinates | `Span.LocationOk`, loader site binding | `Span.locationOk_iff`, loader admission iff strengthened `ProjectSources.Realizes`; exactly one source binding and correct start coordinates for each admitted site |

For caps, exercise 99/100/101-byte components and 199/200/201-byte paths where
all other predicates hold (construct multiple components for whole-path cases).
Prove the universal bounds, not just these examples. For collision completeness,
prove that every violating pair yields a candidate and that any invalid locally
admitted set yields at least one selected collision report. Do not require a report
for every pair: an occurrence already attributed a failure blocks further checks
involving it. Prove admission invariance under permutation of the path set;
occurrence ordinals may affect attribution for equal identities, never admission. Diagnostic ordering follows the decided file/span/rule ordering; admission
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

## Diagnostic contract and decided filename rules

Apply the diagnostic decision already approved at [decisions.md](../decisions.md),
lines 892–904: “option (a) on every question”. The approved
[diagnostic contract](../archive/crucial-2026-11-06/DIAGNOSTICS.md), lines 15–19,
requires `refused (rs : NonEmptyList Refusal)`; lines 35–38 require primary and
related spans; lines 61–63 say: “A judgment that needs a refused result is reported
as blocked by r inside the result, never as another refusal. Order is by file,
span, then rule ID.” Apply also decisions.md:1650: “Each file is checked separately,
and checking a file stops at its first error.” None of these is an owner question.

Use the following deterministic stages; detecting witnesses is not reporting failures.
Input file occurrences have stable input ordinals, even when their bytes are equal.
An occurrence key is `(exact raw path bytes, input ordinal)`, compared
lexicographically (bytes numerically, a proper prefix first; ordinals numerically).
A file here means an input file occurrence, not an equivalence class of spellings.

1. Check local paths independently. Select one local failure per occurrence by the
   offset/priority convention below and stop that occurrence's dependent checks.
2. Among locally admitted occurrences, enumerate unordered distinct occurrence
   pairs. Exact equal full paths yield a duplicate candidate. For each pair with a
   case collision, retain only its shortest colliding component-prefix length;
   this uniquely fixes both prefix ranges even when several prefixes collide.
   A pair in which one full path is an exact proper component prefix of the other
   yields a file/directory conflict candidate. These are separate registered predicates/reasons.
3. Orient each candidate by occurrence key (earlier primary, later related). Sort
   candidates by `(primary occurrence key, primary offset, primary range end,
   related occurrence key, related offset, related range end, rule key)`.
   Greedily select a candidate only if neither occurrence already participates in
   a selected collision report. Attribute that one failure to **both** occurrences;
   a related span is part of that report, not a second reported failure. Mark both
   blocked for further collisions and dependent site checks. Unselected candidates
   touching a selected occurrence are blocked evidence linked to the first selected
   report touching either endpoint in this traversal, never additional refusals.
4. Apply the following site checks. Pre-admission site entries bind to file-entry
   ordinals and retain raw path bytes. A binding naming no input occurrence is a
   project-level failure and its entry is excluded from file-bound checks. Report
   unmapped trapping sites as project-level failures. The authoritative inventory
   is the execution input's trap-site inventory, derived from the executable
   program's trap-capable places independently of the raw site table. Enumerate
   distinct `SiteId`s in ascending numeric order and use their zero-based
   positions as trapping-site input ordinals. The loader checks each inventory
   ID for absence against the raw site table. If raw bindings exist but are
   blocked, link the mapping check to the first blocking report for any of
   those bindings, by the selection order below.

   - Check local span coordinates independently on each file still eligible after
     Stage 3. For each file with a local site failure, select the first by the final
     report key below. Block all its subsequent
     checks, including duplicate-site pairing, by that report. Sites bound to files
     refused earlier remain blocked by their path report.
   - Among entries on the remaining files, enumerate every unordered pair of distinct
     entries sharing a `SiteId`, including pairs on the same file. Order entries by
     `(file occurrence key, span start, span end, raw site-entry ordinal)` and orient
     each pair with the earlier entry primary and the later entry related. Traverse
     candidates lexicographically by primary entry key then related entry key.
     Select a candidate only when neither endpoint file participates in a prior
     selected duplicate-site report. Attribute the single report to both endpoint
     files and block them for later candidates and dependent checks. A same-file
     pair carries both spans and counts once for its file. Link each suppressed
     candidate to the first selected duplicate-site report touching either endpoint.
   - For a raw duplicate pair excluded before enumeration, link it to the first
     report blocking either endpoint. For all Stage 4 blocking links, take the
     first applicable report in selection order: Stage 1 local path reports in
     final report-key order, Stage 3 reports in traversal order, missing-file
     reports in final report-key order, local coordinate reports in final
     report-key order, then duplicate-site reports in traversal order.

No executable partial project is returned. Prove pair enumeration complete, selected
witnesses sound, and the greedy traversal maximal: every unselected candidate touches
an occurrence blocked by a selected report. Thus any nonempty candidate set selects
at least one failure, while each file occurs in at most one reported failure across
all stages (including primary and related occurrences). Independent valid files
continue through sites. Local refusal completeness and project admission iff the
independent validity relation remain universal; diagnostic completeness requires
coverage by a report or its explicit blocking dependency, not a report per pair.
For `a.con`, `a.con`, `A.con`, only one collision report can be selected, with the
remaining pair checks blocked; invalidity is still detected. Include this control
and a disjoint-pairs control showing independent failures are both reported.
For site completeness, each duplicate pair over the raw table either has an
explicit blocking link to a missing-file, path or local site failure, or enters the
complete Stage 4 pair enumeration. Maximal selection then supplies a duplicate
report or an explicit blocking report for each enumerated pair. Require controls
for three distinct files sharing a site ID (one report), a same-file duplicate
(one report with two spans), and a cross-file duplicate with a local coordinate
failure on one endpoint (the local failure blocks the pair).

Sort final reports by `(exact primary raw file bytes, primary span start, primary
span end, rule key, primary ordinal, related occurrence key if present)`. For
path refusals use `(primary witness offset, primary context range end)` as the
two span coordinates, including when the offset is an EOF boundary. For site
refusals use the primary source span start and end. Compare both as numeric byte
coordinates when raw paths tie. Break remaining site report ties by primary
raw site-entry ordinal and then related raw site-entry ordinal when present.
Never sort by folded identity. The exact RuleId key is `(declaration-name components,
revision)`: compare root-to-leaf `Lean.Name` components lexicographically, string
components by UTF-8 bytes and numeric components numerically, with string before
numeric and a proper component-list prefix first; revision is numerical. This is
structural name comparison, not pretty-printed names or registry insertion order.
Use this same key wherever a rule tie-break is required. Project-level failures
sort after file reports by input ordinal then rule key. Use the raw site-entry
ordinal for a missing-file binding and the trapping-site input ordinal for an
unmapped site.

Offsets are zero-based byte boundaries, with EOF equal to byte length, as in
`Spec/Surface0.lean`'s `ScalarFailure`, `FirstError`, and `SourceFailure`.
Carry a half-open offending component range as context as well as the offset:

| Check | Selected offset |
|---|---|
| Empty whole path | 0 |
| Initial slash / ASCII-letter-plus-colon drive introducer | 0 / 1 (colon) |
| Empty component | Separator ending the empty component; EOF for trailing empty |
| Single dot / parent traversal / reserved name | Component start |
| Backslash / non-ASCII / space / control / disallowed printable component byte | First offending byte |
| Leading / trailing dot | First / last component byte |
| Component / whole cap | First excess byte in whole-path coordinates: `componentStart + 100` / `200` respectively |
| Duplicate / case collision | Both full-path starts (0), full-path ranges / both first case-differing bytes in the selected shortest prefix, with both prefix ranges |
| Exact proper file/directory prefix conflict | Shorter file EOF (`shorter.length`), a boundary with context range `[0, shorter.length)`; longer file separating slash at `shorter.length`, with context range `[0, shorter.length + 1)` |

For `dir/` followed by a 101-byte component, the component witness is 104, not
100. For component lengths 60, 60, 101, the third component starts at 122:
whole-cap offset 200 precedes component-cap offset 222. Require both controls,
and prove a component-cap witness identifies the actual first excess byte, not
merely an in-bounds location. Exact proper file/directory prefix witnesses prove the shorter path is
an exact component prefix of the longer, both context ranges are in bounds, and
the longer witness is the actual separating slash.

Classify `.` and `..` before generic endpoint-dot rules; define the generic
endpoint-dot refusal premises to exclude these exact components. This keeps each
reason reachable. Otherwise select the earliest witness offset, breaking ties by
a documented fixed per-check order (empty, absolute, drive, traversal, single-dot,
empty-component, non-ASCII, backslash, control, space, leading-dot, trailing-dot,
reserved, component-cap, whole-cap, then disallowed printable component byte).
The registered rule identity is stable; priority is separate from its spelling.
Prove reason selection agrees with this order, not merely that some failure exists.

Drive syntax is a whole-path initial ASCII letter followed by colon. In
`dir/a:b` and `dir/ab:c` the colon is refused by the component whitelist.
`RawOk` includes the whole-path drive exclusion, so `c:` has a dedicated
drive refusal under the priority above.

On 2026-11-17 the owner answered “ok go ahead” to the coordinator's recommendation
“1a 2a 3a”. The decision is recorded on
`wip/record-2026-11-17` at `docs/decisions.md#decided-2026-11-17-file-names`.

- Each component admits only `A-Z a-z 0-9 _ - .`, subject to the decided
  endpoint-dot exclusions. Internal repeated dots are allowed. The whitelist
  refusal covers printable component bytes outside this set that have no
  dedicated character refusal.
- Match the stem before the first dot (the entire component when it has no dot)
  against the closed list `CON PRN AUX NUL COM1–COM9 LPT1–LPT9`, ignoring ASCII
  case. This refuses `con.con` and directory components with a reserved stem.
  `main.con` and `COM0.con` pass this check. The same ASCII fold serves the
  stem and project case comparisons.
- `SourcePath.admitProject` refuses an exact proper file/directory prefix conflict
  such as `a` beside `a/b.con`, with both input occurrences and a dedicated
  registered rule.

Q3 remains deferred to Phase G. Q4/Q5/Q7 are closed by the existing authority and
engineering choices above.

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
| F03b (proposed coordinator label), immediate; no open policy dependency | Atomic F03/F04 nominal `SourceText` migration: current-main line windows: `Spec/Ident.lean` 702–712 (carrier/arithmetic), 892–902 (`tabSource` and byte-offset examples); `Proofs/Ident.lean` 1018–1062 (six line/column Laws), 2019–2095 (`aSource` through `scalarColumn`, fixtures/mutants); `scripts/proofs/axioms.golden` 2696–2703 and all transitive consumers. Preserve exact byte facts and reuse `otherTabSource`. Enumerate complete commands and regenerate the golden through its owning tooling. Run unchanged local-identity scan now and again on strengthened F19's entire closure. | 350–650 Lean lines, plus generated golden changes; one atomic qualified slice |
| F03p, after F03b | `Spec/Ident.lean`: replace the held `SourcePath.DrivePrefix` through `Span.PathOk` group and held `mainSourcePath`, `tabSpan` and location examples. Reuse F03b SourceText; define `RawOk`, parser/render, refusals, project predicate/checker and Span. `Spec/Rules.lean`: register every distinct check with its own accepted/refused conformance examples. `Proofs/Ident.lean`: include the minimal actual admission/refusal biconditionals, identity/bounds, prefix detection and reason witnesses needed to qualify those new rules, plus corresponding planted controls. | 1,800–2,600; plan two qualified families (local then project), roughly 900–1,300 each, with allocated labels before implementation |
| F04p, after F03p and existing F04; no further authority | Remaining `Proofs/Ident.lean` path family (snapshot commands in 13–279), Span projection (1300–1305), and complete Span controls (2341–2355). Reuse F03 `tabSource` and F04 `otherTabSource`; never extract snapshot line 2269 again. Prove `Span.pathOk` once for all spans from SourcePath; migrate projection proofs without duplicating F03p correctness. | 500–900 |
| F19 first arrival, after F04p; no Q3 dependency | `Spec/Observe.lean`: replace `ProjectSources`, `SourceLoadRefusal`, `SourceLoadResult`, `LoadedProject.parseFiles/load`, `SourceLoadRefusal.Names`, `ProjectSources.Realizes`, `SiteTable.Valid`; use sole project checker and list diagnostics with blocking. Add independent full-set prefix case safety and exact proper file/directory prefix exclusion, exact identity/source preservation and site uniqueness/coordinates to Realizes. Include complete loader soundness/completeness and refusal witness evidence in dedicated `Proofs/SourceLoad.lean` at first arrival (imported by Observe/Observe2 consumers; leave Slice-1 `checked_observation` in `Proofs/Observe.lean:10–77`), registry examples in Spec/Rules, and trust-safe execution adapter/terminal closure described below. | 1,500–2,300 path-related lines beyond base F19; split unrelated observation families first, never land a weak loader |
| F20, after strengthened F19 | `Proofs/Observe2.lean`: migrate every positional path/refusal fixture in complete commands intersecting 847–1072, including 1003–1011 in full. Import first-arrival loader Laws; remaining observation proofs use the strengthened relation. | 350–650 migration lines beyond base F20 |
| F20p, after F20 | Additional production-loader controls for nonadjacent prefix collisions, shared directories, case preservation, caps and new reasons. No production policy or correctness deferred here. | 350–600 |
| F22 and F23, both after strengthened F19 and F20 (and transitively F04p) | F22 `Proofs/ObserveSource.lean` coordinate consumers use canonical byte views; F23 `Proofs/ObserveReport.lean` migrates all PathOk references (snapshot 249–799), structured path/refusal fixtures (complete commands intersecting 818–850), and report ordering/blocking Laws. | 150–300 F22; 250–450 F23 |
| F29 then F29p, after F20p and relevant report closure | `Proofs/Frame.lean`: command closure across 2334–2368 and 2429–2457. The empty-path invalidity helper at 2340–2341 belongs with F29's consuming examples 2354–2368, not stranded in F29p. F29p takes backslash/control helpers with their Instance/Planted consumers 2429–2457 and new cap/collision final-boundary controls. Prove actual boundary/Realizes equivalence, preserving satisfiable positive mixed-case controls. | 150–250 F29 migration; 450–750 F29p |

Sequence: atomic F03b byte/representation migration →
qualified local F03p → qualified project F03p → F04p → strengthened F19 →
F20 → F20p → F29/F29p. Both F22/F23 follow strengthened F19 and F20;
F29/F29p also wait on the report closure they consume.
F03p-design itself never lands. F20p adds controls, not an overdue contract.

### F19 local-identity closure (mandatory, unchanged gate)

`scripts/proofs/local_identity.lean:102–114,191–193,339–365` rejects any reachable
Spec declaration pairing `Var` with raw spelling carriers, including through
structure fields, bodies and inferred application types. A new wrapper alone is
not a scanner exemption. Use the already admitted nominal boundaries:
`ConcreteSpec.Name` for exact path/rendered identity text, and `ConcreteSpec.Numbers`
for program/source bytes (see `Spec/Basics.lean:24–45` and ISS-0074).
The single `SourceText` structure stores a field whose declared type is literally
`ConcreteSpec.Numbers`, never an unannotated `List Nat` or `List UInt8`. F03b owns
this atomic migration, including the F04 Laws/fixtures and axiom golden listed
above. A reducible alias fails: `normalType` at all transparency unfolds it through
Numbers to `List Nat`; a nominal structure stops `whnf`, and constructor inspection
sees the syntactically declared Numbers field (`local_identity.lean:6–7,102–114,
154–168`). Exact projections preserve `lineAt`/`columnAt` without another scanner.
F19 qualification must demonstrate this over the full elaborated execution closure,
not infer a pass from the earlier Ident-only import graph.

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
no composite registration substitutes for these.

Registration must satisfy the existing gates, not merely list refusal constructors:

- `scripts/spec/rules.lean:67–72` and `Spec/Rules.lean` require each registered
  declaration to be a unary `Input → Prop` judgment. Package component position,
  pair occurrences or site/source context in a nominal input record as needed;
  curried predicates, Boolean validators and refusal constructors are not entries.
  Assemble `RawOk` and project validity from these same per-check judgments.
- Link separate trusted, elaborated `RuleAccepted judgment input` and
  `RuleRefused judgment input` theorems on closed constructor/literal data;
  the exporter must recognize the exact linked declaration. Inputs must satisfy
  its inspectable-data test, not contain runtime type/policy fields or free variables.
- A RuleId is the declaration name plus its revision (starting at 1), never an
  allocated code. Revision changes follow the existing semantic revision contract.
- `scripts/spec/rules.py:6–7,49,58–61` requires each rule doc comment to cite a
  governing declared decision row (`Q` with dotted digits, `OD`, `P`, or `R-NNNN`)
  AND this brief requires the precise decision anchor. The row must actually match
  a standalone `- **ID**` or `| ID |` declaration. A local question label such as
  Q1/Q2/Q6, an anchor alone, ISS-0068, or the HTML-prefixed R-0615 row is insufficient.

Use declared R-0496 plus `decided-2026-11-14-trap-sites-and-law-tests` for the
already governed basic loader/path and site-location rules; R-0612 governs C6
observation rules and R-0592 plus `decided-project-diagnostics-2026-11-06` governs
diagnostic composition. Do not cite unrelated OD142 merely to pass the row gate.
ISS-XXXX (coordinator): the November 16 extended path-character, case-prefix and
100/200-byte-cap decision at `decided-2026-11-16-file-paths` needs a declared
governing row, or an explicit update of the existing R-0496 row to govern those
extensions. Its current text leaves path-character policy open. The whitelist,
reserved-stem matcher and exact proper file/directory prefix exclusion likewise
each need a declared governing row (or explicit coverage in that updated row)
citing `decided-2026-11-17-file-names`. Do not manufacture row IDs, hand-edit the inventories in this writer
round, use a merely syntactically accepted unrelated citation, or register those
checks before both their authority and documentation prerequisites exist. Record
this coordination dependency in F03p's manifest; diagnostic and carrier engineering
work need not wait for it.

Specific departures from the old snapshot:

- `DrivePrefix` must use an explicit ASCII-letter predicate, not an undocumented
  reliance on `Char.isAlpha`. Extend local admission with ASCII, space, component
  whitelist, endpoint-dot, reserved-stem and byte-cap evidence.
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
  `Realizes` independently requires exactly one valid source location for every
  ID in the same execution input inventory. In first-arrival F19, prove against
  the execution rules that every place that can trap emits an ID in that
  inventory, and require that proof at the execution boundary for the program
  whose inventory the loader checks. Compose it with loader soundness to prove
  unique valid source coverage for every trapping place, as required by the
  [November 14 trap-site decision](../decisions.md#decided-2026-11-14-trap-sites-and-law-tests).
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
